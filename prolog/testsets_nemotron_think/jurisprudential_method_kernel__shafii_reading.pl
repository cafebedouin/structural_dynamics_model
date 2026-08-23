% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Al-Shafi'i's Four-Tier Hierarchy with Hadith as Arbiter
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   Al-Shafi'i's Risala (d. 204 AH / 820 CE) imposed a strict four-tier
 *   hierarchy on Islamic legal derivation: Qur'an, then authenticated Hadith,
 *   then Ijma (scholarly consensus), then Qiyas (analogical reasoning). This
 *   standardized the chaotic early pluralism of regional schools by making
 *   isnad-verified hadith transmission the arbiter of all legal validity. The
 *   constraint is a kernel reading: the Shafi'i school instantiates the
 *   jurisprudential_method_kernel by elevating hadith authentication to the
 *   primary gatekeeping function, creating a coordination mechanism (shared
 *   method) that simultaneously extracts epistemic rents for hadith scholars
 *   and marginalizes customary practice and analogical extension as
 *   independent sources.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.65).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.75).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Al-Shafi'i's Four-Tier Hierarchy with Hadith as Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, 'f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a').
narrative_ontology:cs_kernel_codification('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', formalized).
narrative_ontology:cs_authority_grounding('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', lineage).
narrative_ontology:cs_interpretation_layer_present('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a').
narrative_ontology:cs_reading_relation('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', foundational, hadith_authentication_as_arbiter).
narrative_ontology:cs_axiom_status(hadith_authentication_as_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', hadith_authentication_as_arbiter, conventional).
narrative_ontology:cs_axiom('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', foundational, four_tier_hierarchy_exhaustive).
narrative_ontology:cs_axiom_status(four_tier_hierarchy_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', four_tier_hierarchy_exhaustive, conventional).
narrative_ontology:cs_reference_frame('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', prophetic_legal_method).
narrative_ontology:cs_drift_state('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', classical_usul_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7ce82b6-9c75-447d-a2ca-aa7d0b286c2a', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafi_i_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_extension_practitioners).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, istihsan_practitioners).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, prophetic_sunna_as_recoverable_through_isnad).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, legal_sources_ranked_hierarchy).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, ijma_as_scholarly_consensus_not_communal_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control hadith authentication through isnad criticism (jarh wa ta'dil); their expertise becomes the gatekeeping mechanism for legal validity. The four-tier hierarchy makes authenticated hadith the primary filter for all legal derivation, concentrating epistemic authority and professional standing in the hands of isnad specialists across the Islamic world.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_scholars, agenda_setter).

% Administer the four-tier hierarchy as the methodological standard of the Shafi'i school; their professional identity is fused with the Risala's system. They transmit, teach, and adjudicate within the framework al-Shafi'i constructed, and the school's institutional continuity depends on the hierarchy's authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafi_i_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Medinan 'amal ahl al-Madina and other regional customary practices are demoted below authenticated hadith in the hierarchy. Their independent epistemic authority — rooted in communal transmission and living practice rather than isnad chains — is overridden by the isnad filter. They can operate only where hadith is silent, and even then their rulings are suspect without isnad backing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_adherents, payer,
    organized, biographical, constrained, regional).

% Extensive qiyas and istihsan (juristic preference) — the hallmarks of the Hanafi and early Iraqi schools — are restricted to the fourth tier and bounded by hadith. Their creative juridical reasoning loses independent legitimacy; analogy becomes a tightly constrained tool rather than a primary source. They pay in reduced methodological freedom and professional marginalization within the standardized framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_extension_practitioners, payer,
    organized, biographical, constrained, regional).

% Literalist hadith-only approach rejects the hierarchy itself: no qiyas, no ijma as independent sources, no ranked tiers beyond Qur'an and hadith. They are excluded from the standardized usul al-fiqh framework because their premise (only unanimous consensus is valid; analogy is bid'ah) contradicts the Shafi'i hierarchy's structure. They persist as a distinct school but cannot participate in the shared methodological conversation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hanbali_adherents, excluded,
    organized, biographical, trapped, regional).

% Modern legal historian or comparative jurist analyzing the methodological standardization from outside the tradition. Sees the full structure: how the isnad system became a toll gate, how regional diversity was compressed into a single hierarchy, and how the four schools negotiated the Shafi'i framework without dissolving their differences.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, observer_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, ranked hierarchy of legal sources (Qur'an > Hadith > Ijma > Qiyas) that resolves conflicts between competing derivations and enables systematic legal reasoning across the Islamic world. Replaces regional plurality with a portable, teachable method.
% TRANSFER_FUNCTION: Moves epistemic authority and professional standing from customary practice and free analogical reasoning to hadith authentication specialists. The isnad system becomes the toll gate for legal validity: a ruling's legitimacy depends on its hadith pedigree, and the scholars who control isnad criticism control the pipeline.
% ABSENT_VOICES: Pre-Shafi'i regional schools (Kufan, Basran, Medinan) whose living traditions were not captured in isnad-verified hadith; local customary practitioners whose knowledge was oral and communal rather than transmission-chain documented; early jurists who used istihsan and unrestricted qiyas as primary tools before the hierarchy fixed their rank.
% DISAPPEARANCE_RATIONALE: If the four-tier hierarchy and hadith-arbiter standard vanished, legal reasoning would fragment back into regional and customary methods. The unified usul al-fiqh framework would dissolve, the institutional position of hadith critics would collapse, and the madhhab system's shared methodological vocabulary would lose its coordinating center.
% FOUNDING_PROBLEM: Early Islamic legal reasoning was inconsistent across regions and schools: the same cases yielded different rulings because each school used its own mix of Qur'an, hadith, local practice, and reasoning with no agreed ranking or method for resolving conflicts. Al-Shafi'i's Risala was written to impose a single, ranked hierarchy that would make legal derivation systematic and portable.
% FOUNDING_PROBLEM_CORROBORATION: Al-Shafi'i's own Risala attests the problem; later usul scholars (al-Ghazali, al-Amidi) corroborate from within the Shafi'i tradition; modern historians (Schacht, Hallaq, Melchert) corroborate from outside — though they dispute whether the 'problem' was a defect requiring correction or a feature of early pluralism that the hierarchy suppressed.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is medium-high because the isnad system functions as a toll gate: legal validity requires hadith pedigree, and the scholars who control jarh wa ta'dil collect professional rents. Suppression (0.75) is high because the hierarchy actively excludes non-isnad sources — Medinan 'amal, unrestricted qiyas, istihsan — not by persuasion but by methodological fiat. Theater ratio (0.4) reflects that the coordination function (systematic method) is real but increasingly performative as taqlid (blind following) replaces living ijtihad. Accessibility collapse (0.8) is high: alternatives like istihsan and 'amal are methodologically closed off within the framework. Resistance (0.5) is moderate: other schools persist but operate within the Shafi'i vocabulary.
 *
 * PERSPECTIVAL GAP:
 *   From the hadith scholar's seat, the hierarchy is genuine coordination: it solves the chaos of competing sources by giving isnad verification a monopoly on epistemic warrant. From the customary practitioner's seat, the same structure is extraction: their living tradition is demoted to irrelevance by a documentary standard their communal knowledge cannot meet. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars are structural beneficiaries (d ~ 0.15): they control the authentication pipeline and collect its rents. Shafi'i jurists are agenda-setters with identity-locked exit (d ~ 0.3): they administer the system but cannot leave it without losing professional identity. Customary practice adherents and analogical extension practitioners are payers with constrained exit (d ~ 0.8): they bear the cost of demotion but can still operate in the interstices. Hanbali adherents are excluded (d ~ 0.9): their premise forecloses the hierarchy itself. The observer sits at analytical exit (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (regional inconsistency) was real, but the solution (hadith-arbiter hierarchy) created a new extractive class (hadith scholars) and froze methodological diversity. The mandate has not resolved: the coordination function persists (usul al-fiqh is still taught globally) but the extraction has accumulated (isnad criticism became a professional guild, taqlid replaced ijtihad). The constraint is a tangled rope, not a scaffold, because no sunset was declared and the extraction is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'How does this reading''s structural relationship to the jurisprudential_method_kernel differ from its siblings, and where exactly is the disagreement located?',
    'Comparative analysis of the four readings'' cs_structure blocks: map each reading''s axioms, reference_frame, and drift_state to identify the precise structural elements that diverge.',
    'If the disagreement is located in the hierarchy''s exhaustiveness (Shafi''i: four tiers fixed; Hanafi: open-ended reasoning; Hanbali: two tiers only), then the kernel is not a single constraint but a family of mutually foreclosing constraints. If the disagreement is in authority_grounding (lineage vs practice vs extraction), then the kernel''s codification is the contested element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Committer-frame structural delta: this reading instantiates the kernel with hadith-authentication-as-arbiter; siblings instantiate with different arbiters.').

omega_variable(
    suppression_mechanism_isnad_vs_internalized,
    'Is the measured suppression (0.75) structural (the isnad system''s documentary requirements) or internalized (jurists across schools internalize the hierarchy as the only legitimate method)?',
    'Post-formative-period trajectory: if suppression persists after the isnad system''s institutional monopoly weakens (e.g., in modern reform movements that still use Shafi''i vocabulary), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the hierarchy with them even when institutional enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_isnad_vs_internalized, empirical, 'Structural vs. internalized suppression in the isnad gatekeeping system.').

omega_variable(
    descriptive_vs_constructive_hierarchy,
    'Was the four-tier hierarchy descriptive (recovering the Prophet''s actual method) or constructive (creating a new method to solve pluralism)?',
    'Historical analysis of pre-Shafi''i legal practice: if regional schools already used a de facto hierarchy matching the Risala, the constraint is descriptive (lower ε); if the Risala invented the hierarchy, it is constructive (higher ε, stronger extraction).',
    'Descriptive framing reduces extractiveness (the method reflects reality); constructive framing increases it (the method creates a toll gate). This directly affects the ε referent for this kernel reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(descriptive_vs_constructive_hierarchy, conceptual, 'Whether the hierarchy discovers or imposes order on the sources.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 150, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_method_tr_t150, jurisprudential_method_kernel__shafii_reading, theater_ratio, 150, 0.15).
narrative_ontology:measurement(shafii_method_tr_t175, jurisprudential_method_kernel__shafii_reading, theater_ratio, 175, 0.2).
narrative_ontology:measurement(shafii_method_tr_t204, jurisprudential_method_kernel__shafii_reading, theater_ratio, 204, 0.28).
narrative_ontology:measurement(shafii_method_tr_t225, jurisprudential_method_kernel__shafii_reading, theater_ratio, 225, 0.35).
narrative_ontology:measurement(shafii_method_tr_t250, jurisprudential_method_kernel__shafii_reading, theater_ratio, 250, 0.4).

% Extraction over time
narrative_ontology:measurement(shafii_method_be_t150, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 150, 0.35).
narrative_ontology:measurement(shafii_method_be_t175, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 175, 0.45).
narrative_ontology:measurement(shafii_method_be_t204, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 204, 0.55).
narrative_ontology:measurement(shafii_method_be_t225, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 225, 0.62).
narrative_ontology:measurement(shafii_method_be_t250, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 250, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shafii_method_su_t150, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(shafii_method_su_t175, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 175, 0.6).
narrative_ontology:measurement(shafii_method_su_t204, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 204, 0.68).
narrative_ontology:measurement(shafii_method_su_t225, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 225, 0.72).
narrative_ontology:measurement(shafii_method_su_t250, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 250, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four readings (Shafi'i, Hanafi, Maliki, Hanbali) with different ε values and beneficiary/victim structures. The Shafi'i reading elevates hadith authentication as arbiter (ε=0.65); Hanafi elevates analogical reasoning (ε lower on hadith, higher on qiyas); Maliki elevates Medinan practice (ε on isnad); Hanbali rejects hierarchy beyond Qur'an/Hadith (ε on consensus). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, institutional, 0.15).
constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
