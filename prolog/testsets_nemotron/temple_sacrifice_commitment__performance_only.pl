% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Sacrifice Commitment as Performance-Only Obligation
 *   domain: religious_law/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The performance_only reading of the temple_sacrifice_commitment kernel
 *   holds that sacrificial law requires material instantiation — actual
 *   animals, actual altar, actual priestly service — and that study of these
 *   laws without performance constitutes archival preservation of a defunct
 *   practice, not occupation of the divine commitment. This reading emerged
 *   historically after the Second Temple's destruction (70 CE) when material
 *   performance became structurally impossible, and it has hardened into the
 *   dominant Orthodox position: the obligation is real but dormant, awaiting
 *   messianic restoration of material conditions. The constraint extracts
 *   almost nothing currently (epsilon ~0.02) because there is no one to
 *   extract from — the obligation has no active subjects. But the
 *   theater_ratio has risen from ~0.10 (when performance was live) to 0.85
 *   (now), as the vast apparatus of study, simulation, and theoretical
 *   precision maintains the *form* of the commitment while its *substance* is
 *   absent. This is a piton: a former Mountain (when the Temple stood) whose
 *   primary function has atrophied, but the constraint remains due to
 *   institutional inertia and theological maintenance. The commitment is a
 *   dormant husk; the study is a low-epsilon rope coordinating potential
 *   future performance.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda_setter (institutional/biographical/constrained/national) — define the terms of the commitment's dormancy and the boundaries of legitimate study
 *   - kollel_scholars: beneficiary (organized/biographical/constrained/global) — receive status, stipends, and communal authority from maintaining the sacrificial corpus
 *   - temple_mount_activists: excluded (moderate/biographical/trapped/local) — push for actual performance now; structurally excluded from the performance_only framework which treats immediate performance as messianic presumption
 *   - animal_welfare_advocates: excluded (organized/biographical/trapped/global) — would be victims if restoration occurred without ethical evolution; absent from current discourse
 *   - secular_israeli_public: payer (powerless/biographical/constrained/national) — bears opportunity costs of Temple Mount status quo and potential future restoration costs
 *   - analytical_observer: observer (analytical/civilizational/analytical/universal) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.02).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, piton).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Sacrifice Commitment as Performance-Only Obligation").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '00824147-03ec-4f5d-9be1-f5b248975b3a').
narrative_ontology:cs_kernel_codification('00824147-03ec-4f5d-9be1-f5b248975b3a', fixed_text).
narrative_ontology:cs_authority_grounding('00824147-03ec-4f5d-9be1-f5b248975b3a', lineage).
narrative_ontology:cs_interpretation_layer_present('00824147-03ec-4f5d-9be1-f5b248975b3a').
narrative_ontology:cs_reading_relation('00824147-03ec-4f5d-9be1-f5b248975b3a', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('00824147-03ec-4f5d-9be1-f5b248975b3a', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('00824147-03ec-4f5d-9be1-f5b248975b3a', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('00824147-03ec-4f5d-9be1-f5b248975b3a', foundational, material_instantiation_requirement).
narrative_ontology:cs_axiom_status(material_instantiation_requirement, holdable).
narrative_ontology:cs_axiom_grounding('00824147-03ec-4f5d-9be1-f5b248975b3a', material_instantiation_requirement, deontological).
narrative_ontology:cs_axiom('00824147-03ec-4f5d-9be1-f5b248975b3a', foundational, archival_vs_occupational_distinction).
narrative_ontology:cs_axiom_status(archival_vs_occupational_distinction, holdable).
narrative_ontology:cs_axiom_grounding('00824147-03ec-4f5d-9be1-f5b248975b3a', archival_vs_occupational_distinction, deontological).
narrative_ontology:cs_reference_frame('00824147-03ec-4f5d-9be1-f5b248975b3a', second_temple_sacrificial_order).
narrative_ontology:cs_drift_state('00824147-03ec-4f5d-9be1-f5b248975b3a', contemporary_post_1967, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('00824147-03ec-4f5d-9be1-f5b248975b3a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, kollel_scholars).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__performance_only, secular_israeli_public).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, material_instantiation_requirement).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, divine_command_immutability).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, archival_vs_occupational_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define the terms of the sacrificial obligation's dormancy: what counts as legitimate study, what restoration would require, and why immediate performance is forbidden. Their authority rests on controlling the interpretation of the suspended commitment. Exit is constrained — leaving the framework means leaving the rabbinic office and its communal authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, rabbinic_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Dedicate careers to kodashim study (sacrificial law). Receive stipends, communal status, and marriage-market advantage from this specialization. The study functions as a coordination rope: it maintains priestly identity boundaries, communal cohesion, and messianic orientation. Exit is constrained — the specialization is a career path with limited transferability.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, kollel_scholars, beneficiary,
    organized, biographical, constrained, global).

% Advocate for immediate resumption of sacrificial performance on the Temple Mount. They are structurally excluded from the performance_only framework, which defines their position as messianic presumption (a theological error). Their exclusion is maintained by rabbinic authority and state security policy. Exit is trapped — they cannot access the site, cannot perform, and cannot change the framework from within.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, temple_mount_activists, excluded,
    moderate, biographical, trapped, local).

% Would oppose any restoration of animal sacrifice under current ethical frameworks. They are absent from the halakhic discourse entirely — the performance_only reading does not engage animal ethics because the obligation is dormant. If restoration were attempted, they would become a victim set. Exit is trapped — they have no voice in the halakhic framework and no standing to influence restoration terms.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, animal_welfare_advocates, excluded,
    organized, biographical, trapped, global).

% Bears opportunity costs of the Temple Mount status quo (access restrictions, security expenditures, political conflict). Would bear costs of any restoration (animal supply chains, ritual infrastructure, social division). Not consulted on halakhic frameworks. Exit is constrained — they live with the geopolitical consequences but have no lever on the theological commitment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, secular_israeli_public, payer,
    powerless, biographical, constrained, national).

% Sees the full structure across all four readings of the kernel. Observes that performance_only has near-zero current extraction but high theater; study_as_exercise converts intellectual labor into performance; hybrid_preparatory treats study as investment; symbolic_transformation treats prayer as instantiation. The observer seat computes the per-seat divergences the engine measures.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains priestly identity boundaries, communal cohesion around a suspended divine command, and messianic orientation — the study of sacrificial law coordinates a community around a commitment that cannot currently be performed, preserving the categories and competencies for potential future restoration.
% TRANSFER_FUNCTION: Moves intellectual labor, communal status, and institutional authority from scholars and students to the rabbinic framework that administers the suspended commitment. No material resources transfer because the obligation is dormant.
% ABSENT_VOICES: Temple Mount activists (who would perform now), animal welfare advocates (who would oppose restoration), secular Israeli public (who bears geopolitical costs), and non-Orthodox Jewish movements (who read the commitment as symbolically transformed) are all structurally excluded from the performance_only framework. The framework defines their positions as either theologically illegitimate or irrelevant to the halakhic commitment.
% DISAPPEARANCE_RATIONALE: If the performance_only constraint vanished overnight, the Orthodox framework would lose its defining account of why sacrifice is suspended and why study is the only legitimate occupation of the commitment. The kodashim curriculum would lose its telos. Temple Mount activism would lose its primary theological opponent. The geopolitical status quo would lose its halakhic anchor. The world would rearrange — but toward what is contested: study_as_exercise, hybrid_preparatory, symbolic_transformation, or secular abandonment.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), the divine command for sacrificial service became materially impossible to perform. The community faced a choice: treat the command as abrogated, substitute a new performance (prayer), or preserve the obligation in dormancy awaiting restoration. The performance_only reading emerged as the dominant answer: the command remains binding but impossible; study preserves it without substituting for it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (material impossibility after 70 CE) is attested by Josephus, the Talmud itself, and early Christian sources — all outside the beneficiary set of the performance_only reading. But the *status* of that problem (whether it remains the live problem or has been superseded by ethical evolution, political change, or authorized transformation) is contested: rabbinic authorities attest it is still live; Temple Mount activists attest it is solvable now; animal welfare advocates attest it should be dead; symbolic_transformation proponents attest it was authoritatively resolved by the sages who instituted prayer as substitute.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is near-zero because the constraint currently has no active subjects to extract from — the obligation is structurally impossible to perform. Suppression is near-zero because no enforcement is needed; the impossibility is material, not coercive. Theater_ratio is 0.85 because the entire edifice of kodashim study, mishnah/talmud precision, and restoration discourse performs the *appearance* of an active commitment while the substantive obligation is absent. Accessibility_collapse is 0.95 because alternatives (symbolic transformation, study-as-performance) are structurally excluded by the performance_only axiom — they are not just disfavored, they are defined out of the commitment. Resistance is 0.03 because the constraint meets almost no active resistance; even critics mostly accept the dormancy framing. The claimed_type is piton: a former Mountain (when the Temple stood, sacrifice was a genuine coordination mechanism with material instantiation) whose function has atrophied but whose form persists theatrically. The divergence between claim (piton) and metrics (near-zero extraction, high theater) is the measurement: a constraint that presents as a dormant Mountain but operates as a theatrical maintenance of institutional authority over a suspended practice.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic_authorities seat, the constraint is a genuine Mountain preserved in dormancy — the obligation is real, the conditions are absent, the study maintains fidelity. From the kollel_scholars seat, it is a low-epsilon rope: study coordinates communal boundaries and professional identity with minimal extraction. From the temple_mount_activists seat, it is a snare: the performance_only framing suppresses legitimate restoration pressure by defining immediate performance as heretical. From the animal_welfare_advocates seat (if consulted), it would be a latent snare: the dormancy masks a future extraction that would violate contemporary ethical frameworks. The engine computes these per-seat classifications from the structural data; the divergence is the signal.
 *
 * DIRECTIONALITY LOGIC:
 *   No current beneficiaries or victims exist because the constraint has no active operation — the obligation is structurally impossible. The rabbinic_authorities are agenda_setters who administer the dormancy; they benefit indirectly (institutional authority over the suspended corpus) but do not collect rents from the constraint itself. The kollel_scholars are beneficiaries of the study-rope (status, livelihood) but not of the performance_only constraint per se. The temple_mount_activists are excluded from the performance_only framework; their exclusion is structural (the framework defines their position as illegitimate). The animal_welfare_advocates are excluded but would become victims if restoration were attempted under performance_only terms. The secular_israeli_public are payers of opportunity costs but not of the constraint directly. Directionality for all seats is near-symmetric (d ~ 0.5) because the constraint is effectively inert — extraction requires active operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (divine command for sacrificial service) has outlived its material conditions. The performance_only reading resolves the mandatrophy by converting the obligation into a theoretical preservation project — the commitment is 'occupied' through study, not abandoned. This prevents mislabeling the coordination function (study maintains communal boundaries, priestly identity, messianic orientation) as pure extraction. But it also prevents recognizing that the coordination function has become the *primary* function, with the original mandate reduced to its symbolic referent. The constraint is a piton because the theatrical maintenance (study, simulation, precision) serves the institutional structure that administers it, not the original divine command.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine Mountain of divine law, or a constructed reading of a contested kernel that benefits identifiable interpretive communities?',
    'Comparative analysis of sibling readings'' structural profiles; if study_as_exercise and hybrid_preparatory show substantially different extraction/suppression signatures on the same referent, the performance_only reading is a framing choice, not a natural-law detection.',
    'If the reading is a framing choice, the constraint is a false summit mountain candidate — the performance_only framing benefits institutional authorities who control restoration discourse by defining the legitimate terms of the commitment''s dormancy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether performance_only is a structural detection or a committer-frame framing of the temple_sacrifice_commitment kernel.').

omega_variable(
    future_victim_set_ambiguity,
    'If restoration were attempted under performance_only terms, who would bear the extraction and would the ethical evolution deficit create a victim set?',
    'Scenario modeling of restoration pathways: identify which populations would be subjected to sacrificial obligations, whether animal welfare frameworks would be overridden, and whether gender/caste exclusions in priestly service would be reinstated without modification.',
    'If restoration would create identifiable victims without ethical evolution, the current dormancy is a scaffold-like pause masking latent extraction — the constraint''s piton classification would be provisional pending restoration attempt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_victim_set_ambiguity, preference, 'Whether latent victimhood in a hypothetical restoration scenario affects current classification of the dormant constraint.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the near-zero suppression structural (no enforcement needed because material conditions are genuinely absent) or internalized (the community has absorbed the impossibility as normative)?',
    'Counterfactual: if material conditions for performance were restored (Temple Mount access, priestly lineage verification, animal supply chains), would the community mobilize performance immediately or resist? Resistance would indicate internalized suppression of the obligation itself.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the community carries the suppression of the obligation as a normative commitment to dormancy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the dormancy of sacrificial obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_po_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(tsc_po_tr_t0, observed).
narrative_ontology:measurement(tsc_po_tr_t500, temple_sacrifice_commitment__performance_only, theater_ratio, 500, 0.25).
narrative_ontology:measurement_basis(tsc_po_tr_t500, observed).
narrative_ontology:measurement(tsc_po_tr_t1000, temple_sacrifice_commitment__performance_only, theater_ratio, 1000, 0.45).
narrative_ontology:measurement_basis(tsc_po_tr_t1000, observed).
narrative_ontology:measurement(tsc_po_tr_t1500, temple_sacrifice_commitment__performance_only, theater_ratio, 1500, 0.65).
narrative_ontology:measurement_basis(tsc_po_tr_t1500, observed).
narrative_ontology:measurement(tsc_po_tr_t1948, temple_sacrifice_commitment__performance_only, theater_ratio, 1948, 0.8).
narrative_ontology:measurement_basis(tsc_po_tr_t1948, observed).
narrative_ontology:measurement(tsc_po_tr_t1967, temple_sacrifice_commitment__performance_only, theater_ratio, 1967, 0.82).
narrative_ontology:measurement_basis(tsc_po_tr_t1967, observed).
narrative_ontology:measurement(tsc_po_tr_t2025, temple_sacrifice_commitment__performance_only, theater_ratio, 2025, 0.85).
narrative_ontology:measurement_basis(tsc_po_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(tsc_po_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.01).
narrative_ontology:measurement_basis(tsc_po_be_t0, observed).
narrative_ontology:measurement(tsc_po_be_t500, temple_sacrifice_commitment__performance_only, base_extractiveness, 500, 0.01).
narrative_ontology:measurement_basis(tsc_po_be_t500, observed).
narrative_ontology:measurement(tsc_po_be_t1000, temple_sacrifice_commitment__performance_only, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement_basis(tsc_po_be_t1000, observed).
narrative_ontology:measurement(tsc_po_be_t1500, temple_sacrifice_commitment__performance_only, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement_basis(tsc_po_be_t1500, observed).
narrative_ontology:measurement(tsc_po_be_t1948, temple_sacrifice_commitment__performance_only, base_extractiveness, 1948, 0.02).
narrative_ontology:measurement_basis(tsc_po_be_t1948, observed).
narrative_ontology:measurement(tsc_po_be_t1967, temple_sacrifice_commitment__performance_only, base_extractiveness, 1967, 0.02).
narrative_ontology:measurement_basis(tsc_po_be_t1967, observed).
narrative_ontology:measurement(tsc_po_be_t2025, temple_sacrifice_commitment__performance_only, base_extractiveness, 2025, 0.02).
narrative_ontology:measurement_basis(tsc_po_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsc_po_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(tsc_po_su_t0, observed).
narrative_ontology:measurement(tsc_po_su_t500, temple_sacrifice_commitment__performance_only, suppression_requirement, 500, 0.4).
narrative_ontology:measurement_basis(tsc_po_su_t500, observed).
narrative_ontology:measurement(tsc_po_suppression_1000, temple_sacrifice_commitment__performance_only, suppression_requirement, 1000, 0.2).
narrative_ontology:measurement_basis(tsc_po_suppression_1000, observed).
narrative_ontology:measurement(tsc_po_su_t1500, temple_sacrifice_commitment__performance_only, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement_basis(tsc_po_su_t1500, observed).
narrative_ontology:measurement(tsc_po_su_t1948, temple_sacrifice_commitment__performance_only, suppression_requirement, 1948, 0.05).
narrative_ontology:measurement_basis(tsc_po_su_t1948, observed).
narrative_ontology:measurement(tsc_po_su_t1967, temple_sacrifice_commitment__performance_only, suppression_requirement, 1967, 0.05).
narrative_ontology:measurement_basis(tsc_po_su_t1967, observed).
narrative_ontology:measurement(tsc_po_su_t2025, temple_sacrifice_commitment__performance_only, suppression_requirement, 2025, 0.05).
narrative_ontology:measurement_basis(tsc_po_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__performance_only, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, daily_prayer_substitution).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, priestly_lineage_preservation).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_mount_access_politics).

% DUAL FORMULATION NOTE:
% Part of the temple_sacrifice_commitment constraint family (4 readings). The performance_only reading has the lowest current extraction (epsilon ~0.02) but highest theater_ratio (0.85). The study_as_exercise reading would show higher extraction (intellectual labor as performance) but lower theater. The hybrid_preparatory reading would show moderate extraction (study as investment in future performance). The symbolic_transformation reading would show low extraction (prayer as authentic instantiation) and low theater (functional substitution). All four share the same referent (the divine command for sacrifice) but instantiate different constraints with different structural profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
