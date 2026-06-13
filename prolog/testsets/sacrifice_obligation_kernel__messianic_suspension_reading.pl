% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation Divinely Suspended Until Messianic Restoration
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   The Jewish obligation to perform Temple sacrifice became impossible after
 *   the Temple's destruction in 70 CE. The halakhic tradition generated four
 *   distinct readings of how the obligation persists: (1) the suspension
 *   reading — the obligation is divinely suspended until messianic
 *   restoration, study maintains readiness; (2) study_as_exercise reading —
 *   study itself constitutes genuine fulfillment; (3) performance_only
 *   reading — the obligation persists but cannot be fulfilled in the current
 *   era (an impossible obligation); and (4) symbolic_archive reading — the
 *   texts are a cultural archive, study preserves identity but makes no
 *   halakhic claim. This constraint story instantiates ONLY the suspension
 *   reading. It is a kernel reading grounded in canonical rabbinic sources
 *   (Talmudic passages on Temple destruction, Maimonidean codification of
 *   study obligation). The reading derives authority from the halakhic
 *   tradition's own reasoning, not from external philosophical argument.
 *
 * KEY AGENTS:
 *   - jewish_study_communities: maintenance agents, identity-locked to the obligation
 *   - rabbinic_authority_structure: preserves and transmits the corpus
 *   - future_generations: beneficiaries whose restoration capacity depends on current study
 *   - philosophical_objectors: excluded but contending voices
 *   - jewish_philosophical_interpreters: analytical observers tracking constraint evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation Divinely Suspended Until Messianic Restoration").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '5431ac88-5e76-4e43-81a9-5bafba0788cb').
narrative_ontology:cs_kernel_codification('5431ac88-5e76-4e43-81a9-5bafba0788cb', fixed_text).
narrative_ontology:cs_authority_grounding('5431ac88-5e76-4e43-81a9-5bafba0788cb', lineage).
narrative_ontology:cs_interpretation_layer_present('5431ac88-5e76-4e43-81a9-5bafba0788cb').
narrative_ontology:cs_reading_relation('5431ac88-5e76-4e43-81a9-5bafba0788cb', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('5431ac88-5e76-4e43-81a9-5bafba0788cb', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('5431ac88-5e76-4e43-81a9-5bafba0788cb', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('5431ac88-5e76-4e43-81a9-5bafba0788cb', foundational, divine_suspension_is_coherent).
narrative_ontology:cs_axiom_status(divine_suspension_is_coherent, holdable).
narrative_ontology:cs_axiom_grounding('5431ac88-5e76-4e43-81a9-5bafba0788cb', divine_suspension_is_coherent, theological).
narrative_ontology:cs_axiom('5431ac88-5e76-4e43-81a9-5bafba0788cb', foundational, study_maintains_operational_readiness).
narrative_ontology:cs_axiom_status(study_maintains_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('5431ac88-5e76-4e43-81a9-5bafba0788cb', study_maintains_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('5431ac88-5e76-4e43-81a9-5bafba0788cb', temple_service_obligation_active).
narrative_ontology:cs_drift_state('5431ac88-5e76-4e43-81a9-5bafba0788cb', contemporary_post_destruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5431ac88-5e76-4e43-81a9-5bafba0788cb', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, jewish_communities_in_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_structure).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_restoration_axiom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the obligation through systematic study of sacrifice law and Temple practice, preserving operational knowledge for the day of restoration. They interpret the study as fulfilling the requirement to 'occupy' the obligation during the suspension period, not as a substitute for performance. The framework is that study is instrumental maintenance work — keeping the knowledge alive so restoration becomes possible.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, jewish_study_communities, agenda_setter,
    organized, generational, identity_locked, global).

% Preserves and transmits the halakhic corpus that encodes sacrifice law. Benefits from the reading because it licenses continued investment in textual study and interpretation — the reading sustains the framework that halakhic study is a form of obligation-fulfilment, not merely historical scholarship. Authority sits 'above' the enforcement question: the suspension is divinely grounded, not rabbinically administered.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_structure, beneficiary,
    institutional, civilizational, analytical, global).

% Will inherit both the knowledge of how to perform sacrifice and the halakhic readiness to perform it when — if — conditions change. The reading positions study-and-preservation as service to futurity: each generation occupies the obligation by passing it on intact, operational, and interpretively sophisticated. They cannot negotiate or exit; their benefit is conditional.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, global).

% Hold that study cannot fulfill a performative obligation; that the suspension reading is a cover story for functional obsolescence; or that the messianic restoration is mythological rather than halakhically binding. They are excluded from the deliberative structure that validates the suspension reading because the reading derives its authority from the halakhic tradition itself, not from philosophical argument. Their objections are heard in academic and interfaith contexts but do not enter the framework that governs the study obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, philosophical_objectors, excluded,
    moderate, biographical, constrained, regional).

% Analyze the suspension reading as a case study in obligation-transformation under structural impossibility. They examine how the constraint evolves across the four sibling readings and how Jewish law manages a performative obligation when the physical conditions for performance do not exist. They occupy no seat with direct stake in the obligation's halakhic status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, jewish_philosophical_interpreters, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the knowledge structure, interpretive sophistication, and operational readiness required to perform the Temple sacrifice obligation, ensuring that if the messianic restoration occurs, the Jewish people possess the intellectual and halakhic capacity to execute the obligation. Study serves as systematic knowledge-maintenance rather than a substitute for performance.
% TRANSFER_FUNCTION: Each generation receives an intact, studied, elaborated corpus of sacrifice law from the prior generation and passes it forward enhanced — the transfer is of transmitted knowledge and interpretive responsibility, not material goods or status. Study itself is the work of transmission.
% ABSENT_VOICES: Performance-focused interpreters who hold that study does not fulfill the obligation; secular Jewish intellectuals who read the corpus as historical archive rather than binding norm; non-Jewish philosophers of obligation who do not grant that divine suspension is a coherent category; and potentially future generations who will inherit the commitment without having consented to it.
% DISAPPEARANCE_RATIONALE: If this reading vanished and were replaced by the study_as_exercise_reading, the structural burden on communities would lighten: study would be understood as fulfilling the obligation in itself, not as preparatory work. If replaced by symbolic_archive_reading, study would be reframed as historical and cultural preservation rather than halakhic obligation. If replaced by performance_only_reading, the obligation would become impossible and potentially produce deep cognitive dissonance about unmet law. The disappearance of THIS reading would not eliminate the obligation itself but would alter its meaning and the justification for study — the world of obligation would reorganize around a different interpretation of the kernel.
% FOUNDING_PROBLEM: When the Temple was destroyed and sacrifice became impossible, the question became: does the obligation persist, and if so, how? The founding problem is the structural impossibility created by loss of the performative conditions while the obligation persists as divine law. The suspension reading solves this by positing that the obligation is held in abeyance — suspended, not violated or obsolete — and that study is the prescribed form of occupation during the suspension.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic sources from the Talmud onward attest the founding problem and this reading: Menachot 110a, Gittin 4b, and subsequent rabbinic literature codify study of sacrifice law as the prescribed response to Temple destruction. Maimonides (Hilchot Temidim u'Musafim 13:13) and the Shulchan Aruch (Orach Chaim 1:1) formalize the principle. The reading is not self-asserted by study beneficiaries but is grounded in canonical textual tradition that predates any modern interpreter. Contemporary Jewish legal scholarship (e.g., Cherlow, Levy) corroborates the halakhic status of the reading from outside the benefiting communities, though they remain within the halakhic tradition itself.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.15) because the reading posits suspension, not violation: the obligation is not being extracted from communities; it is being held in abeyance. No victim set exists in the current era under this reading — future generations are beneficiaries, not victims. Study is instrumental (maintaining capacity) rather than substitutive (replacing performance); this distinction is crucial and is reflected in the low theater_ratio (0.08): study is not theater-substitute but genuine preparatory work. Accessibility_collapse is high (0.92) because once the suspension reading is accepted, the alternative framings (study as exercise, performance only, symbolic archive) are structurally foreclosed for parties within the halakhic tradition — they have chosen to interpret the obligation within the framework the suspension reading establishes. Resistance is low (0.18) because the reading has 1900+ years of textual support and is not actively opposed within communities that accept its authority; objection comes from outside the framework, not from within.
 *
 * PERSPECTIVAL GAP:
 *   The study_communities and rabbinic_authority_structure occupy aligned seats: both benefit from the reading (study maintains their institutional role, textual authority). Future_generations occupy a different seat: they benefit but do not choose the commitment. Philosophical_objectors occupy an excluded seat: they would describe the constraint differently (as functional obsolescence disguised as divine suspension) but are not parties to the halakhic deliberation. The engine should compute identical or near-identical types for the communities and authority (the reading sustains their role as coordinators); different d-values for future_generations (they inherit the obligation without exit); and excluded-seat types for the objectors (their directionality analysis is external to the framework).
 *
 * DIRECTIONALITY LOGIC:
 *   The study_communities and rabbinic_authority occupy the beneficiary end of the directionality spectrum (d ~ 0.1–0.2): they benefit from the reading because it legitimizes sustained study as obligation-fulfillment, not as historical scholarship. Future_generations sit nearer the center (d ~ 0.45–0.55): they benefit from preserved knowledge but also inherit the obligation without negotiation. Their exit is identity_locked at the generational level — they cannot opt out of being Jewish, and the obligation rides on that identity. The philosophical_objectors would compute higher d (0.6–0.75) if they were seated: they bear the burden of being told their objections are not relevant to the halakhic frame. But they are excluded, not seated with power, so the engine's computation should reflect their external position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (impossibility of performance after Temple destruction) was live at the time of Temple destruction and remains live: performance is still impossible. The suspension reading resolves the mandatrophy by reframing the obligation as suspended, not violated, and by positioning study as the prescribed form of occupation. The reading does not deny the founding problem; it reinterprets what the obligation demands given the founding problem. This is mandatrophy resolution through reinterpretation, not through the founding problem becoming dead. The reading's claim that study maintains operational readiness is empirically contestable — it rests on the assumption that messianic restoration is a coherent future event and that study will enable performance when it comes. That is why the founding_problem_status is 'contested' and why an omega variable (below) addresses the coherence of the restoration frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_restoration_coherence,
    'Is messianic restoration a coherent future event within the halakhic framework, or is it a mythological/theological claim that does not bind obligation-structure?',
    'Rabbinic and Jewish philosophical consensus on what qualifies as a coherent halakhic ground for obligation suspension. Examine whether halakhic reasoning requires the future restoration to be empirically plausible, theologically binding, or merely a stated framework.',
    'If restoration is accepted as coherent within the halakhic tradition, the suspension reading stands as halakhically valid. If restoration is deemed mythological or non-binding, the reading collapses toward performance_only or study_as_exercise. If ambiguous, the reading coexists with alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_restoration_coherence, conceptual, 'Whether messianic restoration is a coherent halakhic ground for obligation suspension').

omega_variable(
    study_as_maintenance_vs_substitution,
    'Is study genuinely instrumental maintenance work (preparing the Jewish people to perform sacrifice if restoration occurs) or is it a substitutive practice that functions as a replacement for impossible performance?',
    'Examine the textual record: do halakhic sources frame study as preparatory/maintenance work or as an alternative fulfillment? Are there historical moments when the distinction broke down (e.g., when the restoration frame became attenuated or when study became the primary goal rather than maintenance)?',
    'If study is framed as genuine maintenance, the constraint remains a rope (coordination via obligation suspension and study structure). If study is framed as substitution, the reading collapses toward study_as_exercise. If the distinction eroded over time, the theater_ratio should rise to reflect the shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_maintenance_vs_substitution, empirical, 'Whether study maintains readiness or has become a substitute for performance').

omega_variable(
    suppression_mechanism_in_suspension,
    'What suppresses dissent from the suspension reading within Jewish communities? Is suppression structural (institutional authority enforces the reading) or internalized (communities have internalized the halakhic framework such that alternative readings are unthinkable)?',
    'Historical analysis of halakhic debate: how were the alternative readings engaged or foreclosed? Do contemporary non-Orthodox Jewish movements (which often reject or reinterpret the suspension reading) report structural barriers or identity-fusion barriers to alternatives?',
    'If suppression is structural, the constraint carries institutional enforcement burden. If suppression is internalized/identity-locked, the low suppression_requirement metric is accurate — alternatives are foreclosed by framework-choice, not by active coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_in_suspension, empirical, 'Whether suppression of alternatives is structural or internalized').

omega_variable(
    kernel_reading_authority_grounding,
    'What grounds the authority of the halakhic tradition to adjudicate among the four readings? Is the authority grounded in textual lineage (transmission from Sinai through rabbinic interpretation), in practice (lived halakhic observance), in expertise (interpretive skill), or in extraction (institutions benefit from sustained study)?',
    'Examine the cs_structure fields: the reading derives authority from lineage (Talmudic/Maimonidean codification) and practice (1900+ years of study obligation). Extraction is low (rabbinical authority does not accumulate wealth from the study obligation). No single grounding alone determines authority; the combination is what matters.',
    'If authority is grounded in genuine lineage and practice, the reading is coherent within the tradition. If extraction is high, the reading becomes suspect as institutional cover. The analysis confirms: authority is lineage + practice, extraction is minimal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authority_grounding, conceptual, 'What grounds the halakhic tradition''s authority to adjudicate the four readings').

omega_variable(
    sibling_reading_foreclosure_tests,
    'Does the suspension reading''s core premise (divine suspension is coherent and halakhically binding) logically foreclose the study_as_exercise reading, or can both coexist within a single framework?',
    'Test the logical consistency: if suspension is true, does study still constitute genuine exercise? The study_as_exercise reading claims study fulfills the obligation; the suspension reading claims study maintains readiness while the obligation is suspended. These can coexist if study is both exercise AND maintenance. If they are mutually exclusive, the relation is ''forecloses''; if they can be held simultaneously by different parties, it is ''coexists_with''.',
    'The analysis (below in cs_structure.reading_relations) concludes the relations are ''coexists_with'' for study_as_exercise and symbolic_archive, and ''forecloses'' for performance_only (suspension logically rules out ''impossible obligation''). This is the empirical finding about the kernel''s internal structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_tests, conceptual, 'The logical relationships among sibling readings of the sacrifice obligation kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 300, 0.06).
narrative_ontology:measurement_basis(sacr_tr_t300, observed).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 700, 0.07).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1200, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1200, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t1200, observed).
narrative_ontology:measurement(sacr_tr_t1600, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t1600, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 300, 0.13).
narrative_ontology:measurement_basis(sacr_be_t300, observed).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 700, 0.14).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1200, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1200, 0.15).
narrative_ontology:measurement_basis(sacr_be_t1200, observed).
narrative_ontology:measurement(sacr_be_t1600, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement_basis(sacr_be_t1600, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t300, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 300, 0.09).
narrative_ontology:measurement_basis(sacr_su_t300, observed).
narrative_ontology:measurement(sacr_su_t700, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 700, 0.1).
narrative_ontology:measurement_basis(sacr_su_t700, observed).
narrative_ontology:measurement(sacr_su_t1200, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1200, 0.11).
narrative_ontology:measurement_basis(sacr_su_t1200, observed).
narrative_ontology:measurement(sacr_su_t1600, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement_basis(sacr_su_t1600, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, information_standard).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel constraint family comprises four readings, each instantiating a different ε and claiming a different constraint type. The messianic_suspension_reading (this story) is the dominant reading in Orthodox Jewish law and claims low extractiveness and rope classification. The study_as_exercise_reading claims that study fulfills the obligation, creating different directional relationships for study communities. The performance_only_reading treats the obligation as impossible, creating a different moral framework. The symbolic_archive_reading reframes the texts as cultural rather than halakhic, divorcing the constraint from obligation entirely. Each reading is a separate constraint story with its own ε, beneficiary structure, and omegas documenting the kernel contest. The network links establish that these readings are sibling interpretations of a single contested kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
