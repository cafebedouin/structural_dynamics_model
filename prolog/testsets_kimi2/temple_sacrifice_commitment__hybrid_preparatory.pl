% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment â Hybrid Preparatory Reading
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_preparatory reading of the
 *   temple_sacrifice_commitment kernel: the claim that intensive study of
 *   Temple sacrifice law maintains the commitment in a suspended
 *   stateâneither fully occupied through performance nor reduced to
 *   archival memoryâfunctioning as a preparatory exercise for messianic
 *   restoration. The reading occupies a contested middle ground within
 *   halakhic theology, extracting substantial cognitive and financial
 *   resources while providing a genuine coordination function (preserving
 *   juridical continuity across a 2,000-year rupture). The structural
 *   relationship is asymmetric: rabbinic academies and restorationist
 *   theologians benefit from the institutional and intellectual architecture,
 *   while students and donors bear the concentrated costs of a
 *   non-performable legal regime.
 *
 * KEY AGENTS:
 *   - advanced_talmudic_students (powerless/identity_locked): primary targets â bear the extraction of cognitive labor and life-course redirection
 *   - communal_donor_base (moderate/constrained): secondary targets â fund the institutional apparatus with constrained exit due to social-theological pressure
 *   - rabbinic_academy (institutional/constrained): agenda-setter â administers curriculum, enforces the norm of intensive study, captures resources and prestige
 *   - restorationist_theologians (organized/constrained): beneficiaries â derive scholarly legitimacy from the preparatory framing
 *   - practical_halachic_authorities (institutional/analytical): observers â see the resource drift but do not directly challenge it
 *   - excluded_secular_critics (moderate/mobile): excluded â would argue for resource redirection but are outside the halakhic conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.48).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.55).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.48).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment â Hybrid Preparatory Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'e56e2ce6-887e-4143-bc80-d8a7d956bd84').
narrative_ontology:cs_kernel_codification('e56e2ce6-887e-4143-bc80-d8a7d956bd84', fixed_text).
narrative_ontology:cs_authority_grounding('e56e2ce6-887e-4143-bc80-d8a7d956bd84', lineage).
narrative_ontology:cs_interpretation_layer_present('e56e2ce6-887e-4143-bc80-d8a7d956bd84').
narrative_ontology:cs_reading_relation('e56e2ce6-887e-4143-bc80-d8a7d956bd84', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('e56e2ce6-887e-4143-bc80-d8a7d956bd84', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e56e2ce6-887e-4143-bc80-d8a7d956bd84', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('e56e2ce6-887e-4143-bc80-d8a7d956bd84', foundational, temple_law_not_abrogated).
narrative_ontology:cs_axiom_status(temple_law_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('e56e2ce6-887e-4143-bc80-d8a7d956bd84', temple_law_not_abrogated, deontological).
narrative_ontology:cs_axiom('e56e2ce6-887e-4143-bc80-d8a7d956bd84', foundational, study_preserves_for_restoration).
narrative_ontology:cs_axiom_status(study_preserves_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('e56e2ce6-887e-4143-bc80-d8a7d956bd84', study_preserves_for_restoration, instrumental).
narrative_ontology:cs_reference_frame('e56e2ce6-887e-4143-bc80-d8a7d956bd84', restorationist_halachic_framework).
narrative_ontology:cs_drift_state('e56e2ce6-887e-4143-bc80-d8a7d956bd84', contemporary_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e56e2ce6-887e-4143-bc80-d8a7d956bd84', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_academy).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, restorationist_theologians).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, advanced_talmudic_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the curriculum of Talmudic study including tractates of Temple sacrifice law; certifies expertise and ordination; collects communal donations and state funding; enforces the norm that these tractates must be studied with the same intensity as practical law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_academy, agenda_setter,
    institutional, generational, constrained, global).

% Donates to yeshivas and kollels where advanced students study non-performable Temple law; social and theological pressure makes withholding support for Torah study costly; funds are diverted from applied charitable or educational purposes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, communal_donor_base, payer,
    moderate, biographical, constrained, national).

% Devote prime cognitive years to mastering sacrificial tractates with no material outlet; their labor and life course are structured by the academy's curriculum; exit means abandoning a religious identity fused with total Torah occupation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, advanced_talmudic_students, payer,
    powerless, biographical, identity_locked, global).

% Produce scholarship legitimizing the suspended-but-occupied framing; their scholarly niche and public authority depend on the preparatory theology; they do not administer but intellectually benefit from the institutional focus.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, restorationist_theologians, beneficiary,
    organized, generational, constrained, global).

% Focus on contemporary applied Jewish law; regard Temple tractates as theoretically important but not a current practical priority; they observe the resource allocation without directly challenging it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, practical_halachic_authorities, observer,
    institutional, generational, analytical, national).

% Would argue that resources devoted to non-performable law are wasted and that the preparatory framing is unfalsifiable; excluded from halakhic discourse and funding councils.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, excluded_secular_critics, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, rabbinic_academy).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves detailed juridical competence across a structural rupture (Temple destruction); maintains a continuous line of textual interpretation so that a future restored sacrificial order will not require reconstituting the law from scratch.
% TRANSFER_FUNCTION: Moves communal financial donations and students' cognitive labor from present-applied religious domains into the maintenance of a materially suspended juridical corpus.
% ABSENT_VOICES: Secular critics and pragmatic halachists who would redirect resources to applied law or social welfare; voices within the donor base questioning the marginal utility of sacrificial-tractate mastery; students who might prefer applied rabbinic training but are routed to theoretical Talmudic study.
% DISAPPEARANCE_RATIONALE: If the preparatory commitment vanished, yeshiva curricula would reallocate tractate hours, donor funds would shift toward practical halakha or communal services, the theological narrative of imminent restoration would weaken, and the institutional prestige attached to Temple-law expertise would collapse.
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the material conditions for a major biblical commandment, creating a theological and juridical crisis: how to relate to a divine law that is permanently inscribed but currently unperformable.
% FOUNDING_PROBLEM_CORROBORATION: External historians of Second Temple Judaism and internal rabbinic historiography attest to the rupture. However, the claim that intensive study of non-performable law is the necessary preparatory response is primarily advanced by the rabbinic academies that benefit from it. Independent academic historiography notes alternative responses (mourning, substitutionary prayer, sectarian withdrawal) were historically prominent; no independent corroboration confirms study-centric suspension as the only legitimate continuation.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__hybrid_preparatory, 0.48, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.48) is moderate because the coordination functionâpreserving a complex legal corpus across a civilizational ruptureâis real and costly; but the resource flow exceeds what mere preservation requires, directing prime human capital into scholastic virtuosity with no material outlet. Suppression (0.55) reflects the active social and institutional suppression of alternative framings (archival, transformative, or abrogationist) within the Orthodox halakhic sphere; it is not violent but is enforced through curriculum control, funding allocation, and status hierarchy. Theater ratio (0.40) captures the increasing performative dimension: much study demonstrates commitment to restoration rather than materially preparing for it, yet the intellectual content remains substantive. Accessibility collapse (0.45) indicates that alternative views (e.g., Maimonidean abeyance, Reform archival framing) are intellectually available but socially inaccessible within the community. Resistance (0.35) is moderate: pragmatic authorities and some donors offer passive resistance, but open challenge is rare due to the sacredness of Torah study as a category.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic academy seat, the constraint is genuine coordination preserving divine law across catastrophe; from the student and donor seats, it is enforced extraction of finite resources into a non-recursive domain. The engine computes this divergence from structural data: the academy has generational time horizon and constrained exit (institutional logic), while students are identity-locked and donors are socially constrained. The beneficiary/agenda-setter experience will compute toward rope or low-extraction tangled rope; the payer experience will compute toward snare or high-extraction tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic academies are structural beneficiaries (low d) because they collect resources and prestige while setting the rules. Restorationist theologians are beneficiaries (low-moderate d) through intellectual rent. Advanced students are primary targets (high d): their cognitive labor is the extracted resource, and identity_lock amplifies effective extraction. Communal donors are secondary targets (moderate-high d): their exit is constrained by the theological premium placed on funding Torah study. The directionality derivation from beneficiary/victim declarations plus exit options places payers near the full-target end and agenda-setters near the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing pure extraction (snare) from pure coordination (rope). The founding problemâTemple destructionâis real and historically attested, so the coordination story is not merely cover. However, the constraint has outlived any reasonable transitional horizon (2,000 years), and its active enforcement through curriculum and funding mechanisms suggests the coordination function has become entangled with institutional self-preservation. The mandatrophy is contested: the academy claims the problem remains live until restoration; critics claim the problem has been solved by historical substitution or transformed into prayer. The temporal measurements show slowly rising extractiveness and theater, consistent with coordination decaying into inertial maintenanceâpotential future piton dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_extraction_vs_continuity,
    'Is the study of non-performable Temple law genuine continuity maintenance for future restoration, or does it function as resource extraction clothed in theological necessity?',
    'Comparative analysis of yeshiva resource allocation against applied-law seminaries; historical comparison with Jewish communities that minimized Temple-law study.',
    'If extraction-dominant, the constraint shifts toward snare classification; if continuity-genuine, it remains tangled rope or approaches rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_extraction_vs_continuity, conceptual, 'Whether the constraint is primarily extractive or genuinely coordinative').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings (archival, transformative) structural or internalized?',
    'Examination of curriculum gatekeeping and funding mechanisms versus student self-selection and identity fusion.',
    'If internalized, effective suppression is higher than structural measure suggests; reclassifies toward higher extraction for identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    naturalness_of_preparatory_frame,
    'Is the preparatory framing a natural unfolding of halakhic logic or a constructed institutional response to maintain relevance and funding?',
    'Genealogical study of the ''study as preparation'' motif in responsa literature versus its amplification in the modern yeshiva era.',
    'If constructed, the coordination story is cover and extraction is primary; if natural, the tangled-rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_preparatory_frame, conceptual, 'Whether the preparatory theology is naturally derived or institutionally constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsc_hp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tsc_hp_tr_t10, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 10, 0.25).
narrative_ontology:measurement(tsc_hp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tsc_hp_tr_t30, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 30, 0.35).
narrative_ontology:measurement(tsc_hp_tr_t40, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 40, 0.38).
narrative_ontology:measurement(tsc_hp_tr_t50, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(tsc_hp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tsc_hp_be_t10, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(tsc_hp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(tsc_hp_be_t30, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(tsc_hp_be_t40, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(tsc_hp_be_t50, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(tsc_hp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tsc_hp_su_t10, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(tsc_hp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(tsc_hp_su_t30, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(tsc_hp_su_t40, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(tsc_hp_su_t50, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the temple_sacrifice_commitment family. The kernel decomposes into structurally distinct claims because the epsilon values and stakeholder configurations differ across readings: the hybrid_preparatory reading has moderate extractiveness and a preparatory framing, while performance_only would have low coordination and high extraction (or mountain-like archival status), and symbolic_transformation would have low extraction with transformed beneficiaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
