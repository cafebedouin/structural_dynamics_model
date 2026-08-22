% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading of 'All Men Created Equal' Bounded by 18th-Century Social Taxonomy
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint is the originalist reading of the kernel 'all men created
 *   equal' in American constitutional law. The kernel is the Declaration's
 *   equality clause and its constitutional echoes. This
 *   readingâoriginalistâbounds the scope of equality by the 18th-century
 *   social taxonomy, treating founder intent as exhaustive. Sibling readings
 *   are the universalist reading (equality as iterative expansion regardless
 *   of intent) and the textualist-paradox reading (universal language is
 *   irreconcilable with restricted application). The structural delta for
 *   this reading is high extractiveness: it concentrates the benefits of
 *   constitutional equality on the founding elite and their descendants while
 *   imposing costs on historically excluded groups. The narrow victim set is
 *   not accidental; it is encoded in the interpretive method itself.
 *
 * KEY AGENTS:
 *   - originalist_jurists (agenda_setter / institutional / analytical): administer the interpretive framework that restricts equality to 18th-century taxonomy
 *   - founding_elite_descendants (beneficiary / powerful / mobile): collect the diffuse privileges of a narrowly constructed equality
 *   - historically_excluded_groups (payer / powerless / trapped): bear the costs of exclusion from equality protections
 *   - critical_legal_scholars (observer / analytical / analytical): identify the performative contradiction and document the extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.82).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.78).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading of 'All Men Created Equal' Bounded by 18th-Century Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '9d80cf46-7bf4-4666-adca-009a606072c3').
narrative_ontology:cs_kernel_codification('9d80cf46-7bf4-4666-adca-009a606072c3', fixed_text).
narrative_ontology:cs_authority_grounding('9d80cf46-7bf4-4666-adca-009a606072c3', lineage).
narrative_ontology:cs_interpretation_layer_present('9d80cf46-7bf4-4666-adca-009a606072c3').
narrative_ontology:cs_reading_relation('9d80cf46-7bf4-4666-adca-009a606072c3', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d80cf46-7bf4-4666-adca-009a606072c3', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('9d80cf46-7bf4-4666-adca-009a606072c3', foundational, original_public_meaning_governs).
narrative_ontology:cs_axiom_status(original_public_meaning_governs, holdable).
narrative_ontology:cs_axiom_grounding('9d80cf46-7bf4-4666-adca-009a606072c3', original_public_meaning_governs, conventional).
narrative_ontology:cs_axiom('9d80cf46-7bf4-4666-adca-009a606072c3', foundational, eighteenth_century_equality_taxonomy_is_fixed).
narrative_ontology:cs_axiom_status(eighteenth_century_equality_taxonomy_is_fixed, holdable).
narrative_ontology:cs_axiom_grounding('9d80cf46-7bf4-4666-adca-009a606072c3', eighteenth_century_equality_taxonomy_is_fixed, empirically_contingent).
narrative_ontology:cs_reference_frame('9d80cf46-7bf4-4666-adca-009a606072c3', founding_era_social_compact).
narrative_ontology:cs_drift_state('9d80cf46-7bf4-4666-adca-009a606072c3', contemporary_universal_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9d80cf46-7bf4-4666-adca-009a606072c3', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, historically_excluded_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer constitutional interpretation by tethering the meaning of equality clauses to the 18th-century historical intent and original public meaning. They set the interpretive rules that restrict equality's scope to the founding-era social taxonomy, issuing opinions that exclude universalist and textualist-paradox readings from authoritative constitutional doctrine.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_jurists, agenda_setter,
    institutional, generational, analytical, national).

% Inherit and maintain social, legal, and economic privileges encoded in the original constitutional order. They benefit from the narrow construction of equality because it preserves the hierarchical distributions of the founding eraâproperty, civic standing, and institutional accessâwithout bearing the costs of enforcing the reading themselves.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    powerful, generational, mobile, national).

% Bear the ongoing costs of exclusion from constitutional equality protections. Their claims to equal protection are systematically defeated by originalist arguments that the framers did not intend to include them within 'all men.' Exit is structurally blocked because the interpretive framework itself defines them as outside the scope of equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, historically_excluded_groups, payer,
    powerless, generational, trapped, national).

% Analyze the originalist reading as a mechanism for preserving 18th-century hierarchy beneath the surface of textual fidelity. They document the divergence between the equality rhetoric and the restricted application, identifying the performative contradiction that the originalist framework is designed to suppress.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, critical_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate a polity around fixed constitutional meaning by eliminating judicial discretion and anchoring legal interpretation to historical authorial intent.
% TRANSFER_FUNCTION: Moves the protective scope of constitutional equality from all persons to those fitting the 18th-century propertied white male taxonomy, withholding legal standing, rights, and material protections from everyone outside that category.
% ABSENT_VOICES: Enslaved persons, indigenous nations, women, and non-propertied free men were physically excluded from the drafting and ratifying conventions. Their modern descendants and universalist jurists participate in discourse but their expansive readings are structurally excluded from the authoritative originalist interpretive frame.
% DISAPPEARANCE_RATIONALE: If the originalist taxonomy vanished overnight, constitutional equality doctrine would expand to include historically excluded groups, originalist jurisprudence would lose its foundational premise, and the legal architecture preserving 18th-century hierarchy would dissolve. The polity would rearrange around a universalist or textualist-paradox reading.
% FOUNDING_PROBLEM: To legitimate a revolutionary republic founded on liberty while simultaneously preserving slavery, gender subordination, indigenous dispossession, and property qualifications; to articulate universal-sounding equality rhetoric without disrupting the existing social order the founders inhabited.
% FOUNDING_PROBLEM_CORROBORATION: Historians of slavery and the founding (e.g., Edmund Morgan, Robin Einhorn) and critical race scholars attest the problem was managing the contradiction between revolutionary liberty and entrenched hierarchy. Originalist jurists and originalist-aligned historians attest the problem is fixing constitutional meaning against subjective judicial drift. No corroborating source outside the benefiting parties attests that 18th-century social taxonomy is the live or necessary solution to 21st-century equality disputes.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the reading systematically withholds equality protections from a defined out-group while preserving them for the in-group. Suppression (0.78) is high because maintaining an 18th-century taxonomy against two centuries of universalist pressure requires active doctrinal enforcement, precedent management, and methodological boundary-policing. Theater ratio (0.55) reflects the growing performative dimension: as the social taxonomy became harder to defend openly, originalism increasingly functions as a textual performance that presents restricted application as historical fidelity. Accessibility collapse (0.70) is high because once the originalist frame is accepted, non-originalist alternatives appear illegitimate rather than merely wrong. Resistance (0.72) is high because the excluded groups and their allies have mounted sustained legal and political resistance for the full interval. The temporal series show cyclical dynamics: extraction dips during periods of overt systemic challenge (Civil War, Civil Rights era) and rebounds during periods of retrenchment, while theater rises precisely when the contradiction between universal language and restricted application becomes most visible.
 *
 * PERSPECTIVAL GAP:
 *   The originalist jurist seat experiences the constraint as a genuine commitment to constitutional fidelity and interpretive stabilityâa bulwark against judicial overreach. The historically excluded group seat experiences the identical constraint as an active mechanism of legalized exclusion, where the appeal to history operates as a barrier to recognition. The engine computes this divergence from the structural asymmetry: agenda-setters with analytical exit and beneficiaries with mobile exit face low directionality, while payers with trapped exit face high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding_elite_descendants are declared beneficiaries with powerful status and mobile exit options, placing their directionality near the full-beneficiary end; the constraint subsidizes their legal and social standing. Historically_excluded_groups are declared victims with powerless status and trapped exit, placing their directionality near the full-target end; the constraint extracts equality protections from them. Originalist_jurists are agenda-setters with institutional power and analytical exit; because they are neither declared beneficiaries nor victims, their directionality falls to the canonical fallback for institutional atomsâmoderately low, reflecting their structural position as administrators rather than primary capturers of the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy test prevents mislabeling this constraint as a rope or scaffold. A rope would show symmetric net benefit and minimal coercion; here the benefits are sharply concentrated and the costs are borne by a defined out-group, with enforcement required to maintain the boundary. A scaffold would carry a sunset clause and transitional justification; originalism claims permanence. The constraint's founding problemâlegitimating hierarchy while invoking equalityâhas been contested for two centuries, yet the arrangement persists because it continues to benefit the enfranchised caste, not because the founding problem remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_historical_recoverability,
    'Can the specific intent of 18th-century drafters regarding ''all men created equal'' be recovered with sufficient precision to govern modern adjudication, or does the historical record underdetermine the scope of equality?',
    'Comprehensive archival synthesis by historians outside the originalist tradition, evaluating the full range of founding-era sources on race, gender, and property.',
    'If intent is irrecoverable or internally contradictory, the originalist reading loses its empirical foundation and collapses toward a covert snare; if recoverable and narrowly restricted, the extraction is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_intent_historical_recoverability, empirical, 'Whether 18th-century intent is historically determinate.').

omega_variable(
    originalism_as_coordinating_cover,
    'Does originalism serve a genuine coordination function (interpretive stability) that is separable from its extraction of equality protections from excluded groups, or is the coordination story inseparable from the hierarchy it preserves?',
    'Comparative analysis of jurisdictions with non-originalist interpretive frameworks: do they achieve comparable interpretive stability without analogous exclusion?',
    'If separable, the constraint is a tangled rope; if inseparable, it is a snare whose coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_coordinating_cover, conceptual, 'Coordination function separability from extraction.').

omega_variable(
    committer_frame_originalist_reading,
    'This constraint is the originalist reading of the kernel ''all_men_created_equal''. How would classification change if the universalist reading were adopted instead?',
    'Compare the structural data of this reading against a sibling universalist reading of the same kernel.',
    'The universalist reading would reverse beneficiary/victim structures and collapse extractiveness, producing a rope or mountain classification rather than a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_originalist_reading, conceptual, 'Committe frame marking this as a kernel reading.').

omega_variable(
    textualist_paradox_salience,
    'Does the textualist paradox readingâthat universal language is irreconcilable with restricted applicationârepresent a genuine logical contradiction within the kernel, or a rhetorical effect produced by originalism''s interpretive frame?',
    'Linguistic and philosophical analysis of performative contradiction in legal texts, assessed independently of originalist or universalist commitments.',
    'If the paradox is internal to the kernel, originalism''s foreclosure of it is structurally unstable; if the paradox is frame-dependent, originalism neutralizes it legitimately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualist_paradox_salience, conceptual, 'Status of the textualist paradox as logical or rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 0, 248).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__originalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(all__tr_t30, all_men_created_equal__originalist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(all__tr_t60, all_men_created_equal__originalist_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(all__tr_t90, all_men_created_equal__originalist_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement(all__tr_t150, all_men_created_equal__originalist_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement(all__tr_t200, all_men_created_equal__originalist_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(all__tr_t248, all_men_created_equal__originalist_reading, theater_ratio, 248, 0.55).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__originalist_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(all__be_t30, all_men_created_equal__originalist_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(all__be_t60, all_men_created_equal__originalist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(all__be_t90, all_men_created_equal__originalist_reading, base_extractiveness, 90, 0.8).
narrative_ontology:measurement(all__be_t150, all_men_created_equal__originalist_reading, base_extractiveness, 150, 0.75).
narrative_ontology:measurement(all__be_t200, all_men_created_equal__originalist_reading, base_extractiveness, 200, 0.7).
narrative_ontology:measurement(all__be_t248, all_men_created_equal__originalist_reading, base_extractiveness, 248, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__originalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(all__su_t30, all_men_created_equal__originalist_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(all__su_t60, all_men_created_equal__originalist_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(all__su_t90, all_men_created_equal__originalist_reading, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(all__su_t150, all_men_created_equal__originalist_reading, suppression_requirement, 150, 0.65).
narrative_ontology:measurement(all__su_t200, all_men_created_equal__originalist_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(all__su_t248, all_men_created_equal__originalist_reading, suppression_requirement, 248, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
