% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Usul al-Fiqh: Regional Custom and Medinan Practice as Evidentiary Sources
 *   domain: religious/legal_theory
 *
 * SUMMARY:
 *   The Maliki reading of usul al-fiqh method elevates three non-textual or
 *   partially textual sourcesâMedinan communal practice ('amal ahl
 *   al-Madina), unrestricted public interest (maslaha mursala), and regional
 *   custom ('urf)âto independent or semi-independent status alongside Quran
 *   and hadith. This constraint story instantiates the Maliki reading of the
 *   contested kernel 'usul_al_fiqh_method'; sibling readings (Hanafi,
 *   Shafi'i, Hanbali) produce different source hierarchies and different
 *   beneficiary/victim structures. The constraint is claimed as tangled_rope
 *   because it carries a genuine coordination function (regional legal
 *   continuity and adaptability) while also extracting from universalist
 *   textualism by allowing local practice and juridical discretion to
 *   override textual sources that would otherwise command uniform
 *   application.
 *
 * KEY AGENTS:
 *   - maliki_jurist_community: Primary agenda-setter (institutional/constrained) â administers the usul hierarchy that elevates custom and Medinan practice.
 *   - regional_muslim_communities: Primary beneficiary (moderate/constrained) â customary norms gain legal weight.
 *   - textualist_universalists: Primary payer (institutional/mobile) â bear the cost of textual override by custom.
 *   - non_maliki_jurists: Excluded institutional observers (institutional/mobile) â contest the reading from outside the Maliki framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.58).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Usul al-Fiqh: Regional Custom and Medinan Practice as Evidentiary Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "religious/legal_theory").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'a9e9b19a-627c-46e2-b741-b4fd1f2a223d').
narrative_ontology:cs_kernel_codification('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', formalized).
narrative_ontology:cs_authority_grounding('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', practice).
narrative_ontology:cs_interpretation_layer_present('a9e9b19a-627c-46e2-b741-b4fd1f2a223d').
narrative_ontology:cs_reading_relation('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', foundational, medinan_practice_independent_evidence).
narrative_ontology:cs_axiom_status(medinan_practice_independent_evidence, holdable).
narrative_ontology:cs_axiom_grounding('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', medinan_practice_independent_evidence, empirically_contingent).
narrative_ontology:cs_axiom('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', foundational, public_interest_unrestricted_source).
narrative_ontology:cs_axiom_status(public_interest_unrestricted_source, holdable).
narrative_ontology:cs_axiom_grounding('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', public_interest_unrestricted_source, instrumental).
narrative_ontology:cs_reference_frame('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', medinan_community_practice_framework).
narrative_ontology:cs_drift_state('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', contemporary_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9e9b19a-627c-46e2-b741-b4fd1f2a223d', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_jurist_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_muslim_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, textualist_universalists).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, urf_custom_validity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, maslaha_mursala_legitimacy).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_practice_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the Maliki usul framework, adjudicating when Medinan communal practice, unrestricted public interest, or regional custom overrides isolated hadith and analogical derivation. Their institutional authority depends on preserving the methodological distinctiveness of the school.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_jurist_community, agenda_setter,
    institutional, generational, constrained, global).

% Live under legal rulings that integrate their established customary practices and regional norms into sacred law. Their local customs gain jurisprudential weight and legitimacy through the Maliki framework, provided they do not contradict foundational texts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_muslim_communities, beneficiary,
    moderate, generational, constrained, regional).

% Bear the cost of a methodological framework that permits regional custom and unrestricted public interest to override or bypass isolated authentic hadith and universal textual derivation. Their preference for uniform textual application is systematically deprioritized within Maliki jurisprudence, though they can adhere to other schools.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, textualist_universalists, payer,
    institutional, generational, mobile, global).

% Hanafi, Shafi'i, and Hanbali jurists who operate under different source hierarchies. They would contest the elevation of custom and Medinan practice above authenticated hadith, but their methodological objections are external to the Maliki usul conversation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, non_maliki_jurists, excluded,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic legal derivation across diverse regional contexts by integrating living customary practice, continuous Medinan communal tradition, and unrestricted public interest as sources alongside textual revelation, preventing a rigid universalism that would fracture legal application in localities with established norms.
% TRANSFER_FUNCTION: Moves interpretive authority from isolated textual reports and universalist analogical derivation to regional communal practice and juridical discretion over public interest, transferring power from hadith-centric scholars to Maliki jurists and the communities whose customs they ratify.
% ABSENT_VOICES: Hanbali textualists and hadith purists who reject any source beyond Quran and rigorously authenticated hadith; modernist reformers who view madhhab-bound customary integration as obstructing uniform codified law; minority communities whose customs may be overridden by patriarchal or majoritarian local practice yet are absent from the classical juridical conversation.
% DISAPPEARANCE_RATIONALE: If the Maliki reading vanished overnight, thousands of rulings across North and West Africa governing family law, commerce, and ritual practice would lose their juridical foundation. Courts and muftis would need to re-derive rulings from stricter textual sources, invalidating customary arrangements that currently enjoy legal shelter and likely triggering a shift toward codified state law or competing madhhab adoption.
% FOUNDING_PROBLEM: The early Muslim community faced the problem of deriving divine law for regions far from Medina with diverse pre-Islamic customs and limited textual reports for every local contingency, requiring a framework that honored both revelation and the preserved practice of the Prophet's city.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law attest to the genuine diversity of early legal reasoning and the need for regional adaptation. Hanbali and modern Salafi scholars attest that the founding problem is adequately addressed by strict adherence to Quran and authenticated hadith alone, arguing that the Maliki solution introduces instability into source hierarchy rather than solving a real coordination deficit.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the substantial authority that Maliki jurists and regional custom wield to override or bypass textual sources; suppression (0.58) captures the active juridical enforcement required to maintain that Medinan practice and custom sit above isolated hadith in the source hierarchy. Theater_ratio (0.40) acknowledges that while the coordination function is genuine, a significant share of contemporary Maliki juridical activity is performative maintenance of a methodological distinctiveness that no longer commands the political or institutional power it once held. Accessibility_collapse (0.65) indicates that once inside the Maliki framework, pure textualist alternatives largely collapse as viable internal options; resistance (0.45) reflects ongoing but institutionally contained pushback from textualist scholars and modernizing states.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Maliki jurists) experiences this constraint as a necessary and venerable coordination mechanism preserving prophetic continuity; the payer seat (textualist universalists) experiences the same structure as an extraction of authority from divine text to human custom. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki jurists and regional communities are declared beneficiaries, deriving low directionality from the constraint. Textualist universalists are declared payers/victims, deriving high directionality. Non-Maliki jurists are excluded rather than victims because they operate outside the constraint's scope and have mobile exit to alternative schools.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlegal adaptation to regional diversityâremains live in many Muslim societies, preventing a clean piton verdict. However, modern nation-state codification has partly displaced madhhab jurisprudence, creating partial mandatrophy risk captured by the rising theater_ratio over time. The constraint is not a scaffold because it carries no sunset clause and was not designed as transitional. It is not a snare because the coordination function (regional legal integration) is structurally genuine and not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maliki_reading_kernel_location,
    'This constraint is the Maliki reading of kernel usul_al_fiqh_method; how would the Hanbali reading alter the structural beneficiary and victim arrangement?',
    'Generate the Hanbali sibling constraint story and compare source hierarchies, victim sets, and enforcement mechanisms across the kernel family.',
    'A Hanbali reading would eliminate regional communities as beneficiaries and eliminate textualist universalists as victims, inverting the extraction direction toward hadith purists and radically lowering base extractiveness from textual sources.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maliki_reading_kernel_location, conceptual, 'Sibling reading structural delta for kernel usul_al_fiqh_method').

omega_variable(
    medinan_practice_historicity,
    'Is the independent evidentiary weight of Medinan practice grounded in recoverable historical continuity with the Prophet''s community, or is it a constructed juridical category retroactively authorizing regional Maliki autonomy?',
    'Historical-critical investigation of early Maliki texts and transmission layers against anthropological and documentary records of actual seventh-century Medinan practice.',
    'If largely constructed, the coordination function weakens and extraction toward textualist universalism strengthens; if historically continuous, the coordination function is stronger and the epsilon attribution shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_historicity, empirical, 'Historical versus constructed basis of Medinan practice authority').

omega_variable(
    maslaha_mursala_extraction_boundary,
    'Does maslaha mursala operate as a necessary flexibility mechanism in divine law or as a juridical override permitting jurists to bypass textual constraints under the cover of public interest?',
    'Case-law analysis comparing Maliki rulings invoking maslaha mursala against parallel textualist derivations; measure the rate at which textual sources are bypassed when maslaha is invoked.',
    'If maslaha routinely bypasses clear textual indicators, the constraint tilts toward snare; if reserved for genuinely silent textual zones, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_extraction_boundary, conceptual, 'Coordination versus extraction boundary of unrestricted public interest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_maliki_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_maliki_tr_t25, usul_al_fiqh_method__maliki_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(usul_maliki_tr_t50, usul_al_fiqh_method__maliki_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(usul_maliki_tr_t75, usul_al_fiqh_method__maliki_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(usul_maliki_tr_t100, usul_al_fiqh_method__maliki_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(usul_maliki_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_maliki_be_t25, usul_al_fiqh_method__maliki_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(usul_maliki_be_t50, usul_al_fiqh_method__maliki_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(usul_maliki_be_t75, usul_al_fiqh_method__maliki_reading, base_extractiveness, 75, 0.58).
narrative_ontology:measurement(usul_maliki_be_t100, usul_al_fiqh_method__maliki_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_maliki_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(usul_maliki_su_t25, usul_al_fiqh_method__maliki_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(usul_maliki_su_t50, usul_al_fiqh_method__maliki_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(usul_maliki_su_t75, usul_al_fiqh_method__maliki_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(usul_maliki_su_t100, usul_al_fiqh_method__maliki_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is the Maliki reading of the contested kernel usul_al_fiqh_method. The natural-language label 'usul al-fiqh method' conflates four structurally distinct readings (Hanafi, Maliki, Shafi'i, Hanbali) with different source hierarchies, beneficiary/victim structures, and epsilon values. Each reading is authored as an independent constraint story linked by kernel_context and reading_relations; they form a constraint family under the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
