% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Study as Readiness for Messianic Restoration (Deferral Reading)
 *   domain: religious/halakhic/institutional
 *
 * SUMMARY:
 *   This constraint models the classical rabbinic position that the
 *   commandments of Kodashim (sacrificial law) remain fully and eternally
 *   binding in principle but are temporarily suspended in practice due to the
 *   absence of the Temple, altar, and required purity infrastructure. Under
 *   this reading, sustained study of Kodashim is justified not as fulfillment
 *   of the commandment itself (that is the sibling reading,
 *   study_as_performance) and not as maintenance of a dead husk-law (that is
 *   the sibling reading, performance_only), but as active
 *   readiness-maintenance for a future restoration that has not yet occurred
 *   and whose timeline is theologically open. This is a distinct constraint
 *   from its siblings: its ε is driven by opportunity cost imposed on present
 *   generations in service of a future contingency, not by present ritual
 *   non-performance (performance_only) and not by the coordination-function
 *   of intellectual fulfillment (study_as_performance). Extraction is
 *   moderate rather than severe because the coordination function —
 *   preserving transmissible legal knowledge against loss — is genuine; but
 *   it is real because the doctrine has no internal falsification condition
 *   and therefore cannot self-correct if the restoration premise never
 *   materializes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.38).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Study as Readiness for Messianic Restoration (Deferral Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/institutional").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8').
narrative_ontology:cs_kernel_codification('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', fixed_text).
narrative_ontology:cs_authority_grounding('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', lineage).
narrative_ontology:cs_interpretation_layer_present('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8').
narrative_ontology:cs_reading_relation('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', kodashim_commandment_status__performance_only, influences).
narrative_ontology:cs_axiom('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', foundational, commandment_retains_eternal_bindingness_despite_nonperformance).
narrative_ontology:cs_axiom_status(commandment_retains_eternal_bindingness_despite_nonperformance, holdable).
narrative_ontology:cs_axiom_grounding('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', commandment_retains_eternal_bindingness_despite_nonperformance, theological).
narrative_ontology:cs_axiom('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', secondary, study_constitutes_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_constitutes_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', study_constitutes_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', sinaitic_covenant_full_commandment_set).
narrative_ontology:cs_drift_state('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', contemporary_post_temple_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e79f0c98-fefc-4bc9-88ec-06a6aa7e9bf8', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, kohanic_lineage_claimants).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_practical_halakha_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, students_directed_away_from_livelihood_training).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, temple_will_be_rebuilt).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, torah_is_eternal_and_unchanging).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the curriculum requiring sustained study of Kodashim (sacrificial law) as a core component of advanced Talmudic education, framing it as maintaining communal readiness for restoration. Draws funding, prestige, and enrollment from being the institution that keeps this body of law alive. Faces no binding deadline or falsification condition on the restoration premise, so the study mandate never expires on its own terms.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, beneficiary).

% Hold hereditary status contingent on eventual restoration of sacrificial service; their communal standing and specific halakhic privileges (first aliyah, priestly blessing, purity restrictions) are validated and renewed by ongoing institutional attention to Kodashim. Their identity is partly constituted by a role that has had no functional referent for nearly two millennia, so continued study literally sustains what they are.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, kohanic_lineage_claimants, beneficiary,
    moderate, generational, identity_locked, national).

% Rabbinic authorities who teach and rule that the commandments of Kodashim remain formally binding but are suspended for lack of Temple, altar, and ritual purity infrastructure. They derive interpretive authority and continuity of tradition from maintaining this deferral frame; abandoning it would require either declaring the commandment obsolete (heterodox) or declaring it currently operative (impossible), so the deferral position is the only one that preserves their office's coherence.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, messianic_restorationist_authorities, agenda_setter).

% Young men in advanced yeshiva tracks spend years mastering the minutiae of sacrificial procedures, blood disposal, and altar geometry that has no present application, at direct opportunity cost to vocational training, university study, or earning capacity. Their exit is constrained by social expectation, family investment in their scholarly track, and the reputational cost of leaving Kodashim study for 'lesser' pursuits.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, students_directed_away_from_livelihood_training, payer,
    powerless, biographical, constrained, local).

% The broader community's present, urgent halakhic questions (family law, business ethics, medical halakha, communal governance) compete for the same scholarly attention and institutional prestige that Kodashim study claims. Because Kodashim carries the weight of eternal-and-unchanging Torah status, resources and top scholarly talent are drawn toward it even though it yields zero applicable rulings for anyone alive today.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_practical_halakha_needs, payer,
    powerless, biographical, trapped, local).

% Movements that have effectively treated sacrificial law as obsolete rather than merely suspended are not part of the halakhic conversation that maintains the deferral framing; their position — that the commandment's practical force ended with the Temple's destruction and that continued formal bindingness is theologically unnecessary — is excluded from the institutions that set Kodashim curriculum, though they hold this view openly outside those institutions.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, reform_and_conservative_movements, excluded,
    organized, generational, mobile, national).

% Study the deferral doctrine as a historical and sociological phenomenon: how a legal system maintains formal bindingness of commandments with zero present enforceability, and what institutional functions (identity maintenance, curriculum justification, hope-preservation) the doctrine serves independent of its truth value.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, academic_historians_of_halakha, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, diffuse).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a body of legal and ritual knowledge intact and transmissible across generations so that, should the physical and political preconditions for Temple service ever be restored, the community would not have lost the capacity to resume it. This solves a genuine transmission problem: complex ritual law degrades and is lost if unpracticed and unstudied for centuries.
% TRANSFER_FUNCTION: Moves scholarly attention, institutional funding, curricular hours, and the opportunity cost of years of study from present-applicable halakhic domains and from students' vocational futures, toward the maintenance of a body of law with no current practical referent. Also transfers social status and identity-validation to kohanic lineage holders and to institutions positioned as guardians of the 'complete' Torah.
% ABSENT_VOICES: Reform and Conservative halakhic authorities who hold the sacrificial commandments obsolete rather than suspended are not represented in the institutions that set Kodashim curriculum. Students who leave intensive study tracks rarely publish accounts of the opportunity cost; their absence from the recorded discourse makes the deferral framing's costs invisible in the institutional record.
% DISAPPEARANCE_RATIONALE: If the deferral doctrine were abandoned tomorrow in favor of declaring the commandment obsolete, yeshiva curricula would restructure substantially (Kodashim study would lose its 'eternal Torah' justification and could be reduced to a purely historical elective), kohanic status claims would lose part of their forward-looking rationale, and messianic-restorationist authority would face a genuine theological crisis. But if it were abandoned in favor of declaring it currently fulfilled through study alone (the sibling reading), very little institutional practice would change — the study continues either way. Whether the world 'rearranges' therefore depends entirely on which alternative reading would replace this one, which is exactly the kernel contest this story is one reading of.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the rabbinic tradition needed to explain why commandments explicitly given by God as eternal and binding (the sacrificial system) could go unperformed for an indefinite period without either abrogating divine law or requiring impossible present performance. The deferral doctrine solved this by classifying the commandments as temporarily inoperative due to missing preconditions (Temple, altar, priestly purity infrastructure) rather than repealed.
% FOUNDING_PROBLEM_CORROBORATION: Restorationist authorities and the yeshiva institutions that teach Kodashim attest the founding problem remains live — the Temple has not been rebuilt, so the deferral condition still holds by their own criteria. Academic historians of halakha, writing from outside the benefiting institutions, corroborate that the doctrine functioned historically to preserve legal continuity and communal identity after 70 CE, but note that after nearly two millennia without material progress toward the stated precondition (Temple reconstruction), the doctrine's own falsification criteria have never been tested, making 'still live' unfalsifiable rather than confirmed. No source outside the tradition's own institutions attests the restoration precondition is approaching; the corroboration that exists is historical-functional, not confirmatory of imminence.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the real opportunity cost borne by students and by the broader community's present halakhic needs, without rating this as severe extraction — the knowledge-preservation function is genuine and the cost is diffuse rather than concentrated theft. Suppression (0.38) is moderate: no one is coerced into Kodashim study, but social and institutional pressure within yeshiva culture constrains exit for those already on the track, and the doctrine's theological framing (Torah is eternal and unchanging) makes public dissent costly within the tradition. Theater ratio (0.31) is present but not dominant — study is substantively rigorous, not merely performed, but a portion of institutional emphasis on Kodashim functions to signal completeness of Torah engagement rather than to build applicable competence. Accessibility collapse (0.5) is moderate: once inside the yeshiva system, alternative curricular paths do exist but carry real social cost, so alternatives partially but not fully collapse. Resistance (0.35) is moderate: some students and reform-oriented voices resist the mandate's opportunity cost, but resistance is muted by the doctrine's theological weight.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva institutions and messianic-restorationist authorities sit near the beneficiary end: they derive funding, prestige, and interpretive authority from the study mandate's continuation, and face no binding deadline that would force reckoning with the restoration premise. Kohanic lineage claimants are also beneficiaries — their hereditary status is validated by continued institutional attention to a role with no current function. Present-generation practical-halakha needs and students directed away from livelihood training sit near the target end: they bear the diffuse and direct opportunity costs respectively, with constrained or trapped exit options, since leaving the track carries social and identity costs disproportionate to their power.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) is what prevents mislabeling this as pure extraction. The coordination function is real: complex ritual law is genuinely at risk of being lost if unstudied for two millennia, and restoring lost technical knowledge from fragmentary sources is far harder than maintaining continuous transmission. But the same structure that preserves this knowledge also extracts opportunity cost from students and diverts resources from present halakhic needs, and it does so under active institutional enforcement (curricular requirements, social expectation, identity-conferring status). Calling it a pure Rope would ignore the asymmetric cost borne by the powerless payer seats; calling it a Snare would ignore the genuine and non-trivial knowledge-preservation function that has demonstrable value if restoration ever occurs. The deferral premise's total resistance to falsification over nineteen centuries is the specific feature that keeps this reading from ever transitioning cleanly into either sibling reading — it is built to never have to resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_timeline_indeterminacy,
    'Is the messianic restoration a genuine future event whose timing is simply unknown (making current deferral rational preparation), or is the indefinite deferral itself evidence that the restoration premise functions primarily as an institutional justification device that will never be tested?',
    'No empirical resolution mechanism exists within the tradition''s own epistemics, since the doctrine explicitly declines to specify a timeline; the question can only be addressed by comparing the doctrine''s institutional functions (identity maintenance, curricular justification, status preservation) against its stated theological function (readiness maintenance) and asking whether the former have become self-sustaining independent of the latter.',
    'If the doctrine functions primarily as institutional self-justification rather than genuine future-readiness, the coordination function claimed here is weaker than authored and the constraint would sit closer to snare; if the readiness function is genuine and proportionate, tangled_rope is the accurate classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_timeline_indeterminacy, conceptual, 'Whether unfalsifiable future contingency is genuine preparation or self-perpetuating justification.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the tradition''s own textual and institutional record justifies selecting the deferral reading over the study-as-performance or performance-only readings as the operative one for a given community or authority?',
    'Comparative analysis of which halakhic authorities and communities explicitly adopt which reading, and whether the reading selected correlates with institutional incentive (e.g., yeshiva institutions favoring deferral or study-as-performance over performance-only, since the latter would undercut curricular justification).',
    'If reading selection correlates strongly with institutional incentive rather than independent textual reasoning, that is evidence the kernel contest itself is partly a contest over which reading best serves incumbent institutions, which would apply symmetrically as a caution to all three sibling constraints, not only this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Whether reading-selection across the kernel correlates with institutional incentive.').

omega_variable(
    opportunity_cost_measurement,
    'How large is the actual opportunity cost borne by students directed into extensive Kodashim study relative to counterfactual vocational or alternative scholarly tracks, and how does this vary by community and era?',
    'Longitudinal tracking of yeshiva graduates'' economic and educational outcomes compared to matched cohorts in alternative tracks, controlling for community-level selection effects.',
    'A large measured opportunity cost would support the moderate-to-substantial extractiveness score authored here; a small one would suggest the cost is more theoretical than the story credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_measurement, empirical, 'Empirical magnitude of the diverted opportunity cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.23).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.26).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.28).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.3).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.31).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.39).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(koda_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(koda_su_t40, kodashim_commandment_status__messianic_deferral, suppression_requirement, 40, 0.34).
narrative_ontology:measurement(koda_su_t60, kodashim_commandment_status__messianic_deferral, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(koda_su_t80, kodashim_commandment_status__messianic_deferral, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_commandment_status kernel. messianic_deferral (this file) holds the commandment formally binding but suspended pending future restoration, with moderate extractiveness from opportunity cost imposed in service of that future contingency. study_as_performance holds that study itself constitutes fulfillment, reframing the same activity with a lower extractiveness profile since the coordination function IS the claimed fulfillment rather than mere preparation for it. performance_only holds the commandment simply inoperative absent the Temple, with the lowest extractiveness of the three since no messianic-preparation burden attaches to its study. Each reading is a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification; they share only the underlying textual kernel (the sacrificial law corpus) and the historical fact of Temple absence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
