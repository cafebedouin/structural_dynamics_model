% ============================================================================
% CONSTRAINT STORY: phenomenological_endorsement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_phenomenological_endorsement_reading, []).

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
 *   constraint_id: phenomenological_endorsement_reading
 *   human_readable: Phenomenological Endorsement Test for Preference Authenticity
 *   domain: moral psychology / philosophy of autonomy / political theory of power
 *
 * SUMMARY:
 *   This story instantiates the phenomenological-endorsement reading of the
 *   authentic-preference-boundary kernel: the claim that 'I'm glad I don't
 *   want it' versus 'I didn't want it anyway' names a real, introspectively
 *   detectable difference in mental state, available in principle to any
 *   agent who performs sufficiently disciplined first-person examination,
 *   even though ordinary casual introspection is often unreliable. This
 *   reading is structurally distinct from the sibling readings
 *   (behaviorist/counterfactual, genealogical/origin, capability/traction) —
 *   it does not appear in this file except as named siblings in cs_structure.
 *   Its distinguishing structural feature: no one is formally excluded from
 *   the authenticity test, but the practical burden of passing it falls on
 *   those whose introspective narration is fluent and socially credentialed,
 *   producing a beneficiary/victim split organized around articulateness and
 *   institutional trust rather than around formal eligibility.
 *
 * KEY AGENTS:
 *   - articulate_reflective_reasoners: primary beneficiary (moderate/mobile) — pass the test easily, certified as authentic
 *   - therapeutic_and_counseling_professions: agenda_setter (institutional/arbitrage) — administer and certify the endorsement procedure
 *   - inarticulate_or_traumatized_reporters: primary payer (powerless/trapped) — genuinely endorse but cannot perform fluent narration, discounted
 *   - adaptive_preference_holders_under_scrutiny: payer/excluded (powerless/constrained) — endorsement is scrutinized and frequently overridden
 *   - liberal_autonomy_theorists: beneficiary/agenda_setter (organized/analytical) — theoretical edifice depends on the test being real
 *   - populations_whose_introspection_is_pathologized: excluded (powerless/trapped) — formally eligible, practically disbelieved
 *   - philosophical_observers: analytical seat, traces the asymmetry without a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(phenomenological_endorsement_reading, 0.42).
domain_priors:suppression_score(phenomenological_endorsement_reading, 0.55).
domain_priors:theater_ratio(phenomenological_endorsement_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(phenomenological_endorsement_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(phenomenological_endorsement_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(phenomenological_endorsement_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(phenomenological_endorsement_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(phenomenological_endorsement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(phenomenological_endorsement_reading, tangled_rope).
narrative_ontology:human_readable(phenomenological_endorsement_reading, "Phenomenological Endorsement Test for Preference Authenticity").
narrative_ontology:topic_domain(phenomenological_endorsement_reading, "moral psychology / philosophy of autonomy / political theory of power").

domain_priors:requires_active_enforcement(phenomenological_endorsement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(phenomenological_endorsement_reading, 'f0b30bd2-dbf1-485e-8ad4-a194a8331d7f').
narrative_ontology:cs_kernel_codification('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', distributed).
narrative_ontology:cs_authority_grounding('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', expertise).
narrative_ontology:cs_interpretation_layer_present('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f').
narrative_ontology:cs_reading_relation('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', authentic_preference_boundary__behaviorist_counterfactual_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', authentic_preference_boundary__genealogical_origin_reading, influences).
narrative_ontology:cs_reading_relation('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', authentic_preference_boundary__capability_traction_reading, coexists_with).
narrative_ontology:cs_axiom('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', foundational, endorsement_is_genuine_detectable_mental_state).
narrative_ontology:cs_axiom_status(endorsement_is_genuine_detectable_mental_state, holdable).
narrative_ontology:cs_axiom_grounding('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', endorsement_is_genuine_detectable_mental_state, empirically_contingent).
narrative_ontology:cs_axiom('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', foundational, disciplined_introspection_is_universally_available_in_principle).
narrative_ontology:cs_axiom_status(disciplined_introspection_is_universally_available_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', disciplined_introspection_is_universally_available_in_principle, instrumental).
narrative_ontology:cs_reference_frame('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', introspective_access_thesis_standard).
narrative_ontology:cs_drift_state('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', contemporary_critical_theory_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f0b30bd2-dbf1-485e-8ad4-a194a8331d7f', '').
narrative_ontology:cs_kernel_id(phenomenological_endorsement_reading, authentic_preference_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(phenomenological_endorsement_reading, articulate_reflective_reasoners).
narrative_ontology:constraint_beneficiary(phenomenological_endorsement_reading, therapeutic_and_counseling_professions).
narrative_ontology:constraint_beneficiary(phenomenological_endorsement_reading, liberal_autonomy_theorists).
narrative_ontology:constraint_victim(phenomenological_endorsement_reading, inarticulate_or_traumatized_reporters).
narrative_ontology:constraint_victim(phenomenological_endorsement_reading, adaptive_preference_holders_under_scrutiny).
narrative_ontology:constraint_victim(phenomenological_endorsement_reading, populations_whose_introspection_is_pathologized).
narrative_ontology:constraint_vindicates(phenomenological_endorsement_reading, introspective_access_thesis).
narrative_ontology:constraint_vindicates(phenomenological_endorsement_reading, endorsement_as_genuine_mental_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the vocabulary, leisure, and training to produce a fluent second-order report distinguishing 'glad I don't want it' from 'didn't want it anyway.' Their preferences are readily certified as authentic because they can perform the disciplined introspective examination the test demands. Exit from the test's judgment is easy for them because they rarely fail it.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, articulate_reflective_reasoners, beneficiary,
    moderate, biographical, mobile, national).

% Administer and certify the disciplined introspective procedure — determine what counts as genuine reflective endorsement versus confabulation, run the interviews, write the assessments used in custody, competency, and consent determinations. Their professional and institutional standing depends on the test being real and administrable by them specifically.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, therapeutic_and_counseling_professions, agenda_setter,
    institutional, generational, arbitrage, national).

% Their introspective reports are halting, contradictory, or shaped by trauma responses that do not present as fluent reflective endorsement even when the underlying preference is genuinely held. Under this reading they are not definitionally excluded — the test is in principle available to them — but in practice their reports get read as confabulated or unreliable, and the burden of producing a convincing endorsement narrative falls on them without corresponding resources.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, inarticulate_or_traumatized_reporters, payer,
    powerless, biographical, trapped, local).

% People who report contentment with constrained circumstances (housewives under patriarchal norms, workers who have internalized low expectations) are subjected to the endorsement test to determine whether their stated satisfaction is 'genuine' or adaptive. The test purports to include them but its outcome is frequently that their endorsement is discounted as insufficiently reflective, regardless of what the phenomenology actually contains.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, adaptive_preference_holders_under_scrutiny, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(phenomenological_endorsement_reading, adaptive_preference_holders_under_scrutiny, excluded).

% Build political and legal theory on the premise that authentic preference is introspectively detectable, which lets autonomy-respecting policy avoid paternalism while still filtering out adaptive or coerced preferences. Their theoretical edifice benefits from the test being real and non-arbitrary; their academic and policy standing rides on the endorsement/non-endorsement distinction holding up under scrutiny.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, liberal_autonomy_theorists, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(phenomenological_endorsement_reading, liberal_autonomy_theorists, agenda_setter).

% Groups whose introspective reports are systematically read through a clinical or cultural lens that treats their stated endorsements as unreliable by default — psychiatric patients, colonized subjects whose preferences were long read as false consciousness, disabled people whose competence to self-report is routinely questioned. They would object that the test's 'in principle available to everyone' promise is not honored in practice, but their objection is filtered through the same discounting mechanism the test authorizes.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, populations_whose_introspection_is_pathologized, excluded,
    powerless, biographical, trapped, local).

% Examine whether disciplined introspection actually detects a distinct mental state of endorsement or whether the apparent detection is itself a socially trained performance that tracks articulateness rather than authenticity. They can trace who gets certified and who gets discounted without having a stake in either outcome.
narrative_ontology:constraint_stakeholder(phenomenological_endorsement_reading, philosophical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for distinguishing preferences an agent genuinely stands behind from preferences merely reported or behaviorally exhibited, which is needed wherever consent, competency, or welfare judgments require knowing whether a stated want is truly the agent's own — informed consent doctrine, therapeutic practice, and autonomy-respecting policy all need some such distinction to function.
% TRANSFER_FUNCTION: Moves epistemic authority over what counts as 'genuinely wanting something' toward whoever can perform fluent, disciplined introspective reporting and toward the professions certified to adjudicate that performance, and away from those whose reports are treated as inherently less reliable regardless of content.
% ABSENT_VOICES: Populations whose introspection is pathologized (psychiatric patients, colonized subjects historically read through false-consciousness frames, disabled people whose self-reports are routinely second-guessed) would object that the test's formal inclusiveness is not matched by practical inclusiveness — but their objection is itself often processed through the same discounting apparatus, so it rarely surfaces as a credited voice in the theory's own literature.
% DISAPPEARANCE_RATIONALE: If the phenomenological endorsement test disappeared as a criterion, informed-consent frameworks, therapeutic competency assessments, and autonomy-based policy justifications that currently cite introspective endorsement as the authenticity marker would need a different standard — likely reverting to purely behavioral or counterfactual tests, which would shift who counts as having an authentic preference in custody disputes, medical consent, and welfare economics.
% FOUNDING_PROBLEM: Ordinary preference-satisfaction accounts of welfare and autonomy cannot distinguish a preference an agent has been coerced or conditioned into from one they would still hold under ideal reflection — behaviorism and revealed-preference theory collapse this distinction entirely, and policymakers needed some way to say 'this person doesn't really want this' without simply overriding their stated choice by fiat.
% FOUNDING_PROBLEM_CORROBORATION: Clinical ethicists and disability-rights scholars attest from outside the beneficiary set that the distinction the test claims to detect is real in principle but is applied asymmetrically in practice — fluent reporters are believed, halting reporters are read as confabulating, and no independent instrument exists to check whether the asymmetry tracks genuine reflective failure or merely tracks who was trained to narrate reflection persuasively. No party outside the certifying professions and articulate beneficiaries attests that the asymmetry in practice matches the asymmetry the theory predicts.
narrative_ontology:disappearance_verdict(phenomenological_endorsement_reading, world_rearranges).
narrative_ontology:founding_problem_status(phenomenological_endorsement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(phenomenological_endorsement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(phenomenological_endorsement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(phenomenological_endorsement_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(phenomenological_endorsement_reading_tests).
:- end_tests(phenomenological_endorsement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine — there really is a difference between reflective endorsement and mere non-resistance, and the test does real epistemic work for many agents. Suppression is moderate-high (0.55) because the practical asymmetry in whose reports are credited functions as a soft gatekeeping mechanism even without formal exclusion. Theater ratio is comparatively low (0.28): the introspective procedure is not mostly performance, it does track something real for the beneficiary population, though its performative share grows over the measured interval as certifying professions increasingly rely on procedural markers (fluency, coherence of narrative) as proxies for the underlying mental state they cannot directly observe. Accessibility collapse is moderate (0.35) — reflecting the reading's own claim that alternatives (self-report without the disciplined procedure) are not fully foreclosed, only structurally disadvantaged.
 *
 * PERSPECTIVAL GAP:
 *   From the certifying professions' seat, the test is a genuine tangled_rope: it coordinates a real epistemic need (distinguishing genuine from adaptive preference) while requiring active enforcement (someone must administer and adjudicate) — enforcement that happens to fall disproportionately on less articulate reporters. From the trapped reporter's seat, the same structure looks like a snare: formally available, practically closed, with the closure dressed as a neutral procedural failure ('insufficiently reflective') rather than an asymmetry in whose narration is believed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those whose introspective performance already matches what certifiers expect — articulate reasoners, the professions doing the certifying, and the theorists whose frameworks depend on the distinction holding. Victims are those whose genuine endorsement does not present in the expected narrative form: trauma survivors, adaptively-satisfied agents under scrutiny, and populations whose self-reports are institutionally pathologized. The directionality gradient here tracks articulateness and institutional trust rather than formal eligibility — which is exactly the structural delta distinguishing this reading from the behaviorist/counterfactual sibling, where exclusion is definitional (the beggar and housewife are locked out by the test's own terms) rather than practical (locked out by asymmetric credibility).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing coerced/conditioned preference from genuinely endorsed preference — remains live; behaviorism and revealed-preference theory still cannot make this distinction, so the test's mandate has not become obsolete. What has drifted is the gap between the test's formal promise (available to all reflective agents) and its practical operation (credited unevenly by articulateness), which is a live-mandate-degraded-execution pattern rather than a dead-mandate-zombie-persistence pattern. This forecloses simply dismissing the reading as pure extraction: the coordination function is real and contested, not fictional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    introspection_detects_or_performs,
    'Does disciplined first-person examination genuinely detect a pre-existing mental state of endorsement, or does the examination itself construct/perform what gets reported as endorsement, such that fluency in the procedure is indistinguishable from possessing the state?',
    'Compare endorsement reports elicited through the standard disciplined procedure against reports elicited through radically different elicitation methods (non-verbal, embodied, cross-cultural) for the same underlying preference; convergence would support genuine detection, divergence tracking elicitation method would support performance.',
    'If detection, the beneficiary/victim asymmetry is a correctable measurement problem (train examiners to elicit more inclusively). If performance, the asymmetry is constitutive of the test itself and no amount of procedural refinement removes it — the reading would need to concede its central premise is closer to the genealogical reading''s territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(introspection_detects_or_performs, conceptual, 'Whether disciplined introspection detects endorsement or manufactures its report.').

omega_variable(
    articulateness_confound,
    'Is the observed correlation between narrative fluency and endorsement-certification evidence that fluent people genuinely endorse more often, or evidence that the certifying procedure cannot separate fluency from endorsement?',
    'Study populations matched on independent measures of genuine reflective capacity but differing in narrative fluency (e.g., due to language barriers, disability, trauma-related dissociation) and compare certification outcomes.',
    'If the confound is real and large, the reading''s claim that formal universality equals practical universality is false, and the reading structurally converges toward tangled_rope-with-hidden-exclusion rather than genuine rope; if small, the practical asymmetry is more a remediable implementation gap than a structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(articulateness_confound, empirical, 'Whether articulateness and endorsement are conflated by the test''s own instruments.').

omega_variable(
    kernel_framing_choice,
    'Is the authentic-preference-boundary kernel better read through this phenomenological framing (endorsement as detectable mental state) or through the counterfactual/genealogical framings (authenticity as behavioral or causal-historical fact), and what signals should guide that choice across contexts?',
    'No single resolution; different institutional contexts (medical consent, welfare economics, political theory) may warrant different readings, and the choice itself is a live methodological dispute in the literature rather than a settled fact.',
    'Adopting the counterfactual reading instead would definitionally exclude some of this story''s payer stakeholders (adaptive preference holders, inarticulate reporters) rather than practically disadvantaging them — changing the victim set''s composition and making the exclusion a formal rather than asymmetric-credibility matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Alternative kernel framings and what would change under each.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(phenomenological_endorsement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(phen_tr_t0, phenomenological_endorsement_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(phen_tr_t8, phenomenological_endorsement_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(phen_tr_t16, phenomenological_endorsement_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(phen_tr_t24, phenomenological_endorsement_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(phen_tr_t32, phenomenological_endorsement_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(phen_tr_t40, phenomenological_endorsement_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(phen_be_t0, phenomenological_endorsement_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(phen_be_t8, phenomenological_endorsement_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(phen_be_t16, phenomenological_endorsement_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(phen_be_t24, phenomenological_endorsement_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(phen_be_t32, phenomenological_endorsement_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(phen_be_t40, phenomenological_endorsement_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(phen_su_t0, phenomenological_endorsement_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(phen_su_t8, phenomenological_endorsement_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(phen_su_t16, phenomenological_endorsement_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(phen_su_t24, phenomenological_endorsement_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(phen_su_t32, phenomenological_endorsement_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(phen_su_t40, phenomenological_endorsement_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(phenomenological_endorsement_reading, identity_coordination).
narrative_ontology:affects_constraint(phenomenological_endorsement_reading, behaviorist_counterfactual_reading).
narrative_ontology:affects_constraint(phenomenological_endorsement_reading, genealogical_origin_reading).
narrative_ontology:affects_constraint(phenomenological_endorsement_reading, capability_traction_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the authentic_preference_boundary kernel, each a separately-authored constraint with its own ε and stakeholder structure per the ε-invariance principle. The phenomenological_endorsement_reading claims formal universality of eligibility while producing practical asymmetry by articulateness/credibility; the behaviorist_counterfactual_reading produces formal (definitional) exclusion of agents whose behavior cannot be counterfactually probed; the genealogical_origin_reading locates authenticity in causal history rather than any present state, sidestepping the introspection question entirely; the capability_traction_reading locates authenticity in present capability to act otherwise. All four should be read as distinct constraints linked by shared kernel, not as one constraint measured four ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
