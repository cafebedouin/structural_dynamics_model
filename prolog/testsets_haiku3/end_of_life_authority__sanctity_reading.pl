% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Reading: Intrinsic Value Prohibition on Assisted Dying
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading of end-of-life authority positions the
 *   intrinsic value of human life as a categorical imperative that supersedes
 *   individual preference. Intentional life-ending is prohibited regardless
 *   of the sufferer's autonomous request, the severity of their condition, or
 *   their demonstrated competence. This reading benefits institutional
 *   medical authority, religious frameworks, and disability advocates
 *   operating from a non-commodification position. It imposes costs on
 *   terminally ill and chronically suffering populations whose suffering the
 *   reading recognizes but whose requests for assistance it categorically
 *   denies. The constraint is presented as protective of vulnerable
 *   populations but operates through suppression of patient autonomy and
 *   physician discretion. The claim is tangled_rope (coordination function
 *   protecting the vulnerable + extraction via denial of autonomy and
 *   authority transfer); the metrics describe substantially extractive,
 *   actively enforced operation with rising theater (increasing proportion of
 *   enforcement that serves symbolic affirmation of sanctity principle rather
 *   than coercion prevention).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Reading: Intrinsic Value Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '5dbb59e9-69ba-410b-ad0a-daf2113cc64c').
narrative_ontology:cs_kernel_codification('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', fixed_text).
narrative_ontology:cs_authority_grounding('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', lineage).
narrative_ontology:cs_interpretation_layer_present('5dbb59e9-69ba-410b-ad0a-daf2113cc64c').
narrative_ontology:cs_reading_relation('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', foundational, intrinsic_human_value_categorical).
narrative_ontology:cs_axiom_status(intrinsic_human_value_categorical, holdable).
narrative_ontology:cs_axiom_grounding('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', intrinsic_human_value_categorical, deontological).
narrative_ontology:cs_axiom('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', foundational, vulnerability_requires_external_protection).
narrative_ontology:cs_axiom_status(vulnerability_requires_external_protection, holdable).
narrative_ontology:cs_axiom_grounding('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', vulnerability_requires_external_protection, deontological).
narrative_ontology:cs_axiom('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', secondary, bright_line_rule_necessary).
narrative_ontology:cs_axiom_status(bright_line_rule_necessary, holdable).
narrative_ontology:cs_axiom_grounding('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', bright_line_rule_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', sanctity_of_life_institutional_authority).
narrative_ontology:cs_drift_state('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', contemporary_neurorights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5dbb59e9-69ba-410b-ad0a-daf2113cc64c', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, institutional_medical_establishment).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_authority_structures).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_sanctity_aligned).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_patients_requesting_death).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_economically_pressured).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_populations_at_coercion_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face unbearable suffering at end of life and request physician-assisted death. Under the sanctity reading, their request is categorically denied regardless of their suffering or competence, justified by the intrinsic value of their life that supersedes their preference. They bear the cost of prolonged suffering they find unendurable. Exit options are nonexistent: covert methods carry legal risk, traveling to jurisdictions permitting assisted dying may be financially/physically impossible.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_patients_requesting_death, payer,
    powerless, immediate, trapped, local).

% Aging populations facing both medical decline and economic dependency on family or public systems. The sanctity reading protects them from coercion by denying all assisted-death access — but the protection is predicated on their identity as vulnerable persons needing external defense, not as autonomous agents capable of sound preference. They bear the cost of medical dependence and are structurally positioned as protected objects rather than decision-makers. Their exit from the protection framework means accepting the coercion risk the framework was designed to prevent.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_economically_pressured, payer,
    powerless, immediate, identity_locked, local).

% Live with chronic, non-terminal disabilities and face social pressure (explicit or internalized) that their lives are burdensome. The sanctity reading categorically prohibits assisted dying for non-terminal disability, which prevents coercion into death but also forecloses their expressed autonomy if they genuinely request it. They are protected by the same mechanism that denies their authority to make their own choice. Identity-locked: disability community solidarity networks and self-advocacy groups constitute their identity partly through resisting the ableist narrative that disabled lives are unworthy, making it difficult to publicly request death-option access without seeming to validate the ableist premise.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disabled_populations_at_coercion_risk, payer,
    moderate, biographical, constrained, national).

% Maintains a unified professional identity centered on life preservation. Under the sanctity reading, the physician's role is definitionally bounded: life-preservation is the non-negotiable mandate, never negotiable by patient preference. The constraint benefits medicine by eliminating a domain of contested decision-making and conferring institutional authority to define when life ends (via withdrawal of care, palliative sedation, etc.) while prohibiting the patient from directly requesting death. The medical establishment benefits from role clarity and the constraint's reinforcement of physician authority over end-of-life decisions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, institutional_medical_establishment, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, institutional_medical_establishment, agenda_setter).

% Religious traditions grounding human dignity in the sanctity of life — particularly Christian, Islamic, and Orthodox Jewish frameworks — have significant public authority in many jurisdictions to define the parameters of end-of-life policy. The sanctity reading enforces a religiously-grounded premise (intrinsic value of life supersedes individual preference) through secular law. Religious institutions benefit from the constraint's legal codification of their core moral claim, reaching populations beyond their direct congregational authority.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_authority_structures, beneficiary,
    institutional, civilizational, arbitrage, national).

% Disability rights organizations operating from a sanctity/non-commodification framework oppose assisted dying access as commodifying disability and signaling that disabled lives are not worth living. They benefit from the legal prohibition because it aligns law with their core advocacy position and shields disabled persons from coercive pressure. Their advocacy frames the prohibition as protective; they occupy beneficiary position by receiving reinforcement of their philosophical framing through law.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_advocacy_organizations_sanctity_aligned, beneficiary,
    organized, generational, mobile, national).

% A subset of terminally ill or chronically suffering patients whose suffering is genuine, whose decision-making capacity is confirmed, and whose preference for assisted dying is persistent and autonomous — but who are excluded from the conversation because the sanctity reading does not recognize this category as legitimately different from coercion cases. Their voices would challenge the categorical prohibition by embodying counterexamples (competent, non-coerced, persistent, suffering requests); they are absent from policy-making precisely because the reading's framework does not admit their framing as valid.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, patients_with_autonomous_preferences_for_death, excluded,
    powerless, immediate, trapped, local).

% Physicians who encounter patients in unbearable suffering requesting assistance in dying and experience the constraint between their role as life-preserver and their professional judgment that this specific patient is being harmed by continued life. Their voice is excluded because the sanctity reading does not permit the reconfiguration of professional conscience that would validate end-of-life assistance. They face legal liability and professional discipline if they assist, even when they believe non-assistance causes harm.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, medical_practitioners_conscience_conflicted, excluded,
    powerful, biographical, constrained, local).

% Examines end-of-life frameworks from multiple normative perspectives and produces empirical data on outcomes, coercion risks, and decision-making patterns. Takes testimony and research data from all other seats. Can publish analyses that support or challenge the sanctity reading's empirical assumptions. Positions as analytical observer without direct stake in outcome.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, bioethics_academic_community, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, institutional_medical_establishment).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, legally-binding framework for defining when death may occur: only when medical intervention is withdrawn or withheld, never when intentionally induced by a medical professional or third party at a patient's request. Solves the coordination problem of preventing coercive pressure on vulnerable people to request death by making all intentional life-ending categorically illegal and morally prohibited.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual patients to institutional gatekeepers (physicians, courts, religious authorities, disability advocacy organizations). Moves the authority to define what constitutes a life worth living from the suffering individual to external authorities operating under the sanctity-of-life principle. Moves the burden of unbearable suffering from the institutional system (which would have to justify denying a competent request) to the patient (who must endure what they find unendurable).
% ABSENT_VOICES: Terminally ill and chronically suffering patients whose autonomous, persistent requests for death are treated as invalid ipso facto because the sanctity reading does not recognize a category of competent, non-coerced, terminal or intractably suffering request as morally distinguishable from coercion cases. Physicians whose clinical judgment supports end-of-life assistance are excluded from policy conversation. Disability scholars who argue from autonomy and self-determination rather than from the non-commodification framework are marginalized. These voices would argue that the categorical prohibition protects the vulnerable at the cost of denying autonomy to the non-vulnerable, and that distinguishing cases by competence, persistence, and suffering-trajectory is possible.
% DISAPPEARANCE_RATIONALE: If the sanctity-reading prohibition vanished, jurisdictions would face immediate pressure to establish alternative frameworks (autonomy-based, harm-reduction, coercion-minimization). End-of-life decision-making would reorganize around different normative anchors. Medical institutions would need to reconfigure professional role definitions. Religious authority over secular law would be challenged. The disability advocacy community would split into competing frameworks. Vulnerable populations would face new coercion risks from different directions (market pressures, family financial interest, euthanasia expansion). The world would materially rearrange; this is not a natural law.
% FOUNDING_PROBLEM: Early-to-mid 20th century witnessed involuntary euthanasia programs (Nazi eugenics, forced sterilizations) that targeted disabled and marginalized populations. The founding problem: how to prevent institutional coercion into death while permitting end-of-life care decisions. The sanctity-of-life reading addresses this by making all intentional life-ending categorically prohibited, on the theory that a bright-line rule prevents slippage into coercive expansion.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights advocates and religious authorities attest the founding problem remains live: coercive pressures on vulnerable populations exist in jurisdictions with assisted-dying access, and expansion from terminal to non-terminal cases has occurred historically (Netherlands, Belgium, Canada). Medical ethicists and autonomy advocates attest the founding problem is substantially solved by modern legal safeguards (competence assessment, waiting periods, persistent-request verification) and that the categorical prohibition persists as institutional inertia and ideological commitment rather than empirical necessity. Empirical data from jurisdictions with assisted-dying frameworks show both coercion cases and non-coerced cases; the parties dispute whether the data supports the bright-line categorical rule or a discrimination-sensitive permissive framework.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers authority from patients to institutional gatekeepers and denies access to the only option many find acceptable for intolerable suffering. The transfer is not temporary or provisional — it is categorical and permanent. Suppression is higher (0.72) because the constraint must actively prevent patients from accessing covert or jurisdictional workarounds, must prevent physicians from assisting despite clinical judgment that assistance would reduce harm, and must maintain the prohibition despite persistent countervailing requests. Theater_ratio rises over the interval (0.28 to 0.43, then slight decline to 0.41) because enforcement shifts from direct coercion prevention (early period, when alternative jurisdictions were less accessible) to symbolic affirmation of sanctity principle (later period, as legal frameworks stabilize and the primary enforcement burden shifts to boundary maintenance — preventing test cases, policing rhetoric, disciplining dissident physicians). The slight decline at t=40 reflects a minor reversion as empirical coercion cases mount and theater value decreases. Accessibility_collapse is high (0.79): once the constraint is understood, patients seeking death have almost no alternatives within the jurisdiction (travel is often impossible, covert methods carry legal risk and physical danger). Resistance is moderate (0.58): strong resistance from autonomy advocates and suffering patients, weaker from the medical establishment (which benefits from role clarity) and conditional from disability advocates (whose opposition to commercialization is real but who also experience anxiety about whether opposing the prohibition validates ableism). Measurements are authored on a single shared time grid; all metrics are asserted at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the payer seats and the beneficiary seats should be extreme. Terminally ill patients requesting death experience the constraint as absolute prohibition justified by others' belief in their life's value — extraction of their authority and enforcement of their continued suffering. Medical gatekeepers and religious authorities experience the same constraint as legitimate authority conferral and moral principle codification — no extraction, only proper role definition. The engine computes this divergence from the structural data. The claimed type (tangled_rope) reflects the genuine coordination function (preventing coercive expansion) paired with genuine extraction (denial of autonomy for non-vulnerable cases). The metrics are authored honestly independent of the claim — if the metrics diverge from the claim, that divergence is exactly the signal the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Terminally ill and economically pressured elderly carry high directionality d (near 1.0, full target) because they are the seats the constraint explicitly forbids from acting on their preferences and who bear the extraction (denial of autonomy, continued suffering). Identity-locked elderly also sit at high d — their identity as protected/vulnerable objects is fused with institutional care frameworks, making exit unthinkable. Disabled populations sit at moderate-high d (0.65-0.75) because they are categorically denied access but also benefit from the protection against coercive expansion; their d is higher than beneficiaries' but modulated by genuine protective value. The institutional medical establishment sits at low d (0.1-0.25, near beneficiary end) because the constraint clarifies role, confers authority, and eliminates contested decision-making — though physicians with competing values (mercy, harm-reduction) carry higher d within their specialty. Religious authorities sit at very low d (near 0.0, subsidized) because the constraint codifies their core moral position into secular law without requiring them to bear its costs. Disability advocacy organizations oriented to the sanctity reading sit at very low d (subsidized) because their advocacy is reinforced by law and they do not bear the cost of denied autonomy access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing coercive euthanasia after historical atrocities) was real and the categorical prohibition was a defensible response. The founding problem status is contested: disability advocates and religious authorities attest it remains live (coercive pressures persist), while autonomy advocates and terminal-care practitioners attest it is substantially solved by modern safeguards and that the prohibition persists from institutional inertia rather than empirical necessity. The constraint exhibits mandatrophy: the founding problem (coercion prevention) has been partially superseded by modern legal safeguards (competence assessment, waiting periods, persistent-request verification, safeguarding incompetent populations through discrimination-based access restrictions rather than categorical denial). Yet the categorical prohibition persists because (a) institutional stakeholders benefit from its role-clarifying function, (b) religious authorities benefit from its codification of their principle, and (c) disability advocates committed to non-commodification use it strategically to prevent expansion. The constraint persists not because the founding problem requires it but because constituencies benefit from its persistence. The classification as tangled_rope (coordination + extraction) captures this: the protective coordination function is real but insufficient to justify the extraction once you distinguish competent autonomous requests from coercion cases. Mandatrophy is not fully resolved — the constraint remains functionally justified by the founding problem even as that problem has been partially addressed by alternative mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_constructed_principle,
    'Is the intrinsic value of human life a natural fact that the prohibition reflects, or a constructed principle that the prohibition enforces, or both inextricably?',
    'Genealogical analysis of the sanctity principle: trace its origins to specific religious and philosophical traditions and document how its adoption in secular law required active institutional work (legal coding, professional role redefinition, cultural rhetoric). Track jurisdictions that abandoned the principle and examine whether their societies exhibit different operative values or whether an alternative principle (autonomy, dignity-as-self-determination) fills the same role.',
    'If constructed, the beneficiary set is vindicated — institutional stakeholders actively maintain a principle they benefit from. If natural-law-like, mountain-classified. If both (constructed + normatively binding), the constraint remains tangled_rope but the mandatrophy analysis shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_vs_constructed_principle, conceptual, 'Whether the sanctity principle is a discovered natural law or an enforced institutional construction.').

omega_variable(
    suppression_internalization_trajectory,
    'Is the measured suppression (0.72) structural (legal barriers, professional sanctions) or internalized (patients accepting the principle, abandoning requests), or both in what proportions?',
    'Post-legalization study: track patients in jurisdictions that legalized assisted dying and examine whether their expressed preferences or decision-making patterns changed. Track patients in restrictive jurisdictions and examine whether their preferences persist post-externalization (if externally transported to permissive jurisdiction, do they maintain their request or revise). Examine physician narratives: do they report suppressing their own judgment or accepting the legitimacy of the prohibition?',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests — patients carry the suppression with them even in permissive contexts, indicating identity-fusion. If primarily structural, the suppression is authentic external coercion and would collapse if barriers were removed. If mixed, the constraint operates through both mechanisms and remedies would need to address both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression is structural or internalized or both.').

omega_variable(
    coercion_prevention_efficacy,
    'Does the categorical prohibition actually prevent coercive pressure on vulnerable populations, or does it merely displace coercion to different mechanisms (family pressure to accept suffering in silence, economic pressure to not seek jurisdictional exit, internalized shame)?',
    'Comparative analysis of coercion rates and types in permissive vs. restrictive jurisdictions, controlling for socioeconomic factors. Examine whether vulnerable populations in restrictive jurisdictions report experiencing less coercive pressure or different coercive pressure (family emotional manipulation instead of family pressure to request death, etc.). Examine whether the categorical prohibition prevents the coercion it claims to prevent or redistributes it.',
    'If the prohibition reduces net coercive pressure, it is more justified as protective. If it merely displaces coercion or creates new coercive mechanisms (pressure to accept suffering, pressure to travel), the protective value is weaker and the extraction component more visible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_prevention_efficacy, empirical, 'Whether the prohibition prevents coercion or displaces it.').

omega_variable(
    kernel_reading_boundaries,
    'Is this reading''s core claim (intrinsic value prohibits intentional life-ending) genuinely incompatible with the autonomy reading (individual preference grounds life-ending authority), or can a framework hold both (sanctity of life as a value + autonomy as a decision-making right for competent agents)?',
    'Examine whether philosophical traditions (natural law, religious ethics, bioethics frameworks) have articulated hybrid positions that assert both the sanctity of life AND respect for autonomous choice. If hybrid positions exist and are internally coherent, the readings coexist rather than foreclose. If no coherent hybrid exists, the forecloses relation is accurate.',
    'If coexist, this reading and the autonomy reading are different institutional commitments that different parties hold; they are not logically incompatible, just institutionally divided. If foreclose, one reading''s core premise logically eliminates the other''s core premise within any single framework, and the distinction matters for mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundaries, conceptual, 'Whether the sanctity and autonomy readings are logically incompatible or can coexist within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__sanctity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__sanctity_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__sanctity_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__sanctity_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__sanctity_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__sanctity_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__sanctity_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__sanctity_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__sanctity_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The constraint family end_of_life_authority contains three structurally distinct readings of the same kernel (persistent commitment to resolve end-of-life authority). Sanctity_reading asserts intrinsic life value prohibits intentional death; autonomy_reading asserts individual preference grounds decision authority; slippery_slope_mechanism documents empirical expansion patterns beyond founding cases. Each reading has distinct ε, distinct beneficiary/victim structure, distinct computed type. They are not the same constraint viewed from different angles — their ε values differ by a substantial margin because they have different measurement referents (the standing arrangement as the reading's own lights assess it) and different failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
