% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology as Rhetorical Vocabulary for Policy Critique
 *   domain: epistemological/normative/institutional
 *
 * SUMMARY:
 *   The arrangement under contest is the circulation and use of the
 *   constraint typology vocabulary as an instrument of policy critique. This
 *   story instantiates the rhetorical_scaffold_reading of the kernel
 *   deferential_realism_ontology: on this reading the typology is a normative
 *   vocabulary whose categories are declared rather than discovered, whose
 *   classifications ride on judgments about legitimate and illegitimate
 *   beneficiaries, and whose value lies in persuasive power. The epsilon
 *   referent is the standing arrangement — the typology's actual operation in
 *   policy discourse — assessed by this reading's own lights, never by the
 *   diagnostic reading's standards. The sibling readings
 *   (immutable_diagnostic_reading, hybrid_pragmatic_reading) are separate
 *   constraints with their own files, linked through
 *   network.affects_constraints; they are deliberately not averaged into this
 *   story. The claim/metric gap is deliberate: the story CLAIMS scaffold
 *   (transitional rhetorical support with an implicit retirement) while the
 *   authored metrics describe the vocabulary's actual discursive operation —
 *   the engine measures the divergence. KEY AGENTS (by structural
 *   relationship): - academic_typology_theorists: Agenda-setter
 *   (organized/identity_locked) — maintains the categories, professionally
 *   fused with the framework - policy_advocates: Primary beneficiary
 *   (organized/mobile) — converts the labels into agenda leverage -
 *   civil_society_campaigners: Secondary beneficiary (moderate/mobile) —
 *   borrows the grammar for local disputes - labeled_mechanism_operators:
 *   Primary target (powerful/constrained) — bears reputational and defensive
 *   costs of being characterized - policy_decision_makers: Dual-positioned
 *   consumer (institutional/constrained) — receives ready-made frames,
 *   absorbs misclassification risk - empirical_program_evaluators: Excluded
 *   voice (organized/mobile) — would contest declarations with evidence but
 *   sits outside the venues - rhetorical_studies_scholars: Analytical
 *   observer (moderate/analytical) — tracks the vocabulary's movement without
 *   collecting from it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.18).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology as Rhetorical Vocabulary for Policy Critique").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemological/normative/institutional").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '29b6f09d-05ad-4b8c-8732-f1737e1046aa').
narrative_ontology:cs_kernel_codification('29b6f09d-05ad-4b8c-8732-f1737e1046aa', formalized).
narrative_ontology:cs_authority_grounding('29b6f09d-05ad-4b8c-8732-f1737e1046aa', practice).
narrative_ontology:cs_interpretation_layer_present('29b6f09d-05ad-4b8c-8732-f1737e1046aa').
narrative_ontology:cs_reading_relation('29b6f09d-05ad-4b8c-8732-f1737e1046aa', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('29b6f09d-05ad-4b8c-8732-f1737e1046aa', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('29b6f09d-05ad-4b8c-8732-f1737e1046aa', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('29b6f09d-05ad-4b8c-8732-f1737e1046aa', classification_is_normative_declaration, deontological).
narrative_ontology:cs_axiom('29b6f09d-05ad-4b8c-8732-f1737e1046aa', secondary, persuasive_efficacy_is_value_standard).
narrative_ontology:cs_axiom_status(persuasive_efficacy_is_value_standard, holdable).
narrative_ontology:cs_axiom_grounding('29b6f09d-05ad-4b8c-8732-f1737e1046aa', persuasive_efficacy_is_value_standard, instrumental).
narrative_ontology:cs_reference_frame('29b6f09d-05ad-4b8c-8732-f1737e1046aa', persuasive_normative_vocabulary).
narrative_ontology:cs_drift_state('29b6f09d-05ad-4b8c-8732-f1737e1046aa', contemporary_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29b6f09d-05ad-4b8c-8732-f1737e1046aa', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, civil_society_campaigners).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, academic_typology_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_operators).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_decision_makers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_decision_makers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_declaration_thesis).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, persuasive_efficacy_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and revise the category definitions, publish the framework's canonical statements, and adjudicate contested applications through journals, conferences, and working groups. Their standing, citation networks, and seminar economies are built around the framework's continued circulation. Leaving would mean rebuilding professional identity around a different research program.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, academic_typology_theorists, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, academic_typology_theorists, beneficiary).

% Campaign staff, NGO analysts, and movement communicators who reach for the typology's labels when pressing for reform of arrangements they oppose. The vocabulary lets scattered objections travel as a recognizable package, and gaining a hearing for a campaign often turns on getting its target discussed in these terms. Switching to another critical vocabulary — moral, economic, legal — is always available at modest cost.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates, beneficiary,
    organized, biographical, mobile, national).

% Grassroots organizers and community groups who borrow the labels to frame local disputes — a housing scheme, a utility tariff, a policing practice — in terms that connect to national debates. The framework gives small groups access to an established critical grammar without requiring technical analysis. Local framing needs can be met by any number of vocabularies, so departure is uncomplicated.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, civil_society_campaigners, beneficiary,
    moderate, biographical, mobile, local).

% Agencies, firms, and program administrators whose arrangements get characterized in the framework's critical categories by outside critics. They bear reputational damage, heightened scrutiny, and defensive communication costs whether or not the characterization survives examination. They cannot exit being described; their options are rebuttal, rebranding, litigation, and accommodation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_operators, payer,
    powerful, biographical, constrained, global).

% Legislators, regulators, and agency officials who encounter the typology through testimony, briefing papers, and press coverage. Ready-made critical categories save them analytic effort and supply usable frames for action; the same convenience exposes them to adopting characterizations that later fail review, with the correction costs landing on their calendars and budgets.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_decision_makers, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, policy_decision_makers, beneficiary).

% Quantitative evaluators and trial-based researchers who would point out where typological declarations outrun the underlying evidence. They publish in adjacent literatures and advise the same agencies, but they are rarely seated in the advocacy venues where the vocabulary circulates and hardens into talking points.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, empirical_program_evaluators, excluded,
    organized, biographical, mobile, global).

% Analysts of political language who track how classification vocabularies move through policy debate — which labels stick, which audiences they persuade, and what work the quantitative trappings perform. They take no side in the framework's disputes and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, rhetorical_studies_scholars, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_advocates).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives dispersed critics a shared, portable grammar for characterizing arrangements they oppose, so that scattered objections aggregate into recognizable campaigns without requiring shared data infrastructure or agreed measurements.
% TRANSFER_FUNCTION: Moves discursive authority and agenda-setting leverage from holders of measured evidence and from the self-descriptions of labeled institutions to advocates wielding the vocabulary's categories; moves reputational and scrutiny costs onto the labeled parties.
% ABSENT_VOICES: Empirical program evaluators and the professional staff of labeled institutions would object that categorical declarations outrun the underlying evidence, but they sit outside the advocacy venues — hearings, campaign briefings, op-ed circuits — where the vocabulary circulates and hardens into talking points.
% DISAPPEARANCE_RATIONALE: Critique would not stop — advocates would revert to moral, economic, and legal idioms within a season — but ongoing campaigns would lose their coordination shorthand, cross-movement recognition of parallel cases would slow, and the labeled parties would face less standardized scrutiny. The rearrangement is real but shallow: nothing structural depends on the vocabulary.
% FOUNDING_PROBLEM: Policy criticism was fragmented: each critic redescribed coercive or asymmetric arrangements from scratch in private idioms, so parallel cases went unrecognized and objections failed to accumulate into sustained pressure.
% FOUNDING_PROBLEM_CORROBORATION: Labeled institutions corroborate the vocabulary's operative role from the receiving end — their communications strategies now anticipate and rebut its categories, which they would not bother doing for inert terminology. Policy historians document the migration of its terms into regulatory testimony, and rival methodologists attest its circulation while disputing its warrant. No attesting source sits inside the benefiting advocacy community alone.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): the vocabulary imposes real costs — reputational damage and defensive expenditure on labeled parties, framing-sovereignty and correction costs on decision-makers — but participation is voluntary and most seats retain open exits, so the arrangement cannot pump harder than its voluntary uptake. Suppression is low (0.18) and is a raw structural property, unscaled by power or scope: no barrier prevents any actor from using rival vocabularies, and the mild conformist pressure inside advocacy circles is social, not structural. Accessibility collapse is low (0.22) because moral, economic, legal, and evaluative idioms remain fully available — understanding the typology does not close alternatives. Resistance is moderate (0.45): labeled operators litigate and counter-frame, and rival methodologists contest the framework's warrant in print. Theater is elevated (0.62) and rising: from this seat much of the quantitative apparatus functions as persuasion-by-form — scores lending scientific texture to declaratory acts — though a residual share aids prioritization. The measurement series run on one shared seven-point grid; no suppression_requirement series is authored because the enforcement picture is static — uptake is social diffusion, not coercive machinery whose build-up or decay this story tracks.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the advocate seat the vocabulary is enabling coordination — a shared grammar that aggregates scattered objection. From the operator seat the same vocabulary is unaccountable labeling: costs imposed by declaration, answerable to no measurement. From the evaluator seat it is measurement displacement — assertion crowding out evidence. From the theorist seat it is a life's work. The identity-lock mechanism binding the theorists is professional identity: citation economies, seminar networks, and career ladders are constituted through the framework, so exit means rebuilding a scholarly self; if that frame broke — if the community accepted the rhetorical account of its own instrument — maintenance would collapse quickly, since no material dependency holds it up. Suppression here is almost entirely structural-absence rather than internalization: rivals face no penalty for using other vocabularies, and no cognitive fusion binds the cost-bearing seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: policy_advocates and civil_society_campaigners take the vocabulary's gains (coordination shorthand, agenda leverage) at near-zero structural cost, and their mobile exits place them near the beneficiary pole. academic_typology_theorists administer the categories and collect standing, but their identity-locked exit keeps them short of pure-beneficiary positioning — administration plus fusion. Cost-bearer declarations map to high directionality: labeled_mechanism_operators bear the costs without consent and cannot exit being described. policy_decision_makers carry a dual position — they receive ready-made frames (benefit) and absorb misclassification risk (cost) — placing them near symmetric. No directionality_overrides are authored: the derivation chain already separates the seats cleanly because exit options differ sharply across seats at similar nominal power (mobile advocates versus constrained operators), which is exactly what the structural data should drive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented critique lacking a shared grammar — remains live wherever the vocabulary operates, so the mandate has not outlived its function and mandatrophy is not resolved. The scaffold claim carries an implicit sunset: a rhetorical instrument earns its keep during a contest and should retire when the contest settles; the omega scaffold_retirement_genuineness tests exactly this. The classification discipline cuts both ways: reading the vocabulary as pure extraction would erase the genuine coordination it provides critics; reading it as pure coordination would erase the unaccountable costs it imposes on labeled parties and the measurement it displaces. The R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges — a live mandate with dependent arrangements, so no zombie flag is expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading — rhetorical_scaffold_reading — of the kernel deferential_realism_ontology; what would the sibling readings change about this story''s structure?',
    'No empirical resolution: the reading assignment is a framing choice recorded here. The immutable_diagnostic_reading would replace constructed epsilon values with measured ones and treat misclassification as observational error; the hybrid_pragmatic_reading would split the story into a fixed-core component (physical and coordination constraints) and a contested-periphery component (normative beneficiary judgments).',
    'Under the diagnostic sibling, theater_ratio collapses toward zero and the claimed type shifts toward rope (an observational instrument); under the hybrid sibling, this story narrows to the contested-periphery slice with correspondingly higher constructed-extraction attribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in the epistemic status of classification acts.').

omega_variable(
    epsilon_constructed_vs_discovered,
    'Do the vocabulary''s categorical declarations track independently measurable features of the arrangements they characterize, or are they pure constructions of normative judgment?',
    'Corpus study correlating typological declarations with independent extraction measurements of the same arrangements; convergence would indicate diagnostic content this reading denies.',
    'Convergence would shift this story toward the hybrid reading''s structure and lower the theater_ratio; divergence would confirm the rhetorical account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_constructed_vs_discovered, empirical, 'Whether declarations carry discovered content or are constructed through normative judgment.').

omega_variable(
    quantitative_apparatus_functionality,
    'Is the framework''s quantitative apparatus — metric scores, computed classifications — genuinely performative (persuading by scientific appearance) or partially functional for prioritization?',
    'Ablation comparison: policy uptake of critiques issued with and without the quantitative trappings, holding substantive content constant.',
    'A large functional share would lower theater_ratio and weaken the scaffold claim toward rope; a negligible share confirms the trappings as rhetorical costume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quantitative_apparatus_functionality, empirical, 'Functional versus theatrical share of the framework''s measurement surface.').

omega_variable(
    scaffold_retirement_genuineness,
    'Does the vocabulary actually retire once the policy contests it supports settle, as the scaffold characterization requires, or does it persist as permanent critical infrastructure?',
    'Longitudinal tracking of vocabulary usage in policy domains where the original contest has conclusively ended.',
    'Persistence after settlement would falsify the sunset character and push the classification toward rope or tangled_rope; genuine retirement would confirm the scaffold claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_retirement_genuineness, empirical, 'Tests this story''s own claimed type: whether the rhetorical scaffold is genuinely transitional.').

omega_variable(
    counter_rhetoric_symmetry,
    'Labeled operators deploy mirror-image rhetoric casting critics as self-interested operators; does symmetric strategic labeling undermine the cost-bearing status of the operator seat?',
    'Asymmetry audit comparing resource stakes, repetition rates, and uptake of the two rhetorical campaigns.',
    'Genuine symmetry would move the operator seat toward symmetric directionality and soften the cost attribution; durable asymmetry preserves the current structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_rhetoric_symmetry, conceptual, 'Whether mirrored counter-rhetoric complicates the beneficiary/cost-bearer structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.34).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.51).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.56).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.6).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 24, 0.62).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 24, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferential_realism_ontology__rhetorical_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel, three readings, three files. The epsilon values differ by construction — the diagnostic reading discovers epsilon, the hybrid splits it, this reading constructs it — so the stories must never share a metric profile. The diagnostic reading is upstream in discourse: its measurement rhetoric supplies the scientific texture this reading's deployments borrow. Linked via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
