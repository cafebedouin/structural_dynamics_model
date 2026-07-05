% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Subordinated Speech Protection (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This story instantiates the dignity reading of the contested
 *   speech-harm-boundary kernel: the constitutional commitment that free
 *   expression is not the master value but is instead subordinate to human
 *   dignity, such that certain categories of speech (Holocaust denial, group
 *   defamation, dehumanizing hate propaganda) are excluded from protection
 *   categorically, without a case-by-case balancing test. This is the reading
 *   most associated with post-WWII German constitutionalism and its European
 *   derivatives. It is one of three siblings sharing the kernel: the
 *   absolutist reading (near-total protection, extremely high harm-override
 *   threshold) and the harm-balancing reading (presumptive protection
 *   yielding to proportionality analysis). Each sibling is a separate
 *   constraint with its own ε, beneficiary/victim structure, and
 *   classification — this file does not describe or average across them; it
 *   states only the dignity reading's own structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Subordinated Speech Protection (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '911fc89f-d31d-4320-8d89-11994b434f00').
narrative_ontology:cs_kernel_codification('911fc89f-d31d-4320-8d89-11994b434f00', formalized).
narrative_ontology:cs_authority_grounding('911fc89f-d31d-4320-8d89-11994b434f00', lineage).
narrative_ontology:cs_interpretation_layer_present('911fc89f-d31d-4320-8d89-11994b434f00').
narrative_ontology:cs_reading_relation('911fc89f-d31d-4320-8d89-11994b434f00', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('911fc89f-d31d-4320-8d89-11994b434f00', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('911fc89f-d31d-4320-8d89-11994b434f00', foundational, dignity_supremacy_over_expression).
narrative_ontology:cs_axiom_status(dignity_supremacy_over_expression, holdable).
narrative_ontology:cs_axiom_grounding('911fc89f-d31d-4320-8d89-11994b434f00', dignity_supremacy_over_expression, deontological).
narrative_ontology:cs_axiom('911fc89f-d31d-4320-8d89-11994b434f00', foundational, personhood_denial_categorically_unspeakable).
narrative_ontology:cs_axiom_status(personhood_denial_categorically_unspeakable, holdable).
narrative_ontology:cs_axiom_grounding('911fc89f-d31d-4320-8d89-11994b434f00', personhood_denial_categorically_unspeakable, deontological).
narrative_ontology:cs_reference_frame('911fc89f-d31d-4320-8d89-11994b434f00', post_atrocity_dignity_supremacy_framework).
narrative_ontology:cs_drift_state('911fc89f-d31d-4320-8d89-11994b434f00', contemporary_hate_speech_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('911fc89f-d31d-4320-8d89-11994b434f00', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, historically_targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, holocaust_survivor_communities).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_jurisprudence_courts).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, denialist_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, hate_speech_defendants).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, provocative_political_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional and criminal courts apply the categorical exclusion doctrine, adjudicating whether speech falls into the personhood-denying category (Holocaust denial, incitement to group hatred, dehumanizing propaganda). They administer the boundary, decide test cases, and their rulings define the excluded category's edges. They neither collect a rent nor bear a cost directly, but they hold the discretion that makes the whole structure operate.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_jurisprudence_courts, agenda_setter,
    institutional, generational, analytical, national).

% Groups with histories of genocide, expulsion, or systematic dehumanization (Jewish communities post-Holocaust, targeted ethnic and religious minorities) receive legal shelter from speech that denies their personhood or historical suffering. They cannot exit the polity that hosts the hostile speech, so the categorical bar functions as their primary structural protection rather than a discretionary courtesy.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, historically_targeted_minority_groups, beneficiary,
    organized, generational, constrained, national).

% Direct survivors and their descendants are named beneficiaries of denial-specific statutes. They are aging and cannot relitigate their claims outside the legal system that recognizes the harm; the categorical bar is their only durable recourse against public denial of the atrocity that defines their group history.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_survivor_communities, beneficiary,
    moderate, biographical, trapped, national).

% Individuals who assert Holocaust denial or comparable claims face criminal or civil liability regardless of their subjective sincerity or asserted historical argument. Under this reading their speech is not weighed against a harm threshold — it is categorically excluded from protection, so no balancing test, however favorable, can save them. Exit means self-censorship or emigration to a jurisdiction with the absolutist reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, denialist_speakers, payer,
    moderate, biographical, trapped, national).

% Speakers charged with group defamation or dehumanizing hate speech bear criminal penalties and civil damages. Because the exclusion is categorical rather than balanced, they cannot introduce a public-interest or artistic-value defense to escape liability once the speech is classified as personhood-denying.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, hate_speech_defendants, payer,
    moderate, biographical, constrained, national).

% Dissidents using harsh rhetoric against powerful groups or states risk being swept into the same categorical exclusion if courts read their rhetoric as dehumanizing rather than as legitimate political attack. They have no institutional voice in defining the boundary and no balancing test to fall back on; their speech is judged by the same doctrine built for genocide-denial cases.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, provocative_political_dissidents, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, provocative_political_dissidents, excluded).

% Free-expression advocacy groups argue the categorical approach forecloses case-by-case proportionality and risks chilling legitimate historical inquiry and political criticism. They litigate and lobby but do not control how courts define the excluded category; their objections are heard but structurally subordinate to the dignity framework once it is constitutionally entrenched.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, civil_liberties_organizations, excluded,
    organized, generational, constrained, national).

% Scholars compare the dignity reading against the absolutist and harm-balancing readings across jurisdictions (Germany, France, Canada versus the United States), documenting divergent outcomes on structurally similar speech acts and tracing how each reading's category boundary shifts over time.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared societal commitment that certain speech acts — those that deny a group's historical suffering or categorically dehumanize a class of persons — are excluded from the marketplace of ideas because their propagation itself constitutes an injury to dignity that no counter-speech can adequately remedy.
% TRANSFER_FUNCTION: Moves expressive latitude away from speakers whose speech is classified as personhood-denying and toward the psychological, reputational, and historical-memory security of the groups targeted by that speech; enforced through criminal liability, civil damages, and content removal obligations.
% ABSENT_VOICES: Civil liberties organizations and comparative-law critics object that the categorical bar removes judicial discretion to weigh context, satire, or historical scholarship, but their objections operate at the margins of an already-entrenched constitutional doctrine — they participate in litigation but do not set the category boundary.
% DISAPPEARANCE_RATIONALE: If the categorical exclusion vanished overnight, Holocaust-denial and group-defamation statutes across dignity-reading jurisdictions (Germany, France, several EU states) would become unenforceable as currently written; survivor communities and targeted minorities would lose their primary legal recourse against public dehumanization, while denialist and hate speech would shift from criminal liability to, at most, civil defamation or no liability at all — a substantial reorganization of both expressive freedom and group protection law.
% FOUNDING_PROBLEM: Post-WWII constitutional orders (especially West Germany) confronted the fact that formally neutral free-speech doctrine had provided cover for the propaganda apparatus that preceded and enabled genocide; the dignity reading was built to ensure that a legal order could never again treat dehumanizing speech as merely one more viewpoint in open debate.
% FOUNDING_PROBLEM_CORROBORATION: German constitutional court jurisprudence and post-war legal historians corroborate the founding problem as historically live and directly connected to the Basic Law's human-dignity clause. Independent comparative scholars (outside both the beneficiary groups and the enforcing courts) note the doctrine has since expanded well beyond genocide-adjacent speech into broader hate-speech and defamation contexts, which they read as the founding problem persisting in name while the category's actual scope has drifted toward general content regulation — a status the enforcing courts themselves do not concede.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 (moderate-high, rising over the interval) because the categorical exclusion imposes real, non-negotiable costs on speakers whose expression is classified as personhood-denying — there is no balancing escape valve, which is what makes this reading structurally different from harm_balancing_reading. Suppression is authored higher (0.72) because enforcement (criminal liability, content removal, civil damages) is active and non-discretionary once classification occurs. Theater ratio is low (0.22) because the doctrine performs a real function — it is not vestigial — though it is authored as rising slightly as the category's application drifts beyond its founding genocide-adjacent core into broader hate-speech contexts (a drift the founding_problem_corroboration section documents).
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and survivor communities are the structural beneficiaries: the exclusion transfers expressive latitude away from speakers and toward their dignity security, and they cannot exit the polity to escape whatever speech environment prevails, which sharpens the stakes of the protection they receive. Denialist speakers, hate-speech defendants, and dissidents whose rhetoric gets swept into the category are the targets — high ε, trapped or constrained exit, because the categorical (not balanced) nature of the exclusion removes any escape valve once classification occurs. Political dissidents are marked dually payer/excluded because they bear the cost of misclassification while having no voice in how the category's boundary is drawn.
 *
 * MANDATROPHY ANALYSIS:
 *   The categorical-exclusion reading resists mislabeling as pure extraction because it does solve a genealogically real coordination problem: a constitutional order recognizing that formally neutral speech doctrine could not prevent (and arguably enabled) genocidal propaganda. The tangled_rope classification captures both halves honestly — real coordination function (dignity protection for groups with no other recourse) and real asymmetric extraction (categorical, non-balanced restriction on classified speakers) operating through the same enforcement structure. The mandatrophy risk is category creep: the founding_problem_corroboration flags that comparative scholars see the doctrine's scope drifting from genocide-adjacent speech toward general hate-speech regulation, which is exactly the kind of mandate-outliving-its-function drift the framework is built to detect independent of either reading's self-assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_boundary_location,
    'Is the categorical exclusion mechanism itself distinguishable in practice from a harm-balancing test applied with a very low threshold for override, or does the categorical framing just relabel an implicit balancing judgment made once, at the point of classification, rather than case by case?',
    'Compare adjudicated outcomes across dignity-reading and harm-balancing-reading jurisdictions on structurally matched speech acts (same rhetoric, different courts) to determine whether the categorical framing produces systematically different outcomes or merely different procedural language for the same substantive judgment.',
    'If the categorical and balancing mechanisms converge on the same outcomes, the dignity reading''s distinctiveness is largely rhetorical/procedural rather than substantive, which would lower confidence in treating it as a structurally distinct kernel reading rather than a stylistic variant of harm_balancing_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_balancing_boundary_location, conceptual, 'Whether categorical exclusion is substantively distinct from a strict balancing test or merely relocates the balancing judgment.').

omega_variable(
    category_creep_scope_drift,
    'Has the personhood-denying speech category expanded beyond its founding genocide-adjacent core (Holocaust denial, incitement to genocide) into a broader general hate-speech regulation regime, and if so, at what point does the founding problem stop corroborating the doctrine''s current scope?',
    'Longitudinal doctrinal analysis of case law in dignity-reading jurisdictions, tracking the category''s boundary cases over decades against the founding legislative and constitutional history.',
    'If substantial scope drift is confirmed, the founding_problem_status shifts from contested toward dead-for-current-scope even while remaining live for the narrow original core, which would support a mandatrophy finding for the doctrine''s expanded applications specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_creep_scope_drift, empirical, 'Whether the categorical exclusion has expanded past its founding justification.').

omega_variable(
    cs_framing_kernel_vs_dignity_clause,
    'Is the correct commitment-system kernel the speech-harm-boundary itself, or is it the underlying human-dignity clause (e.g., Basic Law Art. 1) from which the speech exclusion is derived as a downstream application?',
    'Trace whether courts treat the dignity clause as independently binding and prior to the speech doctrine (suggesting dignity is the true kernel) or treat the speech exclusion as a freestanding doctrine that merely invokes dignity rhetorically (suggesting speech-harm-boundary is the kernel).',
    'If the dignity clause is the true kernel, this story is better modeled as a downstream constraint influenced by a prior dignity-clause constraint rather than as a direct reading of the speech-harm-boundary kernel — this would not change the ε or classification but would change the network/family structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_dignity_clause, conceptual, 'Alternative framing: dignity clause as the true kernel with speech exclusion as downstream application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__dignity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__dignity_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__dignity_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__dignity_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__dignity_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__dignity_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__dignity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__dignity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__dignity_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__dignity_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__dignity_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__dignity_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'speech harm boundary' per the ε-invariance principle: dignity_reading (this file, categorical exclusion, ε=0.58, tangled_rope), absolutist_reading (near-absolute protection, expected low ε, rope-leaning), and harm_balancing_reading (proportionality test, expected mid ε, tangled_rope-leaning but with an escape valve absent here). The three share a kernel (the constitutional location of the speech/dignity boundary) but instantiate structurally distinct constraints with different victim sets, different enforcement mechanisms, and different ε values — they are linked via affects_constraints, not merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
