% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Subordinated Speech Boundary (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This story generates the dignity reading of the speech_harm_boundary
 *   kernel: the view, exemplified by postwar German Basic Law jurisprudence,
 *   Canadian Charter equality doctrine, and post-genocide constitutional
 *   orders, that speech protection is subordinate to human dignity and that
 *   personhood-denying speech (Holocaust denial, group defamation,
 *   dehumanizing hate speech) is categorically excluded from protection
 *   rather than weighed case-by-case against a harm threshold. This is a
 *   single, self-contained reading — it does not describe the near-absolute
 *   protection reading or the harm-balancing reading as internal features of
 *   itself; those are separate constraints (absolutist_reading,
 *   harm_balancing_reading) linked only through the shared kernel_id and the
 *   network edges below.
 *
 * KEY AGENTS:
 *   - targeted_minority_groups: beneficiary of categorical protection, organized/constrained
 *   - holocaust_survivor_communities: paradigm beneficiary class, moderate/trapped
 *   - denialist_speakers: primary target of categorical exclusion, moderate/constrained
 *   - hate_speech_defendants: bear liability without incitement showing, moderate/constrained
 *   - provocative_political_commentators: chilled at the boundary, moderate/constrained
 *   - dignity_jurisprudence_scholars: doctrinal agenda-setters, institutional/analytical
 *   - prosecutors_and_enforcement_bodies: discretionary boundary administrators, institutional/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.66).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Subordinated Speech Boundary (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '1d99a9e7-5b54-43dd-9c87-212d8fce35af').
narrative_ontology:cs_kernel_codification('1d99a9e7-5b54-43dd-9c87-212d8fce35af', formalized).
narrative_ontology:cs_authority_grounding('1d99a9e7-5b54-43dd-9c87-212d8fce35af', lineage).
narrative_ontology:cs_interpretation_layer_present('1d99a9e7-5b54-43dd-9c87-212d8fce35af').
narrative_ontology:cs_reading_relation('1d99a9e7-5b54-43dd-9c87-212d8fce35af', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('1d99a9e7-5b54-43dd-9c87-212d8fce35af', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('1d99a9e7-5b54-43dd-9c87-212d8fce35af', foundational, dignity_is_prior_to_expressive_liberty).
narrative_ontology:cs_axiom_status(dignity_is_prior_to_expressive_liberty, holdable).
narrative_ontology:cs_axiom_grounding('1d99a9e7-5b54-43dd-9c87-212d8fce35af', dignity_is_prior_to_expressive_liberty, deontological).
narrative_ontology:cs_axiom('1d99a9e7-5b54-43dd-9c87-212d8fce35af', foundational, personhood_denial_is_categorically_not_speech_the_right_protects).
narrative_ontology:cs_axiom_status(personhood_denial_is_categorically_not_speech_the_right_protects, holdable).
narrative_ontology:cs_axiom_grounding('1d99a9e7-5b54-43dd-9c87-212d8fce35af', personhood_denial_is_categorically_not_speech_the_right_protects, deontological).
narrative_ontology:cs_reference_frame('1d99a9e7-5b54-43dd-9c87-212d8fce35af', post_atrocity_constitutional_settlement).
narrative_ontology:cs_drift_state('1d99a9e7-5b54-43dd-9c87-212d8fce35af', contemporary_hate_speech_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d99a9e7-5b54-43dd-9c87-212d8fce35af', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, holocaust_survivor_communities).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_jurisprudence_scholars).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, denialist_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, hate_speech_defendants).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, provocative_political_commentators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups historically subject to genocide, defamation, or systematic dehumanization. Under this reading, they are shielded from categories of speech that deny their equal personhood — the constraint removes those utterances from the protected sphere entirely rather than balancing them against a speaker's interest. They cannot litigate every instance of ordinary bigotry, but the categorical exclusions (denial, group libel, incitement to dehumanization) are available to them and to prosecutors on their behalf without a case-by-case harm showing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, targeted_minority_groups, beneficiary,
    organized, generational, constrained, national).

% A specific beneficiary class named because Holocaust denial statutes are the paradigm case for this reading. Survivors and descendants cannot make historical fact-denial disappear through ordinary counter-speech; the constraint removes denial from protected expression by legislative and judicial fiat rather than leaving it to public debate. Their exit option is not really exit — the harm is denial of a shared historical fact, which no individual counter-speech act fully repairs.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, holocaust_survivor_communities, beneficiary,
    moderate, generational, trapped, national).

% Individuals who deny, minimize, or relativize documented atrocities. Under this reading they face criminal or civil liability regardless of sincerity of belief or absence of demonstrated downstream harm in a given instance — the category itself is unprotected. Their only exit is silence or relocation to a jurisdiction that does not adopt this reading; there is no argument-based path back into protection once the utterance is categorized.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, denialist_speakers, payer,
    moderate, biographical, constrained, national).

% Speakers whose statements are found to deny the equal personhood or worth of a protected group (group defamation, dehumanizing rhetoric). They bear prosecution or civil suit without the ordinary requirement of showing incitement to imminent harm — the dignity violation itself is the offense. Exit requires either compliance (self-censorship) or migration to a jurisdiction organized around the absolutist or harm-balancing reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, hate_speech_defendants, payer,
    moderate, biographical, constrained, national).

% Commentators whose satire, provocation, or harsh political rhetoric risks classification as dignity-violating even absent genocidal intent. They experience chilling effects at the boundary of the categorical exclusion because the line between protected harsh criticism and unprotected dehumanization is drawn by courts and prosecutors after the fact. They would object that the categorical approach lacks the calibration a balancing test would offer, but their objection is heard mainly in appellate argument, not in the initial charging decision.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, provocative_political_commentators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, provocative_political_commentators, excluded).

% Legal scholars, constitutional courts, and legislative drafters who articulate and administer the dignity-subordination framework (post-war German Basic Law jurisprudence, Canadian Charter equality provisions, various post-genocide constitutions). They set the categorical boundaries, write the doctrinal tests, and could in principle narrow or widen the excluded-speech categories. They bear no direct cost from the boundary they administer.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_jurisprudence_scholars, agenda_setter,
    institutional, civilizational, analytical, national).

% State bodies that bring cases under hate speech and denial statutes. They exercise discretion over which utterances are charged as falling within the categorical exclusion, which gives them substantial power to shape the boundary's practical reach independent of the doctrinal text.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, prosecutors_and_enforcement_bodies, agenda_setter,
    institutional, biographical, analytical, national).

% Legal systems and scholars organized around the sibling readings (near-absolute protection, or harm-balancing) are not parties to this jurisdiction's doctrine but are structurally implicated — comparative law debates cite this reading's categorical exclusions as either a model or a cautionary example. They have no voice within this constraint's own adjudication.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_and_balancing_jurisdictions, excluded,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, litigable line below which certain utterances are removed from the field of protected expression entirely, so that courts and legislatures do not have to re-litigate, case by case, whether denying a group's equal personhood is protected speech — dignity is treated as a threshold condition for participation in the discourse the speech right protects.
% TRANSFER_FUNCTION: Moves protection away from speakers whose utterances deny the equal personhood of an identifiable group, and moves security from ordinary case-by-case harm litigation toward targeted groups — those groups need not prove downstream harm to obtain suppression of the utterance; the categorical exclusion does that work for them.
% ABSENT_VOICES: Speakers in jurisdictions with the absolutist or harm-balancing reading, and civil libertarians within this same jurisdiction who argue the categorical approach lacks calibration, are not decision-makers in the doctrinal process — their objections appear in academic literature and dissenting opinions but do not control charging or drafting decisions.
% DISAPPEARANCE_RATIONALE: If the categorical dignity exclusion vanished, denial and group-defamation statutes would need to be rewritten as ordinary harm-balancing or incitement doctrine, prosecutors would lose a charging category that currently requires no individualized harm showing, and targeted communities would need to rely on civil defamation or the harm-balancing test's higher threshold instead of a categorical bar — a substantial doctrinal and practical rearrangement.
% FOUNDING_PROBLEM: Post-genocide and post-authoritarian legal orders (postwar Germany, post-apartheid South Africa, post-genocide Rwanda) needed a doctrine that would not require the state to wait for a satisfied harm-balancing test before intervening against speech whose function was the delegitimization of a group's right to exist as equal persons — the founding intuition was that some speech accelerates atrocity in ways later harm-balancing cannot undo after the fact.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historians and genocide-prevention scholars outside the direct beneficiary groups attest the founding problem (delegitimization preceding mass violence) remains empirically live, citing contemporary incitement patterns. Free-expression scholars and comparative constitutional theorists, also outside the beneficiary set, attest the doctrine has in practice expanded well beyond its founding atrocity-prevention rationale into broader political speech regulation, making its current operation contested rather than settled.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.58 — substantial but not maximal — because the categorical exclusion removes an entire class of expression from protection without requiring individualized proof of harm, which is a real cost imposed on speakers within that class, but the class itself (personhood-denying speech) is narrower than ordinary offensive or unpopular speech. Suppression (0.66) exceeds extraction because the categorical approach forecloses the case-by-case argument path entirely — a denialist speaker cannot argue their way back into protection by showing their statement caused no harm in this instance, which is a stronger suppressive mechanism than a balancing test's rebuttable presumption. Theater ratio is low (0.22) and rising modestly: enforcement is substantially functional (real prosecutions, real doctrinal work) but a growing share of the doctrine's public defense in academic and political discourse serves legitimation rather than adjudication.
 *
 * PERSPECTIVAL GAP:
 *   From the dignity_jurisprudence_scholars' seat, the categorical exclusion is coordination: it resolves in advance a question (does this speech deny equal personhood) that would otherwise require re-litigating first principles in every case, and it does so in service of a genuine value (post-atrocity social stability, equal standing of vulnerable groups). From the denialist_speakers' and hate_speech_defendants' seats, the same structure is extraction of their expressive liberty without individualized justification — the categorical form is precisely what removes their ability to contest the harm finding. The engine's per-seat computation should reflect this asymmetry: the agenda-setter and beneficiary seats see coordination, the payer seats see enforced extraction, and both are structurally accurate to their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and Holocaust survivor communities are structural beneficiaries: the categorical exclusion transfers the burden of proof away from them entirely, removing the need to demonstrate harm in each instance. Denialist speakers and hate speech defendants are structural targets: they bear liability for the classified utterance itself, with no balancing-test escape valve. Provocative political commentators occupy an intermediate position — not the doctrine's intended target, but caught in the boundary's uncertainty, which produces a chilling effect the doctrine's architects would likely characterize as an acceptable cost rather than a design goal.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — atrocity-adjacent delegitimizing speech accelerating mass violence — is contested as either still live (per genocide-prevention scholars) or substantially expanded beyond its original scope into broader political speech regulation (per free-expression scholars), which is exactly the founding_problem_status: contested verdict this story authors. Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating the doctrine as pure extraction (ignoring its genuine coordination function in post-genocide contexts) or treating it as natural/inevitable (ignoring that it is one of three live competing readings of the same underlying kernel, each defensible, each producing different victim sets).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_calibration,
    'Does the categorical exclusion approach actually produce better outcomes (less atrocity-adjacent speech, less erroneous suppression of legitimate criticism) than a harm-balancing approach would, or does the categorical form simply relocate the calibration problem to the boundary-drawing stage (what counts as ''personhood-denying'')?',
    'Comparative empirical study of prosecution patterns and appellate reversal rates in categorical-exclusion jurisdictions versus harm-balancing jurisdictions, controlling for underlying speech-harm base rates.',
    'If categorical exclusion produces no better calibration than balancing (similar false-positive and false-negative rates at the boundary), the dignity reading''s claimed advantage (certainty, no re-litigation) would be undercut and the extraction on boundary-case speakers would appear less justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_balancing_calibration, empirical, 'Whether categorical exclusion outperforms balancing or merely relocates the calibration problem.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice among the three readings (absolutist, dignity, harm-balancing) itself determined by prior historical trauma (post-genocide states adopt dignity readings) rather than by an independently defensible normative theory of speech, such that the reading is more a symptom of national history than a free philosophical choice?',
    'Comparative constitutional history tracing adoption patterns of each reading against national experience of genocide, authoritarianism, or civil conflict.',
    'If reading choice tracks trauma history strongly, that supports treating the dignity reading as a path-dependent institutional response rather than a universally superior framework — relevant to how strongly the categorical exclusion should be exported to jurisdictions without that history.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading selection is driven by national trauma history or by independent normative reasoning.').

omega_variable(
    boundary_drift_scope_creep,
    'Has the categorical exclusion, originally scoped to genocide-adjacent denial and explicit group defamation, expanded over time to cover broader categories of offensive or provocative political speech, and if so is that expansion a natural extension of the founding dignity principle or a distinct extraction dynamic riding on the original doctrine''s legitimacy?',
    'Doctrinal history tracing the categories of speech actually prosecuted under dignity-exclusion statutes from initial enactment to present, coded for distance from the paradigm genocide-denial case.',
    'Confirmed scope creep would support reclassifying later-era enforcement as tangled_rope with a growing extraction component riding on a narrower, still-legitimate coordination core — consistent with the rising base_extractiveness trajectory authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_drift_scope_creep, empirical, 'Whether the categorical exclusion''s practical scope has expanded beyond its founding rationale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__dignity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__dignity_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__dignity_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__dignity_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__dignity_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__dignity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__dignity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__dignity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__dignity_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__dignity_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__dignity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__dignity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__dignity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__dignity_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__dignity_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__dignity_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__dignity_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__dignity_reading, suppression_requirement, 40, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the shared speech_harm_boundary kernel. absolutist_reading holds that expressive liberty is near-lexically prior to dignity claims, making the categorical exclusion this story authors illegitimate in that framework — hence the 'forecloses' relation authored above (the two core premises, dignity-is-prior versus liberty-is-near-absolute, cannot both hold in one adjudicating framework). harm_balancing_reading shares this reading's premise that dignity matters but rejects the categorical form in favor of case-by-case proportionality — the two readings coexist as live competing doctrinal families across jurisdictions (e.g., contrasted in comparative constitutional law between German dignity-jurisprudence and post-Sullivan American balancing, or Canadian Oakes-test proportionality). Each reading carries its own ε, its own beneficiary/victim structure, and its own claimed_type; they are not merged and this file's ε (0.58) is not comparable across readings without accounting for the differing referents each reading's own lights establish.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
