% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology as Rhetorical Scaffold for Policy Critique
 *   domain: epistemology/normative theory/institutional design
 *
 * SUMMARY:
 *   This story instantiates one reading of the deferential_realism_ontology
 *   kernel: the claim that the constraint typology itself is not a
 *   measurement instrument but a normative vocabulary whose categories are
 *   declared, not discovered. On this reading, 'snare' functions the way
 *   'oppressive' or 'unjust' functions in ordinary moral argument — except
 *   that it borrows the syntax of empirical classification (metrics,
 *   thresholds, gates) to lend its normative verdicts an appearance of
 *   discovered fact. The constraint being classified here is the typology's
 *   OWN social operation as a rhetorical technology, not any particular
 *   object-level policy it is used to classify. Two sibling readings exist as
 *   separate constraints: the immutable_diagnostic_reading (the typology is a
 *   fixed-referent instrument; misclassification is correctable error) and
 *   the hybrid_pragmatic_reading (fixed core, contested periphery). Those are
 *   different constraints with different epsilon values, linked here via
 *   network.affects_constraints — this file does not average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.22).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology as Rhetorical Scaffold for Policy Critique").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative theory/institutional design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'a895f51f-37dc-49c2-bf47-728754a776cc').
narrative_ontology:cs_kernel_codification('a895f51f-37dc-49c2-bf47-728754a776cc', distributed).
narrative_ontology:cs_authority_grounding('a895f51f-37dc-49c2-bf47-728754a776cc', practice).
narrative_ontology:cs_interpretation_layer_present('a895f51f-37dc-49c2-bf47-728754a776cc').
narrative_ontology:cs_reading_relation('a895f51f-37dc-49c2-bf47-728754a776cc', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('a895f51f-37dc-49c2-bf47-728754a776cc', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('a895f51f-37dc-49c2-bf47-728754a776cc', foundational, classification_is_normative_declaration_not_discovery).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration_not_discovery, holdable).
narrative_ontology:cs_axiom_grounding('a895f51f-37dc-49c2-bf47-728754a776cc', classification_is_normative_declaration_not_discovery, conventional).
narrative_ontology:cs_axiom('a895f51f-37dc-49c2-bf47-728754a776cc', secondary, framework_value_lies_in_persuasive_efficacy).
narrative_ontology:cs_axiom_status(framework_value_lies_in_persuasive_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('a895f51f-37dc-49c2-bf47-728754a776cc', framework_value_lies_in_persuasive_efficacy, instrumental).
narrative_ontology:cs_reference_frame('a895f51f-37dc-49c2-bf47-728754a776cc', critique_vocabulary_as_practice_derived_authority).
narrative_ontology:cs_drift_state('a895f51f-37dc-49c2-bf47-728754a776cc', contemporary_framework_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a895f51f-37dc-49c2-bf47-728754a776cc', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates_using_snare_label).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, framework_authors_and_credentialed_interpreters).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snare_without_recourse).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_audiences_relying_on_typology_as_measurement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy the typology's vocabulary — especially the word 'snare' — to reframe existing arrangements (predatory lending, licensing regimes, platform fees) as illegitimate extraction requiring intervention. The label does persuasive work that a purely descriptive term would not: calling something a snare recruits the framework's apparent rigor on behalf of a normative conclusion the advocate already holds. They can walk away from the vocabulary and use other rhetoric if the typology loses persuasive force.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, reform_advocates_using_snare_label, beneficiary,
    organized, biographical, mobile, national).

% Maintain and adjudicate the typology's application — deciding, in contested cases, whether a mechanism counts as tangled_rope or snare. On this reading, that adjudication is openly a normative judgment about which beneficiaries are illegitimate, not a measurement of a fixed referent. They administer the vocabulary's credibility and benefit from its uptake as a critique tool; they can revise category boundaries at will since nothing external anchors them.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_authors_and_credentialed_interpreters, agenda_setter,
    institutional, generational, arbitrage, global).

% Are the targets of 'snare' classification in specific policy debates — a lender, a licensing board, a platform. On this reading they cannot contest the classification empirically, because the classification is declared rather than measured: there is no independent instrument to which they can appeal. They can lobby, litigate reputational harm, or attempt to relabel themselves, but the typology itself offers no falsification procedure they can invoke.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutions_labeled_snare_without_recourse, payer,
    powerful, biographical, constrained, national).

% Legislators, journalists, and voters who encounter the typology's outputs (a given policy classified as 'snare' or 'rope') and treat the classification as if it were a discovered fact about mechanism structure, when on this reading it is a normative verdict dressed in diagnostic language. They bear the cost of mistaking advocacy for measurement — voting or legislating on the basis of a label whose apparent rigor exceeds its actual epistemic warrant.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_audiences_relying_on_typology_as_measurement, payer,
    moderate, immediate, constrained, national).

% Hold that the typology is an observational instrument with fixed referents and that misclassification is a correctable error, not a matter of normative declaration. Under this reading their position is treated as itself one more rhetorical move — a claim to authority through the language of measurement — rather than as a genuine alternative epistemic warrant, so their objection is heard but structurally discounted within this reading's own terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading_proponents, excluded,
    institutional, generational, analytical, global).

% Notes that the typology functions here as vocabulary rather than instrument: it wins arguments by supplying moral weight to labels ('snare,' 'mountain') that sound like discoveries but are declared through the same normative reasoning the vocabulary is meant to police in others.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies advocates and critics with a shared, apparently rigorous vocabulary for describing power arrangements, which lowers the cost of persuading an audience that a given arrangement is illegitimate — coordination among people who already share a normative stance, achieved through common terminology rather than independent verification.
% TRANSFER_FUNCTION: Moves reputational and political capital from institutions labeled 'snare' toward the advocates and interpreters who apply the label — the typology's persuasive force is the transfer mechanism, converting a normative judgment into what reads publicly as an empirical finding.
% ABSENT_VOICES: Institutions on the receiving end of a 'snare' classification have no independent instrument to appeal to; the immutable-diagnostic camp is present in the debate but its claim to measurement is treated, on this reading, as itself rhetorical rather than as a genuine falsification path.
% DISAPPEARANCE_RATIONALE: If the typology's rhetorical authority vanished — if 'snare' stopped functioning as a term that borrows credibility from measurement-sounding language — advocates would need to make their normative case directly, institutions currently labeled snare would lose one avenue of reputational attack, and policy debates would have to rest explicitly on contested values rather than on an apparently discovered classification.
% FOUNDING_PROBLEM: Policy critique needed a vocabulary sharper than 'this is unfair' — a way to name structural illegitimacy that sounds like diagnosis rather than opinion, so that normative claims about who benefits illegitimately from an arrangement could travel with the persuasive weight of an empirical finding.
% FOUNDING_PROBLEM_CORROBORATION: Advocates and framework authors (the benefiting parties) attest the vocabulary does real critical work. Outside corroboration is thinner and contested: the immutable-diagnostic camp explicitly denies that the founding problem was ever 'we need better rhetoric' rather than 'we need better measurement,' and independent philosophers of science who have examined normative-classification frameworks note the same persuasive function without necessarily endorsing it as legitimate — so the founding problem's live status is attested mainly from inside the reading that benefits from it.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, rising over the interval) reflects the real transfer this reading identifies: reputational and political capital moving from labeled institutions to advocates and interpreters, amplified as the vocabulary's uptake grows and its persuasive leverage compounds. Suppression is authored LOW (0.22) deliberately — this is the reading's structural signature: alternative framings (the diagnostic reading, the hybrid reading, plain normative argument without typological dressing) are not blocked, merely out-competed rhetorically. Nothing stops an institution from arguing 'this is not actually a snare' in the immutable-diagnostic idiom; the typology does not suppress that move, it just wins the argument more often when dressed as measurement. Theater ratio (0.4, rising) captures the growing share of the typology's use that is performative classification-as-verdict rather than genuine structural analysis. Accessibility collapse is LOW (0.3) — precisely because this reading holds alternatives are not foreclosed, only disadvantaged in persuasive contest. Resistance is moderate-high (0.55): the diagnostic camp and labeled institutions push back hard, but cannot invoke an independent falsification procedure the rhetorical-scaffold reading recognizes as binding.
 *
 * PERSPECTIVAL GAP:
 *   From the framework-author seat, applying 'snare' to a mechanism is doing honest normative work openly declared as such (or at least is coordination among the like-minded). From the labeled-institution seat, the same act is being subjected to a verdict dressed as diagnosis, with no appeal. The engine computes these as different seat-classifications from the same structural data; this reading does not resolve the gap, it names where it is located — in the gap between declaring a category and discovering one.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework authors/interpreters and reform advocates sit at the beneficiary end: the typology's ambiguity between description and declaration is precisely what gives their normative arguments extra force, and they retain full discretion over boundary-drawing since (on this reading) no external referent constrains them. Labeled institutions and policy audiences sit at the target end: institutions bear reputational cost from a classification they cannot contest on the framework's own terms, and audiences bear the cost of mistaking a normative verdict for a discovered fact. This is a tangled_rope, not a pure snare, because the vocabulary genuinely does coordinate normative discourse — it lets people who share values communicate faster — even as it extracts credibility it has not earned through measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (policy critique needs sharper normative vocabulary) is authored as live, but the corroboration is thin outside the benefiting parties — which is itself evidence for THIS reading's thesis: a vocabulary whose warrant is attested mainly by those who profit from its persuasive force is functioning rhetorically, not diagnostically. This does not resolve the kernel contest; it is what the rhetorical-scaffold reading would predict about its own genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    typology_self_application_regress,
    'If the typology itself is a rhetorical scaffold rather than a diagnostic instrument, does this classification (of the typology''s own operation) inherit the same status — i.e., is ''the typology is a tangled_rope'' itself a declared normative verdict rather than a discovered fact about the typology?',
    'There is no external instrument to resolve this without begging the question; the ambiguity may be irreducible to this reading, since the reading''s own thesis denies that any classification (including this one) escapes normative declaration. A resolution would require either (a) an argument that self-application is exempt, or (b) acceptance that the reading is reflexively consistent by design.',
    'If self-application holds, this story''s own claimed_type and metrics are themselves rhetorical moves rather than measurements — which the reading should accept rather than resist, since resisting it would contradict the reading''s core premise. If self-application fails (some principled exemption exists), the reading''s scope is narrower than claimed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(typology_self_application_regress, conceptual, 'Whether the rhetorical-scaffold thesis applies reflexively to its own classification act.').

omega_variable(
    persuasion_vs_extraction_boundary,
    'Is the reputational cost borne by institutions labeled ''snare'' genuine extraction (unearned transfer of credibility) or legitimate reputational consequence of accurate normative critique — i.e., is the typology''s persuasive power illegitimate leverage or earned rhetorical skill?',
    'Compare outcomes across cases where the ''snare'' label was applied to mechanisms later vindicated by independent inquiry (e.g., subsequent regulatory findings, natural experiments) versus cases where the label did not hold up; a high vindication rate would support legitimate critique, a low rate would support extraction.',
    'High vindication rate would push this reading toward looking more like a rope or scaffold (functional early-warning vocabulary); low vindication rate would support the snare-like extraction reading of the typology''s own operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasion_vs_extraction_boundary, empirical, 'Whether the typology''s persuasive leverage tracks accuracy or operates independent of it.').

omega_variable(
    committer_framing_choice,
    'Two coherent framings compete for how to read the kernel: (a) the typology as a philosophy-of-science instrument whose categories admit error correction (favoring the immutable_diagnostic_reading), versus (b) the typology as an argumentative technology embedded in live policy disputes where no neutral referee exists (favoring this reading). The choice of framing determines whether ''snare'' functions as a discovered category or a declared one.',
    'Examine whether contested applications of the typology (real policy disputes over whether a mechanism is ''snare'' or ''tangled_rope'') were ever resolved by appeal to independent measurement that both sides accepted as dispositive, versus resolved by one side simply prevailing rhetorically or politically.',
    'If independent measurement has ever been dispositive in a contested case, that evidence favors the immutable_diagnostic or hybrid_pragmatic readings over this one. If contested cases are consistently resolved by political/rhetorical victory rather than shared measurement, that evidence favors this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Alternative kernel framings (instrument vs. rhetorical technology) and the signal used to choose this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 24, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferential_realism_ontology__rhetorical_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Three constraints decompose the single natural-language claim 'the constraint typology is [an instrument | a vocabulary | a hybrid]' per the ε-invariance principle: immutable_diagnostic_reading (fixed-referent instrument; low extraction, near-mountain for the typology's core categories, correctable misclassification), hybrid_pragmatic_reading (fixed core / contested periphery; moderate extraction concentrated in the periphery categories), and this story, rhetorical_scaffold_reading (the typology as advocacy vocabulary; substantial extraction via borrowed diagnostic credibility, low suppression of alternatives). Each carries its own stable epsilon under its own reading's lights; they are linked, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
