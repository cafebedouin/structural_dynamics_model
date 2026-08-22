% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the originalist reading of the
 *   constitutional-text-authority kernel: constitutional meaning is fixed at
 *   ratification, and interpretive authority derives from the historical
 *   public understanding of the text at that moment. This is one of three
 *   structurally distinct constraints emitted from a single contested kernel
 *   (the others being the living-constitutionalist and positivist readings,
 *   generated as separate stories). The originalist reading's ε is authored
 *   for the standing arrangement as originalism itself understands it: a
 *   discretion-constraining, popular-sovereignty-grounded interpretive method
 *   that happens to route certain categories of claim toward near-certain
 *   defeat because the required historical evidence structurally cannot exist
 *   for populations and questions absent from the ratifying public's field of
 *   view.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.52).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.58).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '84814e76-c5fb-4da7-8c4c-ecd673437ddd').
narrative_ontology:cs_kernel_codification('84814e76-c5fb-4da7-8c4c-ecd673437ddd', fixed_text).
narrative_ontology:cs_authority_grounding('84814e76-c5fb-4da7-8c4c-ecd673437ddd', lineage).
narrative_ontology:cs_interpretation_layer_present('84814e76-c5fb-4da7-8c4c-ecd673437ddd').
narrative_ontology:cs_reading_relation('84814e76-c5fb-4da7-8c4c-ecd673437ddd', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('84814e76-c5fb-4da7-8c4c-ecd673437ddd', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('84814e76-c5fb-4da7-8c4c-ecd673437ddd', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('84814e76-c5fb-4da7-8c4c-ecd673437ddd', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('84814e76-c5fb-4da7-8c4c-ecd673437ddd', foundational, judicial_review_legitimated_only_by_original_popular_sovereignty).
narrative_ontology:cs_axiom_status(judicial_review_legitimated_only_by_original_popular_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('84814e76-c5fb-4da7-8c4c-ecd673437ddd', judicial_review_legitimated_only_by_original_popular_sovereignty, deontological).
narrative_ontology:cs_reference_frame('84814e76-c5fb-4da7-8c4c-ecd673437ddd', historical_ratification_public_meaning).
narrative_ontology:cs_drift_state('84814e76-c5fb-4da7-8c4c-ecd673437ddd', contemporary_rights_jurisprudence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('84814e76-c5fb-4da7-8c4c-ecd673437ddd', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, legislative_supremacy_advocates).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, settled_property_and_contract_interests).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, groups_excluded_from_ratification_era_franchise).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, litigants_seeking_contemporary_equal_protection_extensions).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, popular_sovereignty_at_founding).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_predictability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and justices who adjudicate by reconstructing the historical public meaning of constitutional text at the time of ratification, using founding-era dictionaries, ratification debates, and contemporaneous practice as the controlling evidence. They administer the interpretive method itself, and their institutional legitimacy and reduced discretion-exposure benefit from the constraint's claim that outcomes are dictated by history rather than by judicial preference.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_judiciary, beneficiary).

% Political actors and scholars who favor resolving contested social questions through elected legislatures and the Article V amendment process rather than judicial recognition of new rights. They benefit because the fixed-meaning constraint routes contested moral and social questions away from courts and toward majoritarian political processes they can more readily influence.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, legislative_supremacy_advocates, beneficiary,
    organized, generational, mobile, national).

% Holders of long-settled economic arrangements whose expectations are protected when constitutional meaning is anchored against reinterpretation. They gain predictability: the rules governing property, contract, and federal power cannot shift beneath them through judicial reading of evolving values.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, settled_property_and_contract_interests, beneficiary,
    powerful, generational, constrained, national).

% Individuals seeking judicial recognition of rights not explicitly named in the text and not demonstrably understood as protected at ratification (privacy, certain bodily autonomy and relational rights, contemporary equal-protection extensions). Under this reading, their claims are gated by historical evidence that, by construction, often cannot exist for harms or identities not contemplated by the ratifying public. Their only remedy is Article V amendment, a near-insurmountable supermajoritarian process.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Descendants of populations who had no voice in the ratifying public (women, enslaved and formerly enslaved people, Indigenous nations, propertyless men) whose interests were structurally absent from the 'public understanding' the constraint treats as authoritative. They bear the cost of a meaning fixed by a public that did not include people like them, with no mechanism inside the interpretive method itself to correct for that absence.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, groups_excluded_from_ratification_era_franchise, payer,
    powerless, generational, trapped, national).

% Parties arguing that constitutional guarantees should extend to circumstances or classifications the ratifying generation did not anticipate. They must litigate against a rule that treats the absence of historical evidence of an original understanding as evidence against the claimed right, shifting the burden of historical proof onto those least likely to appear in the founding-era record.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, litigants_seeking_contemporary_equal_protection_extensions, payer,
    moderate, biographical, constrained, national).

% Scholars and jurists who hold that meaning properly evolves with contemporary moral understanding are treated, within this reading's own framework, as advancing an illegitimate method rather than a competing legitimate reading. Their objection — that fixed original meaning freezes in the exclusions of an unrepresentative ratifying public — is heard in public discourse but is not admissible as a ground for decision within the originalist method itself.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_scholars, excluded,
    organized, generational, mobile, national).

% Researchers who reconstruct ratification-era usage and debate. They supply the evidentiary raw material the method depends on and are positioned to observe where the historical record is thin, contested, or silent on the very questions courts are asked to resolve — without themselves adjudicating the outcome.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, historically anchored decision procedure that constrains judicial discretion, gives legislators and citizens stable expectations about what the text permits, and channels contested moral disagreement toward the amendment process rather than toward unelected judges.
% TRANSFER_FUNCTION: Moves interpretive authority from contemporary judicial moral reasoning to the reconstructed understanding of a historical ratifying public, and correspondingly moves the practical capacity to secure new constitutional protections away from litigants and toward legislative supermajorities.
% ABSENT_VOICES: Living constitutionalist and positivist scholars object that the method either freezes the exclusions of an unrepresentative founding public or smuggles in unacknowledged normative choices about which historical evidence counts; they are active in academic and political discourse but structurally excluded from being a permissible ground for decision within an originalist opinion. Groups absent from the ratifying franchise itself (women, enslaved people, Indigenous nations) are doubly absent: excluded from the original public whose understanding now governs, and without standing inside the method to register that exclusion as a defect.
% DISAPPEARANCE_RATIONALE: If originalism ceased to operate as a controlling interpretive method, courts would more readily recognize unenumerated rights and extend existing guarantees to unanticipated circumstances without waiting for Article V supermajorities; legislative-supremacy advocates would lose their preferred venue for resolving contested moral questions, and settled economic expectations anchored in historical readings could become newly contestable. The practical menu of who can win a constitutional claim, and how, would shift substantially.
% FOUNDING_PROBLEM: The felt need to constrain judicial discretion after mid-20th-century decisions that critics viewed as substituting judges' own moral views for constitutional text, and to ground judicial review's legitimacy in popular sovereignty exercised at ratification rather than in ongoing judicial moral judgment.
% FOUNDING_PROBLEM_CORROBORATION: Originalist judges and legal academics attest the discretion-constraint problem remains live and structurally necessary. Legal historians outside the originalist movement (including some who are sympathetic to textualism generally) attest that the ratifying public's understanding is frequently indeterminate or contested on the precise questions modern courts must answer, and that the method's claim to eliminate judicial discretion is itself contested rather than settled — the discretion is relocated to the selection and weighting of historical sources, not eliminated.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that the method's chief effect on unenumerated-rights claimants and originally-disenfranchised groups is a durable transfer of practical outcome-control to legislative supermajorities and to the historically reconstructed preferences of a ratifying public that in many applications did not include them — while providing genuine, non-trivial coordination value (predictability, discretion constraint) to the judiciary and to settled economic interests. Suppression (0.58) is substantial but not near-total: it operates primarily through evidentiary gatekeeping (absence of historical record functions as a decision rule against the claim) rather than through direct coercive exclusion, and Article V remains a formally open, if practically forbidding, escape valve. Accessibility collapse (0.62) is moderate-high: once a court commits to the method, alternative readings are largely foreclosed within that opinion, but the method itself remains politically contested and reversible by future appointments or doctrinal shifts, which caps the collapse below mountain-level. Resistance (0.71) is high and organized — living constitutionalist and positivist scholars, along with affected litigant communities, actively contest the method in academic, judicial-confirmation, and litigation venues.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judiciary and legislative-supremacy advocates sit near the beneficiary end: they administer or are structurally favored by the method and are not the ones whose claims are foreclosed by evidentiary gates. Settled property and contract interests benefit from the predictability the frozen-meaning approach provides. Unenumerated-rights claimants and originally-disenfranchised groups sit near the full-target end: the method's core operation is that the absence of ratification-era evidence for their claim counts as evidence against it, and they have essentially no exit from a national-scope, constrained-mobility position other than the practically foreclosed Article V route — hence their exit_options are declared trapped rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining perceived judicial policy-making) may still be live in some applications, but the corroboration is genuinely contested: originalists maintain the discretion-elimination claim is structurally sound, while outside historians document that the method relocates rather than eliminates discretion into source-selection and weighting judgments. This mismatch — a founding problem asserted as still fully live by beneficiaries but documented as partially dissolved by non-beneficiary corroborators — is exactly the signal the R5 genealogy interview is built to surface, and it is deliberately left unresolved here rather than adjudicated by the story itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_as_kernel_reading_not_topic,
    'Is ''constitutional interpretation'' properly one constraint with an observer-relative extraction value, or does the originalist commitment instantiate a structurally distinct constraint from the living-constitutionalist and positivist commitments?',
    'Apply the ε-invariance test: since the beneficiary/victim structure, the suppression mechanism (evidentiary gating vs. moral-principle balancing vs. pedigree formalism), and the accessibility-collapse profile all differ sharply across the three readings, they are authored as three separate constraint stories linked via network.affects_constraints rather than as one story with a measurement parameter.',
    'Confirms the decomposition already adopted in this story; if it were instead treated as one constraint, the resulting single ε would average away the very structural facts (differential treatment of unenumerated rights, differential burden allocation) the framework exists to detect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalism_as_kernel_reading_not_topic, conceptual, 'Whether originalism is a reading of a kernel or a self-standing topic.').

omega_variable(
    discretion_elimination_or_relocation,
    'Does the originalist method genuinely eliminate judicial discretion, as its proponents claim, or does it relocate discretion into the selection, weighting, and characterization of historical sources — making the ''fixed meaning'' constraint itself partly a performance of determinacy?',
    'Comparative study of originalist opinions on contested questions (e.g., scope of the Second Amendment, incorporation doctrine) checking for convergence among originalist historians on the same historical record; persistent disagreement among originalist-methodology adherents applying supposedly the same historical evidence would indicate relocation rather than elimination.',
    'If discretion is substantially relocated rather than eliminated, the theater_ratio is understated and the constraint sits closer to a tangled_rope with a larger performative component than currently authored; if genuinely eliminated in the great majority of applications, the coordination function is stronger than the extraction reading credits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_elimination_or_relocation, empirical, 'Whether originalism eliminates or merely relocates interpretive discretion.').

omega_variable(
    ratifying_public_representativeness,
    'Given that the ratifying public whose understanding controls under this reading systematically excluded women, enslaved and formerly enslaved people, Indigenous nations, and propertyless men, is the ''popular sovereignty'' legitimation this reading offers itself a vindicated proposition, or is it substantially undermined by the exclusions baked into the historical record it treats as authoritative?',
    'Historical and normative scholarship on the composition and representativeness of the ratifying and amending publics at each relevant constitutional moment (1788, Reconstruction Amendments, 19th Amendment, etc.), assessed against the specific provisions at issue in contemporary litigation.',
    'If the exclusions are structurally severe for the provisions most often litigated by unenumerated-rights claimants, the beneficiary category vindicated_propositions(''popular_sovereignty_at_founding'') is itself contestable rather than settled, strengthening the case that the constraint''s coordination story functions partly as legitimating cover for the transfer it accomplishes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratifying_public_representativeness, conceptual, 'Whether the popular-sovereignty legitimation is undercut by the ratifying public''s own exclusions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__originalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(cons_tr_t1989, constitutional_text_authority__originalist_reading, theater_ratio, 1989, 0.2).
narrative_ontology:measurement(cons_tr_t1998, constitutional_text_authority__originalist_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(cons_tr_t2007, constitutional_text_authority__originalist_reading, theater_ratio, 2007, 0.24).
narrative_ontology:measurement(cons_tr_t2016, constitutional_text_authority__originalist_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text_authority__originalist_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__originalist_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(cons_be_t1989, constitutional_text_authority__originalist_reading, base_extractiveness, 1989, 0.42).
narrative_ontology:measurement(cons_be_t1998, constitutional_text_authority__originalist_reading, base_extractiveness, 1998, 0.44).
narrative_ontology:measurement(cons_be_t2007, constitutional_text_authority__originalist_reading, base_extractiveness, 2007, 0.46).
narrative_ontology:measurement(cons_be_t2016, constitutional_text_authority__originalist_reading, base_extractiveness, 2016, 0.49).
narrative_ontology:measurement(cons_be_t2025, constitutional_text_authority__originalist_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__originalist_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(cons_su_t1989, constitutional_text_authority__originalist_reading, suppression_requirement, 1989, 0.46).
narrative_ontology:measurement(cons_su_t1998, constitutional_text_authority__originalist_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(cons_su_t2007, constitutional_text_authority__originalist_reading, suppression_requirement, 2007, 0.54).
narrative_ontology:measurement(cons_su_t2016, constitutional_text_authority__originalist_reading, suppression_requirement, 2016, 0.56).
narrative_ontology:measurement(cons_su_t2025, constitutional_text_authority__originalist_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the constitutional_text_authority kernel family. The living-constitutionalist reading inverts the fixed-meaning axiom and correspondingly inverts the beneficiary/victim structure (unenumerated-rights claimants become beneficiaries; legislative-supremacy advocates and settled-expectations holders become the parties bearing the cost of doctrinal instability). The positivist reading occupies an orthogonal axis, bracketing moral content and evaluating only formal enactment pedigree, and so shares structural elements with both other readings without being foreclosed by either. Each story authors its own ε from its own reading's lights, per the ε-invariance principle; none averages across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
