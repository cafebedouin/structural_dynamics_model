% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Dignity-Conditional Speech Protection (Anti-Subordination Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates the dignity reading of the contested
 *   speech-protection kernel: speech protection is conditional on the speech
 *   not functioning as structural subordination of a target group. Group harm
 *   is treated as a category distinct from individual harm — hate speech and
 *   group libel fall outside protection, and the boundary is administered by
 *   courts and rights tribunals applying an equal-dignity threshold. This is
 *   not the harm-threshold reading (which conditions protection on
 *   demonstrable harm generally, individual or group) nor the absolutist
 *   reading (which treats listener/group harm as never sufficient grounds).
 *   The dignity reading is authored here as its own constraint with its own
 *   epsilon; the sibling readings are separate constraints linked via network
 *   only.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.52).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Dignity-Conditional Speech Protection (Anti-Subordination Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '7582135d-e07d-488a-844f-2607f73e196e').
narrative_ontology:cs_kernel_codification('7582135d-e07d-488a-844f-2607f73e196e', distributed).
narrative_ontology:cs_authority_grounding('7582135d-e07d-488a-844f-2607f73e196e', distributed).
narrative_ontology:cs_reading_relation('7582135d-e07d-488a-844f-2607f73e196e', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7582135d-e07d-488a-844f-2607f73e196e', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('7582135d-e07d-488a-844f-2607f73e196e', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('7582135d-e07d-488a-844f-2607f73e196e', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('7582135d-e07d-488a-844f-2607f73e196e', foundational, equal_dignity_as_precondition_for_participation).
narrative_ontology:cs_axiom_status(equal_dignity_as_precondition_for_participation, holdable).
narrative_ontology:cs_axiom_grounding('7582135d-e07d-488a-844f-2607f73e196e', equal_dignity_as_precondition_for_participation, deontological).
narrative_ontology:cs_axiom('7582135d-e07d-488a-844f-2607f73e196e', foundational, group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('7582135d-e07d-488a-844f-2607f73e196e', group_harm_distinct_from_individual_harm, conventional).
narrative_ontology:cs_reference_frame('7582135d-e07d-488a-844f-2607f73e196e', post_war_anti_subordination_consensus).
narrative_ontology:cs_drift_state('7582135d-e07d-488a-844f-2607f73e196e', contemporary_platform_speech_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7582135d-e07d-488a-844f-2607f73e196e', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, equality_jurisprudence_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, anti_discrimination_agencies).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, provocative_political_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, fringe_ideological_organizations).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, satirists_and_polemicists).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_as_precondition_for_speech_rights).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_libel_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups historically subject to organized vilification campaigns receive protection against speech that functions as structural subordination — dehumanizing group libel, incitement framed as commentary, organized harassment campaigns dressed as opinion. They cannot exit the identity category the speech targets, so the protection is aimed at removing a standing burden they otherwise bear disproportionately relative to majority-group speakers.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_targeted_minority_groups, beneficiary,
    powerless, generational, trapped, national).

% Courts, human rights tribunals, and regulatory bodies administer the dignity threshold, deciding case by case whether speech crosses from protected expression into structural subordination. They set doctrine, issue findings, and can order remedies (fines, retractions, platform removal). Their institutional survival and mandate expansion depend partly on continued cases to adjudicate.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, anti_discrimination_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, civil rights organizations, and litigation groups whose intellectual and institutional project is vindicated by dignity-conditional doctrine. They benefit reputationally and professionally from the doctrine's adoption and expansion, and can shift focus to other legal theories if the doctrine loses ground — their exit is mobile, unlike the groups they represent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, equality_jurisprudence_advocates, beneficiary,
    organized, generational, mobile, national).

% Speakers whose harsh, group-targeted political rhetoric risks being reclassified as unprotected subordination rather than protected opinion bear the cost of an uncertain and shifting line. They can self-censor, litigate the classification, or risk sanction; the boundary is not always predictable in advance, which itself constrains what they say.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, provocative_political_speakers, payer,
    moderate, biographical, constrained, national).

% Groups whose organizing message is explicitly premised on group hierarchy or exclusion are directly targeted by the doctrine — their core expressive activity is the paradigm case the doctrine exists to restrict. They have no adjacent lawful framing that preserves their message, so exit from the constraint's reach effectively means abandoning the message.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, fringe_ideological_organizations, payer,
    moderate, biographical, trapped, national).

% Comedians, satirists, and harsh polemicists whose work trades in exaggeration, stereotype-inversion, and provocation about groups face doctrinal uncertainty about whether their work will be read as subordinating or as legitimate (even harsh) commentary. They can moderate tone, seek legal advice before publication, or risk post-hoc reclassification — a real chilling cost even when their work is ultimately protected.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, satirists_and_polemicists, payer,
    moderate, biographical, constrained, national).

% Free-speech absolutists and marketplace-of-ideas theorists would object that the dignity threshold imports an outcome-based content judgment into what should be a viewpoint-neutral protection, but their framework is a rival reading of the same kernel rather than a voice inside this reading's adjudication process — they are heard in public and academic debate but do not sit on the tribunals applying dignity doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_and_marketplace_theorists, excluded,
    organized, generational, mobile, national).

% Apex courts adjudicate the boundary between this reading and its siblings across cases, sometimes adopting elements of the dignity reading and sometimes rejecting it in favor of harm-threshold or absolutist tests. They observe the doctrine's operation across the legal system and can redirect it through precedent.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared norm that political community membership requires a baseline of equal standing — speech that functions to strip a group of that standing is treated as an attack on the conditions of participation itself, not merely as offensive content, allowing courts and legislatures to draw a principled line between harsh criticism and structural subordination.
% TRANSFER_FUNCTION: Moves the burden of proof and the cost of ambiguity from historically targeted groups (who no longer must simply absorb dehumanizing rhetoric as the unavoidable price of others' free speech) to speakers whose rhetoric trades in group-targeted hierarchy claims, who must now internalize legal risk and self-censorship costs they previously did not bear.
% ABSENT_VOICES: Absolutist and marketplace theorists are heard in academic and public discourse but do not sit inside the adjudicative process that applies the dignity threshold case by case; individual members of fringe organizations rarely have resources to litigate the classification question fully and often experience the doctrine only as an enforcement outcome.
% DISAPPEARANCE_RATIONALE: If the dignity threshold vanished overnight, jurisdictions applying it would revert to whichever sibling reading fills the vacuum (typically harm-threshold or absolutist), group libel and organized hate speech currently restricted under this doctrine would become presumptively protected again, and targeted minority groups would lose a distinct legal category for group-directed subordination speech, forcing them back onto individual defamation or harassment law, which does not capture group-directed harm well.
% FOUNDING_PROBLEM: Post-WWII and civil-rights-era legal systems confronted organized speech campaigns (racial hate propaganda, group libel, dehumanizing political rhetoric) that functioned to entrench subordination of identifiable groups, and existing individual-harm doctrines (defamation, direct incitement) failed to capture harm that operated at the group level and accumulated as a standing condition rather than a discrete injury.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the equality-litigation community (including some who are skeptical of the doctrine's scope) corroborate that group-level subordination speech was a real gap in individual-harm frameworks historically; however, civil-liberties organizations that are not beneficiaries of the doctrine's expansion argue the founding problem has been substantially addressed by existing incitement and true-threat doctrines, making continued expansion a solution in search of new problems rather than a response to a persisting gap.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine, once mature, extracts real expressive costs from speakers whose message is group-hierarchy-premised — this is not negligible, but it is lower than a pure censorship regime because the doctrine's core coordination function (protecting equal standing as a precondition for participation) is genuine and its threshold is narrower than general harm-based restriction. Suppression (0.52) is moderate: the doctrine requires active adjudication and produces real chilling effects on borderline speech (satire, harsh polemic) beyond its paradigm targets. Theater ratio is comparatively low (0.28) because the enforcement mechanism (litigation, tribunal findings) does substantive work rather than performing function without effect, though it rises over the measured interval as doctrine ossifies into precedent that is applied more mechanically.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically targeted minority groups and equality jurisprudence advocates are declared beneficiaries: the former receive a removed standing burden, the latter see their legal theory vindicated and their institutional position advanced. Provocative speakers, fringe organizations, and satirists are declared victims: the doctrine's operation directly narrows what they can say without risk, and their exit options are constrained or trapped depending on how central group-hierarchy claims are to their expressive project. Anti-discrimination agencies sit as agenda-setters with institutional (not individual) power and analytical exit — they administer the boundary rather than experiencing it as speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (organized group-subordination speech uncaptured by individual-harm doctrine) is authored as contested rather than resolved: corroborating scholars outside the beneficiary class affirm the historical gap was real, but also note that incitement and true-threat doctrines have since matured to cover much of the same ground, meaning some current applications of the dignity threshold may be extending a doctrine past the problem it was built to solve. This is exactly the kind of divergence the R5 genealogy interview is designed to surface without resolving — the mismatch (contested status, world_rearranges verdict) flags for review rather than adjudicating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_threshold_line_drawing,
    'Where precisely does harsh political criticism of a group''s beliefs or conduct end and structural subordination of the group''s members begin — and is this line administrable without significant arbitrariness across adjudicators?',
    'Comparative analysis of case outcomes across jurisdictions applying dignity-based tests, checking for consistency and predictability in classification of borderline cases (satire, historical/religious criticism, harsh political rhetoric about group conduct).',
    'If the line proves unadministrable with consistency, the doctrine''s suppression cost (chilling effects on legitimate but harsh speech) rises relative to its coordination benefit, pushing the classification toward snare; if administrable with reasonable consistency, the tangled_rope reading is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_threshold_line_drawing, empirical, 'Whether the dignity/subordination line is administrable in practice.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the dignity reading better understood as an independent normative commitment (equal standing as a constitutive precondition for speech rights) or as a specific application of the harm-threshold reading (group dignity harm is simply one category of demonstrable harm)?',
    'Examine whether jurisdictions that adopt harm-threshold tests generally converge on the same case outcomes as jurisdictions with an explicit dignity doctrine; convergence would suggest the readings are not truly independent, divergence would support treating them as distinct kernels-readings.',
    'If the dignity reading collapses into a special case of harm-threshold, the two constraint stories should perhaps be merged or the reading_relations reclassified from coexists_with toward a subsumption relationship; if they diverge systematically (e.g., dignity doctrine restricts speech harm-threshold would permit, or vice versa), they remain properly independent readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether dignity_reading is analytically independent of harm_threshold_reading or a special case of it.').

omega_variable(
    who_defines_the_target_group,
    'Who has standing to invoke the dignity threshold on behalf of a targeted group, and does the doctrine''s benefit flow to the group members themselves or primarily to the advocacy organizations and agencies that administer claims on their behalf?',
    'Track the distribution of litigation outcomes, remedies, and institutional resources across cases: do individual group members receive direct relief, or does benefit concentrate in precedent-setting and organizational capacity-building for advocacy groups?',
    'If benefit concentrates in advocacy and agency institutions rather than flowing to the named beneficiary group, the tangled_rope classification is strengthened (a genuine coordination function riding alongside a distinct extraction/capture dynamic favoring institutional intermediaries over the nominal beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_defines_the_target_group, empirical, 'Whether doctrine benefit flows to named beneficiary groups or concentrates in intermediary institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__dignity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__dignity_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__dignity_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__dignity_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__dignity_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__dignity_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__dignity_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__dignity_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__dignity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__dignity_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__dignity_reading, suppression_requirement, 24, 0.47).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__dignity_reading, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of speech_protection_kernel, each authored as an independent constraint with its own epsilon per the epsilon-invariance principle. The dignity_reading's distinguishing structural feature is recognizing group harm as categorically distinct from individual harm and conditioning protection on maintenance of equal dignity for target groups — hate speech and group libel are unprotected under this reading specifically. The other readings (absolutist, harm_threshold, marketplace, democratic_participation) are separate files with their own metrics and stakeholder structures; they are linked here for contamination-propagation and family-analysis purposes only, not folded into this constraint's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
