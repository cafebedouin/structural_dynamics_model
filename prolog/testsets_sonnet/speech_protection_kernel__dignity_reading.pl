% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   speech-protection kernel: constitutional or statutory speech protection
 *   is treated as conditional on the speech not functioning to structurally
 *   subordinate a target group, distinguishing group harm from individual
 *   defamation harm. This reading is drawn from comparative dignity
 *   jurisprudence (post-WWII German Basic Law free-speech limits, Canadian
 *   s.1 Charter balancing, South African post-apartheid equality
 *   jurisprudence, and hate-speech statutes in many democracies) as opposed
 *   to the American First Amendment tradition's near-categorical protection.
 *   The four sibling readings — absolutist, harm_threshold, marketplace,
 *   democratic_participation — are separate constraints with their own ε and
 *   stakeholder structures, not alternative measurements of this one. Where
 *   they diverge from this reading's premises is documented in the omegas
 *   below, per the ε-invariance discipline: this story does not average over
 *   readings or hedge its ε across them.
 *
 * KEY AGENTS:
 *   - historically_subordinated_groups: primary intended beneficiary — target of the harm the doctrine names
 *   - equality_rights_advocates: agenda_setter who presses the doctrinal project
 *   - anti_discrimination_agencies: institutional agenda_setter administering the line
 *   - speakers_of_contested_group_claims: primary payer — bears liability/sanction risk under an after-the-fact classification
 *   - courts_and_tribunals: analytical observer and simultaneous agenda_setter, drawing and re-drawing the line
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.52).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Dignity-Conditional Speech Protection (Anti-Subordination Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, 'fb91fb09-34e1-456f-bf21-34ef7bfc7b4c').
narrative_ontology:cs_kernel_codification('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', distributed).
narrative_ontology:cs_authority_grounding('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', distributed).
narrative_ontology:cs_reading_relation('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', foundational, group_status_harm_is_constitutionally_cognizable).
narrative_ontology:cs_axiom_status(group_status_harm_is_constitutionally_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', group_status_harm_is_constitutionally_cognizable, deontological).
narrative_ontology:cs_axiom('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', foundational, equal_dignity_is_precondition_for_protected_speech).
narrative_ontology:cs_axiom_status(equal_dignity_is_precondition_for_protected_speech, holdable).
narrative_ontology:cs_axiom_grounding('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', equal_dignity_is_precondition_for_protected_speech, deontological).
narrative_ontology:cs_reference_frame('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', post_atrocity_dignity_baseline).
narrative_ontology:cs_drift_state('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb91fb09-34e1-456f-bf21-34ef7bfc7b4c', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, equality_rights_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, anti_discrimination_agencies).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_contested_group_claims).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, political_dissidents_using_inflammatory_rhetoric).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, religious_speakers_on_contested_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, equal_dignity_as_constitutional_baseline).
narrative_ontology:constraint_vindicates(speech_protection_kernel__dignity_reading, group_libel_distinct_from_individual_libel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups with a documented history of structural subordination (racial minorities, religious minorities, etc.) receive protection from speech that functions to entrench their subordinate status — group libel, dehumanizing propaganda, incitement targeting group membership as such. They cannot exit the social category the speech targets; the protection substitutes for the exit they lack.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% Litigators, scholars, and legislators who press courts and legislatures to recognize group-subordination harm as a category distinct from individual defamation. They shape doctrine, file amicus briefs, and draft hate-speech statutes. Their professional and reputational standing is partly built on this doctrinal project succeeding.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, equality_rights_advocates, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, equality_rights_advocates, beneficiary).

% Administrative bodies (human rights commissions, civil rights divisions) that investigate and adjudicate speech complaints under the dignity standard. They administer the line between protected controversy and unprotected subordination, and their institutional mandate expands with the doctrine's scope.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, anti_discrimination_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Speakers making empirical or normative claims about group characteristics, immigration, crime statistics, or group identity that a tribunal may characterize as subordinating rather than merely offensive. They bear liability, platform deplatforming, or professional sanction risk; the line between protected controversy and unprotected subordination is drawn after the fact by an adjudicator they do not control.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_contested_group_claims, payer,
    moderate, biographical, constrained, national).

% Fringe political actors whose rhetoric against a powerful group or institution (rather than a historically subordinated one) may be treated asymmetrically — protected as political speech when directed upward, restricted as subordinating when directed at a protected class, even where the emotional register is identical. They cannot buy their way out of a subordination finding and often lack resources to litigate the classification.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, political_dissidents_using_inflammatory_rhetoric, payer,
    powerless, biographical, trapped, national).

% Speakers articulating theologically grounded positions on sexuality, gender, or group membership that a dignity standard may treat as subordinating regardless of sincere belief or doctrinal necessity. Their speech is fused with religious identity they cannot abandon without abandoning the faith commitment itself, so the constraint reaches into identity rather than mere expression.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, religious_speakers_on_contested_doctrine, payer,
    moderate, biographical, identity_locked, national).

% Adjudicate where the line between protected controversy and unprotected subordination falls in specific cases, drawing on comparative dignity jurisprudence (Canada, Germany, South Africa) versus the American free-speech tradition. Their rulings both apply and continuously re-draw the doctrine's boundary.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_tribunals, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, courts_and_tribunals, agenda_setter).

% Holders of minority viewpoints that are themselves unpopular but not aligned with any historically recognized subordinated group — they would argue the dignity standard is applied asymmetrically, protecting some group identities while leaving others' equally sincere claims of harm unrecognized, but they are not organized enough to be a party to the doctrinal debate.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, unpopular_minority_viewpoint_holders, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a baseline of equal social standing beneath which political and civil discourse is supposed to operate — preventing speech from functioning as a mechanism that entrenches or re-inscribes a group's subordinate status, so that democratic participation is available on equal terms to members of historically subordinated groups.
% TRANSFER_FUNCTION: Moves speech risk (liability exposure, platform removal, professional sanction, litigation cost) from members of historically subordinated groups to speakers whose expression is classified as functioning to subordinate those groups; moves adjudicative authority from the speaker to courts and anti-discrimination agencies who draw the subordination line after the fact.
% ABSENT_VOICES: Unpopular minority viewpoint holders whose claims of harm are not organized around a recognized subordinated-group category are not represented in the doctrinal contest; political dissidents attacking powerful groups from below argue the standard is applied asymmetrically but lack the organizational capacity to press the point in the forums where the doctrine is made.
% DISAPPEARANCE_RATIONALE: Equality advocates and anti-discrimination agencies would say the world rearranges badly: hate speech and group libel would flow more freely, degrading the equal-participation baseline for subordinated groups. Speakers currently classified as payers would say the world barely changes for ordinary political and religious discourse but improves markedly for contested but sincere claims currently chilled by subordination-finding risk. Courts disagree with each other across jurisdictions on which account is correct, which is itself evidence the verdict is genuinely contested rather than settled.
% FOUNDING_PROBLEM: Historically, formally equal legal status (e.g., post-emancipation, post-decolonization, post-genocide) did not translate into equal capacity to participate in public discourse when speech could still function to reassert a group's subordinate social position — propaganda, group libel, and organized dehumanization campaigns that formal non-discrimination law did not reach because they targeted individuals only through their group membership.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars outside the equality-advocacy movement (drawing on German, Canadian, and South African post-atrocity jurisprudence) corroborate that the founding problem was real and historically documented. Free-speech scholars in the American tradition, also outside the benefiting coalition, corroborate the problem's original reality but argue current application has drifted from atrocity-adjacent group libel toward routine political and religious controversy — a status-shift claim, not a denial of the founding problem itself.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.52) and rising over the interval: the doctrine began narrowly targeted at genocide-adjacent propaganda and group libel and has, in several jurisdictions, expanded to reach ordinary political and religious controversy, which is the extraction-accumulation pattern the temporal series is meant to surface. Suppression (0.58) reflects the real coercive machinery — tribunal proceedings, statutory hate-speech liability, platform-mandated removal — required to enforce the subordination standard against speakers who dispute the classification. Resistance is high (0.71) because the doctrine is continuously contested by free-speech traditionalists, religious speakers, and dissidents who argue the line is drawn asymmetrically. Accessibility collapse is moderate (0.42): the classification genuinely closes off some avenues of expression once a subordination finding issues, but appeal, legislative reform, and cross-jurisdictional forum-shopping remain live alternatives, unlike a mountain's near-total collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the equality-advocate and anti-discrimination-agency seats, this operates as coordination: a necessary correction to a formal-equality regime that otherwise permits group subordination through speech. From the payer seats — especially the powerless dissidents and identity-locked religious speakers — the same structure computes as enforced extraction: liability and sanction risk imposed by a classification they cannot contest on equal footing. The engine's per-seat computation should register this divergence directly from the differentiated power/exit data; it is not resolved by the story picking a winner.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and the institutions built to protect them are structural beneficiaries: they receive a shield against a specific harm (group-targeted subordinating speech) that formal equal-protection doctrine does not otherwise reach, and their exit from the targeted category is limited or impossible (constrained/trapped), which is precisely why the doctrine's coordination logic treats them as needing the extra protection. Speakers whose expression risks a subordination finding are the structural targets: their exit options range from constrained (contested empirical claims) to trapped (fringe dissidents with no resources to litigate classification) to identity_locked (religious speakers whose doctrine is fused with their faith identity) — this range is deliberately differentiated because the doctrine's cost falls unevenly depending on how mobile and resourced the speaker is, not uniformly on all speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-atrocity group libel with no formal-equality remedy) is corroborated as historically real by scholars outside the equality-advocacy coalition. Its current status is contested rather than flatly dead: the mismatch to watch is a status=dead reading (doctrine now used mainly against ordinary controversy, its atrocity-prevention function largely obsolete in stable democracies) paired with disappearance_verdict=world_rearranges (advocates insist removing it would cause real harm) — that combination is exactly the capture/zombie signature the R5 consumer is built to flag, and this story deliberately leaves the founding_problem_status as contested rather than resolving it, since resolving it here would be tuning the claim to a predicted verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_versus_offense_line,
    'Is there a principled, administrable line between speech that functions to structurally subordinate a group and speech that merely offends or contests a group''s claims, or does the line collapse under adjudicative pressure into whichever claim the tribunal finds more sympathetic?',
    'Comparative doctrinal analysis tracking case outcomes across jurisdictions with dignity-based hate speech law (Germany, Canada, South Africa) for consistency in outcomes given structurally similar fact patterns; convergent outcomes across politically opposed tribunals would support a principled line, divergent outcomes tracking tribunal composition would support the collapse hypothesis.',
    'If the line is principled and administrable, this reading functions closer to a genuine coordination mechanism (rope-adjacent) protecting a real, narrow category of harm. If it collapses into ad hoc sympathy, the effective operation is closer to snare: a discretionary suppression tool dressed in dignity language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_versus_offense_line, empirical, 'Whether the subordination/offense distinction is administrable or illusory in practice.').

omega_variable(
    asymmetric_application_across_group_categories,
    'Is the dignity standard applied symmetrically across all groups capable of claiming subordination, or does it systematically protect some group identities (those with organized advocacy and institutional recognition) while leaving equally sincere claims from unrecognized or unpopular groups unaddressed?',
    'Empirical audit of adjudicated cases and legislative hate-speech categories for coverage gaps — which group identities are named in statute or case law versus which analogous claims have been raised and rejected or never brought.',
    'Symmetric application supports the doctrine''s universalist dignity claim; asymmetric application (favoring organized, institutionally recognized groups) would indicate the doctrine functions partly as a a benefit captured by whichever groups have advocacy infrastructure, independent of the underlying subordination logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_application_across_group_categories, empirical, 'Whether dignity protection tracks subordination itself or tracks advocacy organization.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with the harm_threshold_reading and absolutist_reading live — is it a disagreement about facts (does group-targeted speech cause demonstrable harm distinct from individual harm), or a disagreement about the proper unit of constitutional concern (individuals only, versus individuals-as-group-members)?',
    'This is a conceptual/doctrinal question, not resolvable by further data alone — it depends on which theory of the harm the constitutional text is meant to prevent (dignitary/status harm versus discrete injury) one adopts.',
    'If the disagreement is purely factual, convergent empirical findings on group-harm mechanisms could in principle move the harm_threshold_reading toward incorporating group harm, narrowing the gap between readings. If it is a unit-of-concern disagreement, no empirical finding resolves it — the readings remain genuinely incommensurable, each internally coherent, which is why they coexist across jurisdictions rather than one displacing the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the dignity/harm_threshold disagreement is empirical or a disagreement about the constitutional unit of concern.').

omega_variable(
    doctrine_scope_drift_naturalness,
    'Is the observed extraction accumulation (0.30 to 0.52 over the interval) a natural and intended maturation of the doctrine as courts gain experience applying it, or is it scope creep beyond the atrocity-prevention function the doctrine was originally justified by?',
    'Track the fact patterns of adjudicated cases chronologically — do later cases involve genocide-adjacent propaganda and organized dehumanization campaigns (original scope) or ordinary political and religious controversy (expanded scope)?',
    'If later cases remain within the original atrocity-adjacent scope, the rising extraction reflects legitimate doctrinal maturation. If later cases increasingly involve ordinary controversy, the rising extraction is scope creep — evidence for the founding_problem_status=dead-with-persistent-mandate reading flagged in the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_scope_drift_naturalness, empirical, 'Whether rising extraction reflects doctrinal maturation or scope creep beyond the founding problem.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t8, speech_protection_kernel__dignity_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__dignity_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__dignity_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__dignity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spee_be_t8, speech_protection_kernel__dignity_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__dignity_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__dignity_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__dignity_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(spee_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(spee_su_t8, speech_protection_kernel__dignity_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__dignity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__dignity_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__dignity_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(spee_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language label 'speech protection' under the speech_protection_kernel. Each reading (absolutist, harm_threshold, marketplace, democratic_participation, dignity) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, per the ε-invariance principle — they are not the same constraint measured five ways. This dignity_reading story recognizes group harm as distinct from individual harm and conditions protection on non-subordination; the harm_threshold_reading conditions protection on demonstrable harm without a separate group category; the absolutist_reading treats listener/group harm as categorically insufficient grounds for restriction; the marketplace_reading treats counter-speech rather than restriction as the remedy; the democratic_participation_reading weights protection by relevance to self-governance. All five are linked bidirectionally via affects_constraints to preserve the family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
