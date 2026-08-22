% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Protection with Harm Balancing (Proportionality Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   The harm-balancing reading of the speech-harm boundary kernel holds that
 *   free expression receives presumptive protection but yields when
 *   proportionate demonstrable harm is established. Unlike the absolutist
 *   reading (speech protection near-categorical) or the dignity reading
 *   (dehumanizing speech categorically unprotected independent of harm
 *   proof), this reading makes the operative boundary depend on a
 *   case-by-case proportionality assessment: adjudicative bodies weigh the
 *   severity and directness of harm against the speaker's expression
 *   interest, the availability of less restrictive alternatives, and the
 *   foreseeability of harm. This reading produces moderate extractiveness
 *   (0.48) because speakers bear real restriction costs once harm is
 *   demonstrated, but the presumptive protection and proportionality
 *   requirement limit the breadth of unprotected categories compared to
 *   dignity-reading frameworks. The constraint requires active enforcement by
 *   courts and tribunals to apply the proportionality test; harm victims gain
 *   legal standing and remedy mechanisms as beneficiaries. Marginal advocacy
 *   groups occupy dual positions: they bear speaker restrictions while
 *   potentially benefiting from harm remedies against campaigns targeting
 *   their members.
 *
 * KEY AGENTS:
 *   - speakers_subject_to_restriction: Speakers whose expression falls within hate-speech, harassment, or group-libel categories; bear restriction costs and liability risk (power: moderate; exit: constrained)
 *   - harm_victims_seeking_remedy: Individuals experiencing documented harm from targeted speech; benefit from legal standing and remedy mechanisms (power: powerless; exit: trapped)
 *   - marginalized_communities_facing_organized_speech: Groups targeted by coordinated campaigns; benefit from hate-speech provisions but constrained by ongoing exposure (power: organized; exit: constrained)
 *   - advocacy_groups_with_marginal_positions: Organizations advocating positions that may fall within unprotected categories; dual-positioned as both restricted speakers and potential beneficiaries (power: moderate; exit: constrained)
 *   - adjudicative_bodies: Courts, tribunals, and content moderation authorities that apply the proportionality test and set the operative boundary (power: institutional; exit: analytical)
 *   - legislative_bodies: Parliaments enacting hate-speech statutes, harassment laws, and incitement provisions (power: institutional; exit: analytical)
 *   - absolutist_reading_advocates: Civil liberties organizations and scholars excluded from benefit coalition, arguing the framework enables pretextual restriction (power: powerful; exit: mobile)
 *   - dignity_reading_advocates: Civil rights organizations and scholars excluded from benefit coalition, arguing the framework insufficiently protects victims (power: powerful; exit: mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.52).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Protection with Harm Balancing (Proportionality Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, 'ce607e6c-255a-4553-967f-b2290431e597').
narrative_ontology:cs_kernel_codification('ce607e6c-255a-4553-967f-b2290431e597', formalized).
narrative_ontology:cs_authority_grounding('ce607e6c-255a-4553-967f-b2290431e597', lineage).
narrative_ontology:cs_interpretation_layer_present('ce607e6c-255a-4553-967f-b2290431e597').
narrative_ontology:cs_reading_relation('ce607e6c-255a-4553-967f-b2290431e597', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce607e6c-255a-4553-967f-b2290431e597', speech_harm_boundary__dignity_reading, influences).
narrative_ontology:cs_axiom('ce607e6c-255a-4553-967f-b2290431e597', foundational, speech_protection_presumptive_with_harm_override).
narrative_ontology:cs_axiom_status(speech_protection_presumptive_with_harm_override, holdable).
narrative_ontology:cs_axiom_grounding('ce607e6c-255a-4553-967f-b2290431e597', speech_protection_presumptive_with_harm_override, deontological).
narrative_ontology:cs_axiom('ce607e6c-255a-4553-967f-b2290431e597', foundational, proportionality_balancing_required).
narrative_ontology:cs_axiom_status(proportionality_balancing_required, holdable).
narrative_ontology:cs_axiom_grounding('ce607e6c-255a-4553-967f-b2290431e597', proportionality_balancing_required, instrumental).
narrative_ontology:cs_reference_frame('ce607e6c-255a-4553-967f-b2290431e597', presumptive_protection_framework).
narrative_ontology:cs_drift_state('ce607e6c-255a-4553-967f-b2290431e597', contemporary_adjudicative_practice_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ce607e6c-255a-4553-967f-b2290431e597', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harm_victims_seeking_remedy).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, marginalized_communities_facing_organized_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, advocacy_groups_with_marginal_positions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, advocacy_groups_with_marginal_positions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and organizations who wish to express viewpoints that fall within the harm-balancing framework's unprotected categories (hate speech, group libel, harassment, incitement to violence). They must either refrain from expression or face legal liability, civil suits, or platform removal. Their constraint is that presumptive protection yields once proportionate harm is demonstrated; the boundary between protected and unprotected is determined ex post through judicial or administrative review.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_subject_to_restriction, payer,
    moderate, biographical, constrained, national).

% Individuals and groups who experience documented harm from targeted speech: defamation, incitement to violence against them, coordinated harassment, group libel. The harm-balancing reading provides them legal standing to seek remedies (damages, injunctions, platform enforcement). The remedy mechanism is their primary tool for recourse; without it they would have no recourse against organized speech violence. They do not set the rules but benefit from their existence.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harm_victims_seeking_remedy, beneficiary,
    powerless, biographical, trapped, national).

% Communities (religious minorities, ethnic groups, LGBTQ+ populations, refugee populations) targeted by coordinated hate campaigns or systemic slurs. They benefit from the harm-balancing framework's recognition that organized dehumanization and group libel constitute demonstrable harm requiring remedy. Without this reading, organized campaigns against them would receive absolute protection. They have organizational capacity to advocate but constrained exit from exposure to the speech they experience as targeted harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, marginalized_communities_facing_organized_speech, beneficiary,
    organized, generational, constrained, national).

% Organizations advocating positions on immigration restriction, religious criticism, gender ideology, or other contested domains that may fall within hate-speech or harassment categories under the proportionality framework. They experience the constraint as a cost to their advocacy (risk of legal liability); they also—as organizations—may benefit from remedies against targeted harassment of their members. Dual-positioned: they are both restricted speakers and potential beneficiaries of harm remedies.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, advocacy_groups_with_marginal_positions, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, advocacy_groups_with_marginal_positions, beneficiary).

% Courts, administrative tribunals, and content moderation bodies that apply the harm-balancing test. They determine whether speech crosses from protected to unprotected by assessing: (1) the directness and foreseeability of harm, (2) the proportionality between restriction and harm severity, (3) the availability of less restrictive alternatives. They hold the interpretive power to set the operative boundary; the constraint's shape is determined by how they apply the proportionality standard.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, adjudicative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Parliaments and legislative institutions that enact hate-speech statutes, harassment laws, group-libel provisions, and incitement statutes—the codified tools for operationalizing the harm-balancing reading. They set the categorical boundaries and evidentiary standards; their choices determine whether the balancing framework is narrow (high threshold for harm demonstration) or expansive (lower threshold, broader unprotected categories).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, legislative_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Civil liberties organizations and legal scholars advocating the absolutist reading: speech protection should be nearly categorical regardless of claimed harm. They argue that harm-balancing reading enables pretextual restriction and authoritarian mission creep. They would contest the operative definition of harm, the proportionality standard, and the legitimacy of the adjudicative bodies applying it. Their voice is excluded from the benefit structure of the harm-balancing framework but remains a live alternative in legal and political discourse.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_reading_advocates, excluded,
    powerful, generational, mobile, national).

% Civil rights organizations and legal scholars advocating the dignity reading: speech that denies the personhood or human dignity of groups should be categorically unprotected, independent of quantifiable harm demonstration. They view the harm-balancing reading as insufficiently protective—it requires proof of harm before restriction, leaving victims to vindicate their dignity through litigation. They would shift the framework to categorical exclusion of dehumanizing speech regardless of impact threshold. Their voice shapes the political contest but is excluded from the ruling coalition in jurisdictions adopting the harm-balancing reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, dignity_reading_advocates, excluded,
    powerful, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, adjudicative_bodies).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reconciling free expression with protection against demonstrable harms: allows speech protection to remain presumptive (speakers have a default right to expression) while permitting override when proportionate harm is shown. Solves the coordination problem of how to prevent organized speech violence without enabling authoritarian content suppression.
% TRANSFER_FUNCTION: Transfers from speakers (incur restriction costs, risk of liability, platform removal) to harm victims (gain legal standing, remedy mechanisms, protection against targeted campaigns). The transfer is conditional: triggered only when harm is demonstrated and proportionality is satisfied. Proportionality assessment itself transfers authority to adjudicative bodies (courts and tribunals gain power to set harm thresholds and override presumptive protection).
% ABSENT_VOICES: Absolutist reading advocates (civil liberties maximalists) are excluded from the benefit coalition; they argue the framework itself enables pretextual restriction. Dignity reading advocates (categorical dehumanization prohibition) are excluded; they argue the framework insufficiently protects targeted groups by requiring harm proof. Both remain live positions in political and legal discourse but do not shape the operative rules under this reading.
% DISAPPEARANCE_RATIONALE: If the harm-balancing framework vanished overnight, organized speech campaigns against marginalized groups would have no legal remedy, targeted harassment would receive absolute protection, and victims would have only reputation defense (which is typically unavailable against coordinated campaigns). The legal landscape would shift to either absolutist (speech nearly always protected regardless of harm) or dignity-based (categorical exclusion of dehumanizing speech independent of harm proof) frameworks. Speakers currently restricting expression due to liability risk would face different incentive structures; platforms would lose legal clarity on content policy; adjudicative bodies would lose a standard by which to resolve competing claims.
% FOUNDING_PROBLEM: Early free-speech frameworks (and absolutist readings of them) provided speakers with protection but left victims of organized hate campaigns, defamation, and harassment without legal remedy. Marginalized communities faced coordinated campaigns designed to dehumanize and incite violence with no recourse other than counter-speech or social response. The framework was built to enable societies to protect speech while acknowledging that some speech causes demonstrable harm requiring proportionate remedy.
% FOUNDING_PROBLEM_CORROBORATION: Harm victims and marginalized communities document the persistence of organized hate campaigns and targeted harassment as a live problem. Civil liberties scholars (absolutist reading) dispute that the problem justifies restriction, arguing counter-speech and social remedy are sufficient. Dignity reading advocates dispute that harm-balancing adequately protects victims, arguing dehumanizing speech should be categorically excluded. Legislative testimony and comparative constitutional law from jurisdictions with both absolutist and harm-balancing frameworks show competing empirical claims about whether hate-speech legislation reduces harm or enables pretextual restriction.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the constraint imposes real costs on speakers (restriction, liability risk) but presumptive protection and proportionality requirements limit the breadth of unprotected categories. Suppression is slightly higher (0.52) because the constraint requires active enforcement machinery to apply the proportionality test, and that machinery can be deployed to suppress marginal positions. Suppression is not extreme because the presumptive protection and proportionality requirement provide structural limits—a pure extraction mechanism would lack these constraints. Theater ratio remains modest (0.28) because the proportionality assessment is genuinely performed in adjudication; performative elements exist (proportionality language can mask categorical exclusion) but do not dominate. The measurement series show slight rise through t=15 (suppression machinery stabilizes, enforcement capacity increases) then plateau: the constraint reaches a stable state where adjudicative practice has settled and the boundaries are established. Accessibility collapse is moderate-high (0.62): once the harm-balancing framework is understood, speakers have difficult alternatives (self-censor, face liability, litigate); marginalized communities have no exit from exposure to harm. Resistance is elevated (0.71) because the constraint is actively contested by both absolutist and dignity reading advocates, and speakers under restriction continuously challenge the proportionality boundaries.
 *
 * PERSPECTIVAL GAP:
 *   Speakers and harm victims have fundamentally opposed stakes: speakers experience the constraint as suppression of expression, harm victims experience it as necessary remedy. Marginalized communities occupy a precarious position: they benefit from legal protection but cannot exit from exposure to organized speech violence. Advocacy groups with unpopular positions experience the constraint as targeting their advocacy while potentially protecting their members from counter-harassment. Adjudicative bodies experience the constraint as a mandate to balance competing interests; their interpretation power determines whether the framework operates as narrow protection (absolutist-leaning, high harm threshold) or broad restriction (dignity-leaning, lower threshold). The measurement series show the constraint stabilizing as adjudicative practice settles—early periods have higher suppression_requirement because enforcement machinery is being built out and boundaries are contested; later periods plateau as established doctrine guides application.
 *
 * DIRECTIONALITY LOGIC:
 *   The harm-balancing reading makes directionality depend on demonstrable harm: speakers without demonstrable-harm findings remain presumptively protected (low d if they operate within protected categories); speakers whose speech causes demonstrable proportionate harm are targets (high d). This reading-specific structure creates a time-dependent and contingency-dependent directionality not present in absolutist or dignity readings. Harm victims and marginalized communities benefit from the framework's existence—they gain legal standing and remedy mechanisms they lack under absolutist readings. Adjudicative bodies hold interpretive power to set the harm threshold and proportionality standard; their choices determine whether the framework operates narrowly or broadly. Absolutist advocates experience the constraint as suppressive (they would argue many hate-speech statutes misapply proportionality); dignity advocates experience it as insufficiently protective (they would argue more categories should be categorically excluded). The excluded voices are structural: the harm-balancing framework's coherence depends on excluding categorical approaches (both absolutist and dignity-based) in favor of case-by-case proportionality assessment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: harm victims and marginalized communities attest that organized hate campaigns and targeted speech violence are a live problem requiring remedy; absolutist advocates dispute that the problem justifies restriction; dignity advocates dispute that harm-balancing provides adequate protection. The constraint's founding mandate was to balance free expression with harm protection; the mandate remains live insofar as organized speech campaigns continue and harm victims seek remedies. However, there is chronic tension between the presumptive protection principle and the harm-balancing test: as adjudicative practice develops, the boundary between protected and unprotected speech shifts based on how courts apply proportionality. This drift is captured in the measurement series: theater_ratio rises slightly (0.18 → 0.28) as adjudicative practice develops performative elements (proportionality language that masks categorical judgments), but remains modest because genuine balancing occurs in adjudication. The constraint does not show mandatrophy—the founding mandate remains relevant and the enforcement machinery remains in use—but it shows boundary-drift: what counts as 'demonstrable harm' and 'proportionate restriction' evolves as case law accumulates. The classification is tangled_rope (not snare) because the constraint contains a genuine coordination function (balancing free expression with harm protection) alongside extractive elements (speakers bear disproportionate restriction costs relative to harm-severity thresholds set by adjudicative bodies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_standard_instability,
    'What counts as ''demonstrable harm'' and ''proportionate restriction'' in the proportionality test, and how do these definitions change as adjudicative practice evolves?',
    'Systematic comparative analysis of hate-speech case law across jurisdictions and time periods: tracking how courts define harm severity, directness, foreseeability, and proportionality thresholds in actual decisions.',
    'If the proportionality standard drifts toward broader harm definitions and lower thresholds, the operative boundary shifts toward dignity-reading territory (broader unprotected categories). If it drifts toward stricter thresholds, it shifts toward absolutist territory. The stable classification depends on proportionality remaining genuinely balanced; sustained drift in either direction would indicate misclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_standard_instability, empirical, 'Whether the proportionality test remains a genuine balance or drifts toward categorical exclusion/absolute protection.').

omega_variable(
    adjudicative_authority_asymmetry,
    'Do adjudicative bodies actually apply the proportionality test evenhandedly, or do they apply it asymmetrically—protecting popular speech while restricting marginal advocacy?',
    'Empirical analysis of case outcomes: comparing restriction rates for mainstream speech vs. marginal advocacy; tracking whether proportionality findings are applied symmetrically across political/ideological categories.',
    'If application is asymmetric, the constraint operates as a snare—the proportionality language masks categorical targeting of marginal speakers, and the adjudicative bodies function as extractors for the established coalition. If application is symmetric, it is genuinely a tangled rope (coordination + extraction in balance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adjudicative_authority_asymmetry, empirical, 'Whether proportionality testing is applied evenly or asymmetrically by adjudicative bodies.').

omega_variable(
    harm_definition_reading_variance,
    'Does the harm-balancing reading instantiate a different definition of ''harm'' than the dignity reading or absolutist reading would use, and if so, is that definitional difference structural or arbitrary?',
    'Comparative legal and philosophical analysis: documenting what each reading counts as harm (dignity: personhood-denial; harm-balancing: demonstrable impact; absolutist: near-zero); assessing whether the differences follow from the core axioms of each reading or are ad-hoc choices.',
    'If the differences are structural and follow from the readings'' foundational premises, the harm-balancing reading is a coherent alternative framing. If the differences are arbitrary or post-hoc, the constraint''s classification depends more on implicit judicial choices than on the announced framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_reading_variance, conceptual, 'Whether different readings of the speech-harm boundary necessarily generate different harm definitions, or whether the definitions are independent choices.').

omega_variable(
    marginalized_group_exit_constraint,
    'Are marginalized communities truly unable to exit from exposure to organized speech harm, or do they have practical options (geographic mobility, community relocation, cultural exit) that constrain but do not trap them?',
    'Ethnographic and historical analysis of how targeted communities respond to organized campaigns: do they relocate, establish protected spaces, exit public discourse, or remain embedded despite exposure?',
    'If communities are trapped (exit_options: trapped), the constraint''s benefit to them is higher (they depend entirely on the legal remedy); if constrained (exit_options: constrained), the benefit is lower. This affects the directionality gradient for the marginalized_communities stakeholder and the balance between coordination and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_group_exit_constraint, empirical, 'Whether marginalized communities facing organized speech harm are trapped or constrained in their exit options.').

omega_variable(
    reading_relationship_foreclosure_test,
    'Does the core axiom of the harm-balancing reading (proportionate demonstrable harm overrides presumptive protection) logically foreclose the absolutist axiom (speech protection near-absolute regardless of harm), or do they merely occupy different normative frameworks?',
    'Philosophical analysis of the logical relationship: can a single institutional authority hold both axioms simultaneously (applying both proportionality balancing and absolute protection in different contexts), or does adoption of one necessarily reject the other?',
    'If they logically foreclose each other, the reading_relations entry should be forecloses. If they occupy different frameworks without mutual logical contradiction, it should be coexists_with. This affects the cs_structure.reading_relations classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relationship_foreclosure_test, conceptual, 'Whether the harm-balancing axiom logically forecloses the absolutist axiom or they coexist in different frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_harm_boundary__harm_balancing_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_harm_boundary__harm_balancing_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_harm_boundary__harm_balancing_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t25, speech_harm_boundary__harm_balancing_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(spee_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t25, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(spee_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t25, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(spee_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech-harm boundary kernel decomposes into three structurally distinct constraints per OQ-254 ε-invariance: absolutist_reading (speech protection near-categorical; ε ≈ 0.05; mountain-like structure), dignity_reading (dehumanizing speech categorically unprotected; ε ≈ 0.72; snare-like structure), and this harm-balancing_reading (presumptive protection yields to demonstrable harm; ε ≈ 0.48; tangled-rope structure). Each reading instantiates different unprotected categories, different directionalities for speakers and victims, and different operative boundaries. They are not the same constraint viewed from different angles—they have different ε values, different beneficiary/victim structures, and different enforcement mechanisms. They are linked as siblings in the kernel_context field; each reading's cs_structure.reading_relations field documents structural relationships to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
