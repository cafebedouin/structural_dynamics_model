% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection — High Harm Override Threshold
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary treats speech
 *   protection as near-categorically paramount: only the narrowest
 *   unprotected categories (incitement to imminent lawless action, true
 *   threats, defamation, obscenity) permit government restriction. The
 *   reading declares that the harm threshold for override is structurally
 *   extremely high — speakers retain protection even when their speech causes
 *   demonstrable dignitary, psychological, or material harm to targets. This
 *   story instantiates that reading as a constraint: it models how the
 *   absolutist principle operates on the ground, who it benefits, who bears
 *   its costs, and what structural ambiguities attend treating it as natural
 *   law. The constraint's CLAIM is mountain (absolutism as a structural
 *   necessity of autonomous speech); the METRICS describe a reading that
 *   extracts from harm targets while benefiting protected speakers. This
 *   divergence is intentional: the story documents the false-summit question
 *   — is absolutism a natural law or a constructed constraint that benefits
 *   certain speakers?
 *
 * KEY AGENTS:
 *   - Unpopular speakers: benefit from the high harm threshold — their expression is protected even when targets suffer harm. High autonomy, near-zero legal suppression risk. Exit is territorial: move jurisdictions or fora to find less-absolute regimes, but within absolutist jurisdictions they are protected.
 *   - Harm targets (dignitary/incitement): bear the costs of the high threshold — they suffer speech-caused harm with limited legal recourse. Suppression operates partly structurally (legal bars to remedy, lack of counter-speech platform) and partly internalized (belief that their harm is the price of the system, that objection is illegitimate).
 *   - Marginalized political dissidents: benefit from absolutism when the dominant power would suppress them, harmed by it when dominant-group speakers target them with speech.
 *   - Epistemic authorities (media, academia, courts): interpret and administer the boundary between protected and unprotected speech. Beneficiaries through structural immunity from suppression; also targets when their legitimacy is attacked.
 *   - Platform operators (social media, publishers): intermediaries who must choose between absolutist enforcement (hosting all speech within the narrow unprotected categories) or balancing (removing speech that causes harm). Costs come from either choice: absolutism = harassment liability and user attrition; balancing = regulatory/legal liability.
 *   - Jurisdictions with balancing regimes: analytical observers. Their existence defines the sibling reading's live alternative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.52).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.21).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.21).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Speech Protection — High Harm Override Threshold").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:emerges_naturally(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '74f84548-902f-4600-be8d-527ea587f4d8').
narrative_ontology:cs_kernel_codification('74f84548-902f-4600-be8d-527ea587f4d8', fixed_text).
narrative_ontology:cs_authority_grounding('74f84548-902f-4600-be8d-527ea587f4d8', lineage).
narrative_ontology:cs_interpretation_layer_present('74f84548-902f-4600-be8d-527ea587f4d8').
narrative_ontology:cs_reading_relation('74f84548-902f-4600-be8d-527ea587f4d8', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('74f84548-902f-4600-be8d-527ea587f4d8', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('74f84548-902f-4600-be8d-527ea587f4d8', foundational, speech_protection_near_categorical).
narrative_ontology:cs_axiom_status(speech_protection_near_categorical, holdable).
narrative_ontology:cs_axiom_grounding('74f84548-902f-4600-be8d-527ea587f4d8', speech_protection_near_categorical, deontological).
narrative_ontology:cs_axiom('74f84548-902f-4600-be8d-527ea587f4d8', foundational, speaker_autonomy_precedes_target_harm).
narrative_ontology:cs_axiom_status(speaker_autonomy_precedes_target_harm, holdable).
narrative_ontology:cs_axiom_grounding('74f84548-902f-4600-be8d-527ea587f4d8', speaker_autonomy_precedes_target_harm, deontological).
narrative_ontology:cs_reference_frame('74f84548-902f-4600-be8d-527ea587f4d8', strict_categorical_protection).
narrative_ontology:cs_drift_state('74f84548-902f-4600-be8d-527ea587f4d8', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('74f84548-902f-4600-be8d-527ea587f4d8', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, unpopular_speakers).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, marginalized_viewpoint_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, institutional_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, harm_targets).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, platform_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers whose views are unpopular with dominant institutions (political dissidents, fringe ideologies, marginalized perspectives) benefit from absolutist protection: their expression is legally protected even when it offends, upsets, or causes psychological harm. They can speak without fear of legal suppression. Within absolutist jurisdictions, their exit options are low; in balancing jurisdictions, they can relocate but losing local audience. Their power varies: organized political dissidents have institutional backing, isolated individuals do not.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, unpopular_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals and groups subjected to speech that denies their personhood, incites violence, or causes severe dignitary harm (religious minorities targeted in hate speech, women in coordinated campaigns, LGBTQ+ people in systematic speech-based erasure). They bear the costs: legal inability to remedy the harm, ongoing exposure to the speech, psychological injury, sometimes material harm from incited action. They are excluded from the decision-making about the boundary itself — the absolutist framework treats their harm as an unavoidable cost of protection rather than as a policy input. Identity-locked suppression: they cannot exit by changing identities or by ignoring the speech (the harm is targeted at the identity itself).
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, harm_targets, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, harm_targets, excluded).

% Media organizations, politicians, corporations, and established institutions that speak with amplification and structural credibility. They benefit doubly from absolutism: (1) they can speak on controversial topics without legal liability even when causing dignitary harm; (2) they can speak about marginalized groups with near-immunity. Their power is highest because they control platforms and narrative framing. Exit options are near-arbitrage: they can move between jurisdictions, choose which claims to amplify, and often face no consequence for spreading harmful speech because their institutional status protects them from liability.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, institutional_speakers, beneficiary,
    institutional, generational, arbitrage, global).

% Social media companies, publishers, and digital distribution systems that must operationalize the speech boundary. Under absolutism, they must host speech up to the narrow unprotected categories, which exposes them to liability for user harassment and distributes harmful content to targets. They are payers (costs of harassment liability, user attrition from targets who leave); they are also agenda-setters because they control platform design, algorithmic amplification, and enforcement of the boundary. Their power is institutional and their exit options are high: they can change platform policies, move to balancing jurisdictions, or lobby for statutory change. Their position is architecturally contradictory: the absolutist reading externally mandates what they host while making them liable for the consequences.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, platform_operators, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__absolutist_reading, platform_operators, agenda_setter).

% Legal scholars, harm-impact researchers, advocacy organizations, and jurisdictions (European democracies, Canada, Australia) that argue for proportionality balancing instead of absolutism. They are excluded from the absolutist framework's decision-making — their core claim (that speech protection should yield to proportional harm assessment) is not admitted as a policy option in absolutist jurisdiction doctrine. Their exit options are constrained: they can build alternative jurisdictional arguments, teach balancing principles, document harm outcomes, but cannot override the absolutist boundary through normal legal channels.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, balancing_regime_advocates, excluded,
    organized, generational, constrained, national).

% The US judiciary, particularly the Supreme Court, that authoritatively interprets the First Amendment and administers the boundary between protected and unprotected speech. They maintain the absolutist reading through doctrine, interpret edge cases narrowly to preserve protection, and resist statutory attempts to broaden the unprotected categories. They are both administrators (they set the boundary) and analytical observers of its operation. Their exit options are analytical in the sense that doctrine can evolve, but the institutional path-dependence is very high — moving to a balancing regime would require overruling decades of precedent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, first_amendment_courts, agenda_setter,
    institutional, generational, analytical, national).

% UN human rights mechanisms, regional rights courts (European Court of Human Rights, Inter-American Court of Human Rights), and international law bodies that recognize speech protection as a right BUT place it on a parity with dignity and harm prevention rather than as categorically superior. They observe the absolutist reading as a choice point in how to weight competing rights. Their power is soft (advisory, not binding on the US) but their coherent alternative framework provides a live institutional comparison case.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__absolutist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__absolutist_reading, institutional_speakers).
narrative_ontology:fixing_cost_class(speech_harm_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables unpopular speech against power by making suppression legally impossible: a speaker cannot be silenced by government censorship if the standard is nearly-categorical protection. The coordination solves the collective-action problem of protecting dissent without creating a system where majorities (or governments) can selectively silence minorities.
% TRANSFER_FUNCTION: Transfers the cost of protecting unpopular speakers from speakers themselves to harm targets: speakers gain legal immunity (no suppression cost), while targets of harmful speech bear the material, psychological, and dignitary harm cost that absolutism permits to flow unchecked. The transfer is from powerless targets to protected speakers, concentrated where speakers are also institutionally powerful.
% ABSENT_VOICES: Harm targets are substantially excluded from the initial construction of the boundary. People who would argue that speech denying their personhood should receive reduced protection, that incitement thresholds are too high, that dignity rights should constrain speech — these voices are not in the room where the boundary is set (courts, legislatures, academic doctrine) to the extent that absolutism takes their exclusion as settled. Jurisdictions and advocates of balancing regimes are also excluded: their claim that proportionality balancing is preferable is treated as not-serious in absolutist doctrine.
% DISAPPEARANCE_RATIONALE: Absolutism's beneficiaries (unpopular speakers, institutional media, libertarian discourse communities) argue disappearance would devastate press freedom and enable authoritarian suppression — the world would rearrange catastrophically. Harm-target advocates argue the absolutist boundary has become ornamental (political suppression is constrained by other legal and reputational mechanisms, not by needing a categorical rule), and that its removal would enable legal remedies for systematic harm without substantially increasing authoritarian risk — the world would rearrange minimally in practice. Courts and constitutional theory treat the boundary as foundational and near-immovable. The contest is genuine and reflects different empirical beliefs about what maintains freedom and what would happen under change.
% FOUNDING_PROBLEM: In mid-20th-century contexts (Cold War, McCarthy era, segregationist suppression of civil rights speech, authoritarian governments worldwide), political power frequently used censorship laws, defamation suits, obscenity charges, and sedition statutes to silence dissent. The founding problem was: how can dissident speech be protected from state suppression without giving the government discretion to classify dissent as dangerous or offensive? Absolutism was the answer: draw the line so narrowly and so explicitly that government cannot easily use speech restrictions as a disguise for political suppression.
% FOUNDING_PROBLEM_CORROBORATION: Absolutism's defenders point to contemporary authoritarian governments (China, Russia, Hungary) that suppress speech and argue the problem remains live — the threat of suppression never disappears; vigilance requires maintaining the high boundary. Independent historians and legal scholars document that mid-20th-century suppression was severe and justified absolutism in those contexts. However, harm-target advocates, digital-harm researchers, and balancing-regime proponents argue that in liberal democracies, the suppression problem has been substantially internalized (constitutional norms, institutional checks, electoral accountability, media scrutiny) such that political actors face major reputational and electoral costs to naked suppression — the problem is not dead but its salience has shifted. European democracies and Canada have adopted balancing regimes and have not experienced authoritarian collapse, which suggests the founding problem's solution is not uniquely dependent on absolutism. The corroboration is mixed: absolutism's efficacy in preventing suppression is attested by those who maintain it; its necessity in contemporary liberal contexts is disputed by empirical researchers and by functioning balancing jurisdictions.
narrative_ontology:disappearance_verdict(speech_harm_boundary__absolutist_reading, contested).
narrative_ontology:founding_problem_status(speech_harm_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_harm_boundary__absolutist_reading),
    narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the absolutist reading operationally costs harm targets substantially (legal inability to remedy, diffuse harm from unfiltered speech) while benefiting protected speakers (near-immunity from legal consequences for their expression). The metric rises over the interval (0.55 to 0.68) because the expansion of platforms and algorithmic amplification distributes harmful speech more widely, increasing targets' costs without changing the legal boundary. Theater ratio is low (0.21, rising slowly) because the absolutist boundary is administered with relative consistency — most enforcement activity genuinely applies the legal categories rather than performing compliance. Suppression requirement is also low (0.52) because absolutism requires less active enforcement machinery to maintain (the rule is formally simple: nearly everything is protected; boundaries are narrow and relatively stable). Resistance is high (0.74) because harm targets mount continuous contestation — academic work, harm-impact testimony, balancing-regime advocacy, counter-speech initiatives — indicating the reading's persistence is NOT due to universal acceptance but to ongoing defense of the boundary by its beneficiaries. Accessibility collapse is high (0.71) because once the absolutist rule is understood, harm targets' alternatives are genuinely constrained: they cannot appeal to law, cannot easily exit to other speech fora (platforms are global and nearly all have US-origin influences), and cannot rely on social norms of restraint when absolutism explicitly rejects norm-based restraint. The measurement series documents mounting extraction pressure as platforms scale and algorithmic amplification intensifies the reach of harmful speech without changing the boundary.
 *
 * PERSPECTIVAL GAP:
 *   An absolutist speaker (unpopular dissident in an oppressive context, journalist reporting on power) experiences the constraint as liberation — a structural necessity they would defend fiercely. A harm target (person subject to incel incitement, religious minority targeted by denying-personhood speech, woman in a sexualized hate campaign) experiences it as suppression apparatus — the legal system actively prevents their harm-remedy even when the harm is severe and targeted. The engine will compute these divergent d values from the structural data: speaker autonomy is high (low d), harm-target costs are high (high d), making the same constraint read as beneficiary-side mountain (natural law) from the speaker seat and target-side false summit (constructed extraction) from the harm-target seat. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Unpopular speakers are the structural beneficiaries of the absolutist boundary — it protects them from suppression risk. Their directionality is low (d near beneficiary end). Harm targets are the structural targets — they bear suppression costs (legal inability to remedy, psychological/material harm) and cannot escape without exiting the jurisdiction or public discourse entirely. Their directionality is high (d near target end). Platform operators sit between: they benefit from immunity from curating speech (no editorial liability), but they are also targets when absolutism creates liability for user harassment (lack of takedown options creates platform liability in tort). Power atoms matter: institutional speakers (media, government officials) can use absolutism to speak with amplification and zero legal consequence; powerless speakers get protection from oppression but may lack platforms to exercise it. The constraint's most extractive operation is where absolutism protects powerful speakers' harmful speech about powerless targets — the protection asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enabling dissidents to speak against power without legal suppression — was live and urgent in mid-20th-century contexts of governmental censorship and political prosecution. In those contexts, absolutism prevented manifestly unjust state suppression. The status today is CONTESTED: absolutism's supporters argue the problem remains live (power perpetually threatens speech; the boundary must remain high to prevent creep). Harm-target advocates argue the problem is substantially solved in liberal democracies (political suppression has major legal and reputational costs; absolute protection is no longer necessary to prevent authoritarianism) and that the costs to targets have become the constraint's primary function. The engine will detect a mandatrophy signal if the founding problem's solution is no longer operationally necessary but the constraint persists because beneficiaries (unpopular speakers in general, institutional media operating near-absolutely) maintain it. This is not a definitive finding — the contest is real — but it flags for investigation whether the absolutist reading serves its original function or has been captured by later beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the absolutist speech protection principle a natural law of human flourishing (first-amendment-as-structural), or a constructed legal reading that benefits certain speakers and marginalizes harm targets?',
    'Historical and comparative analysis: does the absolutist threshold emerge independently across jurisdictions with divergent legal traditions, or is it specific to US constitutional interpretation? Does speaker autonomy deliver measurably better social epistemic outcomes than balancing regimes? Do marginalized groups report access to speech fora equally under absolutist vs. balancing regimes?',
    'If natural, the constraint is genuinely a mountain — the harm costs are unfortunate but unavoidable structural features of a system that protects autonomy. If constructed, it is a false summit — beneficiaries (unpopular speakers protected by the high threshold) are identifiable, and the constraint''s persistence depends on their interest in maintaining it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, empirical, 'Whether absolutist speech protection is a natural law or a constructed constraint benefiting certain speakers.').

omega_variable(
    harm_target_identity_fusion,
    'Do harm targets (especially those subjected to speech that denies their personhood or incites violence) internalize the suppression — coming to believe they deserve the speech or cannot object to it — or is suppression purely structural (external barriers to counter-speech, platform exclusion)?',
    'Post-exit trajectory: do harm targets who move to fora with balancing regimes or stronger counter-speech norms report changes in their sense of voice and safety? Do communities with high absolutism show different rates of speech attrition (people dropping out of discourse) than communities with balancing?',
    'If internalized, the true suppression of harm targets is higher than the structural measure suggests — the absolutist reading''s operation creates psychological suppression that persists after the constraint''s mechanism is removed. This would elevate the constraint from mountain (unavoidable structural feature) toward false summit (a reading whose persistence depends on targets'' internalized compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_target_identity_fusion, empirical, 'Whether suppression of harm targets is structural or internalized under absolutism.').

omega_variable(
    speaker_autonomy_scope_asymmetry,
    'Does absolutist protection equally protect autonomous speech for all speakers, or does it amplify the speech of already-powerful agents (those with institutional platforms, economic resources, social credibility) while leaving powerless speakers'' autonomy practically constrained?',
    'Empirical mapping of who actually exercises near-absolute protection: institutional speakers (politicians, media, corporations) vs. individual speakers; dominant-group speakers vs. minority speakers; speakers with platform access vs. those dependent on public fora. If protection is asymmetric in practice, the reading''s claim to protect speaker autonomy universally is falsified.',
    'An asymmetric autonomy distribution would suggest the absolutist reading serves the interests of already-empowered speakers more than marginalized ones, despite the stated beneficiary set. This would reframe extraction: from whom does the reading extract, and to whom does it grant asymmetric autonomy?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(speaker_autonomy_scope_asymmetry, empirical, 'Whether absolutist protection operationalizes symmetrically across power positions.').

omega_variable(
    reading_contingency_on_liberal_epistemic_assumptions,
    'Does the absolutist reading depend on a specific set of liberal epistemic assumptions — that truth emerges from unfettered speech contest, that robust debate requires minimal prior restraint, that counter-speech is always available and effective — that may not hold in contexts of power asymmetry or epistemic closure?',
    'Comparative case analysis: do jurisdictions where the epistemic assumptions hold (decentralized media, distributed platforms, public literacy) show better outcomes under absolutism than in balancing regimes? Conversely, in contexts where epistemic assumptions fail (monopolistic media, algorithmic amplification, widespread disinformation), does absolutism degrade autonomy or safety outcomes?',
    'If the reading is contingent on assumptions that fail in many real contexts, its universality claim is undermined — the reading would function as a mountain only under specific (liberal) conditions, not universally. This would support decomposing into multiple constraints per regime type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_on_liberal_epistemic_assumptions, conceptual, 'Whether absolutism depends on liberal epistemic conditions that do not universally hold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t7, speech_harm_boundary__absolutist_reading, theater_ratio, 7, 0.11).
narrative_ontology:measurement_basis(spee_tr_t7, observed).
narrative_ontology:measurement(spee_tr_t14, speech_harm_boundary__absolutist_reading, theater_ratio, 14, 0.15).
narrative_ontology:measurement_basis(spee_tr_t14, observed).
narrative_ontology:measurement(spee_tr_t21, speech_harm_boundary__absolutist_reading, theater_ratio, 21, 0.19).
narrative_ontology:measurement_basis(spee_tr_t21, observed).
narrative_ontology:measurement(spee_tr_t28, speech_harm_boundary__absolutist_reading, theater_ratio, 28, 0.21).
narrative_ontology:measurement_basis(spee_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t7, speech_harm_boundary__absolutist_reading, base_extractiveness, 7, 0.59).
narrative_ontology:measurement_basis(spee_be_t7, observed).
narrative_ontology:measurement(spee_be_t14, speech_harm_boundary__absolutist_reading, base_extractiveness, 14, 0.63).
narrative_ontology:measurement_basis(spee_be_t14, observed).
narrative_ontology:measurement(spee_be_t21, speech_harm_boundary__absolutist_reading, base_extractiveness, 21, 0.66).
narrative_ontology:measurement_basis(spee_be_t21, observed).
narrative_ontology:measurement(spee_be_t28, speech_harm_boundary__absolutist_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement_basis(spee_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t7, speech_harm_boundary__absolutist_reading, suppression_requirement, 7, 0.49).
narrative_ontology:measurement_basis(spee_su_t7, observed).
narrative_ontology:measurement(spee_su_t14, speech_harm_boundary__absolutist_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement_basis(spee_su_t14, observed).
narrative_ontology:measurement(spee_su_t21, speech_harm_boundary__absolutist_reading, suppression_requirement, 21, 0.51).
narrative_ontology:measurement_basis(spee_su_t21, observed).
narrative_ontology:measurement(spee_su_t28, speech_harm_boundary__absolutist_reading, suppression_requirement, 28, 0.52).
narrative_ontology:measurement_basis(spee_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel decomposes into three ε-invariant constraint stories instantiating the three live readings in US constitutional tradition and global jurisprudence. Absolutist_reading (this story) frames protection as near-categorical with extremely high harm threshold; harm_balancing_reading treats protection as presumptive but proportionally defeatable; dignity_reading places dignity rights on parity with speech protection, categorically excluding personhood-denying speech. Each reading has distinct beneficiaries, victims, ε values, and mandatrophy profiles. They coexist as live positions held by different judicial coalitions, legal traditions, and jurisdictions. Absolutism influences both siblings by setting a high reference threshold against which balancing and dignity claims are measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, powerless, 0.87).
constraint_indexing:directionality_override(speech_harm_boundary__absolutist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
