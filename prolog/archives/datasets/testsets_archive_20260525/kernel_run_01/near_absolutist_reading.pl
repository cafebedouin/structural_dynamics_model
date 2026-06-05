% ============================================================================
% CONSTRAINT STORY: near_absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_near_absolutist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: near_absolutist_reading
 *   human_readable: Near-Absolutist First Amendment Reading: Content Restriction Prohibition with Narrow Categorical Exceptions
 *   domain: constitutional_law/free_speech/political_philosophy
 *
 * SUMMARY:
 *   The near-absolutist First Amendment reading holds that the First
 *   Amendment's 'no law' language prohibits content-based restrictions on
 *   speech except for narrow categorical exceptions defined by imminent
 *   physical harm (incitement, fighting words). This reading instantiates one
 *   distinctive reading of the broader speech_protection_boundary kernel —
 *   how to balance free speech protection against other social goods
 *   (dignity, safety, equal access to discourse). The near-absolutist reading
 *   prioritizes protection of dissident and institutional speech against
 *   government suppression over protection of targeted groups from dignitary
 *   harm. It treats government censorship as the primary threat to meaningful
 *   speech and understands content-neutrality as the governing legal
 *   principle. This creates a structural asymmetry: the reading provides
 *   robust protection for speakers while offering minimal protection for
 *   listeners who are targets of hateful speech. The constraint exhibits
 *   tangled_rope structure: it coordinates protection for speech against
 *   government suppression (genuine coordination function) while
 *   systematically distributing dignitary harm to vulnerable groups who
 *   cannot exit their targeted status (asymmetric extraction). The theater
 *   ratio (0.48) reflects that the absolutist principle operates as a
 *   coherent doctrine at the level of constitutional rhetoric but
 *   increasingly operates performatively in practice: courts invoke
 *   absolutism while carving out exceptions (secondary effects doctrine,
 *   compelled speech doctrine, speech integral to conduct doctrine,
 *   obscenity, defamation, harassment, intellectual property restrictions).
 *   The extractiveness has increased over the measurement interval (0.38 to
 *   0.52) as digital communication amplified coordinated hateful speech
 *   targeting vulnerable groups, increasing the dignitary harm distribution
 *   while the legal protection for dissident speakers remained relatively
 *   stable.
 *
 * KEY AGENTS:
 *   - Dissident Speakers: Primary beneficiary (moderate/mobile) — gain robust protection for counter-power speech and unpopular political positions against government suppression
 *   - Institutional Press: Primary beneficiary (institutional/arbitrage) — gains protection against prior restraint and editorial censorship
 *   - Targeted Members of Vulnerable Groups: Primary victim (powerless/identity_locked or trapped) — bear dignitary harm from legally protected hateful speech directed at identity groups; cannot exit targeted status
 *   - Stigmatized Identity Groups (collective): Secondary victim (powerless/trapped) — systematic exposure to coordinated hateful speech with minimal legal recourse; harm is distributed asymmetrically to groups rather than individuals
 *   - Civil Rights / Dignity-Protection Coalition: Organized secondary actor (organized/constrained) — seeks to erode the near-absolutist reading through alternative doctrinal developments and statutory law; developing sunset mechanisms through platform governance and employment discrimination law
 *   - Absolutist Legal Doctrine (as institutional system): Institutional actor (institutional/constrained) — maintains the rhetorical principle while carving out exceptions in practice; exhibits piton characteristics (degraded function preserved through institutional inertia)
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees the true structure: coordinated protection for speech offset by distributed extraction from targeted groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(near_absolutist_reading, 0.52).
domain_priors:suppression_score(near_absolutist_reading, 0.35).
domain_priors:theater_ratio(near_absolutist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(near_absolutist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(near_absolutist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(near_absolutist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(near_absolutist_reading, tangled_rope).
narrative_ontology:human_readable(near_absolutist_reading, "Near-Absolutist First Amendment Reading: Content Restriction Prohibition with Narrow Categorical Exceptions").
narrative_ontology:topic_domain(near_absolutist_reading, "constitutional_law/free_speech/political_philosophy").

domain_priors:requires_active_enforcement(near_absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(near_absolutist_reading, fixed_text).
narrative_ontology:cs_authority_grounding(near_absolutist_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(near_absolutist_reading).
narrative_ontology:cs_kernel_id(near_absolutist_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation(near_absolutist_reading, dignitary_harm_reading, coexists_with).
narrative_ontology:cs_reading_relation(near_absolutist_reading, balancing_reading, coexists_with).
narrative_ontology:cs_axiom(near_absolutist_reading, foundational, speech_protection_categorically_prior).
narrative_ontology:cs_axiom_status(speech_protection_categorically_prior, holdable).
narrative_ontology:cs_axiom(near_absolutist_reading, foundational, content_neutrality_legally_required).
narrative_ontology:cs_axiom_status(content_neutrality_legally_required, holdable).
narrative_ontology:cs_reference_frame(near_absolutist_reading, constitutional_text_priority_over_effects).
narrative_ontology:cs_drift_state(near_absolutist_reading, contemporary_digital_harassment_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(near_absolutist_reading, dissident_speakers).
narrative_ontology:constraint_beneficiary(near_absolutist_reading, unpopular_ideological_positions).
narrative_ontology:constraint_beneficiary(near_absolutist_reading, institutional_press).
narrative_ontology:constraint_victim(near_absolutist_reading, dignity_bearers).
narrative_ontology:constraint_victim(near_absolutist_reading, targeted_vulnerable_groups).
narrative_ontology:constraint_victim(near_absolutist_reading, peaceful_assembly_protection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSIDENT SPEAKER (ROPE) — Moderate power agent with mobility relative to the constraint. The near-absolutist reading protects this agent's capacity to voice unpopular positions without government suppression. Experience is primarily coordinative: the constraint solves the collective action problem of ensuring speech access for those without institutional backing. Some cost (social stigma, marketplace retaliation) but minimal legal suppression. Genuine coordination benefit dominates.
constraint_indexing:constraint_classification(near_absolutist_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: TARGETED MEMBER OF VULNERABLE GROUP (TANGLED ROPE) — Trapped agent with no exit from exposure to hateful speech targeted at their identity group. The near-absolutist reading provides some coordination function (protects future dissident speech that might defend vulnerable groups) but enforces a genuine asymmetry: the powerless bear the cost of dignitary harm from absolutist protection while the main beneficiaries (institutional press, dissident speakers with platforms) gain protection from government suppression. High experienced extraction due to trapped status and asymmetric harm distribution.
constraint_indexing:constraint_classification(near_absolutist_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: STIGMATIZED IDENTITY GROUP (SNARE) — The near-absolutist reading creates a structural mechanism whereby coordinated hateful speech targeting identity groups is legally protected as core First Amendment expression. The victimized group cannot exit their targeted status. The constraint has minimal coordination function from their perspective (they derive no benefit) but maximum extraction (exposure to systematic dignitary harm legally immunized). Identity-locked because exit would require accepting a different identity or relocating. The absolutist framing naturalizes the constraint as legal principle rather than distributional choice.
constraint_indexing:constraint_classification(near_absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 4: INSTITUTIONAL PRESS (ROPE) — Institutional beneficiary with arbitrage options. The near-absolutist reading protects press publication against government censorship and prior restraint. Experiences the constraint as pure coordination: it solves the problem of ensuring editorial independence from state control. Net beneficiary with significant arbitrage (can publish across jurisdictions, has legal resources). Coordination is the primary experience.
constraint_indexing:constraint_classification(near_absolutist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS / DIGNITY-PROTECTION COALITION (SCAFFOLD) — Organized agents (civil rights groups, hate crime prevention organizations) experience the near-absolutist reading as a temporary constraint that is being eroded by alternative doctrinal developments (hostile-environment harassment law, Title VII workplace protections, hate crime statutes, social media platform moderation standards). The coalition sees a sunset mechanism: while First Amendment doctrine remains absolutist at the constitutional level, statutory law (employment discrimination, housing discrimination) and private platform governance are creating parallel spaces where dignity protection operates. The constraint has low effective extraction from the coalition's perspective because they have institutional agency and see structural alternatives emerging.
constraint_indexing:constraint_classification(near_absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ABSOLUTIST LEGAL DOCTRINE (PITON) — The near-absolutist reading as a legal principle has become increasingly performative. The doctrine claims categorical prohibition on content restrictions except narrow exceptions (incitement, fighting words), but actual First Amendment jurisprudence has layered on numerous content-based exceptions and restrictions (secondary effects doctrine, compelled speech doctrine, speech integral to conduct doctrine, obscenity, defamation, harassment). The theater ratio (0.48) reflects that the absolutist principle remains rhetorically dominant while practice has substantially departed. The doctrine persists through institutional inertia: courts invoke absolutism while carving out exceptions, legal scholarship debates whether exceptions undermine the principle, but the core principle remains officially unchallenged. Piton classification reflects degraded function — the principle no longer does the work it claims to do.
constraint_indexing:constraint_classification(near_absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the near-absolutist reading instantiates a genuine but deeply consequential choice: it coordinates robust protection for dissident speech (genuine coordination function) while systematically extracting from groups targeted by hateful speech (asymmetric harm distribution). The reading does solve a real collective action problem (ensuring speech access) but does so by treating dignitary harm as an acceptable cost external to the First Amendment's scope. The analytical classification reflects that both coordination and extraction functions are real and operate together. This is the constraint's true structural character — neither pure coordination nor pure extraction, but a hybrid that distributes benefit and harm asymmetrically.
constraint_indexing:constraint_classification(near_absolutist_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(near_absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(near_absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(near_absolutist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(near_absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(near_absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(near_absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The near-absolutist reading provides genuine protection for dissident speech against government suppression (coordination function) but distributes dignitary harm to vulnerable groups with no legal recourse except defamation law (which requires falsity). The extractiveness value reflects that while the constraint solves a real problem for some speakers, it systematically externalizes costs onto groups without power to negotiate or exit. The measurement trajectory (0.38→0.52) shows increasing extractiveness as digital communication and coordinated hate speech increased dignitary harm distribution without expanding the legal exceptions. Suppression (0.35): Moderate. The near-absolutist reading imposes suppression on speakers seeking to restrict hateful speech (they must accept the constraint or work through alternative channels like platform moderation, statutory law, or social movement pressure). However, suppression is not total — dignity-protection advocates have achieved significant carve-outs through hostile-environment harassment law, Title VII employment discrimination, hate crime statutes, and platform governance. Theater ratio (0.48): Moderate. The absolutist principle operates coherently at the constitutional rhetoric level but performatively in practice: courts invoke 'no law' absolutism while carving out categorical exceptions that approximate content restrictions (secondary effects, compelled speech, speech integral to conduct). The doctrine has not collapsed into pure theater because the principle still meaningfully constrains content-based government censorship. The theater ratio indicates legitimate doctrine with some performative elements rather than pure institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence rooted in unequal exit capacity. The beneficiaries (dissident speakers, institutional press) classify the constraint as rope or pure protection (rope). The victims (vulnerable group members) classify it as snare or asymmetric extraction (snare/tangled_rope). The coalition seeking alternative protections sees a sunset mechanism (scaffold). The absolutist doctrine itself appears degraded (piton) — the principle persists while its function has been substantially displaced by statutory law, platform governance, and social norms. The analytical observer sees the true structure (tangled_rope): coordination and extraction operating simultaneously on different agent populations. The perspectival gap is not merely observational but reflects real differences in what the constraint does for different agents — it genuinely coordinates for beneficiaries while genuinely extracting from victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural position: beneficiaries with exit options experience low d (negative effective extraction); victims with no exit experience high d (high effective extraction). Dissident speakers and institutional press are beneficiaries with substantial exit options (arbitrage: can organize speech outside government scope, can move jurisdictions, have institutional resources) → low d → low/negative f(d) → low chi. Vulnerable group members are victims with no exit from their targeted status (trapped or identity_locked) → high d → high f(d) → high chi. The organized coalition has constrained exit (can pursue alternative doctrinal channels, statutory law, platform governance) but not arbitrage → moderate d → moderate chi. The analytical observer's structural position (analytical/analytical) produces baseline canonical d → represents the aggregate structural picture unweighted by any single actor's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the near-absolutist reading is coherent as tangled_rope rather than pure rope (pure coordination). The absolutist framing naturalizes the constraint as logical necessity ('free speech principle requires absolutism') rather than distributional choice ('we have chosen to protect speech against government suppression at the cost of dignitary protection for targeted groups'). The false summit candidates are: (1) the Mountain perspective from an analytical observer who treats the absolutist principle as immutable natural law of liberal democracies (contradicted by EU democracies functioning differently); (2) the pure Rope perspective from beneficiary-centered analysis that focuses on coordination benefits while externalizing distributional costs. The mandatrophy resolves by accepting the tangled_rope classification: the constraint genuinely coordinates (solves the dissident speech protection problem) AND genuinely extracts (distributes dignitary harm to vulnerable groups). Both functions are real; neither can be dismissed as incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incitement_threshold_operationalization,
    'What operationalizes the ''imminent lawless action'' test for incitement? How predictive must the harm be, and who measures imminence?',
    'Empirical analysis of Brandenburg doctrine application across cases; correlation between speaker''s intent, audience receptivity, temporal proximity to action, and outcome; comparative analysis with jurisdictions using different imminence thresholds (EU ''dangerous tendency'' standard vs US ''imminent harm'')',
    'If incitement exception is applied strictly (requires near-certainty of immediate violence): near-absolutism holds and extraction for dignitary harm victims is high. If applied broadly (includes high probability of harm within hours or days): exception expands and extraction diminishes. If standard depends on speaker''s intent rather than objective probability: power dynamics matter more (institutional speakers get lower threshold, marginalized speakers get higher threshold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incitement_threshold_operationalization, empirical, 'How the incitement exception''s operationalization determines actual protection scope').

omega_variable(
    fighting_words_contextual_determination,
    'Is the ''fighting words'' exception determined by objective content (certain slurs are inherently fighting words) or by context (same utterance might be fighting words in one setting, protected speech in another)?',
    'Doctrinal analysis of fighting words cases and their reasoning patterns; comparison of how courts apply fighting words across different speaker identities (in-group members reclaiming slurs vs out-group members using slurs) and settings (private conversation vs public address vs small group confrontation)',
    'If objective: the exception is narrowly defined and near-absolutism largely holds. If contextual: the exception expands substantially based on power dynamics and audience composition, which means dignity victims have more legal protection but dissident speakers face higher uncertainty. This shapes the constraint''s extractiveness and the victim group''s exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fighting_words_contextual_determination, empirical, 'Whether fighting words is an objective or context-dependent category').

omega_variable(
    primary_beneficiary_identification_ambiguity,
    'Does the near-absolutist reading primarily benefit dissident speakers exercising counter-power speech, or does it primarily benefit institutional speakers and regimes suppressing marginalized movements?',
    'Historical analysis of near-absolutist doctrine''s application across speaker types (corporate speech, government employee speech, labor organizing, hate speech, civil rights activism); measurement of legal protection outcomes stratified by speaker institutional power and marginalization status; analysis of whether doctrine has asymmetrically protected powerful speakers while leaving vulnerable speakers exposed',
    'If benefits dissident speakers: the constraint is genuinely a coordination mechanism for counter-power speech and the tangled_rope/rope classifications hold. If asymmetrically benefits institutional speakers: the constraint is closer to a snare for vulnerable groups with a coordination framing that serves powerful speakers. This affects the reading''s coherence with its foundational axiom (speech access without power hierarchy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_beneficiary_identification_ambiguity, conceptual, 'Whether near-absolutism primarily benefits counter-power or institutional speech').

omega_variable(
    alternative_reading_foreclosure,
    'Does the near-absolutist reading''s logical structure make the dignitary_harm_reading impossible to hold within the same constitutional framework, or do they represent genuinely coexistent but different political commitments?',
    'Philosophical analysis of whether the two readings are logically contradictory or merely represent different value hierarchies. The near-absolutist reading prioritizes: (1) government power as the primary speech threat, (2) content-neutrality as the legal principle. The dignitary_harm_reading prioritizes: (1) social power (including coordinated speech) as a threat to meaningful speech access, (2) context-sensitivity as the legal principle. Are these logically incompatible or different-but-coherent premises?',
    'If forecloses: the dignity reading cannot coexist within the same constitutional framework and one must be abandoned. If coexists: both readings represent live positions that different parties hold simultaneously, and the constraint is the outcome of ongoing political contestation rather than a resolved logical question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether near-absolutism logically forecloses the dignitary harm reading').

omega_variable(
    reading_temporal_contingency,
    'Is the near-absolutist reading''s contemporary operationalization dependent on historical contingencies (specific Supreme Court Justice appointments, timing of doctrine development) such that a different constitutional path was available, or does it represent the inevitable logical outcome of First Amendment text?',
    'Historical analysis of alternative doctrinal paths that could have been taken (the Meiklejohn absolutist vs the Balancing School development paths in 1950s-70s scholarship); counterfactual analysis of how different judicial appointments or social movements would have shaped doctrine; comparative constitutional analysis of how other democracies structured speech protections differently from the same liberal premises',
    'If contingent: the reading is a constructed institutional arrangement with no necessary character, which raises the question of whether other arrangements would better distribute harms. If inevitable: the reading follows necessarily from First Amendment logic and only philosophical reconceptualization could change it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_temporal_contingency, conceptual, 'Whether near-absolutism is contingent or inevitable as constitutional doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(near_absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_tr_t0, near_absolutist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(near_tr_t25, near_absolutist_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(near_tr_t50, near_absolutist_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(near_be_t0, near_absolutist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(near_be_t25, near_absolutist_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(near_be_t50, near_absolutist_reading, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(near_absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(near_absolutist_reading, dignitary_harm_reading).
narrative_ontology:affects_constraint(near_absolutist_reading, balancing_reading).
narrative_ontology:affects_constraint(near_absolutist_reading, hate_speech_exception_doctrine).

% DUAL FORMULATION NOTE:
% The near_absolutist_reading is one of three major readings of the speech_protection_boundary kernel. The three readings decompose into separate constraint stories because they prioritize different legal principles (absolutism vs contextualism vs balancing) and therefore operate with different ε values and different victim/beneficiary structures. Linking these stories via network relationships enables the engine to track how doctri_nal contestation creates multiple simultaneous constraints on the same underlying domain (speech protection).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(near_absolutist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
