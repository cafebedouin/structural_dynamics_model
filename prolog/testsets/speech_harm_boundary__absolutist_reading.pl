% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Speech Harm Boundary: Absolutist Reading (High-Threshold Doctrine)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   The absolutist reading of the speech-harm boundary establishes a high
 *   threshold for permissible speech restrictions, typically protecting all
 *   political speech and most offensive expression while banning only
 *   incitement to imminent violence, true threats, defamation, obscenity, and
 *   a narrow category of unprotected speech. This constraint story models
 *   this reading as ONE interpretation of a contested kernel (the
 *   speech_harm_boundary) that remains live in constitutional jurisprudence
 *   alongside harm-balancing and dignity-based readings. The absolutist
 *   reading creates an asymmetric extraction dynamic: speakers with
 *   institutional resources and platform access benefit from maximum
 *   protection, while targeted groups bearing speech harms (via harassment,
 *   dehumanizing rhetoric, epistemic exclusion) experience these harms as
 *   effectively unremediable through law. The constraint exemplifies how a
 *   doctrine framed as protection of a fundamental value (democratic
 *   self-governance, individual autonomy) can simultaneously function as an
 *   extraction mechanism when the distribution of speaking power is
 *   asymmetric. The extractiveness value (0.58) reflects that the harm
 *   threshold is set so high that most harmful speech remains legal, and the
 *   suppression value (0.72) reflects that targets have few legal or
 *   institutional remedies. The theater ratio (0.55) is moderate because the
 *   doctrine genuinely does protect important speech values, but increasingly
 *   performs this protection while the real threat environment (private
 *   platform curation, algorithmic amplification) has shifted away from the
 *   original justification (government censorship).
 *
 * KEY AGENTS:
 *   - Targeted Groups Bearing Speech Harms (powerless/trapped) — Primary victims experiencing harassment, dehumanization, epistemic exclusion with minimal legal remedy
 *   - Vulnerable Populations (moderate/constrained) — Secondary victims facing hostile discourse environment; can exit (withdraw from public participation) but at severe cost
 *   - Epistemic Commons Integrity (powerless/trapped) — Abstract collective bearing costs of misinformation, disinformation, and coordinated epistemic poisoning
 *   - Institutional Speakers & Media Elites (institutional/arbitrage) — Primary beneficiaries with platform access and resources; freedom to speak benefits them asymmetrically
 *   - Organized Counter-Speech Coalition (powerful/mobile) — Mixed position: benefit from protection of their own counter-speech but suffer asymmetry in platform access vs. hate speakers
 *   - Platform Governance Reformers (organized/constrained) — See absolutist doctrine as temporary constraint being replaced by alternative frameworks (platform standards, international law, algorithmic moderation)
 *   - Constitutional Doctrine Authority (institutional/arbitrage) — The legal doctrine itself persists through institutional inertia; performs legitimacy via precedent while functional justification has atrophied
 *   - Analytical Observer (analytical/analytical) — Risks naturalizing contingent institutional arrangement (speaker protection asymmetry) as unchangeable requirement of free society
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.58).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.72).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, snare).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Speech Harm Boundary: Absolutist Reading (High-Threshold Doctrine)").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, '66d72b2a-64e4-4e53-a514-2f03c9be8f00').
narrative_ontology:cs_kernel_codification('66d72b2a-64e4-4e53-a514-2f03c9be8f00', fixed_text).
narrative_ontology:cs_authority_grounding('66d72b2a-64e4-4e53-a514-2f03c9be8f00', lineage).
narrative_ontology:cs_interpretation_layer_present('66d72b2a-64e4-4e53-a514-2f03c9be8f00').
narrative_ontology:cs_reading_relation('66d72b2a-64e4-4e53-a514-2f03c9be8f00', harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('66d72b2a-64e4-4e53-a514-2f03c9be8f00', dignity_reading, forecloses).
narrative_ontology:cs_axiom('66d72b2a-64e4-4e53-a514-2f03c9be8f00', foundational, speech_protection_paramount_to_all_harms).
narrative_ontology:cs_axiom_status(speech_protection_paramount_to_all_harms, holdable).
narrative_ontology:cs_axiom_grounding('66d72b2a-64e4-4e53-a514-2f03c9be8f00', speech_protection_paramount_to_all_harms, deontological).
narrative_ontology:cs_axiom('66d72b2a-64e4-4e53-a514-2f03c9be8f00', foundational, government_censorship_threat_imminent).
narrative_ontology:cs_axiom_status(government_censorship_threat_imminent, overridden).
narrative_ontology:cs_axiom_grounding('66d72b2a-64e4-4e53-a514-2f03c9be8f00', government_censorship_threat_imminent, empirically_contingent).
narrative_ontology:cs_reference_frame('66d72b2a-64e4-4e53-a514-2f03c9be8f00', founding_era_anti_tyranny_principle).
narrative_ontology:cs_drift_state('66d72b2a-64e4-4e53-a514-2f03c9be8f00', digital_era_private_platform_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('66d72b2a-64e4-4e53-a514-2f03c9be8f00', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_with_institutional_resources).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, hate_speech_proponents).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, misinformation_spreaders).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targeted_groups_bearing_speech_harms).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, epistemic_commons_integrity).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_populations_excluded_from_discourse).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED GROUPS (SNARE) — Structurally trapped. No exit from the speech environment; cannot avoid hate speech, harassment, or defamatory claims without abandoning public participation. Experiences the absolutist doctrine as pure extraction: doctrine sets the harm threshold so high that most speech targeting them remains unprotected by law, yet affects them materially through reputation damage, safety threats, and epistemic exclusion. Maximum suppression of their counter-speech options — legal recourse exhausted, social media moderation inconsistent, institutional recourse absent.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VULNERABLE POPULATIONS / IDENTITY-BASED EXCLUSION (SNARE) — High costs to participation in public discourse due to speech targeting their identity. Hate speech, slurs, and dehumanizing rhetoric create hostile epistemic environment. Can exit (withdraw from public discourse) but at severe cost: loss of political voice, cultural erasure, capitulation to intimidation. The absolutist harm threshold means targeted speech remains legal even when it functions as discriminatory exclusion mechanism. Effective extraction: speaker gains amplified platform while targets must subsidize safety and emotional labor of hostile environment.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EPISTEMIC COMMONS INTEGRITY (SNARE) — Abstract collective good with no agent to defend it. Absolutist doctrine permits systematic misinformation, coordinated disinformation campaigns, and epistemic poisoning without legal remedy. The commons cannot exit, organize, or recover. Maximum extraction: speakers with resources can saturate information space; public epistemic trust erodes; institutions lose capacity to coordinate on shared facts. Theater ratio is moderate because misinformation masquerades as speech freedom rather than as coordinated extraction.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL SPEAKERS / MEDIA AND POLITICAL ELITES (ROPE) — Primary beneficiaries with full arbitrage options. Experiences the absolutist doctrine as pure coordination: freedom to speak enables public discourse function. Can amplify messages, define narrative frames, shape political agendas. Suppression of their speech is what they resist — not suppression of others. For this agent, the constraint provides unambiguous benefit (unrestricted voice) with minimal cost. The perspective sees the doctrine as coordination mechanism that enables democratic speech, not extraction.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED COUNTER-SPEECH COALITION (TANGLED ROPE) — Organized agents (civil rights organizations, marginalized communities with media capacity) benefit from the absolutist doctrine's protection of their own counter-speech but suffer from the doctrine's application to hate speech. Mobile because they can organize alternative speech platforms, legal strategies, and institutional pressure. Mixed experience: genuine coordination function (their speech is also protected) but asymmetric extraction (hate speakers have more resources and platforms). The doctrine protects both speech types equally, but asymmetric power means equal protection produces unequal outcomes.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PLATFORM GOVERNANCE REFORMERS (SCAFFOLD) — Organized agents (policy advocates, tech companies, international bodies) see the absolutist doctrine as a temporary constraint being replaced by alternative frameworks: platform community standards, algorithmic content moderation, international human rights law, and digital rights norms. The scaffold perspective sees the doctrine as increasingly obsolete — constrained by legacy constitutional doctrine but heading toward sunset through institutional pressure and technological substitution. Low effective extraction because reformers see agency and an exit path (alternative governance frameworks).
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CONSTITUTIONAL DOCTRINE AUTHORITY (PITON) — The legal doctrine itself as an institutional actor. The absolutist reading persists through institutional inertia: precedent, judicial path-dependency, formalist interpretation methodology. The doctrine's primary function (protecting political speech from government censorship) has atrophied in the digital age — the real threat to speech is now private platform curation, not government suppression. Yet the doctrine remains enforced through constitutional amendment difficulty and judicial conservatism. Theater ratio high because the doctrine performs its own legitimacy (precedent as authority) while its functional justification has degraded. Measured performance of doctrine maintenance without underlying function.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizable perspective, some speech must be absolutely protected to prevent government tyranny — individual autonomy and democratic self-governance require unrestricted discourse. No alternative institutional arrangement can protect these values without vulnerability to totalitarian capture. This perspective sees the harm threshold as an unchangeable requirement of any free society. However, the presence of identifiable beneficiaries (institutional speakers, hate speech proponents) and the specificity of the harm vector (targeting marginalized groups) reveal this as a false summit: the natural law framing naturalizes a contingent institutional arrangement that benefits some agents while imposing costs on others.
constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_harm_boundary__absolutist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, increasing from 0.35): The absolutist harm threshold means most harmful speech remains legally protected, creating extraction mechanism that transfers harm costs to targeted groups. The trajectory shows rising extractiveness over 20 years, indicating that as digital platforms and algorithmic amplification have scaled, the costs of the doctrine (harassment at scale, epistemic saturation, coordinated disinformation) have increased while the benefits (protection from government censorship) remain stable. Suppression (0.72, increasing from 0.60): Targets have limited legal recourse, weak social media moderation, inconsistent institutional response. The trajectory shows rising suppression as platform reach scales and targeted harassment becomes more coordinated. Theater ratio (0.55, increasing from 0.40): The doctrine invokes foundational values (democratic self-governance, individual autonomy) to justify its position, but these justifications apply less to contemporary threat environment (private monopoly curation rather than government censorship). The doctrine performs fidelity to founding principles while those principles no longer address dominant harms. The rising trajectory reflects increasing gap between rhetorical justification and actual function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Institutional speakers see pure coordination (Rope): the doctrine enables their speech and public discourse function — it is not extraction from their perspective, it is freedom. Targeted groups see pure extraction (Snare): no exit, high costs, minimal legal remedy — the doctrine transfers harm to them. The analytical observer risks seeing natural law (Mountain): free speech is an unchangeable requirement of any free society — but the presence of beneficiaries and asymmetric power reveals this as a false summit. The platform reformers see it as temporary (Scaffold): alternative governance frameworks (content standards, international law, algorithmic moderation) are replacing the doctrine through institutional pressure. The doctrine authority sees its own degradation (Piton): it performs legitimacy through precedent while its functional justification has eroded. The organized counter-speech coalition is caught in the middle (Tangled Rope): their own speech is protected but they lack the resources and platforms of institutional speakers, so equal protection produces unequal outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation flows from beneficiary/victim status and exit options. Institutional speakers (beneficiaries + arbitrage) derive low d, negative chi — the doctrine benefits them, not extracts from them. Targeted groups (victims + trapped) derive high d, high chi — maximum experienced extraction. Organized counter-speakers (mixed status + mobile) derive moderate d, moderate chi — they benefit from protection of their own speech but suffer from asymmetric power in platform access. The analytical observer position derives d from the structural relationship: seeing the doctrine as natural law (high accessibility_collapse, low resistance) vs. seeing it as contingent institutional arrangement favoring institutional speakers (beneficiaries present) produces different directionality interpretations. The false summit detector evaluates the mountain claim against beneficiary presence: if beneficiaries exist, the mountain classification is reclassified through the override chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (the classification problem when a constraint simultaneously exhibits features of multiple types) is resolved by indexing to observer position. From the targeted-group perspective, it is pure Snare (extraction with suppression). From the institutional-speaker perspective, it is pure Rope (coordination with benefit to all parties). From the analytical civilizational perspective, it appears to be Mountain (unchangeable requirement of free society), but the presence of beneficiaries triggers false summit detection, reclassifying it as Snare or Tangled Rope. The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' All six types are legitimate readings of the same constraint from different (P,T,E,S) tuples. The presheaf of classifications over the observation site IS the complete answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_measurement_boundary_ambiguity,
    'What constitutes actionable ''harm'' from speech sufficient to override protection? Is the threshold material violence, severe emotional harm, epistemic exclusion, or something else?',
    'Comparative analysis of harm outcomes under absolutist vs. balancing regimes; longitudinal tracking of harms to targeted groups; measurement of epistemic participation rates by population group under each regime.',
    'If epistemic harm (exclusion, degraded deliberation capacity) counts: doctrine reclassifies toward tangled_rope or snare for more perspectives. If only violent incitement counts: doctrine reclassifies as rope or scaffold for more perspectives. The question is where the doctrine''s harm boundary actually sits, not where absolutists claim it sits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_measurement_boundary_ambiguity, empirical, 'Boundary definition for actionable speech harm').

omega_variable(
    institutional_reading_vs_core_principle_gap,
    'Does the absolutist reading reflect the founding principle of speech protection (preventing government tyranny), or has it become a doctrine serving contemporary institutional interests (speakers with platform access, wealthy litigants)?',
    'Historical analysis of doctrine evolution; comparison of speech harms in founding era vs. contemporary era; measurement of whose speech is actually chilled by alternative regimes; examination of who successfully invokes the doctrine.',
    'If doctrine now serves contemporary interests rather than founding principle: reclassifies as snare or tangled_rope from analytical perspective (false summit confirmed). If principle remains operative: reclassifies as mountain or rope (doctrine remains justified despite contemporary costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_reading_vs_core_principle_gap, conceptual, 'Whether absolutist doctrine serves its original principle or contemporary institutional interests').

omega_variable(
    comparative_institutional_competence,
    'Which institutional arrangement (courts enforcing harm thresholds, platforms moderating content, international human rights law, community oversight) produces better outcomes for speech values and harm reduction?',
    'Cross-national and cross-platform comparison of speech outcomes, harm patterns, and deliberation quality; measurement of political expression and minority voice protection under different regimes; evaluation of systemic censorship risk.',
    'If alternative arrangements produce better outcomes on both speech protection and harm reduction: doctrine reclassifies as scaffold or piton (outdated arrangement being replaced). If courts uniquely protect speech values: doctrine reclassifies as rope or mountain (justified despite contemporary costs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(comparative_institutional_competence, empirical, 'Comparative institutional competence for speech protection and harm regulation').

omega_variable(
    false_summit_natural_law_ambiguity,
    'Is the absolutist speech boundary a fundamental constraint of free society (Mountain), or a contingent institutional arrangement that benefits speakers with resources (Snare)?',
    'Engine false_summit_mountain signature evaluation: beneficiary presence (institutional speakers, misinformation spreaders) contradicts natural law claim. Structural analysis of beneficiary distribution reveals asymmetric extraction. Historical contingency analysis shows doctrine could be different without invalidating core principle.',
    'If false summit confirmed: doctrine reclassifies to beneficiary-adjusted type (snare or tangled_rope). If mountain confirmed: benefits to some groups are coordination costs of necessary protection, not extractive overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_ambiguity, conceptual, 'Whether absolutist reading is natural law or constructed institutional arrangement').

omega_variable(
    reading_kernel_relationship,
    'What is the contestation within the speech_harm_boundary kernel? How do the absolutist, harm_balancing, and dignity readings differ in their treatment of the same underlying commitment to free expression?',
    'Analysis of reading_relations: what does each reading foreclose, coexist with, or influence about the others? Historical tracing of jurisprudential evolution showing readings as competing interpretations.',
    'If readings foreclose each other: only one framework can be valid (legal/constitutional closure is required). If readings coexist: multiple frameworks remain live options (jurisprudential pluralism). This structures the temporal evolution of the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Structural relationships among competing readings of speech protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_abs_tr_t0, speech_harm_boundary__absolutist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(speech_abs_tr_t10, speech_harm_boundary__absolutist_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(speech_abs_tr_t20, speech_harm_boundary__absolutist_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(speech_abs_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(speech_abs_be_t10, speech_harm_boundary__absolutist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(speech_abs_be_t20, speech_harm_boundary__absolutist_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(speech_abs_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(speech_abs_su_t10, speech_harm_boundary__absolutist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(speech_abs_su_t20, speech_harm_boundary__absolutist_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, platform_content_moderation_authority).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, hate_speech_legal_remedy_availability).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel has multiple readings that constitute distinct constraints. This file models the ABSOLUTIST READING (high protection threshold, narrow unprotected category). Sibling readings (harm_balancing and dignity) have different ε values, different beneficiary/victim structures, and different classifications from the same (P,T,E,S) positions. All three readings inherit from the same kernel_codification (fixed_text) and authority_grounding (lineage via constitutional jurisprudence), but instantiate different axioms. The three stories form a constraint family linked by network.affects_constraints edges. The network also affects downstream constraints on platform moderation (which operates in the gap left by absolutist doctrine) and legal remedy availability (which is restricted by the doctrine's high harm threshold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
