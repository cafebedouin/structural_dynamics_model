% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__near_absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__near_absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__near_absolutist_reading
 *   human_readable: First Amendment Near-Absolutist Reading: Content-Based Restriction Prohibition
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The near-absolutist reading of the First Amendment holds that the 'no
 *   law' language categorically prohibits content-based speech restrictions
 *   except for narrowly defined categories grounded in imminent physical harm
 *   (incitement, fighting words). This reading is one of three competing
 *   interpretations of the First Amendment kernel — alongside the balancing
 *   reading (no single constitutional right overrides others; case-by-case
 *   adjudication required) and the dignitary harm reading (speech causing
 *   tangible systemic subordination can be restricted). The near-absolutist
 *   reading presents itself as doctrine and principle but is actually one
 *   strategic framing of the contested kernel. From the perspective of
 *   political dissidents and marginalized advocates using speech against
 *   power, the near-absolutist reading provides genuine protective
 *   coordination. From the perspective of targets of slur and harassment, it
 *   functions as a pure extraction mechanism — they are forced to tolerate
 *   ongoing degradation without legal recourse. From the analytical
 *   observer's perspective, the reading risks naturalizing as immutable law
 *   what is actually a contingent institutional choice that benefits some
 *   speakers and harms others. The measurement trajectory shows increasing
 *   theater ratio and extractiveness over the 60-year interval (1964–2024),
 *   reflecting the growing gap between the doctrine's claimed neutrality and
 *   its actual capacity to protect some speakers while leaving others
 *   vulnerable to organized harassment and slur campaigns, particularly as
 *   digital platforms have amplified speech reach and persistence.
 *
 * KEY AGENTS:
 *   - Political Dissidents: Primary beneficiaries (powerful/mobile) — experience the near-absolutist reading as protective, enabling challenge to government and institutional power
 *   - Marginalized Advocates: Secondary beneficiaries AND victims (moderate/constrained) — benefit from protection for their own group's speech; bear costs of tolerating speech denigrating their group
 *   - Slur and Harassment Targets: Primary victims (powerless/trapped) — subordinated groups forced to tolerate speech designed to demean and exclude; minimal exit options
 *   - Courts and Judges: Institutional actors (institutional/constrained) — bound by precedent to apply the near-absolutist reading; experience pressure to address harms the reading does not accommodate; navigate through doctrinal narrowing and inconsistent application
 *   - Platform Operators and Moderation Bodies: Organized actors (organized/constrained) — constrained by First Amendment doctrine at the state-action level but autonomous in their private platform governance; building alternative structures (algorithmic ranking, community moderation) that bypass the constitutional constraint
 *   - First Amendment Orthodoxy: Institutional framework (institutional/arbitrage) — benefits from the clarity and teaching utility of a bright-line rule; maintains the reading through legal education and professional practice despite acknowledged limitations
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice as immutable constitutional principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__near_absolutist_reading, 0.38).
domain_priors:suppression_score(speech_protection_boundary__near_absolutist_reading, 0.42).
domain_priors:theater_ratio(speech_protection_boundary__near_absolutist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__near_absolutist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_boundary__near_absolutist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__near_absolutist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__near_absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__near_absolutist_reading, "First Amendment Near-Absolutist Reading: Content-Based Restriction Prohibition").
narrative_ontology:topic_domain(speech_protection_boundary__near_absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__near_absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__near_absolutist_reading, 'b08b52fb-04da-45a1-add1-f60c1c874117').
narrative_ontology:cs_kernel_codification('b08b52fb-04da-45a1-add1-f60c1c874117', formalized).
narrative_ontology:cs_authority_grounding('b08b52fb-04da-45a1-add1-f60c1c874117', lineage).
narrative_ontology:cs_interpretation_layer_present('b08b52fb-04da-45a1-add1-f60c1c874117').
narrative_ontology:cs_reading_relation('b08b52fb-04da-45a1-add1-f60c1c874117', speech_protection_boundary__dignitary_harm_reading, coexists_with).
narrative_ontology:cs_reading_relation('b08b52fb-04da-45a1-add1-f60c1c874117', speech_protection_boundary__balancing_reading, influences).
narrative_ontology:cs_axiom('b08b52fb-04da-45a1-add1-f60c1c874117', foundational, content_restriction_categorical_prohibition).
narrative_ontology:cs_axiom_status(content_restriction_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('b08b52fb-04da-45a1-add1-f60c1c874117', content_restriction_categorical_prohibition, deontological).
narrative_ontology:cs_axiom('b08b52fb-04da-45a1-add1-f60c1c874117', secondary, robust_speech_protection_prerequisite_for_democracy).
narrative_ontology:cs_axiom_status(robust_speech_protection_prerequisite_for_democracy, holdable).
narrative_ontology:cs_axiom_grounding('b08b52fb-04da-45a1-add1-f60c1c874117', robust_speech_protection_prerequisite_for_democracy, instrumental).
narrative_ontology:cs_reference_frame('b08b52fb-04da-45a1-add1-f60c1c874117', categorical_speech_protection_doctrine).
narrative_ontology:cs_drift_state('b08b52fb-04da-45a1-add1-f60c1c874117', contemporary_digital_platform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b08b52fb-04da-45a1-add1-f60c1c874117', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__near_absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__near_absolutist_reading, speakers_with_unpopular_views).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__near_absolutist_reading, political_dissenters).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__near_absolutist_reading, marginalized_advocates_using_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__near_absolutist_reading, targets_of_slur_and_harassment).
narrative_ontology:constraint_victim(speech_protection_boundary__near_absolutist_reading, systemically_subordinated_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL DISSIDENT (ROPE) — Mobile speaker with resources and platform access. Experiences the near-absolutist reading as protective coordination: broad speech protection enables their political challenge to entrenched power. The constraint functions as intended from this agent's perspective — robust debate against government censorship. Experiences net benefit; can exit via exit_options (platform switching, geographic mobility, status transitions).
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: SLUR/HARASSMENT TARGET (SNARE) — Trapped in exposure to speech designed to demean, threaten, and subordinate. Under the near-absolutist reading, such speech is protected unless it meets the narrow 'imminently incites physical violence' threshold — an extremely high bar. Targets cannot exit the speech environment (subject to ongoing harassment in workplaces, schools, online platforms, public spaces). Zero escape mechanisms except geographic relocation or social withdrawal. Maximum experienced extraction; experiences constraint as pure coercion with minimal coordination benefit.
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MARGINALIZED ADVOCATE (TANGLED ROPE) — Constrained agent whose group is both beneficiary and victim of the near-absolutist reading. Benefits from protection for their own group's speech and political organizing (civil rights advocates, immigrant advocates, religious minorities defending their own speech). Also bears costs: must tolerate speech denigrating their group and face ongoing harassment campaigns. Moderate exit options — can shift platforms, disengage from digital spaces, but cannot fully exit the social and institutional environments where the protected speech operates. Mixed experience: genuine coordination function (protection for dissident speech) alongside extraction (forced tolerance of slur and harassment).
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM/MODERATION COALITION (SCAFFOLD) — Organized agents (content moderation teams, platform governance structures, civil society organizations) see the near-absolutist reading as imposing costs (protecting speech they believe causes harm) alongside benefits (constraining government censorship). This perspective sees the constraint as temporary and improvable: platform-level moderation, community standards, algorithmic intervention, and counter-speech are building alternative structures that bypass government coercion while still reducing harmful speech exposure. Sunset horizon: community-governed moderation systems and decentralized platforms may reduce the tension between free speech and harassment mitigation. Low theater because these agents have concrete agency (moderation policy, feature design) and can point to measurable outcomes (harassment reduction).
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FIRST AMENDMENT ORTHODOXY (PITON) — The institutional commitment to the near-absolutist reading persists through legal education, judicial precedent, and professional norms despite acknowledged failure to protect dignity and equality. Legal doctrine maintains the near-absolutist frame through strategic interpretation (narrowing exceptions, reading harm categories strictly) even when real-world consequences (increased hate speech, harassment campaigns, radicalization pipelines) suggest the framework is not functioning. Piton classification: the near-absolutist reading is largely performative — courts apply it selectively (protecting some speakers more than others, allowing platform moderation, permitting injunctions in some contexts), but the orthodoxy persists as the baseline doctrinal frame because it has become institutionally embedded. Theater ratio (0.55) reflects this selective application and doctrinal drift.
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE COURT (INSTITUTIONAL/CONSTRAINED) — Courts are constrained by precedent and doctrine to apply the near-absolutist reading broadly while also being pressured (via dissent, public contestation, new cases) to recognize harms the reading does not address. Courts benefit from the clarity and administrability of a bright-line rule (near-absolutist reading requires minimal line-drawing), but bear costs of defending the rule against growing evidence of subordination-enabling outcomes. Constrained exit: courts cannot simply reverse decades of precedent without severe legitimacy costs; they navigate through doctrinal narrowing, exception-creation, and inconsistent application. Mixed experience: genuine coordination function (clear rule reduces arbitrary censorship) alongside extraction-enabling (rule permits unlimited degradation of subordinated groups).
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the near-absolutist reading appears to rest on an immutable principle: robust protection of speech is a structural requirement for all other freedoms, democracy, and human dignity. Content-based restrictions are inherent to tyranny; therefore the near-absolutist reading is a natural law of legitimate political order. However, this perspective is a FALSE SUMMIT. The structural data reveals that the reading benefits identifiable groups (dissidents, speakers with platforms), harms others (slur targets, subordinated groups), and requires active enforcement to suppress alternative framings (dignitary harm, balancing approaches). The 'natural law' framing naturalizes what is actually a contingent institutional choice.
constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__near_absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(speech_protection_boundary__near_absolutist_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(speech_protection_boundary__near_absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(speech_protection_boundary__near_absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The near-absolutist reading benefits certain speakers (dissidents, advocates with platforms) at the cost of others (slur targets, marginalized groups). The extraction is not maximal because: (1) the beneficiaries are not a unified extractive class — marginalized advocates also benefit from the protection; (2) alternatives exist and are growing (platform moderation, counter-speech, community standards); (3) the reading's coordination function is genuine (prevents government censorship of political dissent). Suppression (0.42): Moderate. Targets of slur and harassment cannot exit the speech environment without significant cost (geographic relocation, social withdrawal, platform abandonment), but suppression is not total — they can organize counter-speech, build alternative communities, demand platform enforcement of community standards, and engage in legal/legislative advocacy for alternative readings. Theater ratio (0.55): Moderate-high. The near-absolutist reading presents itself as neutral principle ('content-blind') but is actually applied selectively and strategically — courts narrow exceptions, create new doctrinal categories, permit platform moderation, and interpret 'imminence' more expansively in some political contexts than others. The reading's appearance as stable doctrine obscures the ongoing political and social contestation. Theater has increased from 0.38 (1964, when the doctrine was more internally coherent) to 0.55 (2024, when the gap between doctrine and practice is widely acknowledged).
 *
 * PERSPECTIVAL GAP:
 *   The near-absolutist reading generates a full perspectival gap. The political dissident sees coordination (rope) — the reading enables their challenge to power. The slur target sees pure extraction (snare) — the reading provides zero protection. The marginalized advocate sees mixed coordination and extraction (tangled rope) — their own speech is protected while they are forced to tolerate degradation. The court sees a tension between coordination (clear rule) and extraction (unaddressed harms). The platform operator sees a temporary problem with emerging solutions (scaffold) — moderation and community governance are creating alternative pathways. The first amendment orthodoxy sees its own degradation (piton) — the reading persists through institutional inertia even as its actual function diverges from its doctrinal claim. The analytical observer risks seeing natural law (mountain) — the near-absolutist reading appears to rest on immutable principle — but the structural data reveals this as a false summit: the reading's boundaries are historically contingent, its beneficiaries are identifiable, and its costs fall on specific groups.
 *
 * DIRECTIONALITY LOGIC:
 *   The near-absolutist reading's effective extractiveness (chi) varies across agents based on their structural position. Political dissidents with resources and platforms experience low d (they are net beneficiaries) → low/negative f(d) → protection appears as rope. Slur targets with no exit and no platform access experience high d (they are net victims) → high f(d) → protection appears as snare. Marginalized advocates with constrained exit experience moderate d (mixed beneficiary/victim status) → moderate f(d) → moderate experienced extraction. The suppression metric (0.42) is unscaled and applies globally — it reflects the structural barrier that targets cannot exit the speech environment without significant cost. The theater ratio (0.55) reflects the gap between the reading's doctrinal presentation (neutral, categorical) and its actual application (selective, politically responsive).
 *
 * MANDATROPHY ANALYSIS:
 *   The near-absolutist reading resolves the mandatrophy through explicit commitment to a specific kernel interpretation. The mandatrophy question — 'Does the First Amendment prohibit all content-based restrictions or does it admit exceptions?' — is answered by declaring a reading: ONLY narrowly defined categories (incitement, fighting words) grounded in imminent physical harm. This reading is logically coherent but politically contested. The sibling readings (dignitary harm, balancing) contest this answer and propose alternative kernels readings. All three readings are defensible as constitutional interpretations — the mandatrophy is resolved not by choosing the 'correct' one but by recognizing that the kernel (the 'no law' language) admits multiple readings and that the choice among readings is a political decision disguised as interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incitement_threshold_malleability,
    'Is the ''imminent incitement'' threshold a fixed legal boundary or a historically contingent doctrinal construct that shifts based on political and social pressure?',
    'Longitudinal analysis of incitement doctrine: compare Brandenburg standard (1969) to post-January 6 litigation; examine whether courts expand/contract the imminence requirement based on perceived threat level; analyze congressional testimony and constitutional scholarship across political cycles',
    'If threshold is fixed: the near-absolutist reading provides a stable, predictable legal boundary. If contingent: the reading''s claim to be a stable constitutional law collapses into a politics-responsive doctrine that naturalizes itself as principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incitement_threshold_malleability, empirical, 'Whether incitement threshold is constitutionally fixed or historically contingent').

omega_variable(
    subordination_harm_measurability,
    'Can the subordination harm caused by protected slur and harassment (reduced participation, psychological trauma, exclusion from public discourse, group-based threat) be measured with sufficient rigor and non-speculativity to constitute a cognizable harm under alternative readings?',
    'Systematic evidence review: longitudinal studies of harassment targets'' public participation rates, employment outcomes, educational access; psychological research on targeted group trauma; comparative legal analysis of dignitary harm standards in other democracies (Canada, Germany, South Africa); experimental studies measuring speech deterrence effects',
    'If measurable: the dignitary harm and balancing readings gain structural coherence — they are not merely value preferences but responses to documentable harms. If not measurable: the near-absolutist reading''s minimization of non-physical harm appears justified by epistemic limits rather than political choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_harm_measurability, empirical, 'Whether subordination harm from slur/harassment can be measured rigorously').

omega_variable(
    reading_kernel_identity,
    'Is the First Amendment kernel''s ''no law'' language a categorical prohibition on content restriction, or is it a principle that admits categorical exceptions (including dignitary harm, equality, systemic subordination)?',
    'Hermeneutic and historical analysis: original public meaning of ''no law''; founding-era practice (seditious libel prosecutions, slave speech restrictions); comparative constitutional texts (other democracies'' speech provisions and their exceptions); doctrinal coherence across existing categorical exceptions (incitement, fighting words, obscenity, defamation, fraud, blackmail)',
    'If ''no law'' is categorical (near-absolutist reading correct): the kernel admits only narrow exceptions defined by imminent physical harm. If kernel admits broader categories: the sibling readings (dignitary harm, balancing) have equal doctrinal standing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Hermeneutic identity of First Amendment ''no law'' language across readings').

omega_variable(
    democracy_robustness_empirics,
    'Is robust protection of unpopular speech (as claimed by the near-absolutist reading) empirically necessary for democratic resilience, or does tolerance of dignity-protecting restrictions compatible with democracy (as claimed by dignitary harm reading) produce comparable or superior democratic outcomes?',
    'Comparative democratic quality study: analyze democracies with strong near-absolutist speech protection (US, Denmark) vs those with dignitary harm exceptions (Canada, Germany, South Africa); measure freedom of participation, representation, polarization, protest efficacy, minority voice, epistemic commons health across both groups; longitudinal analysis of US democratic metrics before/after social media amplification of hate speech',
    'If robust near-absolutist protection correlates with superior democracy: the near-absolutist reading''s instrumental justification is vindicated. If dignitary harm democracies perform comparably or better: the near-absolutist reading trades democracy quality for categorical purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democracy_robustness_empirics, empirical, 'Empirical relationship between speech protection strength and democratic robustness').

omega_variable(
    false_summit_naturalness_claim,
    'Is the near-absolutist reading''s appearance as a ''natural law'' of constitutional order a genuine structural feature, or a rhetorical frame that benefits speakers with power and disadvantages subordinated speakers?',
    'Comparative frame analysis: document how the near-absolutist reading is presented in legal education, journalism, and political discourse as ''the Constitution'' rather than ''one reading''; analyze alternative framings in legal scholarship; measure citation patterns (does near-absolutist reading receive presumptive deference vs competing readings?); examine what becomes invisible or illegible under the near-absolutist frame (subordination harms, systemic effects, equality costs)',
    'If frame is transparent: the near-absolutist reading appears as natural law because it is most coherent with constitutional structure. If frame is strategic: the reading''s naturalization is ideological work that benefits certain speakers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness_claim, conceptual, 'Whether near-absolutist reading''s naturalization is structural transparency or strategic framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__near_absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_na_tr_t0, speech_protection_boundary__near_absolutist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(speech_na_tr_t30, speech_protection_boundary__near_absolutist_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(speech_na_tr_t60, speech_protection_boundary__near_absolutist_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(speech_na_be_t0, speech_protection_boundary__near_absolutist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(speech_na_be_t30, speech_protection_boundary__near_absolutist_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(speech_na_be_t60, speech_protection_boundary__near_absolutist_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(speech_na_su_t0, speech_protection_boundary__near_absolutist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(speech_na_su_t30, speech_protection_boundary__near_absolutist_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(speech_na_su_t60, speech_protection_boundary__near_absolutist_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__near_absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__near_absolutist_reading, speech_protection_boundary__dignitary_harm_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__near_absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__near_absolutist_reading, hate_speech_platform_moderation).
narrative_ontology:affects_constraint(speech_protection_boundary__near_absolutist_reading, radicalization_pipeline_amplification).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel admits three structurally distinct readings with different ε values and beneficiary/victim structures. This constraint (near_absolutist_reading) decomposes the kernel into its near-absolutist component. The dignitary_harm_reading and balancing_reading are sibling constraints with the same kernel_id but different reading_id values. They are not variants of this constraint — they are alternative structural interpretations of the same kernel, each with its own extractiveness, suppression, theater ratio, and perspective set. The network edges document the logical and causal relationships: the near-absolutist reading affects (enables/constrains) the sibling readings through doctrinal precedent, and the near-absolutist reading affects downstream constraints (platform moderation dynamics, radicalization amplification) by determining what speech is protected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__near_absolutist_reading, powerless, 0.92).
constraint_indexing:directionality_override(speech_protection_boundary__near_absolutist_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
