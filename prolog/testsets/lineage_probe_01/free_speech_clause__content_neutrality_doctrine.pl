% ============================================================================
% CONSTRAINT STORY: free_speech_clause__content_neutrality_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_speech_clause__content_neutrality_doctrine, []).

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
 *   constraint_id: free_speech_clause__content_neutrality_doctrine
 *   human_readable: First Amendment Content Neutrality Doctrine
 *   domain: constitutional_law/free_speech
 *
 * SUMMARY:
 *   The content neutrality doctrine is the First Amendment's master
 *   classification rule: speech regulations targeting the content of
 *   expression are presumptively unconstitutional and subject to strict
 *   scrutiny, while regulations of time, place, and manner that are
 *   content-neutral receive rational-basis review. This reading instantiates
 *   one specific jurisprudential commitment about how the First Amendment
 *   operates — a commitment that forecloses some alternatives (the
 *   categorical-exceptions reading's claim that certain speech categories are
 *   simply outside the Amendment's scope) while coexisting with others (the
 *   forum doctrine's claim that constitutional protection varies by
 *   location). The doctrine protects disfavored viewpoints by refusing to
 *   allow governments to suppress speech based on its message. However, the
 *   doctrine also embeds a significant extraction mechanism: governments
 *   seeking to achieve content-based regulatory aims (promoting national
 *   security, protecting children, managing dissent, curating public
 *   discourse) face near-insurmountable hurdles, even when those aims might
 *   survive rational-basis review if cast in content-neutral form. The
 *   doctrine's extractiveness has increased over 50 years as courts have
 *   tightened the content-neutrality gate and limited the categorical
 *   exceptions.
 *
 * KEY AGENTS:
 *   - Disfavored Viewpoint Speakers: Primary beneficiary (powerless/trapped) — doctrine shields them from selective silencing by government
 *   - Governments with Content-Based Aims: Primary victim (institutional/constrained) — face strict-scrutiny barriers to regulations targeting message, even when aims are legitimate
 *   - Courts Administering the Doctrine: Secondary beneficiary (institutional/arbitrage) — doctrine provides bright-line rule (content-based vs. neutral) that reduces unprincipled discretion
 *   - Content-Neutral Proxy Regulators: Secondary victim (institutional/arbitrage) — doctrine requires pretextual neutrality, creating theater of regulatory form
 *   - Categorical-Exceptions Defenders: Competing interpreter (institutional/constrained) — seek to narrow content-neutrality principle by expanding exceptions
 *   - Forum-Doctrine Advocates: Competing interpreter (institutional/constrained) — seek to make content-neutrality requirements vary by location type
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(free_speech_clause__content_neutrality_doctrine, 0.38).
domain_priors:suppression_score(free_speech_clause__content_neutrality_doctrine, 0.52).
domain_priors:theater_ratio(free_speech_clause__content_neutrality_doctrine, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(free_speech_clause__content_neutrality_doctrine, extractiveness, 0.38).
narrative_ontology:constraint_metric(free_speech_clause__content_neutrality_doctrine, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(free_speech_clause__content_neutrality_doctrine, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(free_speech_clause__content_neutrality_doctrine, tangled_rope).
narrative_ontology:human_readable(free_speech_clause__content_neutrality_doctrine, "First Amendment Content Neutrality Doctrine").
narrative_ontology:topic_domain(free_speech_clause__content_neutrality_doctrine, "constitutional_law/free_speech").

domain_priors:requires_active_enforcement(free_speech_clause__content_neutrality_doctrine).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(free_speech_clause__content_neutrality_doctrine, 'ee2dfba9-3dd4-481b-98c8-de92d13284d7').
narrative_ontology:cs_kernel_codification('ee2dfba9-3dd4-481b-98c8-de92d13284d7', formalized).
narrative_ontology:cs_authority_grounding('ee2dfba9-3dd4-481b-98c8-de92d13284d7', lineage).
narrative_ontology:cs_interpretation_layer_present('ee2dfba9-3dd4-481b-98c8-de92d13284d7').
narrative_ontology:cs_reading_relation('ee2dfba9-3dd4-481b-98c8-de92d13284d7', free_speech_clause__categorical_exceptions_doctrine, coexists_with).
narrative_ontology:cs_reading_relation('ee2dfba9-3dd4-481b-98c8-de92d13284d7', free_speech_clause__public_forum_doctrine, influences).
narrative_ontology:cs_axiom('ee2dfba9-3dd4-481b-98c8-de92d13284d7', foundational, content_based_regulations_require_strict_scrutiny).
narrative_ontology:cs_axiom_status(content_based_regulations_require_strict_scrutiny, holdable).
narrative_ontology:cs_axiom_grounding('ee2dfba9-3dd4-481b-98c8-de92d13284d7', content_based_regulations_require_strict_scrutiny, deontological).
narrative_ontology:cs_axiom('ee2dfba9-3dd4-481b-98c8-de92d13284d7', foundational, viewpoint_neutrality_principle_applies_universally).
narrative_ontology:cs_axiom_status(viewpoint_neutrality_principle_applies_universally, holdable).
narrative_ontology:cs_axiom_grounding('ee2dfba9-3dd4-481b-98c8-de92d13284d7', viewpoint_neutrality_principle_applies_universally, deontological).
narrative_ontology:cs_reference_frame('ee2dfba9-3dd4-481b-98c8-de92d13284d7', strict_content_neutrality_framework).
narrative_ontology:cs_drift_state('ee2dfba9-3dd4-481b-98c8-de92d13284d7', contemporary_exception_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ee2dfba9-3dd4-481b-98c8-de92d13284d7', '').
narrative_ontology:cs_kernel_id(free_speech_clause__content_neutrality_doctrine, free_speech_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(free_speech_clause__content_neutrality_doctrine, disfavored_viewpoints).
narrative_ontology:constraint_beneficiary(free_speech_clause__content_neutrality_doctrine, speech_claimants_challenging_content_restrictions).
narrative_ontology:constraint_victim(free_speech_clause__content_neutrality_doctrine, governments_with_content_based_regulatory_aims).
narrative_ontology:constraint_victim(free_speech_clause__content_neutrality_doctrine, competing_public_goods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISFAVORED VIEWPOINT (SNARE) — A speaker expressing a viewpoint the majority or state finds objectionable has minimal power and no genuine exit. Content neutrality doctrine is their only shield. Without it, they would be trapped in a regime of selective silencing with no legal recourse. From this perspective, the doctrine appears as snare-like protection against pure extraction of their speech rights — maximum extraction prevented by doctrine.
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATED GOVERNMENT (TANGLED ROPE) — A local government seeking to regulate speech faces both coordination obligations and extraction barriers. Content neutrality doctrine requires governments to achieve legitimate regulatory aims (traffic control, noise management, public safety) through viewpoint-neutral means. The doctrine coordinates around shared interest in preventing censorship while allowing reasonable regulation. But the strict-scrutiny gate also extracts from governments: they cannot easily achieve content-based goals even when those goals seem legitimate. Mixed extraction and coordination.
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (ROPE) — The courts experience content neutrality as a coordination mechanism that reduces unprincipled discretion. The doctrine provides clear rules (content-based = strict scrutiny; content-neutral = rational basis) that judges can apply with relative consistency. Arbitrage-positioned institutional actors benefit from rules that constrain discretion and generate predictable precedent. The doctrine coordinates judicial review around a master principle rather than case-by-case balancing.
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONTENT-BASED REGULATOR (TANGLED ROPE) — Some governments have genuine content-based aims that survive strict scrutiny: child protection (obscenity exception), security (true threats exception), dignity (defamation exception). These governments benefit from the categorical exceptions that allow content-based regulations within narrow domains. But the doctrine extracts from them: they cannot expand those categories, and their good-faith content regulations outside the exceptions face near-automatic invalidation. Constrained institutional actor experiencing both coordination (the exceptions) and extraction (the strict gatekeeping).
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONTENT-NEUTRAL PROXY (PITON) — Much regulatory practice involves the theater of content-neutral proxies: governments cannot directly regulate viewpoints, so they regulate 'time, place, and manner' in ostensibly neutral ways. But these proxies are often pretextual — a ban on demonstrations 'before dawn to 10am' targets nighttime protest without mentioning viewpoint. The doctrine requires content neutrality on its face, but enforcement of the underlying intent is largely theatrical. The piton classification reflects high theater_ratio in proxy-writing and court-review of proxies.
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, content neutrality can be seen as a natural law of liberal democracy: any rule treating viewpoints differently is logically excluded from a system committed to equal citizenship. The asymmetry is presented as immutable — content-based speech regulations are constitutionally impossible, not merely disfavored. However, the structural data contradicts the mountain classification. The categorical exceptions (incitement, obscenity, defamation, true threats, fighting words) show that content-based regulations ARE permitted in specific domains. This perspective naturalizes what is actually a contingent doctrinal choice.
constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(free_speech_clause__content_neutrality_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(free_speech_clause__content_neutrality_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(free_speech_clause__content_neutrality_doctrine, TR),
    TR >= 0.70.

:- end_tests(free_speech_clause__content_neutrality_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The doctrine significantly constrains government's ability to regulate based on content, which is extraction from governments' perspective. But the constraint is not total — categorical exceptions allow content-based regulations in narrow domains, and content-neutral proxies provide workarounds. The measurement trajectory shows rising extractiveness over the interval (0.22 → 0.38) as courts have tightened the content-neutrality gate and demanded more rigorous proof of content neutrality. Suppression (0.52): Moderate-high. The doctrine suppresses viewpoint-based regulations — governments cannot easily silence disfavored speech through content-based law. But suppression is not absolute — categorical exceptions and content-neutral proxies allow selective suppression within limits. Suppression has remained relatively stable (0.42 → 0.52) as the mechanism of suppression has shifted from explicit content-neutrality demands to stricter scrutiny of government's claimed purpose. Theater ratio (0.58): Moderate-high. Regulatory practice involves significant theater around content-neutral proxies: governments write regulations on their face content-neutral but apply them in ways that target specific viewpoints. Courts must engage in theatrical scrutiny of legislative intent (did the legislature have a content-based purpose?), which is highly contestable. The theater ratio has increased (0.35 → 0.58) as litigation has expanded and judicial review of government purpose has become more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The disfavored speaker sees protection (snare prevention). The constrained government sees extraction (strict scrutiny barrier). The arbitrage-positioned court sees coordination (clear rule reduces discretion). The categorical-exceptions defender sees undermining principle (exceptions create carve-outs). The forum-doctrine advocate sees incomplete constraint (neutrality applies uniformly across forum types, limiting geographic variation). The analytical observer risks naturalizing the doctrine as inevitable law rather than contingent doctrinal choice. The perspectival gap reveals that 'content neutrality' means different things from different positions: principle from the speaker's view, extraction from the regulator's view, efficiency from the judge's view, discretionary exception from the categorical reading's view.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. Disfavored speakers (powerless/trapped) experience low d — the doctrine benefits them. Governments seeking content-based regulation (institutional/constrained) experience high d — the doctrine extracts from them. Courts (institutional/arbitrage) experience low d — clear rules reduce discretion-costs. The content-neutral proxy regulator (institutional/arbitrage) experiences moderate d — the doctrine benefits them by allowing their claimed purpose while extracting from governments with hidden purposes. The analytical observer risks seeing the doctrine as a mountain (a necessary truth of liberal democracy: no content-based regulations possible), but the structural data shows it is a contingent institutional choice — categorical exceptions exist, making content-based regulations possible in specific domains.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_basedness_definition_boundary,
    'What precise boundary separates content-based from content-neutral regulation?',
    'Analysis of judicial tests: purpose vs. effect; facial neutrality vs. discriminatory application; legitimate reason vs. pretextual reason. Empirical assessment of which test courts actually apply across doctrine.',
    'If boundary is sharp: doctrine is mechanical and predictable. If boundary is fuzzy: doctrine is doctrine-in-name only, and judges exercise de facto discretion hidden behind content-neutrality language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_basedness_definition_boundary, empirical, 'Boundary definition between content-based and content-neutral regulation').

omega_variable(
    categorical_exceptions_coherence,
    'Do the historic categorical exceptions (incitement, obscenity, defamation, true threats, fighting words) form a coherent principled set, or are they ad hoc carve-outs that undermine the content-neutrality master rule?',
    'Doctrinal analysis: can a unified principle explain why these categories are excluded? Or do they reflect political compromise and historical accident? Consistency check: do courts treat within-category and cross-category extensions symmetrically?',
    'If coherent: the doctrine is a genuine synthesis of content neutrality + justified exceptions. If ad hoc: the doctrine is aspirational framing around discretionary exceptions, and content-neutrality is theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_exceptions_coherence, conceptual, 'Whether categorical exceptions form a coherent principle or ad hoc carve-outs').

omega_variable(
    content_neutral_proxy_pretextuality,
    'How often do ostensibly content-neutral regulations (time, place, manner restrictions) function as pretextual content-based regulations?',
    'Empirical analysis: legislative history, regulatory application patterns, correlation between viewpoint and regulatory impact. Comparative case study: regulations with identical stated purposes but different viewpoint impacts.',
    'If pretextuality is rare: content neutrality doctrine effectively constrains content-based extraction. If pretextuality is common: the doctrine is largely performative — governments achieve content-based ends through neutral-on-its-face proxies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_neutral_proxy_pretextuality, empirical, 'Prevalence of pretextual content-neutral proxy regulations').

omega_variable(
    reading_contest_categorical_vs_content_neutral,
    'This reading (content_neutrality_doctrine) coexists with categorical_exceptions_doctrine. Do they foreclose each other, or are both live positions?',
    'Jurisprudential analysis: can a court apply categorical exceptions while maintaining content-neutrality as the master rule? Or does the categorical-exceptions reading require abandoning the master rule.',
    'If they foreclose each other: only one reading can be institutionalized in a coherent legal framework. If they coexist: the doctrine accommodates both simultaneously, and the tension is managed through hierarchical application (content neutrality applies except within categorical exceptions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_categorical_vs_content_neutral, conceptual, 'Foreclosure relationship between content-neutrality and categorical-exceptions readings').

omega_variable(
    reading_contest_forum_doctrine_relationship,
    'This reading (content_neutrality_doctrine) coexists with public_forum_doctrine. Do they operate on orthogonal axes, or do they constrain each other?',
    'Doctrinal analysis: can content-neutral regulations be applied differently across forum types? Does the forum classification override content-neutrality requirements, or supplement them?',
    'If orthogonal: both doctrines apply independently. If constraining: one doctrine has interpretive priority over the other, and content-neutrality requirements shift by forum type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_forum_doctrine_relationship, conceptual, 'Relationship between content-neutrality and forum doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(free_speech_clause__content_neutrality_doctrine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(free_tr_t0, free_speech_clause__content_neutrality_doctrine, theater_ratio, 0, 0.35).
narrative_ontology:measurement(free_tr_t20, free_speech_clause__content_neutrality_doctrine, theater_ratio, 20, 0.48).
narrative_ontology:measurement(free_tr_t50, free_speech_clause__content_neutrality_doctrine, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(free_be_t0, free_speech_clause__content_neutrality_doctrine, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(free_be_t20, free_speech_clause__content_neutrality_doctrine, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(free_be_t50, free_speech_clause__content_neutrality_doctrine, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(free_su_t0, free_speech_clause__content_neutrality_doctrine, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(free_su_t20, free_speech_clause__content_neutrality_doctrine, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(free_su_t50, free_speech_clause__content_neutrality_doctrine, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(free_speech_clause__content_neutrality_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(free_speech_clause__content_neutrality_doctrine, free_speech_clause__categorical_exceptions_doctrine).
narrative_ontology:affects_constraint(free_speech_clause__content_neutrality_doctrine, free_speech_clause__public_forum_doctrine).

% DUAL FORMULATION NOTE:
% The free_speech_clause kernel has three distinct readings instantiated as separate constraints. The content_neutrality_doctrine reading (this file) models the master rule as content neutrality + strict scrutiny. The categorical_exceptions_doctrine reading models the claim that certain categories are simply outside protection. The public_forum_doctrine reading models geographic variation in protection. These are not three views of one constraint; they are three structurally distinct constraints sharing a kernel (the First Amendment). They interact via network.affects_constraints: each reading constrains the others by limiting their domain. Decomposition is necessary because the ε values differ significantly (content_neutrality ≈0.38, categorical_exceptions ≈0.15, public_forum ≈0.32) and the beneficiary/victim relationships differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
