% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__conceptual_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__conceptual_emergence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__conceptual_emergence_reading
 *   human_readable: Statute of Anne: Conceptual Emergence of Copyright as Limited Regulatory Tool
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) created a novel legal category: copyright as a
 *   limited regulatory tool for incentivizing learning and authorship, rather
 *   than a perpetual monopoly granted by the Crown. Prior to the statute, the
 *   Stationers' Company held a perpetual monopoly on printing and reprinting
 *   licensed works, enforced through royal charter. The statute replaced this
 *   with individual author rights lasting 14 years (with potential 14-year
 *   renewal). This constraint story instantiates ONE reading of a contested
 *   kernel: the conceptual-emergence reading. This reading holds that the
 *   statute's primary innovation was conceptual—it made possible a new way of
 *   thinking about intellectual property as a bounded, time-limited tool for
 *   learning, distinct from both perpetual monopoly and unrestricted commons.
 *   The statute 'created a new conceptual space' in which IP could be
 *   something other than permanent property. This reading is distinct from
 *   the institutional-reallocation reading (which emphasizes the transfer of
 *   rights from the Stationers' Company to individual authors) and the
 *   entangled-event reading (which treats conceptual and institutional change
 *   as inseparable). This constraint exhibits the Tangled Rope
 *   classification: it contains both a genuine coordination function
 *   (coordinating multiple interests around limited-term protection and
 *   eventual public access) and significant asymmetric extraction
 *   (foreclosing the perpetual monopoly expectation, creating rents for the
 *   14-year term). The constraint is active and enforced through statute and
 *   judicial interpretation.
 *
 * KEY AGENTS:
 *   - Public Learning Commons: Primary beneficiary (organized/constrained) — benefits from eventual access to reprinted works and the conceptual establishment that learning requires limited monopolies, not perpetual ones
 *   - Perpetual Monopoly Expectation: Primary victim (powerless/trapped) — the prior regime's assumption of perpetuity is destroyed; no exit exists once the statute enacts
 *   - Author Incentive Cohort: Secondary beneficiary (organized/mobile) — gains protection for intellectual labor and reversion rights; participates in coordination of the incentive structure
 *   - Stationers' Company: Secondary victim (moderate/constrained) — loses perpetual control but retains licensing benefits during limited terms; cannot exit the new framework
 *   - Crown / Parliament: Institutional regulator (institutional/constrained) — enacts and maintains the enforcement mechanism; extracts control over term length and renewal conditions
 *   - Licensing Administration System: Institutional actor (institutional/arbitrage) — maintains the tracking and registration apparatus; benefits from the statutory framework but sees function degrade over time
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the statute as discovery of an inevitable principle rather than constructed political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.35).
domain_priors:suppression_score(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__conceptual_emergence_reading, "Statute of Anne: Conceptual Emergence of Copyright as Limited Regulatory Tool").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__conceptual_emergence_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__conceptual_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__conceptual_emergence_reading, 'bd8dea3f-de47-419e-b4f4-cd7e40835d78').
narrative_ontology:cs_kernel_codification('bd8dea3f-de47-419e-b4f4-cd7e40835d78', formalized).
narrative_ontology:cs_authority_grounding('bd8dea3f-de47-419e-b4f4-cd7e40835d78', lineage).
narrative_ontology:cs_interpretation_layer_present('bd8dea3f-de47-419e-b4f4-cd7e40835d78').
narrative_ontology:cs_reading_relation('bd8dea3f-de47-419e-b4f4-cd7e40835d78', statute_of_anne_ip_foundation__institutional_reallocation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd8dea3f-de47-419e-b4f4-cd7e40835d78', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('bd8dea3f-de47-419e-b4f4-cd7e40835d78', foundational, copyright_is_conceptually_distinct_from_perpetual_property).
narrative_ontology:cs_axiom_status(copyright_is_conceptually_distinct_from_perpetual_property, holdable).
narrative_ontology:cs_axiom_grounding('bd8dea3f-de47-419e-b4f4-cd7e40835d78', copyright_is_conceptually_distinct_from_perpetual_property, conventional).
narrative_ontology:cs_axiom('bd8dea3f-de47-419e-b4f4-cd7e40835d78', foundational, learning_and_knowledge_diffusion_justify_term_limits).
narrative_ontology:cs_axiom_status(learning_and_knowledge_diffusion_justify_term_limits, holdable).
narrative_ontology:cs_axiom_grounding('bd8dea3f-de47-419e-b4f4-cd7e40835d78', learning_and_knowledge_diffusion_justify_term_limits, deontological).
narrative_ontology:cs_reference_frame('bd8dea3f-de47-419e-b4f4-cd7e40835d78', copyright_as_bounded_learning_tool).
narrative_ontology:cs_drift_state('bd8dea3f-de47-419e-b4f4-cd7e40835d78', contemporary_copyright_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd8dea3f-de47-419e-b4f4-cd7e40835d78', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, public_learning_commons).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__conceptual_emergence_reading, author_incentive_cohort).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, perpetual_monopoly_expectation).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__conceptual_emergence_reading, stationers_company_monopoly_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERPETUAL MONOPOLY EXPECTATION (SNARE) — The prior regime (Stationers' Company perpetual monopoly over reprinting) had no exit option once the statute was enacted. The structural condition that perpetuity was 'natural' and inevitable was foreclosed. From the perspective of actors invested in perpetual control, the statute is pure extraction: the monopoly term is limited to 14 + 14 years (renewable once), and the conceptual frame that enabled perpetuity — the notion that intellectual property should be permanent like physical property — is destroyed. No alternatives exist to perpetual monopoly once the 'limited term' concept is institutionalized. Maximum suppression of the prior expectation regime.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AUTHOR INCENTIVE COHORT (ROPE) — Authors and their assignees benefit from the statute's recognition of intellectual labor as deserving limited protection. The statute solves a coordination problem: how to incentivize creative production when monopoly control is perpetual and concentrated in guilds? The limited term + author reversion rights coordinate multiple interests: authors get protection, the public gets eventual access, booksellers get stable licensing. Low suppression from this perspective — authors have agency and alternatives (patrons, manuscript circulation). The constraint is seen as pure coordination: balancing protection duration with public access.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: STATIONERS' COMPANY (TANGLED ROPE) — The Stationers face a mixed structural position. They lose perpetual monopoly but retain licensing control during the limited terms and benefit from the statute's formalization of printing rights (enabling licensed markets). Constrained exit: they cannot revert to perpetual monopoly after the statute enacts; they cannot refuse to accept the new framework while continuing to operate. But the constraint also coordinates their business by creating clear, legally-defined licenses and preventing title chaos. Moderate extraction + genuine coordination function = tangled rope. This is the most complex perspectival position.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CROWN / PARLIAMENT (TANGLED ROPE) — Parliament enacts the statute as an active enforcement mechanism: it creates the legal categories, sets the terms, and reserves reversion rights. This is institutional power coordinating a market (balancing monopoly rents with public access). But Parliament also extracts value — by controlling the terms of protection, it controls innovation incentives and can adjust them for political ends. The constraint is enforced (requires active legislative renewal for updates); it coordinates a market; it also extracts institutional control. Moderate chi with genuine coordination and extraction both present.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC LEARNING COMMONS (SCAFFOLD) — The public benefits from the statute's terms eventually expiring and works entering the commons. The 14 + 14 year structure creates a built-in sunset: after term expiration, the work reverts to unrestricted circulation. This is temporary coordination with a structural endpoint. Low theater (the mechanism is transparent: temporary monopoly + reversion). The constraint is scaffolding for a transition from pure monopoly to eventual commons access. Chi is low because the mechanism is lightweight and the sunset is built in.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal perspective, the statute merely codified what is inevitably true: intellectual work cannot be monopolized forever because knowledge naturally spreads, and perpetual monopoly is unsustainable against the laws of knowledge diffusion. The statute is read as discovering a natural limit, not creating one. This perspective risks false-summitism: it naturalizes a contingent political choice (limited terms as preferable policy) as an invariant law of nature. The statute's conceptual innovation is treated as inevitable rather than constructed. The engine's false summit detector should flag this as a naturalization of institutional preference.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LICENSING ADMINISTRATION SYSTEM (PITON) — The statute created a new administrative apparatus: registration of copyrights, term tracking, renewal processes. Over time, this administrative function becomes largely performative — the terms are tracked, but the activity's primary function (adjudicating competing claims to intellectual work) atrophies as practice shifts to contract law and common knowledge. The administrative ritual persists through institutional inertia rather than functional necessity. Theater ratio is high; coordination function has degraded. Piton classification applies.
constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statute_of_anne_ip_foundation__conceptual_emergence_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(statute_of_anne_ip_foundation__conceptual_emergence_reading, TR),
    TR >= 0.70.

:- end_tests(statute_of_anne_ip_foundation__conceptual_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The statute extracts value from the perpetual monopoly expectation and concentrates benefits during the limited terms. However, the constraint is not maximally extractive because it includes a genuine coordination function — the limited-term structure balances multiple legitimate interests (author incentive, reading access, bookseller licensing). The extractiveness trajectory shows increasing extraction over the 14-year interval as the statute's implications solidify and the licensing system matures. At t0 (statute enactment), extractiveness is lower (0.22) because the constraint's meaning is still contested and the perpetual monopoly regime persists in practice alongside the statute. By t14 (end of first license term), extractiveness rises to 0.35 as the statute's enforcement becomes routine and the public access provisions are formalized. Suppression (0.42): Moderate. The statute suppresses the perpetual monopoly expectation through legal prohibition and eventually through market practice, but suppression is not total because the underlying monopoly power (the ability to print valuable books during the license term) remains intact. Suppression declines over the interval (0.55 → 0.42) as the limited-term structure becomes normalized and no longer requires active suppression — it is simply institutional background. Theater ratio (0.38): Moderate-low. The statute's mechanism is relatively transparent: a formal term limit, author assignment, reversion on expiration. Theater arises from the licensing administration ritual (tracking renewals, registering copyrights) but the core function (limiting monopoly term) is direct. Theater declines over the interval as the system matures and administration becomes routine rather than performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a maximal perspectival gap across all six classifications. The perpetual monopoly expectation sees a Snare (pure extraction, no exit, maximum suppression of prior regime). The author cohort sees a Rope (pure coordination, balancing interests, mutual benefit). The Stationers' Company sees a Tangled Rope (mixed coordination benefits in the licensing system + extraction of perpetual monopoly loss). Parliament sees a Tangled Rope (coordination of incentives + institutional control over terms). The public learning commons sees a Scaffold (temporary monopoly with built-in sunset). The licensing administration sees a Piton (degraded coordination through administrative ritual). The civilizational analytical observer risks seeing a Mountain (naturalizing the limited-term structure as inevitable). The gap between Snare and Rope is maximum (one perspective sees pure extraction, another sees pure coordination), demonstrating the constraint's hybrid structure and the power of indexical classification to reveal how the same structural mechanism is experienced radically differently depending on agent position. The perspectival gap is not a defect in the constraint's classification—it is the constraint's core signature. The analytical question is not 'which perspective is correct?' but 'what does the gap reveal about the constraint's structure?'
 *
 * DIRECTIONALITY LOGIC:
 *   The statute affects different agents along the extraction flow in opposite directions. The perpetual monopoly expectation is the maximum victim: structurally mobile in principle (it could have persisted without the statute) but locked into the prior regime; captured by the institutional inertia of Crown-Stationer partnership. The statute's enactment redirects the extraction flow: beneficiary power shifts from the Crown-Stationer apparatus to individual authors and (eventually) the public commons. Author agents experience low effective extraction (d ≈ 0.15) because they benefit from the statute's recognition of their labor; they have mobile exit options (they could write anyway without statutory protection). The Stationers experience moderate extraction (d ≈ 0.55) because they lose perpetual control but retain interim benefits. Parliament experiences low extraction in its role as regulator (d ≈ 0.20) because it is the enacting authority and maintains arbitrage options (it can adjust terms). The perpetual monopoly expectation experiences maximum extraction (d ≈ 0.95) because it is foreclosed with no exit. These directionality values are derived structurally from beneficiary/victim declarations plus exit options; no overrides are needed. The sigmoid f(d) magnifies the extraction experienced by trapped agents (f(0.95) ≈ 1.42) and dampens extraction experienced by arbitrage agents (f(0.20) ≈ 0.02). The result is that chi varies dramatically across perspectives even though ε is constant at 0.35.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through the analytical recognition of genuine coordination function alongside asymmetric extraction. The statute is not mislabeled as pure coordination (which would ignore the foreclosure of perpetual monopoly expectations) nor as pure extraction (which would ignore the author incentive coordination and public access provisions). The Tangled Rope classification is correct because it captures both dimensions: the statute does coordinate multiple interests around limited-term protection, AND it does extract value from the perpetual monopoly expectation. The mandatrophy is dissolved by showing that the perspectival gap is structural — different observers see different classifications because they occupy different positions in the extraction flow. The public learning commons sees coordination (Scaffold) + eventual access. Authors see coordination (Rope). The Stationers see mixed coordination and extraction (Tangled Rope). The perpetual monopoly expectation sees pure extraction (Snare). These are not contradictory — they are coherent descriptions of different structural positions. The constraint's type is Tangled Rope at the analytical level because the overall structure contains both coordination and extraction, even though specific perspectives perceive only one dimension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_emergence_vs_institutional_reallocation,
    'Did the statute create a genuinely new conceptual category (limited copyright as a distinct regulatory tool for learning) or merely reallocate existing rights from the Stationers'' Company to individual authors within a pre-existing property framework?',
    'Textual and rhetorical analysis of the statute and contemporary legal discourse: Does the statute''s language present copyright as a fundamentally new category (limited, author-centered, learning-oriented) or as a modification of existing perpetual monopoly? Did subsequent legal interpretation treat copyright as categorically distinct from property or as a species of property? How did the statute''s language propagate into Dutch and French legal systems — were they adopting the conceptual novelty or the institutional reallocation?',
    'If genuine conceptual emergence: the statute expands the conceptual space of legal instruments; extractiveness is lower because a truly new tool was created rather than redistributing a fixed pie. If institutional reallocation only: extractiveness is higher because one party''s monopoly is simply transferred to another; the conceptual space did not expand. This distinction determines whether the constraint is tangled_rope (mixed coordination + extraction via new concept) or snare (pure extraction via institutional shift).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_emergence_vs_institutional_reallocation, conceptual, 'Whether the statute created a new conceptual category or reallocated existing rights').

omega_variable(
    limited_term_as_inherent_vs_contingent,
    'Is the limited-term structure inherent to the concept of copyright for learning, or is it a contingent policy choice that could have been otherwise (perpetual with periodic renewal, perpetual with sunset provisions, etc.)?',
    'Comparative legal history: examination of how different jurisdictions that adopted copyright after 1710 justified their term choices (France''s longer terms, Germany''s different structures). Philosophical analysis of what ''copyright for learning'' logically requires vs. what is policy preference. Analysis of subsequent statutory changes (extension of term lengths in later centuries) to test whether limited-term structure is contingent.',
    'If inherent: the statute discovered the only possible structure; the conceptual innovation is that limited terms are the natural logical endpoint. If contingent: the statute made a political choice among multiple possible structures; the conceptual innovation is that copyright can be a regulatory tool, but the specific term length is arbitrary. This affects whether the constraint''s beneficiary (public learning) is permanently secured or could be overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_term_as_inherent_vs_contingent, conceptual, 'Whether limited terms are inherent to copyright concept or contingent policy').

omega_variable(
    beneficiary_identification_public_learning,
    'Who is the actual beneficiary of the ''public learning commons''? Is it abstract collective knowledge, specific reading populations (the literate, the educated, the merchant class), or future generations?',
    'Historical analysis of who actually accessed reprinted works under the statute, what literacy rates were, whether the statute materially expanded reading populations. Analysis of licensing practices: who could license reprints under reasonable cost? Did the statute''s public access provisions materially benefit anyone in practice?',
    'If beneficiary is abstract knowledge: the constraint''s victim (perpetual monopoly) is more clearly defined than the beneficiary; extractiveness assessment must account for a highly abstract victim. If beneficiary is specific populations: the constraint looks more like redistribution (from monopolists to specific merchants/readers) and less like expansion of abstract commons. If beneficiary is only future generations: the constraint''s benefit is deferred; extraction of rents during the 14 + 14 year term is less constrained by public benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_public_learning, empirical, 'Who actually constitutes the beneficiary of public learning access').

omega_variable(
    stationers_suppression_mechanism,
    'How much of the Stationers'' Company''s suppression of perpetual monopoly was structural (legal prohibition with enforcement) vs. contingent (institutional inertia, market shifts, loss of political favor)?',
    'Historical analysis of the Stationers'' Company''s power before and after the statute. Did they attempt to resist or revert the statute? What enforcement mechanisms did the statute actually deploy against Stationer monopoly claims? Were there statutes or court decisions that had to actively enforce the limit against Stationer counter-claims?',
    'If structural and actively enforced: suppression is high and durable; the constraint requires ongoing enforcement. If contingent and institutional: suppression is high initially but might degrade over time as institutional capacity weakens; the constraint is vulnerable to attrition. This affects theater_ratio and piton risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_suppression_mechanism, empirical, 'Nature of suppression mechanism against perpetual monopoly').

omega_variable(
    reading_contest_boundaries,
    'Are the three readings (conceptual_emergence, institutional_reallocation, entangled_event) genuinely different constraints or are they framings of a single constraint from different analytical perspectives?',
    'ε-invariance test: Do the readings produce different extractiveness values and different structural metrics when measured carefully? If yes, they are separate constraints. If no, they are perspectival readings of one constraint. Examination of whether the readings assign different beneficiary/victim sets or whether they merely narrate the same structural relationships differently.',
    'If separate constraints: this story should be the only one instantiating the conceptual_emergence reading; separate JSON files should exist for the other readings, linked via network.affects_constraints. If single constraint with different framings: the reading should be documented as an omega (conceptual under-determination) rather than treated as a kernel reading. This determination affects the entire analytical scope of this JSON file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_boundaries, conceptual, 'Whether the readings are distinct constraints or perspectival framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statute_emerge_theater_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(statute_emerge_theater_t7, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 7, 0.42).
narrative_ontology:measurement(statute_emerge_theater_t14, statute_of_anne_ip_foundation__conceptual_emergence_reading, theater_ratio, 14, 0.38).

% Extraction over time
narrative_ontology:measurement(statute_emerge_extractiveness_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(statute_emerge_extractiveness_t7, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 7, 0.32).
narrative_ontology:measurement(statute_emerge_extractiveness_t14, statute_of_anne_ip_foundation__conceptual_emergence_reading, base_extractiveness, 14, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(statute_emerge_suppression_t0, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(statute_emerge_suppression_t7, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(statute_emerge_suppression_t14, statute_of_anne_ip_foundation__conceptual_emergence_reading, suppression_requirement, 14, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__conceptual_emergence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__conceptual_emergence_reading, 0.12).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__institutional_reallocation_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__conceptual_emergence_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% This constraint (conceptual_emergence_reading) is one of three analytically distinct readings of the Statute of Anne kernel. The sibling constraints instantiate different framings of the same historical event, each with different ε values and structural claims. The conceptual_emergence_reading emphasizes the statute's role in creating a new category of legal thought; the extractiveness (0.35) reflects moderate asymmetry around the foreclosure of perpetual monopoly expectations. The institutional_reallocation_reading would emphasize power transfer and likely show higher extractiveness (~0.45) as pure redistribution. The entangled_event_reading would deny the decomposition and treat the statute as an indivisible event. All three should be authore separately and linked via network.affects_constraints. This decomposition follows the ε-invariance principle: different observable-dependent measurements (focusing on concept vs. institution vs. event-unity) would yield different ε estimates, requiring separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
