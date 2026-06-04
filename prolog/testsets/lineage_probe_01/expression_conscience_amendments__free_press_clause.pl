% ============================================================================
% CONSTRAINT STORY: expression_conscience_amendments__free_press_clause
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_free_press_clause, []).

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
 *   constraint_id: expression_conscience_amendments__free_press_clause
 *   human_readable: Free Press Clause: Institutional Protection for Public Information
 *   domain: constitutional_law/political_institutions
 *
 * SUMMARY:
 *   The Free Press Clause establishes that the press, as an institutional
 *   institution, deserves constitutional protection distinct from general
 *   speech protections. The clause forbids prior restraint and licensing,
 *   treating the press as essential to democratic self-government. This
 *   reading instantiates one of six possible readings of the broader
 *   expression_conscience_amendments kernel — a contested constitutional
 *   commitment that generates different constraints depending on which
 *   foundational principle is prioritized. The Free Press reading emphasizes
 *   institutional role and public information as the legitimating principle.
 *   This constraint exhibits multiple classification types depending on the
 *   observer's structural position: it appears as coordination protecting
 *   democratic function (Rope/Scaffold from reader and publisher
 *   perspectives), as asymmetric extraction denying information control
 *   (Snare from silenced official perspective), as mixed
 *   coordination-and-extraction hybrid (Tangled Rope from state apparatus
 *   perspective), and as purported natural law of self-government (Mountain
 *   from analytical perspective — a false summit). The constraint's low
 *   theater ratio (0.38) reflects that information distribution through the
 *   press is substantially functional, not merely performative — the clause
 *   enables real communication, not ritual. Rising theater over time reflects
 *   increasing digital-era questions about what counts as 'the press' and
 *   whether institutional credentialing remains necessary for press
 *   protection. The reading is distinguished from its siblings by its
 *   specific focus on institutional role and the prohibition of prior
 *   restraint as the enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Publishers (Editorial Institutions): Primary beneficiary (institutional/arbitrage) — protected from prior restraint and licensing; control what reaches public
 *   - Reading Public: Secondary beneficiary (moderate/mobile) — access to information about government conduct; mobile because they can choose which sources to trust
 *   - Government Officials Subject to Exposure: Primary victim (powerful/constrained) — exposed conduct cannot be restrained or prevented by prior restraint; exit constrained by constitutional prohibition
 *   - State Information Control Apparatus: Secondary victim (institutional/constrained) — licensing and prior restraint tools are forbidden; must rely on indirect suppression through market gatekeeping
 *   - Wealthy Media Conglomerates: Institutional beneficiary with hidden extraction (powerful/mobile) — gain institutional protection while gatekeeping information flow through market concentration
 *   - Independent and Community Publishers: Marginal agents (powerless/trapped) — may not qualify for constitutional protection if not recognized as credentialed 'press'; face institutional barriers despite clause protection
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable feature of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expression_conscience_amendments__free_press_clause, 0.28).
domain_priors:suppression_score(expression_conscience_amendments__free_press_clause, 0.62).
domain_priors:theater_ratio(expression_conscience_amendments__free_press_clause, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expression_conscience_amendments__free_press_clause, extractiveness, 0.28).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_press_clause, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(expression_conscience_amendments__free_press_clause, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expression_conscience_amendments__free_press_clause, tangled_rope).
narrative_ontology:human_readable(expression_conscience_amendments__free_press_clause, "Free Press Clause: Institutional Protection for Public Information").
narrative_ontology:topic_domain(expression_conscience_amendments__free_press_clause, "constitutional_law/political_institutions").

domain_priors:requires_active_enforcement(expression_conscience_amendments__free_press_clause).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(expression_conscience_amendments__free_press_clause, '4d998c13-0a59-4437-b9d6-27c56118aeb5').
narrative_ontology:cs_kernel_codification('4d998c13-0a59-4437-b9d6-27c56118aeb5', formalized).
narrative_ontology:cs_authority_grounding('4d998c13-0a59-4437-b9d6-27c56118aeb5', lineage).
narrative_ontology:cs_interpretation_layer_present('4d998c13-0a59-4437-b9d6-27c56118aeb5').
narrative_ontology:cs_reading_relation('4d998c13-0a59-4437-b9d6-27c56118aeb5', expression_conscience_amendments__free_speech_clause, influences).
narrative_ontology:cs_reading_relation('4d998c13-0a59-4437-b9d6-27c56118aeb5', expression_conscience_amendments__assembly_petition_clause, coexists_with).
narrative_ontology:cs_reading_relation('4d998c13-0a59-4437-b9d6-27c56118aeb5', expression_conscience_amendments__establishment_clause, coexists_with).
narrative_ontology:cs_reading_relation('4d998c13-0a59-4437-b9d6-27c56118aeb5', expression_conscience_amendments__free_exercise_clause, coexists_with).
narrative_ontology:cs_axiom('4d998c13-0a59-4437-b9d6-27c56118aeb5', foundational, institutional_press_essential_to_democracy).
narrative_ontology:cs_axiom_status(institutional_press_essential_to_democracy, holdable).
narrative_ontology:cs_axiom_grounding('4d998c13-0a59-4437-b9d6-27c56118aeb5', institutional_press_essential_to_democracy, instrumental).
narrative_ontology:cs_axiom('4d998c13-0a59-4437-b9d6-27c56118aeb5', secondary, prior_restraint_prohibition_sufficient).
narrative_ontology:cs_axiom_status(prior_restraint_prohibition_sufficient, overridden).
narrative_ontology:cs_axiom_grounding('4d998c13-0a59-4437-b9d6-27c56118aeb5', prior_restraint_prohibition_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('4d998c13-0a59-4437-b9d6-27c56118aeb5', institutional_press_against_state_censorship).
narrative_ontology:cs_drift_state('4d998c13-0a59-4437-b9d6-27c56118aeb5', digital_era_distributed_publishing, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4d998c13-0a59-4437-b9d6-27c56118aeb5', '').
narrative_ontology:cs_kernel_id(expression_conscience_amendments__free_press_clause, expression_conscience_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_press_clause, publishers_editorial_entities).
narrative_ontology:constraint_beneficiary(expression_conscience_amendments__free_press_clause, reading_public).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_press_clause, government_officials_subject_to_exposure).
narrative_ontology:constraint_victim(expression_conscience_amendments__free_press_clause, state_information_control_apparatus).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SILENCED OFFICIAL (SNARE) — A government actor whose conduct is exposed by press investigation cannot exit the exposure mechanism; the press is constitutionally protected from prior restraint or licensing. Extraction is maximum — the official bears reputational cost with no legal remedy against the publication itself. The constraint operates as pure extraction from this angle: information control is forbidden, alternatives to exposure are suppressed.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLISHER (ROPE) — The institutional press benefits from constitutional protection against prior restraint and licensing. Experiences the constraint as coordination: the clause enables publishers to communicate findings to the public without pre-publication government interference. Net beneficiary — the constraint allocates to publishers the right to determine what to publish. Low extraction experienced because publishers have agency and exit via editorial judgment.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: READING PUBLIC (ROPE) — Citizens benefit from access to information about government conduct, enabling informed participation in self-government. The constraint coordinates the flow of information without suppressing the public's exit options. Public can choose what to read, which outlets to trust, whether to act on information. Experiences the constraint as enabling coordination for democratic self-governance.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (TANGLED ROPE) — Government as an institution experiences the constraint as both coordination and extraction. The clause coordinates a role for the press in democratic accountability (coordination function: government conduct is scrutinized, improving decision-making). Simultaneously, it extracts by denying government the tools of prior restraint and licensing (suppression high — state cannot prevent publication). Exit is constrained: state cannot simply abolish the press or impose licensing without constitutional amendment. Mixed experience of coordination (public accountability improves governance) and asymmetric extraction (information control forbidden).
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WEALTHY MEDIA CONGLOMERATE (TANGLED ROPE) — Large institutional publishers with market dominance experience genuine coordination function (the clause protects their institutional role as information intermediaries). Simultaneously, they extract by gatekeeping which stories reach the public, which sources are credible, which narratives dominate. Suppression is enforced via market concentration and editorial discretion — viable independent publishers are suppressed not by prior restraint but by capital barriers. Extractiveness moderate because powerful actors can exit (start competing outlets, use alternative platforms) and can be held accountable through market pressure and antitrust scrutiny.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, this constraint protects a universal structural feature of self-government: any system of distributed authority requires information flow uncontrolled by any single center of power. Prior restraint and licensing are seen as universally corrupting — immutable barriers to legitimate governance. From this perspective, the clause appears to protect an irreducible feature of the human condition: the need for information freedom in any stable political system. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit revealing that a universal principle is being naturalized.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DIGITAL-ERA FREE PRESS MOVEMENT (SCAFFOLD) — Organized actors (digital journalists, platform cooperatives, decentralized media networks, press freedom organizations) see the traditional Free Press Clause as a temporary institutional form with a sunset. Digital platforms distribute publishing power beyond institutional press monopolies. The constraint that once protected only credentialed publishers now must protect bloggers, citizen journalists, and decentralized information networks. This perspective sees the institutional press clause as transitional: as publishing becomes technically democratized, the constitutional framework must migrate from protecting 'the press' as an institution to protecting 'press function' as a distributed capability. Exit path visible (decentralized infrastructure), sunset logic applicable (institutional press protection may become obsolete as publishing technology diffuses). Theater relatively low because actual information distribution is occurring, not merely performative.
constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expression_conscience_amendments__free_press_clause_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expression_conscience_amendments__free_press_clause, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(expression_conscience_amendments__free_press_clause_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The Free Press Clause explicitly forbids prior restraint and licensing — the state extraction mechanisms are constitutionally prohibited. However, the clause generates asymmetric benefits: publishers retain editorial discretion (extraction mechanism redirected), and institutional press gatekeeping creates barriers for independent publishers (hidden extraction through credentialing). The measured extractiveness reflects that formal extraction mechanisms are forbidden but that material extraction of power and information control persists through market structure and institutional prestige. Suppression (0.62): High. Despite constitutional protection, the press faces substantial suppression mechanisms: market concentration of institutional press, capital barriers to entry for independent publishers, professional credentialing requirements, advertiser pressure, legal liability for defamation/libel, and state informal pressure (intimidation, legal harassment, exclusion from access). The clause forbids prior restraint specifically but does not address these alternative suppression mechanisms. Theater ratio (0.38): Moderate-low. The Free Press Clause operates substantially through actual information distribution, not ritual — newspapers publish real investigations, courts enforce actual First Amendment protections, and journalists face real consequences. The theater component reflects institutional performativity: the credentialed press performs a gatekeeping role, and constitutional protection partly rests on this performance of institutional legitimacy. Rising theater over time reflects the digital era: institutional press protection now coexists with decentralized publishing, raising questions about whether the constraint's institutional focus is performative (protecting a form) rather than functional (protecting a capability).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim perspectives reveals the constraint's hybrid nature. The publisher (Rope perspective, beneficiary) experiences coordination: the clause enables publishers to communicate findings. The silenced official (Snare perspective, victim) experiences maximum extraction: exposure is guaranteed and cannot be prevented by prior restraint. The reading public (Rope perspective, secondary beneficiary) experiences enabling coordination: they gain information for self-government. The state apparatus (Tangled Rope perspective, victim) experiences mixed coordination and extraction: accountability improves governance but information control tools are forbidden. The wealthy media conglomerate (Tangled Rope perspective, beneficiary-with-extraction) experiences coordination plus hidden extraction: constitutional protection plus market-based gatekeeping. The analytical observer risks misclassifying this hybrid as a Mountain (universal principle of self-government) when the structural data shows it is a contingent institutional arrangement that distributes benefits asymmetrically. The digital-era free press movement sees the constraint as transitional (Scaffold) — as publishing technology diffuses, institutional press protection becomes obsolete and must migrate to protecting press *function* rather than press *institution*.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to this specific constraint. Publishers and readers benefit from the clause (low d values); government officials subject to exposure and the state information control apparatus bear costs (high d values). The state apparatus perspective is particularly complex: at the institutional level, the state experiences mixed effects — loss of information control tools (extraction cost) but gain in accountability and legitimacy through free press function (coordination benefit), producing the Tangled Rope classification. The wealthy media conglomerate perspective shows how institutional beneficiaries can extract even within a coordination framework: protected from state prior restraint, they gatekeep through market concentration instead. Independent publishers face a peculiar directionality: nominally protected by the clause, but structurally unable to claim the protection because they lack institutional credential. The analytical observer at the civilizational level risks assuming d=0.0 (universal beneficiary, no extraction) by treating free press as a natural law rather than examining the beneficiary/victim distribution that the structural data reveals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_press_vs_distributed_publishing,
    'Is the Free Press Clause a permanent protection for free expression, or a transitional protection for an institutional form (credentialed press) that is being displaced by distributed publishing technology?',
    'Historical analysis of what ''the press'' meant in 1791 (institutional gatekeepers) vs. contemporary meaning (anyone with publishing capacity); tracking of court decisions extending press protections to non-institutional publishers; technological displacement of institutional press gatekeeping role',
    'If permanent: the constraint protects free expression as a universal principle, classifies as Mountain or Rope. If transitional: the constraint protects a specific institutional form, classifies as Scaffold, and will require constitutional reframing as technology makes institutional gatekeeping obsolete.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_press_vs_distributed_publishing, empirical, 'Whether Free Press Clause protects expression universally or an institutional form specifically').

omega_variable(
    prior_restraint_suppression_mechanism,
    'Is the suppression (0.62) correctly attributed to prior restraint bans, or does it derive from the hidden extraction mechanisms that the explicit ban on prior restraint conceals (market concentration, editorial gatekeeping, capital barriers to entry)?',
    'Counterfactual analysis: if prior restraint were permitted but market gatekeeping remained, how much suppression would persist? Comparison of suppression levels in jurisdictions with and without prior restraint law but with equivalent market concentration.',
    'If suppression is genuinely reduced by prior restraint ban: the clause is a functional coordination mechanism (Rope from all perspectives except beneficiaries and victims). If suppression persists due to market gatekeeping despite prior restraint ban: the clause is a performative protection that redirects rather than eliminates suppression (Piton from analytical perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_restraint_suppression_mechanism, empirical, 'Whether prior restraint suppression comes from state action or market structure').

omega_variable(
    reading_contest_kernel_identity,
    'Is the Free Press Clause reading of the expression_conscience_amendments kernel coherent with the sibling readings (Free Speech, Assembly, Petition, Establishment, Free Exercise), or does it foreclose one or more siblings?',
    'Doctrinal analysis: does protecting institutional press require limiting free speech (e.g., via media regulation or licensing)? Does press institutional role conflict with assembly/petition rights or free exercise? Does the instrumental rationale (informed self-government) require a specific view of establishment or free exercise?',
    'If coherent/coexists: all six readings remain live positions; the kernel contest is a genuine family of alternatives. If foreclosing: certain siblings cannot be held within a single constitutional framework that accepts the Free Press reading as foundational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether Free Press reading is logically coherent with sibling readings').

omega_variable(
    false_summit_natural_law_claim,
    'Is free expression a universal natural law of self-government, or is the claim of universality a naturalization of a historically contingent institutional settlement?',
    'Comparative constitutional analysis: do all functional democracies protect press freedom, or do some achieve legitimate self-government with different press institutions? Historical reconstruction: did the Founders discover a natural law or enact a policy choice?',
    'If universal: classifies as Mountain (immutable feature of legitimate governance). If contingent: classifies as Tangled Rope or Scaffold (coordinate while extracting, or temporary coordination form).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether free press is universal natural law or contingent institutional arrangement').

omega_variable(
    beneficiary_extraction_through_credentialing,
    'Does the Free Press Clause protect all publishers equally, or does it disproportionately benefit credentialed institutional press while creating barriers for independent and community publishers?',
    'Legal doctrine comparison: do courts extend First Amendment press protections equally to bloggers and institutional journalists? Do constitutional press protections effectively require institutional infrastructure (lawyers, capital, professional credentials)? Empirical tracking of which publishers actually claim and successfully defend press clause rights.',
    'If equal protection: the clause is a genuine coordination mechanism for all publishers (Rope). If institutional bias: the clause extracts from independent publishers by requiring institutional gatekeeping to access constitutional protection (Snare or Tangled Rope from independent publisher perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_extraction_through_credentialing, empirical, 'Whether Free Press Clause protects all publishers or privileges institutional press').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expression_conscience_amendments__free_press_clause, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpc_tr_t0, expression_conscience_amendments__free_press_clause, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fpc_tr_t50, expression_conscience_amendments__free_press_clause, theater_ratio, 50, 0.32).
narrative_ontology:measurement(fpc_tr_t100, expression_conscience_amendments__free_press_clause, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(fpc_be_t0, expression_conscience_amendments__free_press_clause, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fpc_be_t50, expression_conscience_amendments__free_press_clause, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(fpc_be_t100, expression_conscience_amendments__free_press_clause, base_extractiveness, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expression_conscience_amendments__free_press_clause, information_standard).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_press_clause, free_speech_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_press_clause, assembly_petition_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_press_clause, establishment_clause).
narrative_ontology:affects_constraint(expression_conscience_amendments__free_press_clause, free_exercise_clause).

% DUAL FORMULATION NOTE:
% The expression_conscience_amendments kernel contains six separate constraint stories, one for each reading (Free Press, Free Speech, Free Exercise, Establishment, Assembly/Petition, and the kernel contest itself). Each reading generates a distinct constraint with its own epsilon value, beneficiary/victim structure, and classification type. They are linked via network.affects_constraints to show family relationships and mutual influences. The Free Press reading (this story) has ε=0.28 (moderate extraction through market gatekeeping despite prior restraint ban). Sibling readings will have different epsilon values reflecting different structural properties: Free Speech may have lower epsilon (less institutional extraction because less gatekeeping); Free Exercise may have different victims and suppressors (religious practitioners); Establishment may protect different principles entirely. Do not average or conflate them — each is a complete constraint story with its own structural identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(expression_conscience_amendments__free_press_clause, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
