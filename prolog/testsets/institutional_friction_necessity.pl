% ============================================================================
% CONSTRAINT STORY: institutional_friction_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_friction_necessity, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_friction_necessity
 *   human_readable: Institutional Friction as Epistemic Necessity
 *   domain: epistemology/information_theory/institutional_analysis
 *
 * SUMMARY:
 *   Institutional friction mechanisms — peer review, editorial gatekeeping,
 *   adversarial sourcing, credentialing requirements — are defended as
 *   structurally necessary to maintain epistemic standards under volume
 *   pressure. The upstream constraint positional_coherence_gradient
 *   establishes that ambient selection pressure drives positional narrowing
 *   (agents optimize for legibility and virality rather than truth).
 *   Deliberate friction counteracts this pressure by imposing costs on
 *   publication and requiring institutional vetting. However, the specific
 *   implementation of this friction concentrates power in institutional
 *   gatekeepers, excludes non-credentialed contributors, and naturalizes this
 *   exclusion as epistemic necessity. The constraint exhibits high extraction
 *   (0.68) because the friction mechanisms serve dual functions: genuine
 *   quality maintenance AND rent extraction through barrier maintenance.
 *   Theater ratio (0.58) reflects that much institutional review is
 *   performative — reviewers assess prestige signals and conformity to norms
 *   rather than verifying claims directly. The constraint has drifted over
 *   the 16-year interval as publication volume increased: what began as
 *   necessary quality control has accumulated extractive overhead as
 *   institutions optimized for their own positional advantage.
 *
 * KEY AGENTS:
 *   - Institutional Gatekeepers (journal editors, editorial boards, credentialing bodies): Primary beneficiaries (institutional/arbitrage) — control access to legitimacy, extract rents through barrier maintenance, experience friction as pure coordination
 *   - Agents Without Institutional Access (independent researchers, non-credentialed contributors, citizen journalists): Primary victims (powerless/trapped) — excluded from epistemic participation by friction mechanisms that naturalize institutional affiliation as quality proxy
 *   - Early-Career Journalists: Secondary victims (moderate/constrained) — face business model collapse and resource concentration in legacy institutions; adversarial sourcing requirements create barriers to independent investigative work
 *   - Junior Academics: Mixed position (moderate/mobile) — benefit from friction when it filters noise, suffer when it enforces prestige hierarchy; experience as tangled rope
 *   - Legacy News Organizations: Beneficiaries (institutional/arbitrage) — friction mechanisms serve as competitive moats protecting market position while maintaining quality standards
 *   - Open Access Coalition: Organized agents (organized/mobile) — building alternative friction mechanisms (preprint servers, post-publication review, distributed verification) with lower extraction; see current system as temporary with generational sunset
 *   - Analytical Observer: Recognizes hybrid structure (analytical/analytical) — friction is genuinely necessary (coordination function) but current implementation is extractive (asymmetric distribution, barrier maintenance, naturalization)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_friction_necessity, 0.68).
domain_priors:suppression_score(institutional_friction_necessity, 0.72).
domain_priors:theater_ratio(institutional_friction_necessity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_friction_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_friction_necessity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(institutional_friction_necessity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_friction_necessity, snare).
narrative_ontology:human_readable(institutional_friction_necessity, "Institutional Friction as Epistemic Necessity").
narrative_ontology:topic_domain(institutional_friction_necessity, "epistemology/information_theory/institutional_analysis").

domain_priors:requires_active_enforcement(institutional_friction_necessity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_friction_necessity, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(institutional_friction_necessity, established_credentialed_actors).
narrative_ontology:constraint_victim(institutional_friction_necessity, agents_without_institutional_access).
narrative_ontology:constraint_victim(institutional_friction_necessity, independent_researchers).
narrative_ontology:constraint_victim(institutional_friction_necessity, non_credentialed_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT RESEARCHER (SNARE) — Lacks institutional credentials and access to gatekept publication venues. Cannot exit the credentialing system without abandoning epistemic participation entirely. Experiences maximum extraction: the friction mechanisms that supposedly ensure quality also exclude contributions based on institutional affiliation rather than merit. The 'necessity' framing naturalizes their exclusion.
constraint_indexing:constraint_classification(institutional_friction_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER JOURNALIST (SNARE) — Constrained by business model collapse and institutional resource requirements for investigative work. Can technically exit journalism but at high career cost. Experiences high extraction: the adversarial sourcing and editorial review processes that ensure quality also concentrate resources in legacy institutions, making independent investigative journalism structurally unviable. The friction is real but the distribution of who bears its cost is asymmetric.
constraint_indexing:constraint_classification(institutional_friction_necessity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUNIOR ACADEMIC (TANGLED ROPE) — Has institutional access but experiences peer review as both coordination (quality signal, feedback mechanism) and extraction (arbitrary gatekeeping, prestige hierarchy maintenance). Mobile across institutions but constrained within the academic system. Benefits from the friction when it filters noise; suffers when it enforces positional narrowing. Mixed experience reflects genuine hybrid structure.
constraint_indexing:constraint_classification(institutional_friction_necessity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: JOURNAL EDITOR (ROPE) — Primary beneficiary of the friction mechanism. Experiences peer review as pure coordination: managing quality, allocating attention, maintaining standards. Can arbitrage across venues and roles. The extraction runs toward this position, not away from it. The 'necessity' framing serves institutional interests by naturalizing the gatekeeper role.
constraint_indexing:constraint_classification(institutional_friction_necessity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY NEWS ORGANIZATION (ROPE) — Benefits from adversarial sourcing and editorial review as competitive moats. The friction mechanisms that ensure investigative quality also create barriers to entry that protect market position. Experiences the constraint as coordination: maintaining standards, building trust, allocating scarce investigative resources. Can arbitrage across business models and platforms.
constraint_indexing:constraint_classification(institutional_friction_necessity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN ACCESS COALITION (SCAFFOLD) — Organized agents building alternative friction mechanisms with lower extraction. Preprint servers, post-publication review, open peer review, and distributed verification systems aim to preserve epistemic hygiene while reducing gatekeeping extraction. Sees current institutional friction as temporary: the sunset occurs as distributed verification mechanisms mature and prove they can maintain quality without concentrating power. Estimated sunset: 15-25 years for norms to stabilize across disciplines.
constraint_indexing:constraint_classification(institutional_friction_necessity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (friction counteracts ambient selection pressure for positional narrowing, as established by upstream constraint positional_coherence_gradient) AND the asymmetric extraction (friction mechanisms concentrate power in institutional gatekeepers, exclude non-credentialed contributors, and naturalize this exclusion as epistemic necessity). The constraint is structurally a tangled rope: the friction is genuinely necessary to maintain epistemic standards under volume pressure, but the specific institutional implementation extracts rents and suppresses alternatives.
constraint_indexing:constraint_classification(institutional_friction_necessity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_friction_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_friction_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_friction_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_friction_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(institutional_friction_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The friction mechanisms serve genuine coordination functions (filtering low-quality submissions, providing feedback, maintaining standards) but also extract significant rents through barrier maintenance. Institutional gatekeepers capture positional advantage by controlling access to legitimacy. Non-credentialed contributors are excluded regardless of contribution quality. The 'necessity' framing naturalizes this extraction by conflating the need for friction (genuine) with the need for institutional concentration (contingent). The value reflects that roughly two-thirds of the constraint's effect is extractive overhead beyond what distributed friction mechanisms could achieve. Suppression (0.72): High. Alternatives to institutional friction are actively suppressed through multiple mechanisms: credentialing requirements exclude non-institutional actors, publication bias favors institutional affiliations, funding concentrates in legacy organizations, and the 'necessity' narrative delegitimizes distributed verification attempts. Independent researchers cannot participate without institutional access; early-career journalists cannot do investigative work without institutional resources. The suppression is structural, not merely incidental. Theater ratio (0.58): Moderate-high. Much institutional review is performative: reviewers assess prestige signals (author affiliations, citation patterns, conformity to disciplinary norms) rather than verifying empirical claims or logical validity directly. Peer review for complex empirical work often cannot verify the claims it purports to validate. Editorial decisions correlate more strongly with author status than with contribution quality in blinded studies. The theater has increased over the interval as volume pressure has outpaced reviewer capacity, making substantive review increasingly infeasible while maintaining the ritual.
 *
 * PERSPECTIVAL GAP:
 *   The institutional gatekeepers see pure coordination (Rope): they are solving the legitimate problem of maintaining quality under volume pressure. The open access coalition sees a temporary problem with a sunset (Scaffold): distributed friction mechanisms are maturing and will eventually provide comparable quality maintenance with lower extraction. The junior academic sees mixed coordination and extraction (Tangled Rope): peer review both helps and harms, depending on whether it filters noise or enforces conformity. The independent researcher and early-career journalist see pure extraction (Snare): the friction mechanisms exclude them based on institutional affiliation rather than contribution quality, and they cannot exit without abandoning epistemic participation. The analytical observer sees the structural hybrid (Tangled Rope): the friction is genuinely necessary to counteract positional narrowing (coordination function established by upstream constraint positional_coherence_gradient) but the institutional implementation extracts rents through barrier maintenance and naturalizes this extraction as epistemic necessity. The perspectival gap reveals that 'necessity' claims are position-dependent: what appears necessary from the gatekeeper position appears extractive from the excluded position.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional gatekeepers are primary beneficiaries: they control access to legitimacy, extract rents through credentialing and publication gatekeeping, and experience the friction as pure coordination (managing quality, allocating attention). Their structural position (institutional power, arbitrage exit options) produces low directionality values and negative effective extraction — the constraint subsidizes them. Agents without institutional access are primary victims: they are excluded from epistemic participation by friction mechanisms that use institutional affiliation as a quality proxy. Their structural position (powerless, trapped) produces maximum directionality and maximum effective extraction. Early-career journalists and junior academics occupy intermediate positions: they have some institutional access but experience the friction as both coordination (quality maintenance) and extraction (barrier maintenance, prestige hierarchy enforcement). Their moderate power and constrained/mobile exit options produce intermediate directionality values. The open access coalition has organized power and sees an exit path (distributed friction mechanisms maturing), producing lower effective extraction despite bearing coordination costs. The analytical observer recognizes the hybrid structure: friction is genuinely necessary (low base extraction for the coordination function) but the institutional implementation adds extractive overhead (high total extraction from asymmetric distribution).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by demonstrating that institutional friction has a genuine coordination function (counteracting ambient selection pressure for positional narrowing, as established by the upstream mountain constraint positional_coherence_gradient) while simultaneously extracting rents through asymmetric implementation. The friction is structurally necessary — some mechanism must impose costs to counteract volume-driven quality collapse — but the specific institutional implementation is contingent and extractive. The analytical classification is Tangled Rope, not Snare, because the coordination function is real and measurable: publication quality does degrade in venues without friction, investigative journalism does collapse without adversarial sourcing requirements, and epistemic hygiene does fail under pure volume pressure. However, the extraction is also real and measurable: institutional gatekeepers capture positional advantage, non-credentialed contributors are excluded regardless of quality, and alternative friction mechanisms (distributed review, post-publication verification, reputation systems) are suppressed. The constraint prevents mislabeling pure extraction as coordination (the gatekeeper's Rope perspective is incomplete) while also preventing mislabeling genuine coordination as pure extraction (the independent researcher's Snare perspective is incomplete). The full structure is visible only from the analytical position, which recognizes both functions simultaneously. The mandatrophy is resolved by showing that necessity claims must be decomposed: the need for friction is genuine (mountain-level constraint from positional_coherence_gradient) but the need for institutional concentration is contingent (tangled rope-level constraint from implementation choices).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    friction_distribution_necessity,
    'Is the current distribution of friction (concentrated in institutional gatekeepers) structurally necessary, or could distributed friction mechanisms achieve the same epistemic function with lower extraction?',
    'Comparative analysis of epistemic outcomes: traditional peer review vs preprint+post-publication review vs distributed verification. Measure false positive rates, correction speeds, and access barriers across systems.',
    'If distributed mechanisms achieve comparable quality: current institutional friction is extractive overhead, not structural necessity. If concentrated gatekeeping is required: extraction is unavoidable cost of epistemic hygiene.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_distribution_necessity, empirical, 'Whether friction must be institutionally concentrated or can be distributed').

omega_variable(
    credentialing_signal_vs_barrier,
    'Do institutional credentials primarily signal quality (coordination function) or primarily exclude competitors (extraction function)?',
    'Correlation analysis: credential status vs contribution quality in domains with measurable outcomes. Compare credentialed vs non-credentialed contributions in open venues (arXiv, Wikipedia, open-source software) where both can participate.',
    'If credentials strongly predict quality: gatekeeping is justified coordination. If credentials weakly predict quality: gatekeeping is primarily extractive barrier maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credentialing_signal_vs_barrier, empirical, 'Whether credentials signal quality or exclude competition').

omega_variable(
    volume_pressure_threshold,
    'At what volume does ambient selection pressure for positional narrowing become severe enough to require deliberate friction, and does current institutional friction match this threshold or overshoot it?',
    'Historical analysis of epistemic degradation rates at different publication volumes. Identify inflection points where quality collapsed without friction vs where friction prevented collapse. Compare to current friction levels.',
    'If current friction matches threshold: extraction is minimized. If current friction overshoots: excess friction is extractive rent-seeking disguised as quality maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(volume_pressure_threshold, empirical, 'Whether current friction level matches structural necessity or overshoots').

omega_variable(
    alternative_friction_viability,
    'Can non-institutional friction mechanisms (reputation systems, prediction markets, distributed review) maintain epistemic standards, or do they fail under adversarial pressure?',
    'Adversarial testing of alternative mechanisms: deliberate injection of false claims into systems with different friction architectures. Measure detection rates, correction speeds, and gaming resistance.',
    'If alternatives prove robust: institutional friction is contingent, not necessary. If alternatives fail under adversarial pressure: institutional concentration may be structurally required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_friction_viability, empirical, 'Whether non-institutional friction mechanisms can resist adversarial gaming').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_friction_necessity, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_fric_tr_t0, institutional_friction_necessity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(inst_fric_tr_t8, institutional_friction_necessity, theater_ratio, 8, 0.48).
narrative_ontology:measurement(inst_fric_tr_t16, institutional_friction_necessity, theater_ratio, 16, 0.58).
narrative_ontology:measurement(inst_fric_tr_t4, institutional_friction_necessity, theater_ratio, 4, 0.41).
narrative_ontology:measurement(inst_fric_tr_t12, institutional_friction_necessity, theater_ratio, 12, 0.53).

% Extraction over time
narrative_ontology:measurement(inst_fric_be_t0, institutional_friction_necessity, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(inst_fric_be_t8, institutional_friction_necessity, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(inst_fric_be_t16, institutional_friction_necessity, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(inst_fric_be_t4, institutional_friction_necessity, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(inst_fric_be_t12, institutional_friction_necessity, base_extractiveness, 12, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_friction_necessity, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of positional_coherence_gradient (mountain: ambient selection pressure drives positional narrowing) and propagation_weight_mechanism (tangled rope: attention allocation creates winner-take-all dynamics). The upstream constraints establish that friction is structurally necessary; this constraint models the specific institutional implementation of that friction and its extractive overhead. The necessity is genuine (inherited from the mountain upstream) but the extraction is contingent (produced by institutional concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
