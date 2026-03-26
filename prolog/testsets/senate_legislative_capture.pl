% ============================================================================
% CONSTRAINT STORY: senate_legislative_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senate_legislative_capture, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: senate_legislative_capture
 *   human_readable: Senate Legislative Capture by Concentrated Corporate Interests
 *   domain: political_economy/governance
 *
 * SUMMARY:
 *   Senate legislative capture represents the structural extraction of
 *   legislative authority from the general public and diffuse constituencies
 *   toward concentrated corporate and wealthy interests. The constraint
 *   operates through the asymmetry between campaign finance requirements
 *   (concentrated, coordinated funding from corporate donors) and constituent
 *   voice mechanisms (dispersed, individualized voting and communication).
 *   Over the 50-year interval (roughly 1975-2025), both the theater ratio and
 *   extractiveness have increased steadily as campaign finance costs have
 *   escalated, lobbying infrastructure has professionalized, and constituent
 *   organizing capacity has relatively declined. The theater ratio's rise to
 *   0.81 reflects that legislative deliberation (committee hearings, floor
 *   debate, constituent services) increasingly performs the ritual of
 *   representative democracy while predetermined donor-aligned outcomes
 *   dominate actual legislative results. The extractiveness rise to 0.68
 *   reflects that the benefits to concentrated corporate interests have grown
 *   through targeted legislation, while diffuse public costs have accumulated
 *   in areas where concentrated interests profit (healthcare, finance,
 *   energy, pharma, telecom). This constraint exemplifies how snare
 *   classification emerges from multiple perspectives while false summit
 *   risks appear in civilizational views that naturalize capture as inherent
 *   to democracy.
 *
 * KEY AGENTS:
 *   - General Public / Unrepresented Constituencies: Primary victim (powerless/trapped) — diffuse stakeholders whose aggregate interests are structurally outweighed by concentrated interests; experience maximum extraction
 *   - Civic Idealists: Secondary victim (powerless/identity_locked) — constituents and activists whose identity is fused with democratic participation; structurally mobile but locked by internalized belief in democratic responsiveness
 *   - Corporate Beneficiaries / Wealthy Donors: Primary beneficiary (institutional/arbitrage) — concentrated interests that coordinate campaign funding to achieve legislative advantage; experience constraint as coordination mechanism (Rope perspective)
 *   - Captured Senators: Institutional intermediaries (institutional/constrained) — oscillate between constituent responsiveness and donor dependency; maintain formal deliberation rituals while voting patterns align with funding sources
 *   - Reform Coalition: Organized opposition (organized/constrained) — campaign finance reformers, voting rights organizations, transparency advocates attempting to reduce capture through political organizing and institutional change
 *   - Political Economy Analysts: Civilizational observers (analytical/analytical) — risk naturalizing capture as immutable feature of democracy, producing false mountain classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senate_legislative_capture, 0.68).
domain_priors:suppression_score(senate_legislative_capture, 0.72).
domain_priors:theater_ratio(senate_legislative_capture, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senate_legislative_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(senate_legislative_capture, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(senate_legislative_capture, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senate_legislative_capture, snare).
narrative_ontology:human_readable(senate_legislative_capture, "Senate Legislative Capture by Concentrated Corporate Interests").
narrative_ontology:topic_domain(senate_legislative_capture, "political_economy/governance").

domain_priors:requires_active_enforcement(senate_legislative_capture).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senate_legislative_capture, concentrated_corporate_interests).
narrative_ontology:constraint_beneficiary(senate_legislative_capture, wealthy_campaign_donors).
narrative_ontology:constraint_victim(senate_legislative_capture, general_public).
narrative_ontology:constraint_victim(senate_legislative_capture, unrepresented_constituencies).
narrative_ontology:constraint_victim(senate_legislative_capture, diffuse_stakeholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREPRESENTED VOTER (SNARE) — Bears full extraction cost through legislation favoring corporate interests over constituent welfare. No viable exit from the political system; voice mechanisms (voting, organizing) produce negligible legislative response when interests conflict with donor preferences. Maximum extraction experienced.
constraint_indexing:constraint_classification(senate_legislative_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIC IDEALIST (SNARE) — Structurally mobile (can exit politics, migrate, opt out of civic participation) but identity-fused with democratic participation and belief in democratic responsiveness. The identity lock prevents recognizing the constraint as changeable — perceives legislative capture as immutable feature of democracy itself rather than as contingent institutional capture. High extracted value but locked in place by internalized civic identity.
constraint_indexing:constraint_classification(senate_legislative_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: CORPORATE BENEFICIARY (ROPE) — Experiences the constraint as coordination: organizing concurrent campaign contributions creates a collective action mechanism that achieves favorable legislation. No coercion required from the corporate perspective — the mechanism works through aligned incentives. Access and legislative advantage appear earned through normal political participation.
constraint_indexing:constraint_classification(senate_legislative_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPTURED SENATOR (PITON) — Senator maintains formal independence and participates in committee deliberations but votes predictably align with donor preferences when interests align. The constraint persists through institutional inertia: the senator's career depends on campaign funding sources, but the formal structures (committee process, floor votes, constituent services) continue as though independent judgment is being exercised. Theater ratio high — performative deliberation masking predetermined outcomes.
constraint_indexing:constraint_classification(senate_legislative_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized agents (campaign finance reformers, voting rights organizations, transparency advocates) see the constraint as having both extraction and coordination components. The constraint must be actively enforced (through lobbying for advantageous legislation, campaign financing tactics), but also accomplishes genuine coordination of dispersed corporate interests. Reformers face high barriers to exit (career/resource constraints) but retain some agency through political organizing.
constraint_indexing:constraint_classification(senate_legislative_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLITICAL ECONOMY THEORIST (MOUNTAIN) — Civilizational view risks naturalizing legislative capture as an immutable feature of representative democracy: concentrated corporate interests will always outbid dispersed public interests; campaign finance advantage is inherent to electoral mathematics. However, this mountain classification is a false summit — comparative institutional analysis reveals that other democracies achieve lower capture through parliamentary structures, campaign finance restrictions, and party discipline. The constraint is contingent, not natural.
constraint_indexing:constraint_classification(senate_legislative_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senate_legislative_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senate_legislative_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senate_legislative_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senate_legislative_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senate_legislative_capture, TR),
    TR >= 0.70.

:- end_tests(senate_legislative_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Concentrated corporate interests extract significant legislative advantage through campaign finance mechanisms. The extraction grows over time as campaign costs escalate and corporate political spending increases. This is not trivial coordination cost — it represents measurable redistribution of legislative outcomes toward corporate beneficiaries. Suppression (0.72): High. Constituents face multiple suppression mechanisms: (1) Structural barriers to political access (fundraising requirements, lobby infrastructure); (2) Cognitive barriers (internalized belief that individual voice doesn't matter in large-scale politics); (3) Institutional design (gerrymandering, voter suppression, campaign contribution deductibility reducing transparency). The combination produces high total suppression. Theater ratio (0.81): Very high. Senate deliberation maintains extensive performative elements (committee hearings, constituent services, floor speeches) that follow proper democratic ritual while legislative outcomes are increasingly predetermined by donor preferences. The rise in theater over 50 years reflects that formal procedures have been preserved (maintaining legitimacy) while actual decision-making has shifted to donor-aligned informal channels. This is the signature of piton degradation — the machine still runs but the primary function has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) classifications reveals the mechanism of capture: what appears to the corporate beneficiary as natural coordination of aligned interests appears to the powerless voter as extraction with no escape. No single perspective is 'wrong' — they are measuring the same constraint from structurally different positions. The critical insight is that the constraint can only sustain itself if these perspectives remain incommensurable. The moment voters perceive the structure (shift from identity_locked to analyzing the mechanism), or the moment reformers successfully reorganize (shift from constrained to organized opposition), the classification at those indices changes and the constraint's sustainability is threatened.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by structural position relative to the extraction flow. Corporate beneficiaries have low d (approximately 0.10-0.20) — they benefit from the constraint and have arbitrage options (can exit to other political systems, other business strategies). Trapped voters have high d (approximately 0.90-0.95) — they bear extraction and cannot exit the political system without fundamental life reorganization (migration). Civic idealists have identity-locked exit (d approximately 0.88) — structurally mobile but cognitively captured by internalized democratic ideology. Captured senators have constrained exit (d approximately 0.65) — dependent on donor funding for career continuation but retaining some discretionary votes. Reform coalitions have organized exit (d approximately 0.50) — they experience extraction but also have some agency through political organizing and can constrain senator behavior through threat of primary challenges. The sigmoid function f(d) maps these d values to effective power multipliers, which when combined with ε and scope σ(S) produce the experienced χ for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by rejecting the false mountain classification and accepting that legislative capture is a Snare. The mountain view ('concentrated interests always beat dispersed interests; this is inherent to democracy') is a false summit that naturalizes contingent institutional arrangements. Comparative evidence shows democracies with different campaign finance structures, party discipline, and proportional representation systems achieve significantly lower capture rates. Legislative capture is therefore not a natural law but a structural outcome of specific institutional choices. The mandatrophy resolves as: SNARE is the correct classification from the victim and analytical perspectives. The Rope perspective from corporate beneficiaries is legitimate but narrow — it elides the extraction visible from other vantage points. The Piton perspective reveals important degradation (theater rising, actual deliberation declining) but misses the active enforcement mechanisms maintaining the snare. The mountain false summit must be rejected. Resolving the mandatrophy requires accepting that all six types are analytically coherent from their respective indices but that the democratic legitimacy frame requires centering the victim and analytical perspectives, which both classify as Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    donor_intent_vs_implicit_alignment,
    'Does legislative capture require explicit quid pro quo coordination between donors and senators, or does implicit alignment of incentives produce equivalent outcomes without conscious coordination?',
    'Investigation of campaign finance records, senator voting patterns, and testimony from donors and staffers; analysis of whether donor intent and legislative outcome require coordinated planning or emerge from aligned incentives',
    'If explicit coordination required: captures as snare or tangled_rope requiring active collusion. If implicit alignment sufficient: could classify as piton (institutional inertia maintaining coordination). Legal and political consequences differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(donor_intent_vs_implicit_alignment, empirical, 'Whether legislative capture requires explicit coordination or emerges from implicit incentive alignment').

omega_variable(
    diffuse_vs_concentrated_interest_asymmetry,
    'Is the power asymmetry between concentrated corporate interests and diffuse public interests a structural feature of democracy or a contingent artifact of campaign finance rules?',
    'Comparative analysis of democracies with different campaign finance restrictions, party systems, and legislative structures; correlation between institutional design and degree of legislative capture',
    'If structural: legislative capture is mountain-like (inherent to democracy). If contingent: capture is snare or tangled_rope that could be reformed through institutional design. Determines whether mandatrophy resolves as ''this is how systems work'' or ''this is how we chose to structure it.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diffuse_vs_concentrated_interest_asymmetry, conceptual, 'Whether diffuse-vs-concentrated asymmetry is structural or contingent').

omega_variable(
    constituent_voice_suppression_mechanism,
    'Are constituents suppressed from legislative influence by structural barriers (money, access, institutional design) or by internalized expectations that their voices don''t matter (cognitive/identity suppression)?',
    'Analysis of constituent contact attempts, response rates, legislative responsiveness to constituent pressure; intervention studies testing whether constituent organizing increases responsiveness when donor interests align vs conflict',
    'If structural: classified as trapped exit. If internalized: classified as identity_locked. Determines whether reform requires changing institutions (structural) or changing constituent perception of voice efficacy (cognitive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_voice_suppression_mechanism, empirical, 'Whether constituent suppression is structural or internalized').

omega_variable(
    reform_reform_capture,
    'Do campaign finance reforms themselves become captured by the same concentrated interests they were designed to constrain?',
    'Historical analysis of campaign finance reform outcomes; tracking of how lobbying industries adapt to new regulations; measurement of legislative capture rates pre- vs post-reform',
    'If reforms are repeatedly captured: suggests snare is immutable via structural barriers (true high-extraction). If reforms produce temporary but sustainable reductions: suggests piton with institutional memory of non-captured periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_reform_capture, empirical, 'Whether campaign finance reforms become captured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senate_legislative_capture, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(senatecap_tr_t0, senate_legislative_capture, theater_ratio, 0, 0.62).
narrative_ontology:measurement(senatecap_tr_t15, senate_legislative_capture, theater_ratio, 15, 0.73).
narrative_ontology:measurement(senatecap_tr_t30, senate_legislative_capture, theater_ratio, 30, 0.81).
narrative_ontology:measurement(senatecap_tr_t45, senate_legislative_capture, theater_ratio, 45, 0.85).

% Extraction over time
narrative_ontology:measurement(senatecap_be_t0, senate_legislative_capture, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(senatecap_be_t15, senate_legislative_capture, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(senatecap_be_t30, senate_legislative_capture, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(senatecap_be_t45, senate_legislative_capture, base_extractiveness, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senate_legislative_capture, enforcement_mechanism).
narrative_ontology:affects_constraint(senate_legislative_capture, campaign_finance_asymmetry).
narrative_ontology:affects_constraint(senate_legislative_capture, regulatory_capture).
narrative_ontology:affects_constraint(senate_legislative_capture, lobbying_access_inequality).

% DUAL FORMULATION NOTE:
% Senate legislative capture is downstream of campaign finance structural inequalities and upstream of specific regulatory capture instances. Each constituent regulation (pharmaceutical pricing, financial sector rules, energy policy) represents a specific instantiation of the general legislative capture constraint. The family of constraints includes the general structural capture mechanism and its domain-specific manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(senate_legislative_capture, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
