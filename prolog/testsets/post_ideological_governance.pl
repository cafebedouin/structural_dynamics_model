% ============================================================================
% CONSTRAINT STORY: post_ideological_governance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_ideological_governance, []).

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
 *   constraint_id: post_ideological_governance
 *   human_readable: Post-Ideological Governance as Coordination and Extraction
 *   domain: political_economy/institutional_design
 *
 * SUMMARY:
 *   Post-ideological governance refers to institutional arrangements that
 *   claim to transcend ideological contestation by delegating policy
 *   decisions to technical experts, evidence-based reasoning, and supposedly
 *   neutral administrative processes. The constraint emerged prominently
 *   after the Cold War's ideological defeat of Soviet communism, crystallized
 *   through the 1990s Washington Consensus and central bank independence
 *   doctrine, and became the dominant governance frame across developed
 *   democracies by the 2000s. However, the constraint exhibits the full
 *   structural signature of a Tangled Rope: it coordinates genuine public
 *   goods delivery (technocratic governance can solve infrastructure,
 *   monetary, and regulatory coordination problems with lower partisan
 *   gridlock) while systematically extracting by converting particular
 *   ideological preferences—especially market liberalism, growth orientation,
 *   and capital-friendly property regimes—into the status of 'neutral
 *   technical necessity.' The constraint suppresses alternative ideological
 *   visions not through explicit prohibition but through the rhetorical
 *   mechanism of declaring them 'ideological' (and thus irrational,
 *   emotional, unscientific) while encoding beneficiary preferences as
 *   'technical' (rational, evidence-based, scientific). The theater_ratio has
 *   increased over the measurement interval, indicating that as material
 *   legitimacy has declined (financialization, inequality growth, climate
 *   inaction, pandemic mismanagement), the constraint relies increasingly on
 *   elaborate expertise-theater to maintain its framing.
 *
 * KEY AGENTS:
 *   - Technocratic Administrative Class: Primary beneficiary (institutional/arbitrage) — central banks, regulatory agencies, expert commissions gain epistemic authority, insulation from electoral volatility, and career advancement through post-ideological framing
 *   - Incumbent Economic Interests: Primary beneficiary (institutional/arbitrage) — corporations, financial sectors, incumbent industries benefit from exclusion of redistributive and regulatory ideologies from legitimate governance deliberation
 *   - Democratic Deliberation: Primary victim (powerless/identity_locked) — institutional practice of public contestation over competing visions of the good is structurally suppressed; suppression is internalized as 'maturity' and 'realism'
 *   - Ideological Minorities: Secondary victim (powerless/trapped) — citizens whose worldview is structurally excluded cannot exit the polity; trapped within national borders and institutional structures claiming neutrality
 *   - Democratic Constituency: Moderate victim (moderate/constrained) — faces extraction (suppressed ideological voice) but receives coordination benefits (material stability, infrastructure, public goods delivery); exit is costly but theoretically possible
 *   - Ideological Apparatus: Degraded institution (institutional/arbitrage) — political parties, labor organizations, ideological journalism persist through inertia but their deliberation function has atrophied; maintained through historical prestige rather than current function
 *   - Analytical Observer: Sees hybrid structure (analytical/analytical) — recognizes both genuine coordination and systematic extraction through rhetorical coding of preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_ideological_governance, 0.58).
domain_priors:suppression_score(post_ideological_governance, 0.65).
domain_priors:theater_ratio(post_ideological_governance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_ideological_governance, extractiveness, 0.58).
narrative_ontology:constraint_metric(post_ideological_governance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(post_ideological_governance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_ideological_governance, tangled_rope).
narrative_ontology:human_readable(post_ideological_governance, "Post-Ideological Governance as Coordination and Extraction").
narrative_ontology:topic_domain(post_ideological_governance, "political_economy/institutional_design").

domain_priors:requires_active_enforcement(post_ideological_governance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_ideological_governance, technocratic_administrative_class).
narrative_ontology:constraint_beneficiary(post_ideological_governance, incumbent_economic_interests).
narrative_ontology:constraint_victim(post_ideological_governance, democratic_deliberation).
narrative_ontology:constraint_victim(post_ideological_governance, ideological_minorities).
narrative_ontology:constraint_victim(post_ideological_governance, distributed_publics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDEOLOGICAL MINORITY (SNARE) — Citizens whose worldview or value framework is structurally excluded from governance cannot exit the polity. Trapped within national borders and institutional structures that claim neutrality while suppressing non-technocratic frames. Experiences maximum extraction: genuine policy preferences cannot be articulated, deliberated, or represented within legitimate governance channels. The 'post-ideological' frame naturalizes their exclusion as apolitical necessity.
constraint_indexing:constraint_classification(post_ideological_governance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC DELIBERATION (SNARE, identity_locked) — The institutional practice of public deliberation around competing visions of the good is structurally trapped. Cannot exit from the governance frame while remaining politically engaged. The identity-lock is cognitive: citizens internalize the post-ideological frame as 'maturity' and 'realism,' making ideological deliberation unthinkable from within. The constraint extracts the very practice of democratic contestation itself.
constraint_indexing:constraint_classification(post_ideological_governance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: DEMOCRATIC CONSTITUENCY (TANGLED ROPE) — Moderate agents face extraction (suppressed ideological voice) but also benefit from coordination: technocratic governance may deliver material stability, infrastructure, and public goods. Exit is costly (emigration, disengagement from political process) but theoretically possible. Genuine coordination function exists (solving collective action problems) alongside asymmetric extraction (whose values get encoded in 'technocratic' solutions).
constraint_indexing:constraint_classification(post_ideological_governance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECHNOCRATIC ADMINISTRATIVE CLASS (ROPE) — Institutional actors (central banks, regulatory agencies, expert commissions) experience post-ideological governance as pure coordination: solving technical problems without ideological baggage. Benefits from the constraint through epistemic authority, career advancement, and insulation from electoral volatility. Experiences low extraction because the constraint aligns with their interests and framing. Net beneficiary with arbitrage options (can move between national bureaucracies, think tanks, international organizations).
constraint_indexing:constraint_classification(post_ideological_governance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENT ECONOMIC INTERESTS (ROPE) — Corporations and financial sectors benefit from post-ideological governance framing because it excludes redistributive and regulatory ideologies from legitimate deliberation. Experiences the constraint as coordination (technical problem-solving) while extracting disproportionate policy benefits. Exit options include capital flight, regulatory arbitrage, and relocation. Net beneficiary — the constraint aligns their preferences with the 'neutral technocratic' consensus.
constraint_indexing:constraint_classification(post_ideological_governance, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: IDEOLOGICAL APPARATUS (PITON) — The institutions and practices that once enabled ideological contestation (political parties, labor organizations, ideological journalism, public intellectuals) have atrophied or been captured. They persist through inertia (electoral legitimacy, historical prestige) while their core function (enabling competing visions of the good) has degraded. Theater_ratio is high: formal institutional structures remain, but their ideological deliberation function has been hollowed out by the post-ideological frame.
constraint_indexing:constraint_classification(post_ideological_governance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination (depoliticizing technical governance reduces partisan gridlock on infrastructure, monetary policy, regulatory coherence) alongside systematic extraction (whose preferences get coded as 'neutral technical truth'). From civilizational scope, the constraint is neither immutable natural law nor pure extraction — it is a hybrid institutional arrangement that bundles real coordination benefits with concealed asymmetric extraction through the rhetorical move of declaring certain preferences 'ideological' (excluded) and others 'technical' (included).
constraint_indexing:constraint_classification(post_ideological_governance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_ideological_governance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_ideological_governance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_ideological_governance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_ideological_governance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(post_ideological_governance, TR),
    TR >= 0.70.

:- end_tests(post_ideological_governance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts ideological deliberation capacity and systematically encodes particular preferences as neutral while excluding others. However, extraction is not maximal because genuine coordination benefits exist — technocratic governance does deliver infrastructure, monetary stability, and regulatory coherence with lower partisan gridlock than purely ideological systems. The extractiveness value reflects the tradeoff: real material delivery partially justifies the suppression of deliberative voice. The measurement trajectory (0.35→0.58) indicates accumulating extraction as technocratic governance has increasingly defined itself against 'ideology' and as material legitimacy has declined, forcing escalation of expertise-theater. Suppression (0.65): Moderate-high. Multiple barriers to ideological deliberation exist: institutional channels that claim neutrality exclude explicitly ideological framings; media structures amplify expert voices while marginalizing ideological contestation; educational systems encode post-ideological frames as sophisticated reasoning; elite networks self-select for post-ideological orientation. However, suppression is not total — ideological contestation persists outside technocratic channels (populist movements, social media, alternative institutions), and some populations retain capacity for ideological reasoning. Theater ratio (0.68): High and increasing. Post-ideological governance relies increasingly on elaborate expertise-theater: technical jargon masking value choices, evidence-based justification for pre-determined outcomes, expert commissions performing deliberation without genuine public participation, regulatory theater maintaining appearance of neutrality. As material delivery has faltered (financialization, inequality, climate crisis, pandemic failures), theater has escalated to sustain the frame.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival diversity, spanning from Rope (beneficiary view) to Snare (victim view) to Tangled Rope (moderate and analytical views). This diversity reflects the constraint's core mechanism: it appears as neutral coordination to those whose preferences align with the technocratic consensus, while appearing as pure extraction to those excluded. The identity-locked classification for democratic deliberation itself reveals that the constraint operates partly through cognitive capture — it naturalizes its own suppression as maturity and realism. This gap between external suppression (barriers to ideological deliberation) and internal identity-locking (deliberation becoming unthinkable as educated reasoning) is the deepest structural feature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the extraction flow. Technocratic administrators and incumbent economic interests occupy the beneficiary position (d ≈ 0.15-0.20): they benefit from the constraint's framing and have high exit options (career mobility, capital flight, regulatory arbitrage). Democratic deliberation is trapped (d ≈ 0.95): it cannot exit because suppressing it is the constraint's defining mechanism. Ideological minorities are trapped (d ≈ 0.95): they cannot exit the polity even as it excludes their worldview. The moderate constituency faces constrained exit (d ≈ 0.65-0.75): they can potentially emigrate or disengage from politics, but at high cost. The piton institutional perspective has arbitrage options but experiences degradation (d ≈ 0.25): it benefits structurally from historical prestige but cannot restore its functional capacity. The analytical observer's effective extraction is high (d ≈ 0.75) because the observer must account for both the real coordination benefits and the systematic extraction — the observer's position is to see what beneficiaries deny (capture) and what victims cannot articulate (cognitive suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ANALYSIS: The post-ideological governance constraint resolves the mandatrophy by acknowledging that both coordination and extraction are real and coexistent. The constraint genuinely solves coordination problems (reducing partisan gridlock, enabling infrastructure investment, stabilizing monetary policy) — these benefits are not theater or delusion. However, the constraint simultaneously extracts by systematically encoding particular ideological preferences as neutral technical truth while excluding alternatives. The mandatrophy is resolved by recognizing that the coordination function is real but captured: the coordination is not universal but rather conditional on particular interests' preferences being coded as technical. The Tangled Rope classification captures this exactly: genuine coordination function (χ's lower bound) with asymmetric extraction (χ's upper bound). The analytical perspective and the piton perspective together prevent mislabeling: the analytical view shows that the capture is real and systematic, not incidental; the piton view shows that the ideological apparatus (which would historically serve as counterweight and deliberative vehicle) has degraded into theater. If ideological minorities had robust organizational capacity (strong labor movements, influential ideological parties, independent media), the constraint might still be Tangled Rope but with lower effective extractiveness. The degradation of the ideological apparatus (piton status) is what allows extraction to accumulate over the measurement interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technocratic_neutrality_illusion,
    'Is post-ideological governance actually neutral technical problem-solving, or does it systematically encode particular ideological preferences (market liberalism, growth-orientation, capital-friendly property regimes) as ''neutral'' while excluding alternatives?',
    'Historical policy analysis: examine which policy domains claimed ''technical'' vs ''ideological'' status; correlate with measurable beneficiary distributions; identify systematic reversals (what counts as technical when which party holds office); cross-national comparison of supposedly neutral policies that diverge significantly',
    'If genuinely neutral: post-ideological governance is legitimate coordination, extractiveness drops to 0.30 (Rope from all perspectives). If systematically coded: extractiveness remains high (0.58+), revealing that the coordination function is real but captured by particular interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technocratic_neutrality_illusion, empirical, 'Whether post-ideological governance is actually neutral or encodes particular ideological preferences as technical').

omega_variable(
    democratic_deliberation_substitution,
    'Can public reason and technocratic expertise substitute for democratic deliberation over ends, or does deliberation perform a irreducible function that technical governance cannot fulfill?',
    'Longitudinal public legitimacy and satisfaction surveys across countries with varying ratios of technocratic vs deliberative governance; measurement of protest, anti-establishment sentiment, and institutional trust; identification of issues where populations feel their values were excluded despite accepting the technical outcome',
    'If substitutable: the extraction mechanism is overstated; post-ideological governance genuinely trades ideological conflict for material delivery. If irreducible: suppression of deliberation creates accumulating legitimacy deficit that manifests in populist backlash, reducing the constraint''s sustainability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deliberation_substitution, empirical, 'Whether democratic deliberation is substitutable by technocratic expertise or irreducibly necessary').

omega_variable(
    identity_lock_reversibility,
    'Is the cognitive frame that makes ideological deliberation ''unthinkable'' reversible through exposure to alternative frameworks, or has it become constitutive of educated civic identity?',
    'Generational cohort analysis: do younger generations educated entirely within post-ideological frames show lower capacity for ideological reasoning? Experimental exposure to ideological deliberation contexts; measurement of whether re-exposure to ideological contestation shifts perceived legitimacy or triggers identity threat responses',
    'If reversible: identity_locked classification is contingent; a shift in institutional framing could restore deliberative capacity. If constitutive: the cognitive suppression is durable and the constraint''s extraction is more deeply embedded than structural barriers alone would predict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock to post-ideological framing is reversible or constitutive of educated civic identity').

omega_variable(
    supply_side_legitimacy_collapse,
    'Does post-ideological governance depend on sustained material delivery and competence, or can it persist through pure theater once material legitimacy begins to fail?',
    'Historical cases of technocratic governance under performance stress (austerity after 2008, pandemic response failures, climate inaction); measurement of theater escalation (more elaborate justification, expertise-theater) correlated with declining material outcomes; identification of tipping points where populations reject technocratic framing',
    'If dependent on material delivery: extractiveness will collapse if governance performance declines, potentially shifting to pure Snare. If theater-resilient: the constraint exhibits strong piton characteristics and may persist long after functional degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_side_legitimacy_collapse, empirical, 'Whether post-ideological governance can persist through theater if material delivery fails').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_ideological_governance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(post_tr_t0, post_ideological_governance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(post_tr_t15, post_ideological_governance, theater_ratio, 15, 0.58).
narrative_ontology:measurement(post_tr_t30, post_ideological_governance, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(post_be_t0, post_ideological_governance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(post_be_t15, post_ideological_governance, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(post_be_t30, post_ideological_governance, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_ideological_governance, identity_coordination).
narrative_ontology:affects_constraint(post_ideological_governance, central_bank_independence).
narrative_ontology:affects_constraint(post_ideological_governance, regulatory_capture).
narrative_ontology:affects_constraint(post_ideological_governance, neoliberal_policy_consensus).
narrative_ontology:affects_constraint(post_ideological_governance, epistemic_gatekeeping).

% DUAL FORMULATION NOTE:
% Post-ideological governance is a meta-constraint that operates across multiple institutional domains. The downstream constraints (central bank independence, regulatory capture, neoliberal consensus, epistemic gatekeeping) each exhibit their own extractive mechanisms, but they are all enabled and reinforced by the broader post-ideological frame that codes market-friendly preferences as technical necessity. Central bank independence and regulatory capture are the most direct institutional instantiations. Neoliberal policy consensus represents the ideological content that is rhetorically suppressed while actually governing. Epistemic gatekeeping refers to how expertise is monopolized to exclude ideological contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(post_ideological_governance, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
