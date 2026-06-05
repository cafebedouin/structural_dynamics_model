% ============================================================================
% CONSTRAINT STORY: ulysses_chp12
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_cyclops_1904, []).

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
 *   constraint_id: ulysses_chp12
 *   human_readable: The Cyclopean Snare (Barney Kiernan's Pub)
 *   domain: social/political/nationalist
 *
 * SUMMARY:
 *   Chapter 12 of James Joyce's Ulysses (the 'Cyclops' episode) depicts
 *   Leopold Bloom's encounter with militant Irish nationalism in Barney
 *   Kiernan's pub in Dublin. The constraint operates as a dual mechanism: (1)
 *   Rope of coordination for the nationalist in-group (organized around
 *   shared anti-English sentiment, Irish identity, linguistic performance),
 *   and (2) Snare of xenophobic extraction targeting Bloom, a Jewish outsider
 *   and resident alien. The pub environment progressively constructs an
 *   inescapable social space where Bloom's outsider status is ritualized,
 *   mocked, and ultimately violently enforced. The Citizen, the primary
 *   enforcer, executes nationalist performance theater while simultaneously
 *   extracting humiliation and submission from Bloom. The constraint's
 *   evolution from verbal snare to physical violence (the biscuit tin assault
 *   following Bloom's blessing invocation) reveals the suppressed violent
 *   core of the nationalist coordination mechanism. The constraint is
 *   structurally a Snare: high extractiveness (0.68), high suppression
 *   (0.72), and high enough theater (0.58) to indicate that much of the
 *   activity is performative status-assertion rather than functionally
 *   directed political organization. The measurement trajectory shows
 *   extraction increasing from 0.35 to 0.68 across the encounter, indicating
 *   escalating coercion as Bloom's position becomes more untenable.
 *
 * KEY AGENTS:
 *   - Leopold Bloom: Primary victim (powerless/trapped) — outsider, Jew, cosmopolitan, unable to exit without provoking escalation; bears sustained verbal assault and eventual physical violence
 *   - The Citizen: Primary beneficiary and enforcer (powerful/mobile) — nationalist in-group leader; derives status, dominance, and rhetorical authority from the xenophobic constraint; enforces through verbal assault and threat
 *   - Nationalist In-Group ('True Men'): Beneficiary collective (organized/mobile) — coordinates around shared nationalist ideology; experiences constraint as pure coordination (Rope) within group, as snare against outsider
 *   - Martin Cunningham: Moderate participant (moderate/constrained) — knows Bloom, sympathetic, but constrained by in-group loyalty; trapped between moral conviction and social pressure
 *   - Dublin Pub Institution: Institutional setting (institutional/constrained) — maintains nationalist performance theater; embeds the constraint in social ritual and habitat
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing nationalist xenophobia as inevitable human tribalism rather than contingent political arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp12, 0.68).
domain_priors:suppression_score(ulysses_chp12, 0.72).
domain_priors:theater_ratio(ulysses_chp12, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp12, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp12, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp12, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp12, snare).
narrative_ontology:human_readable(ulysses_chp12, "The Cyclopean Snare (Barney Kiernan's Pub)").
narrative_ontology:topic_domain(ulysses_chp12, "social/political/nationalist").

domain_priors:requires_active_enforcement(ulysses_chp12).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp12, nationalist_in_group).
narrative_ontology:constraint_beneficiary(ulysses_chp12, the_citizen).
narrative_ontology:constraint_victim(ulysses_chp12, leopold_bloom).
narrative_ontology:constraint_victim(ulysses_chp12, cosmopolitan_outsiders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEOPOLD BLOOM (SNARE) — Trapped in the pub by social obligation (buying drinks, maintaining civility). Cannot exit without triggering explosive confrontation. Faces sustained verbal assault, mockery of his religion, ancestry, and financial precarity. No alternatives available within the social context. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(ulysses_chp12, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE NATIONALIST IN-GROUP (ROPE) — 'True men' of Ireland coordinate around shared nationalist ideology, linguistic performance, and ritual mockery of outsiders. The pub functions as coordination mechanism: shared language, shared enemies, shared performative masculine nationalist identity. Exit available (leave the pub, pursue other social circles) but the coordination benefits (belonging, shared purpose, status within the group) create strong incentive to maintain participation. d≈0.18, f(d)≈0.02, σ=0.8 → χ≈0.01. Minimal effective extraction because this group experiences the constraint as pure coordination.
constraint_indexing:constraint_classification(ulysses_chp12, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: MARTIN CUNNINGHAM / MODERATE PARTICIPANT (SNARE) — Constrained participant: knows Bloom personally, sympathetic to his plight, but cannot openly oppose the in-group without losing standing. Experiences the constraint as coercive normalization of xenophobia — forced choice between loyalty to fellow nationalist Irishmen and empathy for the outsider/Jew. d≈0.70, f(d)≈1.07, σ=0.8 → χ≈0.49. Moderate extraction because the social constraint prevents him from exercising moral agency.
constraint_indexing:constraint_classification(ulysses_chp12, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE CITIZEN (TANGLED ROPE) — Primary beneficiary and enforcer. Derives status, audience, and rhetorical dominance from nationalist coordination (rope function) while simultaneously extracting social submission and humiliation from the outsider (snare function). Mobile exit option (could leave Dublin, could abandon nationalist politics) but chooses to remain, indicating benefit from current arrangement. Requires active enforcement (verbal assault, threat of violence, eventual physical violence with biscuit tin). d≈0.25, f(d)≈0.12, σ=0.8 → χ≈0.08. Low effective extraction for the beneficiary because he experiences the constraint as power, not constraint.
constraint_indexing:constraint_classification(ulysses_chp12, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: DUBLIN PUB INSTITUTION (PITON) — The pub as social institution maintains nationalist performance theater: drinks, banter, rhetorical excess. Much of the activity is performative (the exaggerated speeches, the stylized mockery) rather than functionally directed toward actual political change. The pub persists as a site of nationalist performance through institutional inertia and social habit. theater_ratio=0.58 is moderate; the piton classification emerges from the observation that the functional coordination (sharing political views, organizing action) is minimal relative to the performative content (rhetoric, mockery, ritual assertion of identity). d≈0.35, f(d)≈0.31, σ=1.0 → χ≈0.09. Constrained exit because the pub institution is embedded in Dublin's social fabric but could be abandoned.
constraint_indexing:constraint_classification(ulysses_chp12, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN RISK) — Observer might be tempted to naturalize the constraint as an immutable feature of tribal/nationalist human behavior: 'In-group/out-group conflict is inevitable; scapegoating of outsiders is a universal law.' This perspective risks reifying contingent historical arrangements (post-colonial Irish nationalism, Bloom's precarious position as a Jew in Dublin) as natural laws. The structural data (ε=0.68, suppression=0.72, theater=0.58) contradicts a mountain classification. The engine will flag this as a false summit: the constraint is a social/political arrangement, not a law of nature.
constraint_indexing:constraint_classification(ulysses_chp12, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp12_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp12, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp12, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp12, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp12, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp12_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The Citizen and nationalist in-group extract substantial value from Bloom: his humiliation provides audience for their rhetoric, his outsider status validates their in-group identity, his precarious economic position makes him vulnerable to social exclusion. The extraction is not maximal (0.70+) because Bloom retains basic human dignity and the possibility of walking out; but the extraction is severe and sustained. The measurement trajectory (0.35 → 0.68) reflects escalating coercion as the encounter progresses: initial verbal posturing gives way to focused mockery of Bloom's religion, ancestry, financial situation, and identity. Suppression (0.72): High. Multiple barriers to Bloom's exit and resistance: (1) Social obligation to remain and maintain civility in order to preserve employment and reputation, (2) Nationalist in-group has numerical superiority and social legitimacy within Dublin's post-colonial context, (3) Bloom's precarious position (Jewish, foreign-origin, modest income) makes him vulnerable to economic/social retaliation, (4) No institutional protection or alternative venue available, (5) Escalating threat of physical violence. Theater ratio (0.58): Moderate. The pub's nationalist performance is genuinely rhetorical (long speeches, stylized mockery, linguistic excess) and partly authentic political conviction. However, the functional coordination — actual political action, actual resistance to English rule — is minimal. Much of the activity is performative assertion of nationalist identity and in-group status. As the constraint escalates toward violence, the performative content (speeches, mockery) gives way to the underlying enforcement mechanism (threat, assault).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. The nationalist in-group sees pure coordination (Rope) — shared language, shared enemies, shared purpose, positive group identity. The Citizen sees power and status (Tangled Rope with low effective extraction for him). Martin Cunningham sees coercive normalization (Snare with moderate extraction for moderate agents). Bloom sees pure extraction and entrapment (Snare with high extraction). The Dublin pub institution sees performative ritual (Piton — theater is doing the work, function is degraded). The analytical observer risks seeing immutable human nature (Mountain — tribalism is inevitable) but the structural data reveals this as a false summit: the constraint is a contingent post-colonial Irish political arrangement, not a law of nature. The perspectival gaps are large and antagonistic: what the in-group experiences as beneficial coordination, Bloom experiences as violent extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Bloom: Victim + trapped → d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64. Maximum extraction because Bloom has no exit. The Citizen: Beneficiary + mobile → d≈0.25, f(d)≈0.12, σ=0.8 → χ≈0.08. Low effective extraction for the primary enforcer because he experiences the constraint as power, not as coercive burden. Nationalist in-group: Mixed beneficiary/moderately mobile → d≈0.20, f(d)≈0.05, σ=0.8 → χ≈0.03. Minimal extraction because the group's exit options are genuine (could leave the pub, pursue other social circles) but the coordination benefits are substantial. Martin Cunningham: Victim + constrained → d≈0.70, f(d)≈1.07, σ=0.8 → χ≈0.49. Moderate extraction because Cunningham is trapped between moral conviction and social pressure; he cannot openly oppose without losing standing. The dual directionality structure (beneficiaries have low d, victims have high d) confirms Snare classification: the constraint's effectiveness derives from asymmetric extraction, not from symmetric coordination. The progression from beneficiary d-values (0.05-0.25) to victim d-values (0.70-0.92) reveals the structural inequality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by maintaining clear distinction between coordination (Rope) and extraction (Snare). The nationalist in-group genuinely coordinates around shared ideology and identity — this is not illusory, and the Rope perspective is accurate for that collective. However, the constraint's PRIMARY function (from the system-wide perspective) is extractive: it creates value for the in-group BY extracting humiliation, exclusion, and submission from the outsider. The constraint cannot be classified as pure Rope because the coordination is purchased through and dependent upon the exclusion/extraction. It is a Snare because the extraction is primary and the coordination is achieved through reinforcing the snare's victims. The tangled rope perspective (The Citizen) shows the hybrid nature: one agent simultaneously benefits from coordination and exercises extraction. The piton perspective shows that the constraint is increasingly theatrical — performative nationalism is substituting for functional political organization. The false summit risk (analytical mountain) is caught by the structural data: high extractiveness and suppression indicate this is not a natural law of human nature, but a political arrangement that could be otherwise. The constraint's legitimacy derives entirely from nationalist post-colonial ideology; it is contingent, not necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bloom_exit_feasibility,
    'Could Bloom have exited the pub earlier without severe social or economic consequences?',
    'Textual analysis of exit points and counterfactual social consequences; historical research on Jewish-Irish social dynamics in 1904 Dublin; economic precarity of Bloom''s position',
    'If exit feasible at multiple points: classification shifts toward tangled_rope (constrained exit, mixed experience). If exit genuinely impossible without severe reputational/economic harm: snare classification confirmed (trapped exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bloom_exit_feasibility, empirical, 'Whether Bloom had genuine exit options from the pub').

omega_variable(
    citizen_nationalist_authenticity,
    'Does The Citizen''s nationalist fervor represent authentic political conviction or performative in-group status-seeking?',
    'Textual examination of Citizen''s rhetorical consistency, historical positioning of Dublin nationalist pub culture, comparison with political actors of the period',
    'If authentic: the coordination (rope) function is genuine; the snare emerges from xenophobic asymmetry. If performative: the entire constraint is extraction theater (piton rising to snare), and the rope function is illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citizen_nationalist_authenticity, conceptual, 'Whether nationalist ideology is authentic conviction or performance').

omega_variable(
    jewish_outsider_status_contingency,
    'Is Bloom''s victimization a necessary consequence of being Jewish in 1904 Dublin, or contingent on specific historical/political factors that could have been otherwise?',
    'Historical research on Jewish-Irish relations, comparative analysis with Jewish integration in other Irish cities or time periods, counterfactual analysis of alternative colonial/nationalist contexts',
    'If necessary: the snare approaches mountain status (inherent to the social structure). If contingent: the snare is a political/institutional arrangement that could be dismantled, and the extraction is not inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_outsider_status_contingency, conceptual, 'Whether Jewish outsider status is contingent or necessary to the constraint').

omega_variable(
    physical_violence_threshold,
    'What transforms the verbal snare into literal violence (the biscuit tin assault)? Is this escalation structural or contingent on Bloom''s final provocation (the blessing)?',
    'Textual analysis of Bloom''s final statement (''And the Saviour of the world ... blessing''); comparison with other pub encounters; historical research on nationalist violence triggers in early 1900s Dublin',
    'If structural: the constraint contains latent violence; suppression (0.72) underestimates the physical threat. If contingent: Bloom''s final act crosses a negotiated line, and the constraint has a (fragile) boundary of acceptable behavior.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_violence_threshold, empirical, 'Whether violence is structural or triggered by Bloom''s provocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp12, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_cyclops_theater_t0, ulysses_chp12, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ulysses_cyclops_theater_t5, ulysses_chp12, theater_ratio, 5, 0.51).
narrative_ontology:measurement(ulysses_cyclops_theater_t10, ulysses_chp12, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(ulysses_cyclops_extract_t0, ulysses_chp12, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ulysses_cyclops_extract_t5, ulysses_chp12, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(ulysses_cyclops_extract_t10, ulysses_chp12, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp12, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp12, dublin_colonial_identity).
narrative_ontology:affects_constraint(ulysses_chp12, jewish_diaspora_integration).

% DUAL FORMULATION NOTE:
% The Cyclopean Snare downstream of Dublin's post-colonial nationalist identity crisis and Jewish diaspora vulnerability. Upstream constraint (dublin_colonial_identity) establishes the nationalist coordination structure; the Snare shows how this coordination is enforced through xenophobic extraction. Lateral relationship with jewish_diaspora_integration: the snare mechanism depends on Bloom's outsider status and limited integration options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ulysses_chp12, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
