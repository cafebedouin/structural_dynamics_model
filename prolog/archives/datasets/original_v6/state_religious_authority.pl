% ============================================================================
% CONSTRAINT STORY: state_religious_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_religious_authority, []).

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
 *   constraint_id: state_religious_authority
 *   human_readable: State Religious Authority and Institutional Legitimacy
 *   domain: political_sociology/religious_studies
 *
 * SUMMARY:
 *   State religious authority represents the institutionalized coupling of
 *   political power with religious legitimacy claims. This constraint
 *   manifests across cultures and historical periods with structural
 *   consistency: the state enforces religious orthodoxy in exchange for
 *   religious authority's conferral of legitimacy, moral framing, and social
 *   binding. The constraint operates through multiple mechanisms: legal
 *   incorporation of religious norms (marriage, inheritance, education),
 *   institutional monopolies (religious authority over certain ceremonies),
 *   enforcement of conformity through state apparatus (criminal penalties,
 *   social exclusion), and epistemological domination (religious framing of
 *   law and governance as natural or divine). The constraint exhibits
 *   distinct characteristics depending on the observer's structural position.
 *   Beneficiaries (state apparatus, privileged religious authority)
 *   experience it as a coordination mechanism that solves the legitimacy and
 *   social cohesion problems. Victims (religious minorities, secular
 *   populations) experience it as pure extraction or highly asymmetric
 *   coercion. The constraint's extractiveness has declined over the
 *   measurement interval (0.72 to 0.58) as international norms against
 *   religious discrimination, education pluralism, and secular legal
 *   frameworks have proliferated. Theater ratio has increased (0.45 to 0.62),
 *   indicating that religious framing in law and governance has become
 *   increasingly performative rather than functionally necessary — suggesting
 *   piton dynamics at civilizational scale.
 *
 * KEY AGENTS:
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — gains legitimacy, enforcement capacity, and social binding through religious authority coupling
 *   - Privileged Religious Authority: Primary beneficiary (institutional/arbitrage) — captures state enforcement power, legal monopolies, and resource access
 *   - Religious Minorities: Primary victim (powerless/trapped) — bear extraction through legal disadvantage, educational exclusion, and social sanctions
 *   - Secular Citizens: Primary victim (powerless/trapped) — bear extraction through mandatory religious framing of law and governance
 *   - Reform Coalition: Organized actor (organized/constrained) — can mobilize within constraints but cannot exit; experience tangled rope of organizing capacity against institutional suppression
 *   - Secularization Pathway: Structural force (organized/constrained) — institutional decoupling occurring through education pluralism, legal rationalization, and international norms; generates scaffold perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable properties of social organization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_religious_authority, 0.58).
domain_priors:suppression_score(state_religious_authority, 0.68).
domain_priors:theater_ratio(state_religious_authority, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_religious_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_religious_authority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_religious_authority, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_religious_authority, tangled_rope).
narrative_ontology:human_readable(state_religious_authority, "State Religious Authority and Institutional Legitimacy").
narrative_ontology:topic_domain(state_religious_authority, "political_sociology/religious_studies").

domain_priors:requires_active_enforcement(state_religious_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_religious_authority, state_institutional_apparatus).
narrative_ontology:constraint_beneficiary(state_religious_authority, privileged_religious_authority).
narrative_ontology:constraint_victim(state_religious_authority, religious_minorities).
narrative_ontology:constraint_victim(state_religious_authority, secular_populations).
narrative_ontology:constraint_victim(state_religious_authority, alternative_epistemologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY (SNARE) — Trapped within a national territory where state-endorsed religion structures legal, educational, and social institutions. Cannot opt out of religious authority through legal frameworks; faces material penalties (education access, employment, legal standing, family law) for non-compliance. The constraint extracts legitimacy and behavioral conformity with minimal coordination benefit. Maximum suppression: state apparatus enforces orthodoxy through law, education, and social sanction.
constraint_indexing:constraint_classification(state_religious_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SECULAR CITIZEN (SNARE) — Structurally trapped by citizenship in a religiously-constituted state. Must navigate religious authority embedded in law (marriage, inheritance, education, oath-taking). No exit option within the jurisdiction; emigration is economically and socially costly. Extraction: religious framing of law deprives secular citizens of secular legal alternatives. Suppression enforced through institutional dependence (cannot marry without religious ceremony, cannot educate without religious curriculum, cannot testify without religious oath).
constraint_indexing:constraint_classification(state_religious_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — Experiences the constraint as coordination mechanism. State authority gains legitimacy through religious endorsement (divine mandate, moral authority, social cohesion narrative). Religious framework provides infrastructure for law-making, oath-administration, ceremony, and social binding without needing to generate these mechanisms independently. Net beneficiary: extraction flows toward state; minimal suppression experienced because the state is the agent implementing the constraint.
constraint_indexing:constraint_classification(state_religious_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIVILEGED RELIGIOUS AUTHORITY (ROPE) — Primary beneficiary. Gains state enforcement power, legal monopoly on certain functions (marriage, education, legitimacy certification), and access to state resources. The constraint coordinates the religious institution's authority with state power. Religious leadership can enforce orthodoxy via state mechanisms; state can enforce religious compliance via religious authority. Pure coordination from this perspective — the relationship is mutually reinforcing.
constraint_indexing:constraint_classification(state_religious_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized agents (civil rights movements, secular organizations, minority religious groups) operate under high constraints: they cannot exit the jurisdiction, but they can organize within it. They experience both coordination and extraction. The state-religious authority constraint does provide them with identifiable legal targets and organizational resources, which enables organizing. But it also suppresses their alternatives through institutional barriers and social pressure. Significant agency within constraints — can negotiate, build coalitions, win incremental reforms.
constraint_indexing:constraint_classification(state_religious_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SECULARIZATION PATHWAY (SCAFFOLD) — From a generational perspective, many states have successfully decoupled religious authority from state power through constitutional reform, education pluralism, and legal rationalization. This perspective sees the state-religious authority constraint as a temporary institutional arrangement with a sunset clause: as societies rationalize (legal codes become explicit rather than derived from religious tradition), as education diversifies (secular knowledge credentials compete with religious authority), as communication networks reduce isolation (exposure to alternative epistemologies spreads), the structural basis for the constraint dissolves. The theater (religious framing of inherently secular functions) becomes increasingly visible and unsustainable. Sunset estimated at 20-50 years depending on region and international pressure.
constraint_indexing:constraint_classification(state_religious_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: VESTIGIAL RELIGIOUS STATE (PITON) — From a civilizational view, some formerly hegemonic state-religious authority constraints persist through institutional inertia despite reduced functional necessity. The constraint no longer coordinates state legitimacy effectively (alternative legitimacy sources: democratic participation, economic performance, security provision, international standing). Religious framing persists in law and ceremony because alternatives haven't been fully established and because institutional actors have investments in the status quo. Theater ratio high: religious language in constitutions, oaths, and ceremonies increasingly performative rather than genuinely generative of legitimacy. The state continues to enforce religious authority not because it provides coordination benefits but because institutional paths-dependency makes change costly.
constraint_indexing:constraint_classification(state_religious_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, some versions of this constraint might be framed as a natural law: religious meaning-making is inherent to human societies; states inevitably draw legitimacy from shared meaning systems; the coupling of religious and political authority is an immutable feature of social organization. However, empirical history contradicts this framing: secular states function and generate legitimacy without religious authority; religious authority persists absent state enforcement; the coupling is contingent, not necessary. The analytical observer risks naturalizing what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(state_religious_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_religious_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_religious_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_religious_authority, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_religious_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_religious_authority, TR),
    TR >= 0.70.

:- end_tests(state_religious_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically advantages privileged religious groups and the state apparatus while disadvantaging religious minorities and secular citizens. Career benefits flow asymmetrically: religious leadership gains institutional position, marriage and education authorities gain monopoly power, state officials gain legitimacy and enforcement capacity. However, extractiveness has declined over the measurement interval due to international pressure, education pluralism, and secular legitimacy alternatives becoming credible. Suppression (0.68): High. Multiple barriers prevent exit: legal frameworks embed religious requirements (marriage, oath-taking, inheritance); educational systems require religious instruction; social institutions (employment, community membership) enforce religious conformity; emigration is economically costly. Suppression is both structural (legal barriers) and internalized (populations socialized into religious authority acceptance). Theater ratio (0.62): Moderate-high. Religious framing persists in law and governance but increasingly serves performative rather than functional purposes. Modern states generate legitimacy through democratic participation, rule of law, and economic performance — religious framing is supplementary. The increase from 0.45 to 0.62 indicates growing dissonance between the performative religious content and actual legitimacy sources.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiaries (rope) and victims (snare) is extreme — nearly binary classification of the same constraint. This gap reflects genuine structural difference: the beneficiary experiences voluntary coordination; the victim experiences coerced conformity. The reform coalition's tangled rope classification bridges the gap by acknowledging both elements: the constraint does coordinate something (legitimacy), but distribution is asymmetric. The scaffold and piton perspectives add temporal dynamics: the constraint is deteriorating (theater increasing, extractiveness declining) due to structural alternatives becoming viable. The mountain perspective is a false summit — the constraint naturalizes a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.05 (privileged religious authority, full beneficiary) to 0.95 (religious minorities, full victim). The state apparatus occupies intermediate position (d ≈ 0.15-0.25): benefits from legitimacy gain but also bears institutional cost of maintaining enforcement apparatus. The reform coalition occupies moderate position (d ≈ 0.55-0.70): constrained by the enforcement mechanism but capable of organizing and winning incremental reforms. The secular citizen trapped at the national level experiences d ≈ 0.90-0.95 because they bear legal disadvantage without benefits or organizing capacity. Religious minorities experience d ≈ 0.85-0.95 depending on whether they can form coalitions (lowering d toward 0.70) or remain atomized (raising d toward 0.95). The analytical observer at civilizational scale experiences d ≈ 0.72-0.75 (observer baseline) because the universality of the perspective distributes the constraint across all agents equally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that all six types are legitimate but apply to different aspects or time horizons of the same phenomenon. (1) The piton perspective is correct: the constraint's theater ratio is high (0.62) and increasing, indicating degradation toward performative-only status. (2) The scaffold perspective is correct: education pluralism and legal rationalization create genuine sunset dynamics. (3) The rope perspectives (state and privileged religion) are correct: the constraint does coordinate legitimacy and social binding. (4) The snare perspectives (minorities, secular citizens) are correct: the constraint does extract conformity and distribute disadvantage asymmetrically. (5) The tangled rope perspective (reform coalition) is correct: the constraint contains both elements and admits of incremental reform. (6) The mountain perspective is INCORRECT: it naturalizes what is contingent. The mandatrophy resolves by specifying which perspective applies to which temporal and spatial scope. At immediate/national scale, the constraint operates as snare and rope simultaneously (depending on position). At generational scale, it degrades toward piton. At civilizational scale with international norms, it shows scaffold dynamics. No single classification is 'true' — the constraint is a presheaf over multiple observation sites, each with legitimate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_source_substitutability,
    'Can state legitimacy be sustainably generated through non-religious sources (democratic participation, rule of law, economic performance, national identity)?',
    'Historical analysis of states that have successfully decoupled religious authority from state legitimacy; comparison of legitimacy stability metrics across secular and religious states; survey data on source of legitimacy attribution across populations',
    'If legitimacy sources are substitutable: the constraint is contingent and the scaffold/secularization pathway is real. If not substitutable: state-religious authority may be more structurally necessary than contingent, and secularization pressures will face deeper resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_source_substitutability, empirical, 'Whether non-religious sources can substitute for religious legitimacy').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression experienced as structural constraint (external legal barriers) or internalized (populations have accepted religious authority as legitimate)?',
    'Post-reform measurement: compare suppression levels before and after legal decoupling; track emigration and organizing patterns; survey attitude shifts across generations',
    'If internalized: constraint persists even after legal barriers are removed (higher effective suppression). If purely structural: removing legal barriers reduces suppression significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    minority_religious_coalition_formation,
    'Can religious minorities and secular movements form stable coalitions against state-religious authority, or do their interests diverge?',
    'Historical case study of coalition formation/breakdown; analysis of minority religious group positions on state-religious authority in contexts with religious diversity',
    'If coalitions are stable: reform pressure is higher and scaffold/organized reform perspective is strengthened. If coalitions collapse: each group faces the constraint atomized, and extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_religious_coalition_formation, empirical, 'Coalition formation dynamics among minorities and secular groups').

omega_variable(
    international_pressure_effectiveness,
    'Does international pressure (human rights frameworks, trade conditionality, soft power) actually reduce state-religious authority constraints, or does it entrench them through nationalist backlash?',
    'Time-series analysis of state-religious authority measures pre- and post-international pressure events; correlation with nationalist identity strengthening vs. institutional reform',
    'If effective: external pressure supports scaffold perspective and accelerates sunset. If counterproductive: international pressure strengthens resistance and extends the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_pressure_effectiveness, empirical, 'Effectiveness of international pressure on state-religious authority').

omega_variable(
    education_pluralism_sufficiency,
    'Does secular education credential competition actually displace religious authority in legitimacy generation, or does religious framing persist alongside secular knowledge?',
    'Longitudinal tracking of credential competition; analysis of persistence of religious authority claims in secular-educated populations; measurement of belief in religious authority across education levels',
    'If education is sufficient: secularization pathway is confirmed and theater ratio should decline over generational time. If religious framing persists: constraint may be more resilient than scaffold perspective assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(education_pluralism_sufficiency, empirical, 'Displacement of religious authority by secular education credentials').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_religious_authority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sra_tr_t0, state_religious_authority, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sra_tr_t10, state_religious_authority, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sra_tr_t20, state_religious_authority, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(sra_be_t0, state_religious_authority, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(sra_be_t10, state_religious_authority, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(sra_be_t20, state_religious_authority, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_religious_authority, identity_coordination).
narrative_ontology:affects_constraint(state_religious_authority, religious_education_monopoly).
narrative_ontology:affects_constraint(state_religious_authority, marriage_law_religious_authority).
narrative_ontology:affects_constraint(state_religious_authority, religious_oath_legal_standing).

% DUAL FORMULATION NOTE:
% State religious authority decomposes into multiple constraint stories reflecting distinct institutional sites: marriage law, education, oath-taking, legal inheritance, and legitimacy narratives. Each site has its own ε value and its own network of dependents. The state religious authority story operates at the meta-level, integrating these downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_religious_authority, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
