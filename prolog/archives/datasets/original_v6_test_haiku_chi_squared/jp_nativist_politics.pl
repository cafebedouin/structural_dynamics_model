% ============================================================================
% CONSTRAINT STORY: jp_nativist_politics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jp_nativist_politics, []).

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
 *   constraint_id: jp_nativist_politics
 *   human_readable: Nativist Online Rhetoric as a Pathway to Political Power in Japan
 *   domain: political/social
 *
 * SUMMARY:
 *   The nativist online rhetoric strategy exemplified by Mio Sugita and
 *   allied figures represents a systematic extraction mechanism that
 *   weaponizes democratic freedoms (speech, assembly, electoral
 *   participation) to suppress minority voices and consolidate political
 *   power. The constraint operates through coordinated harassment campaigns,
 *   inflammatory rhetoric about minorities (particularly Zainichi Koreans,
 *   Chinese nationals, and LGBTQ+ individuals), and strategic positioning
 *   within the mainstream Liberal Democratic Party to gain legitimacy and
 *   influence. The mechanism extracts political power by converting
 *   identity-based grievances into mobilizable base support while suppressing
 *   the ability of minorities to contest narratives, organize responses, or
 *   enjoy equal participation in democratic discourse. The constraint
 *   exhibits hall-of-mirrors ambiguity: the beneficiary (nativist politician)
 *   frames the activity as coordination (mobilizing base, expressing cultural
 *   pride); the victims experience pure extraction (harassment, exclusion,
 *   forced silence); institutional actors negotiate between base mobilization
 *   and reputational costs; and constitutional guardrails persist as theater
 *   while enforcement atrophies. Theater has increased substantially over the
 *   interval (0.38 to 0.62) as constitutional protections are affirmed while
 *   nativist rhetoric operates within plausible deniability. The
 *   extractiveness has also risen (0.42 to 0.58) as the political strategy
 *   matured from fringe social media to mainstream party alignment.
 *
 * KEY AGENTS:
 *   - Nativist Politicians (institutional/arbitrage): Primary beneficiary — capture political power, media attention, and base support through inflammatory rhetoric; gain mainstream party legitimacy
 *   - Marginalized Minorities (powerless/trapped): Primary victim — ethnic Koreans, Chinese nationals, LGBTQ+ individuals face coordinated harassment with no exit option or institutional defense
 *   - Democratic Discourse Commons (moderate/constrained): Secondary victim — reasoned political debate is degraded by systematic inflammatory rhetoric; journalists and civil society actors face reputational damage and doxing for opposition
 *   - LDP Institutional Leadership (organized/constrained): Mixed position — benefits from base mobilization but bears costs from diplomatic friction, reputational damage, and erosion of coalition relationships
 *   - Constitutional Democratic Guardrails (institutional/constrained): Performative — Article 14 equality and international human rights commitments persist as ritual affirmed by leadership while enforcement atrophies
 *   - Analytical Observer (analytical/analytical): Witnesses the extractive structure common to all nativist populist movements — target suppression and threat narrative substituting for policy substance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jp_nativist_politics, 0.58).
domain_priors:suppression_score(jp_nativist_politics, 0.68).
domain_priors:theater_ratio(jp_nativist_politics, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jp_nativist_politics, extractiveness, 0.58).
narrative_ontology:constraint_metric(jp_nativist_politics, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jp_nativist_politics, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jp_nativist_politics, snare).
narrative_ontology:human_readable(jp_nativist_politics, "Nativist Online Rhetoric as a Pathway to Political Power in Japan").
narrative_ontology:topic_domain(jp_nativist_politics, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jp_nativist_politics, nativist_politicians).
narrative_ontology:constraint_victim(jp_nativist_politics, marginalized_minorities).
narrative_ontology:constraint_victim(jp_nativist_politics, political_discourse_quality).
narrative_ontology:constraint_victim(jp_nativist_politics, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED MINORITY (SNARE) — Ethnic Koreans, Chinese, Ainu, LGBTQ+ individuals, and other minorities face sustained coordinated harassment campaigns with no viable exit or defense mechanism within the system. Suppress exit options: cannot leave Japan without abandoning social networks, employment, citizenship. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Snare classification is structurally stable: high extraction + high suppression + victim trapped.
constraint_indexing:constraint_classification(jp_nativist_politics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEMOCRATIC DISCOURSE COMMONS (SNARE) — The epistemic commons of reasoned political debate is degraded by systematic inflammatory rhetoric. Constrained exit: journalists, academics, and civil society actors who oppose the rhetoric face reputational damage, doxing, and career consequences, creating self-censorship. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.78. The commons is victimized because it cannot defend itself against coordinated disruption.
constraint_indexing:constraint_classification(jp_nativist_politics, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NATIVIST POLITICAL MOVEMENT (ROPE) — The politician and her coalition experience the rhetoric as a coordination mechanism: mobilizing base support, establishing identity-based political organization, and signaling party membership. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Negative effective extraction = net beneficiary. The constraint solves a genuine coordination problem for this coalition (aggregate supporters who share nativist orientation). The ambiguity here is critical: the rope classification from the beneficiary's perspective disguises the snare mechanism from the victim's perspective.
constraint_indexing:constraint_classification(jp_nativist_politics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LDP INSTITUTIONAL LEADERSHIP (TANGLED ROPE) — The mainstream LDP leadership experiences the nativist rhetoric as both coordination (base mobilization) and extraction (erosion of moderate LDP's international reputation, diplomatic relationships, internal party discipline). The party benefits from the base energy but bears costs from alienation of coalition partners, diplomatic friction, and reputational damage to centrist candidates. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.35. Tangled Rope gate: beneficiaries (nativist base) + victims (party institutions bearing diplomatic costs) + active enforcement (party leadership tolerating and amplifying the rhetoric) = mixed type.
constraint_indexing:constraint_classification(jp_nativist_politics, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL DEMOCRATIC GUARDRAILS (PITON) — Japan's constitutional commitment to minority protection, Article 14 equality, and international human rights treaties persists as an institutional performance with degraded enforcement. Theater_ratio=0.62: constitutional protections are ritually affirmed by LDP leaders while nativist politicians operate within plausible deniability ('free speech', 'cultural pride'). The guardrails remain due to institutional inertia and international pressure, not because they function to prevent harassment. Enforcement capacity has atrophied.
constraint_indexing:constraint_classification(jp_nativist_politics, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the civilizational/global perspective, this constraint exhibits the signature extractive structure of nativist populism: it systematically extracts political power and media attention by suppressing alternative voices and minority exit options, maintains hegemony through coordinated harassment, and stages its victims' powerlessness as natural majorities vs artificial minorities. The structure is not contingent on the Japanese context — it is a repeatable template. Snare from the analytical perspective confirms the victim perspectives' structural reality.
constraint_indexing:constraint_classification(jp_nativist_politics, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jp_nativist_politics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jp_nativist_politics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jp_nativist_politics, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jp_nativist_politics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jp_nativist_politics, TR),
    TR >= 0.70.

:- end_tests(jp_nativist_politics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant political power, media attention, electoral support, and influence over party direction. The extraction is substantial but not maximal (0.70+) because: (1) the mechanism requires sustained narrative work — it is not automatic rent extraction like price controls, (2) exit options exist in principle (political realignment, generational change), though they are costly in practice, and (3) some beneficiaries (mainstream LDP) experience offsetting costs. The trajectory upward from 0.42 to 0.58 reflects the strategy's maturation and mainstreaming. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) coordinated harassment campaigns create chilling effects on minority speech, (2) doxing and reputation attacks on opponents, (3) institutional inertia in human rights enforcement, (4) social pressure against dissent in consensus-driven Japanese culture, (5) legal ambiguity enabling plausible deniability. However, suppression is not maximal (0.90+) because some institutional resistance persists (constitutional provisions, international pressure, civil society advocacy), and some exit options remain costly but available. Theater ratio (0.62): Moderate-high. Constitutional protections are ritually affirmed (theater) while enforcement atrophies. The politician's invocation of cultural preservation and free speech establishes legitimacy theater. But the mechanism contains real functional extraction (genuine political power accrual, genuine base mobilization, genuine minority suppression) — this is not a purely theatrical constraint. The rising trajectory (0.38 to 0.62) reflects increasing reliance on plausible deniability as the strategy's mainstream status increases the cost of explicit minoritarian harm.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces sharp perspectival disagreement on whether the mechanism is coordination or extraction. The nativist politician views it as Rope: mobilizing supporters around shared identity and policy vision. The victims view it as Snare: systematic harassment with no defense or exit. The institutional party leadership experiences Tangled Rope: real base benefits + real reputational costs. The constitutional order perceives Piton: ritual equality protections with degraded enforcement. The analytical observer confirms Snare: the structure is a repeatable template for extractive nativist politics. The gap reveals that 'democracy', 'free speech', and 'self-determination' are being claimed by the beneficiary as coordination goods while being weaponized as extraction mechanisms against powerless victims. The beneficiary's rope classification is not false — base mobilization IS coordination — but it masks the asymmetric extraction from those who cannot exit the polity.
 *
 * DIRECTIONALITY LOGIC:
 *   Nativist politicians: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; has exit option (could stop rhetoric) but chooses not to because benefits exceed costs. Marginalized minorities: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; cannot exit polity without abandoning lives, families, employment. Democratic discourse commons: Victim + constrained → d≈0.88, f(d)≈1.35. Cannot exit without abandoning democratic participation; faces harassment for defense. LDP institutional leadership: Both beneficiary (base support) and victim (diplomatic costs, internal faction pressure) + constrained → d≈0.48, f(d)≈0.60. Mixed directionality reflecting tangled rope position. Constitutional guardrails: Nominal beneficiary of rule of law but actual victim of enforcement failure + constrained → d≈0.70, f(d)≈1.10. High d because the constraint operates by exploiting guardrails' weakness, not respecting them.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deniability_boundary,
    'Where is the boundary between protected political speech and coordinated harassment that enables suppression?',
    'Comparative analysis of platform moderation decisions, legal cases, and police complaints; investigation of coordination infrastructure (private chat groups, messaging apps, organized campaigns)',
    'If deniability is maintained: constraint continues as snare with institutional cover (piton theater). If deniability collapses: politician and coalition face accountability, reducing extraction potential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deniability_boundary, conceptual, 'Boundary between protected speech and coordinated harassment suppression').

omega_variable(
    coalition_stability,
    'Is the nativist political movement stable as a long-term coalition or dependent on the continued salience of minorities as threat?',
    'Longitudinal tracking of nativist politician support; analysis of electoral returns when minority-threat narratives are high vs low; identification of coalition member commitment to other shared policy goals',
    'If stable: snare persists indefinitely. If dependent on threat narrative: constraint may degrade if minority issues fade in salience or if alternative organizing frameworks emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_stability, empirical, 'Stability of nativist coalition beyond threat narratives').

omega_variable(
    transnational_intervention_capacity,
    'Can transnational human rights institutions (UN mechanisms, ICC, diplomatic pressure) meaningfully constrain nativist suppression, or does territorial sovereignty protect the mechanism?',
    'Analysis of UN reports, diplomatic démarches, and their domestic political consequences; tracking of international reputation effects on trade and alliance relationships; examination of domestic implementation of international recommendations',
    'If constrained by transnational pressure: extraction costs rise, potentially degrading the snare. If protected by sovereignty: snare structure insulated from external intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transnational_intervention_capacity, empirical, 'Effectiveness of transnational human rights intervention').

omega_variable(
    generational_shift,
    'As digital natives become the primary audience and younger demographics show lower nativist orientation, does the constraint''s extraction potential decline?',
    'Demographic analysis of nativist support; generational cohort tracking of attitudes toward minorities; analysis of younger politicians'' alignment with nativist rhetoric',
    'If generational decline: snare weakens as base shrinks. If nativist framing adapts to younger cohorts: extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_shift, empirical, 'Generational sustainability of nativist political model').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jp_nativist_politics, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jpnat_tr_t0, jp_nativist_politics, theater_ratio, 0, 0.38).
narrative_ontology:measurement(jpnat_tr_t5, jp_nativist_politics, theater_ratio, 5, 0.5).
narrative_ontology:measurement(jpnat_tr_t10, jp_nativist_politics, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(jpnat_be_t0, jp_nativist_politics, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(jpnat_be_t5, jp_nativist_politics, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(jpnat_be_t10, jp_nativist_politics, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jp_nativist_politics, enforcement_mechanism).
narrative_ontology:affects_constraint(jp_nativist_politics, democratic_accountability_erosion).
narrative_ontology:affects_constraint(jp_nativist_politics, minority_representation_exclusion).
narrative_ontology:affects_constraint(jp_nativist_politics, platform_moderation_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is downstream of broader populist political strategy structures but represents a distinct constraint focused on the rhetoric-to-power pathway in the Japanese context. The upstream constraints involve institutional vulnerability to populism generally; this story models the specific nativist instantiation with ε=0.58.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jp_nativist_politics, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
