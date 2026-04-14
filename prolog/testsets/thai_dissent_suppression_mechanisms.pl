% ============================================================================
% CONSTRAINT STORY: thai_dissent_suppression_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_dissent_suppression_mechanisms, []).

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
 *   constraint_id: thai_dissent_suppression_mechanisms
 *   human_readable: Thai Dissent Suppression Mechanisms
 *   domain: political/state_control
 *
 * SUMMARY:
 *   Thailand's dissent suppression mechanisms represent a structurally pure
 *   extraction apparatus maintained through legal formalism, military power,
 *   and palace protection doctrine. The constraint combines explicit legal
 *   instruments (lèse-majesté law Article 112, Emergency Decree powers,
 *   Cybercrime Act provisions) with institutional coercion (military
 *   autonomy, coup authority, arbitrary detention) and internalized loyalty
 *   norms (Buddhist reverence framing, national-security legitimation).
 *   Multiple coup d'états (1997, 2006, 2014) have dissolved elected
 *   governments and constitutions, establishing that democratic processes are
 *   subordinate to military/palace interests. The mechanism's theater ratio
 *   has increased as successive governments have enacted increasingly
 *   sophisticated legal frameworks to justify suppression while maintaining
 *   constitutional appearance. Extractiveness has accumulated as broader
 *   categories of speech and assembly have been criminalized. The suppression
 *   apparatus benefits military institutional power and palace protection
 *   apparatus while extracting from dissenting citizens, political
 *   opposition, civil society, and press freedom.
 *
 * KEY AGENTS:
 *   - Dissenting Citizens: Primary victims (powerless/trapped) — face imprisonment, criminal prosecution, social ostracism, asset seizure for protected speech under lèse-majesté law
 *   - Political Opposition: Secondary victims (moderate/constrained) — face arrest threats, party dissolution, electoral exclusion, coup-driven government dissolution
 *   - Military Leadership: Primary beneficiary (institutional/arbitrage) — maintains institutional autonomy, coup authority, budgetary independence, political veto power
 *   - Palace Protection Apparatus: Co-beneficiary (institutional/arbitrage) — enforces lèse-majesté law, maintains monarch's legal inviolability, controls succession security
 *   - Judiciary and Law Enforcement: Mixed-position institutional actors (institutional/constrained) — coordinate suppression apparatus, depend on it for authority, captured by it
 *   - Civil Society Organizations: Collective victims (powerful/constrained) — operate under restrictions on assembly, association, and speech; risk organizational dissolution
 *   - Press Freedom: Systemic victim (moderate/trapped) — self-censorship enforced through legal liability, arrest threats, newspaper closures, online speech prosecution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_dissent_suppression_mechanisms, 0.68).
domain_priors:suppression_score(thai_dissent_suppression_mechanisms, 0.78).
domain_priors:theater_ratio(thai_dissent_suppression_mechanisms, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_dissent_suppression_mechanisms, extractiveness, 0.68).
narrative_ontology:constraint_metric(thai_dissent_suppression_mechanisms, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(thai_dissent_suppression_mechanisms, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_dissent_suppression_mechanisms, snare).
narrative_ontology:human_readable(thai_dissent_suppression_mechanisms, "Thai Dissent Suppression Mechanisms").
narrative_ontology:topic_domain(thai_dissent_suppression_mechanisms, "political/state_control").

domain_priors:requires_active_enforcement(thai_dissent_suppression_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(thai_dissent_suppression_mechanisms, military_institutional_power).
narrative_ontology:constraint_beneficiary(thai_dissent_suppression_mechanisms, palace_protection_apparatus).
narrative_ontology:constraint_victim(thai_dissent_suppression_mechanisms, dissenting_citizens).
narrative_ontology:constraint_victim(thai_dissent_suppression_mechanisms, political_opposition).
narrative_ontology:constraint_victim(thai_dissent_suppression_mechanisms, civil_society_organizations).
narrative_ontology:constraint_victim(thai_dissent_suppression_mechanisms, press_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING CITIZEN (SNARE) — Faces criminal prosecution, imprisonment, and social ostracism for protected speech. The lèse-majesté law (Article 112 of Criminal Code) and cybercrime provisions create severe punishment for criticism of monarchy/military. Exit options do not exist within Thailand; emigration is costly and incomplete escape from threat (family remains, assets at risk). Maximum experienced extraction.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL OPPOSITION (SNARE) — Constrained by arrest threats, party dissolution powers (Emergency Decree), arbitrary detention, and exclusion from electoral systems. Multiple coup d'états have dissolved elected governments; political leaders face imprisonment. Exit is theoretically possible (exile) but economically and socially costly. High extraction with some agency.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL OBSERVER (SNARE) — Can exit the constraint through international relocation but faces significant career, cultural, and family costs. From outside, the mechanism is visible as pure coercion with minimal coordination function. Observer has geographic mobility that Thai citizens lack.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY LEADERSHIP (ROPE) — Primary beneficiary. Experiences the suppression apparatus as a coordination mechanism for maintaining institutional cohesion and preventing political threats to organizational power. Can arbitrage exit — military leadership can dissolve or reform the constraint through policy change. Net beneficiary — extraction flows toward this institution.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIARY AND LAW ENFORCEMENT (TANGLED ROPE) — Constrained institutional actors who benefit from the constraint structure (resources, institutional autonomy, authority) while also bearing costs (reputational damage, international isolation, institutional capture by military/palace). They coordinate the suppression apparatus but are also extracted from through it — lack independent exit power.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THAI CIVIL SOCIETY (SNARE) — Constrained at civilizational scale. Despite some powerful actors (NGOs, educational institutions, religious organizations), the cumulative suppression of assembly rights, association rights, and free expression creates structural extraction. Exit requires leaving the nation or accepting subordination to state authority. Mixed power but high suppression creates snare classification.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: FORMAL DEMOCRATIC INSTITUTIONS (PITON) — Constitutionally mandated democratic processes (elections, parliament, judicial review) persist but are substantially performative. Multiple constitutions and elected governments have been dissolved by coup d'état. The institutions exist as theater maintaining the fiction of democratic order while real power concentrates in military/palace. Theater ratio elevated by the gap between constitutional provisions and actual political power distribution.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a global, civilizational view, the Thai suppression apparatus is a structurally pure extraction mechanism. The constraint has minimal coordination function (unlike a legitimate security apparatus that coordinates genuine threats and collective safety). Its primary function is maintaining power concentration. No inherent naturalization: the constraint is explicitly political, not claimed as natural law.
constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_dissent_suppression_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_dissent_suppression_mechanisms, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(thai_dissent_suppression_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(thai_dissent_suppression_mechanisms, TR),
    TR >= 0.70.

:- end_tests(thai_dissent_suppression_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint systematically extracts political autonomy and speech rights from Thai citizens and opposition groups to concentrate power in military/palace institutions. Base extraction has increased from 0.42 (2004-2006 period) to 0.68 (post-2014 coup) as legal frameworks have broadened and enforcement has intensified. The measurement trajectory shows accumulating extraction through successive coup cycles — each transition creates new legal justifications for suppression. Suppression (0.78): Very high. Multiple barriers prevent exit: geographical (Thailand is geographically bounded; emigration is costly), legal (criminal penalties for speech and association), social (family and community ties), and economic (asset seizure, employment discrimination). Suppression combines structural barriers (law enforcement apparatus, court system) and internalized loyalty mechanisms (nationalist identity, Buddhist reverence framing). Theater ratio (0.61): Moderate-high and increasing. The constraint maintains constitutional form (parliamentary system, judicial review, electoral processes) while these institutions remain subordinate to coup-capable military. The gap between constitutional promise and actual power distribution creates significant performative content. Increasing theater reflects successive constitutions that attempt to legitimize what is structurally contingent on military acquiescence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how pure extraction structures appear as rope to beneficiaries but snare to victims. Military leadership perceive the mechanism as solving a genuine coordination problem (preventing political instability, maintaining institutional cohesion). Dissenting citizens perceive it as pure coercion with no coordination benefit. The perspectival gap reveals that coordination-language benefits the beneficiary even when the constraint's primary function is extraction. The piton perspective is diagnostically important: formal democratic institutions persist (constitutions, elections, courts) despite coup authority, suggesting that democratic form serves as theater legitimizing the suppression apparatus. This is not unique to Thailand — the mechanism appears in many post-coup regimes that maintain constitutional facade while military retains veto power.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality flows from beneficiary/victim status and exit options. Dissenting citizens are victims with no exit (trapped) — d approaches 1.0 (full target). Military leadership are beneficiaries with arbitrage exit (can dissolve constraint through policy change) — d approaches 0.0 (full beneficiary). Opposition groups are victims with constrained exit (can partially escape through exile, but at high cost) — d intermediate (0.75-0.85). International observers are outside the primary extraction flow but witness the mechanism (analytical perspective at d~0.72). The judiciary are constrained institutional actors simultaneously benefiting from (institutional authority, resources) and subordinated by (legal capture, reputational damage) the apparatus — intermediate d value (0.45-0.55). The consistent pattern: those with exit power or arbitrage options show low d (beneficiaries); those with constrained or trapped exit show high d (victims). No perspective shows near-zero d (full beneficiary protection), indicating the constraint has minimal net coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint's classification as snare is robust across victim perspectives and unambiguous in the analytical view. No mandatrophy exists because the claimed type matches all non-beneficiary perspectives. The constraint does not risk mislabeling coordination as extraction (or vice versa) — the extraction is explicit and acknowledged. The theater ratio (0.61) reflects the gap between constitutional democracy and actual military power concentration, but this does not change the snare classification because the victims correctly perceive snare even while formal institutions maintain democratic theater. The mandatrophy would arise only if beneficiaries falsely claimed rope classification while victims perceived snare — here, beneficiaries' rope claim is accurate to their perspective, and victims' snare claim is accurate to theirs. The perspectival gap is properly classified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    palace_versus_military_domination,
    'Is primary beneficiary the Thai monarchy (palace protection apparatus) or the military institution itself?',
    'Historical analysis of coup justifications, constitutional provisions, military autonomy in policy, and succession dynamics. Track which institution makes suppression policy vs which institution benefits most.',
    'If palace-dominant: constraint is monarchical protection mechanism (extractiveness may be slightly lower due to broader institutional buy-in for palace legitimacy). If military-dominant: constraint is institutional coup-protection mechanism (extractiveness consistent with current analysis). Classification remains snare either way, but beneficiary structure differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palace_versus_military_domination, empirical, 'Whether palace or military is primary beneficiary of suppression').

omega_variable(
    identity_lock_versus_coercion,
    'To what degree is dissent suppression internalized through nationalist/loyalist identity vs externally coerced through legal threats?',
    'Survey data on self-censorship motivations; analysis of spontaneous vs coerced compliance; comparison of suppression effectiveness in pre-coup vs post-coup periods; social media sentiment analysis.',
    'If primarily identity-locked: some perspectives might shift to identity_locked exit option, changing biographical/generational classifications from snare toward rope. If primarily coercive: snare classification is robust across time horizons.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_versus_coercion, empirical, 'Proportion of suppression that is internalized identity vs external coercion').

omega_variable(
    emergency_decree_termination_credibility,
    'Are emergency decree provisions (in place since 2014 coup) genuinely temporary scaffolding or permanent suppression mechanism misclassified as provisional?',
    'Examine pattern of emergency decree renewals, stated sunset dates vs actual termination history, constitutional amendments that entrench emergency powers, and military statements about permanence vs temporality.',
    'If genuinely temporary: significant portions of suppression apparatus could be reclassified as scaffold (χ ≤ 0.30, sunset clause). If permanent: scaffolding narrative is theater covering snare structure. Current analysis assumes permanent; evidence of credible sunset would require decomposition into multiple stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_decree_termination_credibility, empirical, 'Whether emergency decree suppression measures are genuinely temporary or permanently entrenched').

omega_variable(
    lese_majeste_enforcement_consistency,
    'Is lèse-majesté law (Article 112) applied consistently as legal norm or selectively as political weapon?',
    'Prosecution statistics: distribution of cases across time, political affiliation of defendants, severity of alleged offense vs sentence received, cases dismissed vs convicted. Comparison of enforcement rigor before vs after political events.',
    'If consistently applied: law operates as constitutional constraint (suppression rate ~0.60). If selectively applied: mechanism is purely extractive (suppression rate ~0.85). Current analysis assumes selective application; consistent application would lower both extractiveness and suppression values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lese_majeste_enforcement_consistency, empirical, 'Whether lèse-majesté prosecution is consistent legal norm or selective political weaponization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_dissent_suppression_mechanisms, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_tr_t0, thai_dissent_suppression_mechanisms, theater_ratio, 0, 0.35).
narrative_ontology:measurement(thai_tr_t5, thai_dissent_suppression_mechanisms, theater_ratio, 5, 0.48).
narrative_ontology:measurement(thai_tr_t10, thai_dissent_suppression_mechanisms, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(thai_be_t0, thai_dissent_suppression_mechanisms, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(thai_be_t5, thai_dissent_suppression_mechanisms, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(thai_be_t10, thai_dissent_suppression_mechanisms, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_dissent_suppression_mechanisms, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_dissent_suppression_mechanisms, thai_electoral_system_subordination).
narrative_ontology:affects_constraint(thai_dissent_suppression_mechanisms, thai_royal_insult_legal_instrument).
narrative_ontology:affects_constraint(thai_dissent_suppression_mechanisms, thai_military_institutional_autonomy).

% DUAL FORMULATION NOTE:
% Thai dissent suppression decomposes into three structurally distinct constraints: (1) lèse-majesté law enforcement (ε~0.72, snare), (2) military coup authority and electoral system subordination (ε~0.65, snare), (3) emergency decree and arrest powers (ε~0.60, snare → scaffold if genuinely temporary). Current story integrates all three; decomposition into separate constraint family is recommended if omega_emergency_decree resolution yields temporality evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(thai_dissent_suppression_mechanisms, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
