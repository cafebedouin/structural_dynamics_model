% ============================================================================
% CONSTRAINT STORY: nsl_hk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_hk, []).

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
 *   constraint_id: nsl_hk
 *   human_readable: Hong Kong National Security Law (2020)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The National Security Law imposed on Hong Kong by the Chinese National
 *   People's Congress Standing Committee in June 2020 represents a structural
 *   constraint that decomposes clearly into a pure extraction mechanism
 *   (Snare) with secondary theatrical components. The law criminalized
 *   secession, subversion, terrorism, and 'collusion with foreign forces'
 *   through vague, retroactively-applied language, established a National
 *   Security Committee with majority Beijing representatives who can override
 *   Hong Kong officials, and created new law enforcement units accountable to
 *   mainland authorities. From Beijing's institutional perspective, NSL
 *   solves a coordination problem: integrating Hong Kong's political system
 *   into unified CCP governance while maintaining operational control and
 *   preventing what it perceived as foreign-backed destabilization. From the
 *   perspective of Hong Kong's civil society, opposition parties, independent
 *   press, and academics, NSL is a mechanism of pure extraction: political
 *   suppression dressed as security. The theater_ratio (0.62) reflects that
 *   NSL maintains performative compliance with rule-of-law and judicial
 *   independence rhetoric while operating as a discretionary political
 *   control tool. The rapid increase in extractiveness over the first four
 *   years (from 0.45 at announcement to 0.68 in implementation) reflects the
 *   constraint's transition from stated authority to active enforcement
 *   through prosecutions, disqualifications, and self-censorship cascades.
 *
 * KEY AGENTS:
 *   - Beijing Central Government: Primary beneficiary (institutional/arbitrage) — achieves political control and integrates Hong Kong into unified governance with zero extraction cost
 *   - Hong Kong Civil Society: Primary victim (powerless/trapped) — criminalized for lawful speech and assembly; bears maximum extraction through suppression and coercion
 *   - Political Opposition: Secondary victim (moderate/constrained) — faces prosecution risk, disqualification, and emigration barriers; retains some organizational capacity but structurally constrained
 *   - Independent Press: Victim (moderate/constrained) — operates under self-censorship and prosecution threat; key outlets emigrated or shut down
 *   - Academic Freedom: Victim (powerful/constrained) — universities maintain autonomy in form but operate under NSL constraints; faculty and researchers face prosecution risk for teaching and research
 *   - International Business/Finance: Mixed (powerful/mobile) — experiences selective enforcement risk and political risk premium but retains exit option; observes extraction disguised as coordination
 *   - Hong Kong Legal Institutions: Piton (institutional/constrained) — formally independent judiciary degraded into performative autonomy; courts transmit CCP directives while maintaining rule-of-law theater
 *   - Analytical Observer: Snare classifier (analytical/analytical) — sees pure extraction mechanism masked by security framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_hk, 0.68).
domain_priors:suppression_score(nsl_hk, 0.78).
domain_priors:theater_ratio(nsl_hk, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_hk, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_hk, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_hk, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_hk, snare).
narrative_ontology:human_readable(nsl_hk, "Hong Kong National Security Law (2020)").
narrative_ontology:topic_domain(nsl_hk, "political/legal").

domain_priors:requires_active_enforcement(nsl_hk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_hk, beijing_central_government).
narrative_ontology:constraint_beneficiary(nsl_hk, hong_kong_security_apparatus).
narrative_ontology:constraint_victim(nsl_hk, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_hk, political_opposition).
narrative_ontology:constraint_victim(nsl_hk, independent_press).
narrative_ontology:constraint_victim(nsl_hk, academic_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG CIVIL SOCIETY (SNARE) — Powerless to resist, trapped within territorial jurisdiction. Faces maximum suppression through broad vague charges (secession, subversion, foreign collusion, sedition), criminalization of speech and assembly, and reversals of legal protections. No exit option except physical emigration. Extraction is coercive control over political expression.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HONG KONG POLITICAL OPPOSITION (SNARE) — Moderate organized power but structurally constrained. NSL criminalizes opposition speech, disqualifies candidates, enables prosecution of past protest activity, and chills political organizing. Exit options severely constrained: emigration incurs costs (loss of assets, family separation); staying incurs prosecution risk. Effective extraction is severe despite some residual organizational capacity.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL BUSINESS & FINANCE (TANGLED ROPE) — Powerful but mobile. NSL creates selective enforcement risk and unpredictable legal environment. Some coordination benefit (stable governance claim), but extraction occurs through compliance costs, self-censorship, political risk premium, and vulnerability to selective prosecution or asset seizure. Mobile exit to Singapore, Dubai available but costly. Mixed: coordination function (predictable rule of law) vs extraction (political control leveraged for compliance).
constraint_indexing:constraint_classification(nsl_hk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: BEIJING CENTRAL GOVERNMENT (ROPE) — Institutional beneficiary with maximum arbitrage options. NSL solves a coordination problem from Beijing's perspective: integrating Hong Kong political system into unified CCP governance while maintaining operational control. Experiences law as coordination mechanism (establishing unified legal authority). Zero extraction cost to this actor — benefits accrue entirely to them.
constraint_indexing:constraint_classification(nsl_hk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: HONG KONG LEGAL INSTITUTIONS (PITON) — Formally independent judiciary degraded into institutional performance of autonomy. Courts operate under NSL directives from National Security Committee (majority Beijing-appointed). Judicial review authority curtailed. Theater_ratio high: appearances of impartial review persist while substantive independence has atrophied. Maintained through institutional inertia (legacy common law structure) and performative legitimacy rather than functional autonomy.
constraint_indexing:constraint_classification(nsl_hk, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From global perspective on institutional autonomy and political freedom, NSL is a textbook snare: high suppression (vague, retroactive charges; weaponized prosecution; chilling effects), high extractiveness (political subordination to external authority), no coordination benefit proportional to extraction cost. Mandatrophy resolved: this is pure extraction dressed as 'national security,' not hybrid coordination-extraction.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_hk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_hk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_hk, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_hk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nsl_hk, TR),
    TR >= 0.70.

:- end_tests(nsl_hk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. NSL transfers political authority from Hong Kong government to Beijing-controlled National Security Committee, criminalizes speech and assembly with vague retroactive charges, enables prosecution for lawful opposition activity, and creates selective enforcement risk. The extraction is both material (imprisonment, asset seizure, professional destruction) and structural (political subordination). The value reflects the scope and severity of political control extraction. Theater ratio (0.62): Moderate-high. NSL maintains fiction of rule of law (laws published, procedures outlined) while operating as discretionary political control. Courts conduct trials with appearance of due process while substantive independence has degraded. This gap between performative legality and actual political direction produces theater_ratio above 0.5. Suppression (0.78): Very high. Vague charges (subversion, foreign collusion) create legal uncertainty. Retroactive application to pre-2020 protest activity. Reversal of presumption of innocence in practice. No meaningful political recourse. Self-censorship cascades. Multiple overlapping enforcement mechanisms. Only constraint preventing higher suppression: some international scrutiny, residual Hong Kong judicial independence in non-politically-sensitive cases, and formal appeal procedures (albeit ultimately controlled).
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiary (Beijing, Snare-as-Rope) and victim (Hong Kong civil society, Snare-as-pure-extraction). Beijing experiences NSL as solving a coordination problem: unified authority, predictable governance, integration of Hong Kong into mainland system. This manifests as Rope from their perspective — they pay minimal enforcement cost and receive maximum political benefit. Hong Kong civil society experiences maximum extraction: criminalization of speech, assembly, and thought; prison sentences; professional destruction; forced emigration. This manifests as Snare — high extraction, high suppression, no coordination benefit to this actor. The analytical observer resolves this gap by recognizing that Beijing's 'coordination' is a euphemism for unidirectional control extraction from a previously-autonomous polity. NSL coordinates Beijing's governance, not Hong Kong's. The International Business perspective (Tangled Rope) occupies a middle position: experiences selective enforcement risk and political uncertainty, but retains sufficient exit options and operational autonomy that the constraint functions as mixed coordination-extraction rather than pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position and exit options. Civil society victims (powerless/trapped) have d = 0.95, producing maximum f(d) ≈ 1.42, thus maximum experienced chi. Opposition actors (moderate/constrained) have d = 0.70-0.75, producing f(d) ≈ 1.05-1.15, moderate-high chi. International business (powerful/mobile) has d = 0.55-0.60, producing f(d) ≈ 0.75-0.85, moderate chi — they experience extraction but retain exit options. Beijing's perspective (institutional/arbitrage) has d ≈ 0.05, producing f(d) ≈ -0.12, negative chi — they are subsidized by the constraint, not extracted from. The Hong Kong legal institutions (institutional/constrained) have d ≈ 0.30-0.35, producing f(d) ≈ 0.30-0.40 — they are partially controlled but retain formal autonomy, thus moderate experienced extraction despite being institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: NSL cannot be plausibly classified as anything other than a Snare from the victims' structural positions or as Rope from Beijing's position. The mandatrophy (confusion between extraction and coordination) is resolved by recognizing that NSL coordinates Beijing's unified governance while extracting from Hong Kong's prior autonomy. This is not a hybrid coordination-extraction constraint — it is a pure extraction constraint that Beijing frames as coordinating 'national security.' The analytical observer's perspective confirms: from a civilizational standpoint on political autonomy and institutional independence, NSL is textbook Snare (vague charges, high suppression, no proportional coordination benefit, no exit options for powerless actors, theater ratio indicating performative legality). The constraint's framing as 'security' does not constitute genuine coordination function — it constitutes theft of the legitimacy frame. Mandatrophy fully resolved: Snare is the correct classification for all victim perspectives and the analytical perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foreign_collusion_scope,
    'How broad is ''collusion with foreign forces'' as grounds for prosecution, and does it encompass entirely lawful international engagement?',
    'Analysis of prosecutions brought under NSL Article 29; examination of divergence between stated scope and applied scope in cases',
    'If scope remains narrow to genuine espionage: extractiveness reduces to 0.55-0.60 (moderate snare). If scope expands to all international advocacy, journalism, or funding: extractiveness remains at 0.68+ (pure snare). This is the primary uncertainty determining whether ''subversion'' and ''collusion'' are genuinely defined or weaponized vaguely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_collusion_scope, empirical, 'Actual scope of foreign collusion prosecutions under Article 29').

omega_variable(
    judicial_independence_retention,
    'To what degree do Hong Kong courts retain functional independence in NSL cases despite National Security Committee oversight?',
    'Comparative analysis of acquittal rates in NSL cases vs pre-NSL political cases; court decisions contradicting prosecution positions; timing and pattern analysis of reversals on appeal',
    'If significant independence retained: classification drops to Tangled Rope (hybrid with genuine coordination component). If courts are pure transmission mechanism: Snare classification confirmed. This determines whether legal institutions constitute a check or merely theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_independence_retention, empirical, 'Degree of Hong Kong judicial independence in NSL cases').

omega_variable(
    emigration_cost_trajectory,
    'What is the trajectory of costs and barriers to emigration for NSL-endangered actors, and how does this affect exit_options classification?',
    'Tracking of emigration application processing, asset freezing incidents, family separation policies, and availability of refuge jurisdictions over time',
    'If barriers remain moderate: exit_options for opposition remain ''constrained'' rather than ''trapped.'' If barriers increase to near-total (passport revocation, asset seizure, family penalties): reclassify to ''trapped,'' increasing d and thus chi. This affects the gap between ''moderate'' opposition perspective (currently constrained) and ''powerless'' civil society (currently trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emigration_cost_trajectory, empirical, 'Trajectory of emigration barriers for NSL-affected actors').

omega_variable(
    mainland_expansion_precedent,
    'Is NSL in Hong Kong a prototype for similar laws in Taiwan, Macau, or other jurisdictions, and if so, what does this indicate about its extraction function?',
    'Analysis of legislative proposals in other jurisdictions; statements by CCP officials about replicating the model; effectiveness of Hong Kong NSL as a template',
    'If NSL serves as a documented blueprint for political control replication: confirms extraction function is structural (subordinating autonomous polities), not situational. Mandatrophy resolution strengthened — this is not context-dependent security but systematic institutional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainland_expansion_precedent, conceptual, 'Whether NSL serves as replicable template for political control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_hk, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_tr_t0, nsl_hk, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nsl_tr_t2, nsl_hk, theater_ratio, 2, 0.55).
narrative_ontology:measurement(nsl_tr_t4, nsl_hk, theater_ratio, 4, 0.62).

% Extraction over time
narrative_ontology:measurement(nsl_be_t0, nsl_hk, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nsl_be_t2, nsl_hk, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(nsl_be_t4, nsl_hk, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_hk, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_hk, one_country_two_systems_framework).
narrative_ontology:affects_constraint(nsl_hk, hong_kong_press_freedom).
narrative_ontology:affects_constraint(nsl_hk, hong_kong_academic_freedom).
narrative_ontology:affects_constraint(nsl_hk, political_autonomy_mainland_territories).

% DUAL FORMULATION NOTE:
% NSL operates at multiple structural levels: as a legal mechanism imposing security constraints (this story), as a political control apparatus (affects one_country_two_systems_framework), and as suppression mechanism for specific freedoms (affects press_freedom, academic_freedom). Each downstream constraint has its own ε reflecting specific domain impacts. Network linkage preserves causal and institutional dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_hk, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
