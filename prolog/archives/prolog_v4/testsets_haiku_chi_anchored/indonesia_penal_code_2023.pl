% ============================================================================
% CONSTRAINT STORY: indonesia_penal_code_2023
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indonesia_penal_code_2023, []).

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
 *   constraint_id: indonesia_penal_code_2023
 *   human_readable: Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties
 *   domain: political/social/legal
 *
 * SUMMARY:
 *   Indonesia's 2023 Penal Code (KUHP) represents a paradigmatic snare: a
 *   legal mechanism that criminalizes private consensual behavior (unmarried
 *   cohabitation, extramarital sex, homosexual conduct) and restricts
 *   political speech (insulting the president or state institutions) while
 *   appearing to be a neutral moral or constitutional framework. The
 *   constraint operates through three interlocking mechanisms: (1)
 *   criminalization of intimacy—marriage becomes the only legal gateway to
 *   sexual legitimacy, binding women particularly to spousal economic and
 *   social control; (2) criminalization of dissent—vague articles on
 *   'insulting' state entities provide law enforcement with pretexts for
 *   political arrest; (3) delegation to religious and security
 *   establishments—the constraint's enforcement capacity derives from
 *   mobilizing Islamic organizations and police as state-backed enforcers,
 *   creating a hybrid structure where formal law authority is exercised by
 *   religious groups, and religious legitimacy is backed by state coercion.
 *   The theater ratio (0.65) reflects that enforcement is episodic and
 *   politically motivated: high-profile arrests of LGBTQ activists or
 *   cohabiting couples generate media coverage and deter behavior through
 *   fear, but prosecution rates remain low and sentences inconsistent—the
 *   law's primary function is deterrence through threat rather than
 *   systematic punishment. The extractiveness (0.62) captures that the
 *   constraint does not simply prohibit behavior; it extracts authority from
 *   civil society, enabling state and religious actors to regulate intimate
 *   life as a domain of power.
 *
 * KEY AGENTS:
 *   - Unmarried cohabitants and LGBTQ persons: Primary victims (powerless/trapped) — face criminal liability for private consensual conduct; cannot exit without emigration
 *   - Civil society organizations and human rights advocates: Secondary victims (moderate/constrained) — constrained by NGO deregistration risk, funding dependencies; cannot fully exit domestic jurisdiction
 *   - Islamic mass organizations (NU, Muhammadiyah, Islamic Defenders Front): Primary beneficiaries/enforcers (powerful/mobile) — benefit from state codification of religious morality; gain influence over law enforcement
 *   - State security apparatus (police, military, prosecution): Secondary beneficiaries (institutional/arbitrage) — gain expanded enforcement authority and political discretion under vague statutes
 *   - Political elite and legislative parties: Tertiary beneficiaries (institutional/arbitrage) — mobilize KUHP as institutional commitment; benefit from coalition cohesion with religious constituencies
 *   - Analytical observer: Civilizational scope (analytical/analytical) — risks naturalizing contingent state moral regulation as immutable sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indonesia_penal_code_2023, 0.62).
domain_priors:suppression_score(indonesia_penal_code_2023, 0.78).
domain_priors:theater_ratio(indonesia_penal_code_2023, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indonesia_penal_code_2023, extractiveness, 0.62).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(indonesia_penal_code_2023, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indonesia_penal_code_2023, snare).
narrative_ontology:human_readable(indonesia_penal_code_2023, "Indonesia's 2023 Penal Code (KUHP) Criminalizing Personal Liberties").
narrative_ontology:topic_domain(indonesia_penal_code_2023, "political/social/legal").

domain_priors:requires_active_enforcement(indonesia_penal_code_2023).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, state_security_apparatus).
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, religious_establishment).
narrative_ontology:constraint_beneficiary(indonesia_penal_code_2023, political_elite).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, unmarried_cohabitants).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, lgbtq_persons).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, political_dissidents).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, civil_society_organizations).
narrative_ontology:constraint_victim(indonesia_penal_code_2023, women_reproductive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMARRIED COHABITANTS & LGBTQ PERSONS (SNARE) — Cannot exit the criminal jurisdiction of Indonesia without emigration (high cost). Face arrest, fines, imprisonment for private consensual behavior. No advocacy mechanism within the constraint structure. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.85. Pure extraction mechanism relying on complete suppression of alternatives.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL SOCIETY ORGANIZATIONS & HUMAN RIGHTS ADVOCATES (SNARE) — Constrained by funding dependencies, professional networks, and risk of NGO deregistration under vague 'state endangerment' articles. Cannot fully exit Indonesian legal jurisdiction while maintaining domestic operations. d≈0.85, f(d)≈1.22, σ=1.0 → χ≈0.75. High extraction with real but limited coercive options.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS ESTABLISHMENT & ISLAMIC MASS ORGANIZATIONS (TANGLED ROPE) — Simultaneously benefit from state enforcement of religious moral codes (coordination function: state-backed religious morality) AND extract authority over civil law domain. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.31. Hybrid: gains both coordination (religious values codified in law) and extraction (influence over state coercive power).
constraint_indexing:constraint_classification(indonesia_penal_code_2023, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICE, MILITARY, STATE SECURITY (ROPE) — Benefits from expanded enforcement authority and resources. The constraint solves a coordination problem: state actors need clear rules for what counts as 'threatening state stability.' Vague articles (insulting president, state institutions) provide legitimate justification for political control. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary; experiences constraint as enabling coordination.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE & POLITICAL PARTIES (PITON) — Enacted KUHP through formal constitutional procedures, but enforcement mechanisms have become increasingly performative: arrests for cohabitation are sporadic and publicity-driven rather than systematic; prosecution rates are selective and politically motivated. The law persists through institutional inertia and periodic high-profile enforcement theater (arrests announced, media coverage, then charges dropped or minimal sentences). theater_ratio≈0.65. Parties maintain the law as an institutional commitment without full enforcement.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT DETECTION) — Risk of naturalizing KUHP as inevitable response to 'moral coherence' or 'state sovereignty.' From civilizational scope, the constraint might appear immutable (all nation-states need laws; all societies regulate sexuality). However, structural data (ε=0.62, suppression=0.78, theater=0.65) reveals this is contingent institutional choice, not natural law. The false summit occurs when observers frame coercive personhood regulation as inherent to sovereignty rather than a specific state extraction mechanism.
constraint_indexing:constraint_classification(indonesia_penal_code_2023, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indonesia_penal_code_2023_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indonesia_penal_code_2023, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indonesia_penal_code_2023, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indonesia_penal_code_2023, TR),
    TR >= 0.70.

:- end_tests(indonesia_penal_code_2023_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): The code extracts authority from individuals over their own intimate autonomy (criminalized marriage gating) and from civil society over the domain of permissible political speech. The extraction is not total—many cohabitants avoid arrest, and prosecution remains selective—but the legal mechanism creates a pervasive chilling effect on behavior and speech. The rising trajectory (0.45→0.62 over 2 years) reflects that enforcement intensity has increased as the coalition consolidated: initial rollout was cautious; by year 2, high-profile enforcement actions (arrests of LGBTQ activists in 2023-2024) signal that the extraction mechanism is operationalizing. Suppression (0.78): Extremely high. The code criminalizes private conduct, leaving exit options minimal: marriage (for heterosexual actors seeking intimacy) or emigration (for all victims). No legal pathway exists to challenge the constraint from within—constitutional court challenges have failed; amendment requires supermajority legislative action controlled by pro-code parties. State monopoly on enforcement (police, prosecution) forecloses non-state alternatives. Theater ratio (0.65): Moderate-high. The law is partially performative—arrests of cohabitants are sporadic and often followed by minimal sentences; prosecution rates are far below potential violations; media coverage suggests political motivation. However, the constraint also achieves real behavioral change: couples do avoid cohabitation; activists do self-censor. Theater increases over time (0.52→0.65) as enforcement becomes more selective and publicity-driven rather than systematic.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. For unmarried cohabitants and LGBTQ persons (powerless/trapped), it is a snare of maximum severity (d≈0.92, χ≈0.85)—they bear full cost with no escape except leaving the country. For civil society organizations (moderate/constrained), it is snare at slightly lower intensity (d≈0.85, χ≈0.75)—they can partially operate but face funding cuts and deregistration threat. For the religious establishment (powerful/mobile), it is tangled rope (d≈0.45, χ≈0.31)—they gain both the coordination benefit of state-backed religious morality AND extraction authority, but with genuine agency (can withdraw support, could negotiate amendments). For the state security apparatus (institutional/arbitrage), it is rope (d≈0.08, χ≈-0.05)—they experience the constraint as enabling coordination (clear rules for political control) without bearing costs. For legislative parties (institutional/arbitrage), it is piton (d≈0.08)—they maintain the law through institutional commitment, but enforcement is increasingly performative theater rather than systematic. The analytical observer risks seeing a mountain (natural law of state sovereignty) but the structural data reveals this as a false summit: KUHP is a contingent institutional choice, not an inevitable feature of sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   Unmarried cohabitants & LGBTQ persons: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum d because they cannot exit the jurisdiction without extreme cost; cannot organize collectively within the constraint (criminalization prevents assembly); no legal remedy pathway. f(d) at maximum reflects that powerless agents in trapped positions suffer highest effective extraction. Civil society organizations: Victims + constrained → d≈0.85, f(d)≈1.22. High d but not maximal because organizations retain some agency (can appeal to international bodies, can work through legal gray areas); exit is possible but very costly (loss of domestic operations, funding cuts). Religious establishment: Beneficiary + mobile + requires enforcement → d≈0.45, f(d)≈0.50. Moderate d reflects that religious actors benefit from the constraint AND have agency to modify or withdraw support. Tangled rope classification captures that they contribute to enforcement (gain extraction from the legal system) but also provide legitimate coordination function (codify religious moral values). State security apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Low d reflects institutional beneficiary status with arbitrage exit (could withdraw enforcement, negotiate reduced scope); negative χ indicates net benefit position. Legislative parties: Beneficiary + arbitrage → d≈0.08. Same as security apparatus; piton classification comes from high theater_ratio gate (≥0.70), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by acknowledging that a single structural phenomenon (criminalized intimacy) can simultaneously be snare (for victims), tangled rope (for religious enforcers), rope (for state security), and piton (for legislatures maintaining through inertia). The resolution is perspectival: there is no single 'correct' type. The error would be assuming that because KUHP is law (formally neutral, applying to all citizens), it must be either pure coordination (rope) or immutable law (mountain). The snare classification is correct from the victim perspective because the structural relationship IS extraction: the state/religious actors benefit from regulation of intimate autonomy; the targets bear the cost; the mechanism relies on suppression (criminalization). The tangled rope for religious enforcers is correct because they simultaneously provide coordination (religious values codified) and extract authority. The piton for legislatures is correct because institutional actors maintain the law increasingly through theater and coalition inertia rather than systematic enforcement. Mandatrophy is resolved by recognizing that KUHP's classification depends entirely on observational position: it is simultaneously a coordinating mechanism for some observers and a purely extractive snare for others. The presheaf over different observer positions captures the constraint's full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity,
    'Is KUHP enforcement politically selective (targeting dissidents disguised as moral enforcement) or morally neutral (applied equally regardless of political status)?',
    'Systematic analysis of arrest patterns: correlation between political opposition activity and prosecution under ''insulting state'' articles vs cohabitation articles; tracking of charges dropped after political negotiations; geographic variation in enforcement rates',
    'If selective: constraint is political control tool (Snare). If neutral: constraint is genuine moral regulation (Tangled Rope). Selectivity evidence would confirm snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether enforcement targets political dissidents or applies neutrally').

omega_variable(
    exit_capacity_gender,
    'Does gendered access to exit options (marriage as exit for women vs flight/emigration as primary exit for LGBTQ persons) create categorically different experience of suppression?',
    'Disaggregated analysis of victim populations: women cohabitants vs LGBTQ persons vs political dissidents; measurement of relative costs of different exit pathways for each group',
    'If true: victims are not homogeneous; some perspectives should segregate by gender/orientation. Might require decomposition into separate stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_capacity_gender, empirical, 'Whether exit options vary systematically by victim category').

omega_variable(
    international_enforcement_pressure,
    'Will sustained pressure from UN human rights mechanisms, donor conditions, or ASEAN peer states force KUHP amendments before 2030?',
    'Tracking international diplomatic pressure, conditionality in development assistance, internal government cost-benefit analyses of international isolation vs domestic political constituency demands',
    'If pressure succeeds: scaffold perspective gains credibility (sunset mechanism). If pressure fails: snare classification hardens (no external check on extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_enforcement_pressure, empirical, 'Likelihood of international pressure forcing KUHP amendment').

omega_variable(
    religious_coalition_stability,
    'Will the religious establishment coalition supporting KUHP remain stable, or will sectarian fragmentation (Sunni vs non-Sunni factions) enable civil society counter-mobilization?',
    'Monitoring internal debates within Islamic organizations (NU, Muhammadiyah, FPI factions); tracking policy positions of different religious parties; measuring cohesion scores via coalition voting patterns',
    'If coalition fractures: enforcement authority weakens; snare becomes piton (degraded by internal conflict). If stable: extraction mechanism maintains coercive force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_coalition_stability, empirical, 'Stability of religious establishment coalition enforcing KUHP').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indonesia_penal_code_2023, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kuhp_tr_t0, indonesia_penal_code_2023, theater_ratio, 0, 0.52).
narrative_ontology:measurement(kuhp_tr_t1, indonesia_penal_code_2023, theater_ratio, 1, 0.58).
narrative_ontology:measurement(kuhp_tr_t2, indonesia_penal_code_2023, theater_ratio, 2, 0.65).

% Extraction over time
narrative_ontology:measurement(kuhp_be_t0, indonesia_penal_code_2023, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kuhp_be_t1, indonesia_penal_code_2023, base_extractiveness, 1, 0.55).
narrative_ontology:measurement(kuhp_be_t2, indonesia_penal_code_2023, base_extractiveness, 2, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indonesia_penal_code_2023, enforcement_mechanism).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, religious_establishment_authority_indonesia).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, lgbtq_legal_status_southeast_asia).
narrative_ontology:affects_constraint(indonesia_penal_code_2023, womens_reproductive_autonomy_muslim_majority_states).

% DUAL FORMULATION NOTE:
% KUHP 2023 decomposition: this constraint story addresses the personhood criminalization mechanism (ε=0.62, Snare). Related stories address: (1) the religious establishment's authority gain from state codification of moral law (higher ε, Tangled Rope), and (2) women's reproductive autonomy loss through marriage gatekeeping (could yield separate story with potentially higher ε=0.75 if decomposed). These are linked via network.affects_constraints because they share enforcement infrastructure and constituency but have distinct extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indonesia_penal_code_2023, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
