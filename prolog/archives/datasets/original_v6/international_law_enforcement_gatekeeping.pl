% ============================================================================
% CONSTRAINT STORY: international_law_enforcement_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_law_enforcement_gatekeeping, []).

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
 *   constraint_id: international_law_enforcement_gatekeeping
 *   human_readable: International Law Enforcement Gatekeeping
 *   domain: international_law/governance/institutional_power
 *
 * SUMMARY:
 *   International law enforcement gatekeeping is the structural constraint
 *   that controls who can invoke international legal mechanisms and against
 *   whom. The system creates an elaborate legal framework (UN Charter, Rome
 *   Statute, International Court of Justice, International Criminal Court)
 *   that appears neutral and universal but whose enforcement is asymmetric:
 *   permanent Security Council members have veto power, powerful nations can
 *   selectively comply or ignore rulings, and weak states face enforcement
 *   disproportionately. The constraint exhibits a dual nature. For great
 *   powers and their allies, it functions as coordination — a shared legal
 *   framework that constrains potential adversaries while preserving their
 *   own exemptions through veto, immunity provisions, and forum shopping. For
 *   weak states, it functions as extraction — an asymmetric enforcement
 *   regime where they cannot exit and cannot veto. The constraint's
 *   extractiveness has increased over the 45-year measurement interval (0.32
 *   → 0.58) as international institutions have accumulated more authority
 *   over sovereignty claims while enforcement mechanisms have remained
 *   dependent on geopolitical alignment. Theater ratio has increased
 *   correspondingly (0.42 → 0.68) as the legal machinery has grown more
 *   elaborate and procedurally complex while actual enforcement capacity has
 *   not scaled proportionally.
 *
 * KEY AGENTS:
 *   - Permanent Security Council Members (US, UK, France, Russia, China): Institutional/arbitrage — primary beneficiaries; control veto over enforcement; can exempt selves through diplomatic immunity and selective vetoes
 *   - Weak States & Non-Aligned Nations: Powerless/trapped — primary victims; face enforcement asymmetrically; have no veto or immunity options; bear maximum suppression
 *   - Regional Middle Powers (India, Brazil, Indonesia, etc.): Moderate/constrained — experience mixed coordination and extraction; rules constrain aggressive neighbors but enforcement remains selective; exit is costly but possible
 *   - International Criminal Court & UN Legal Bodies: Institutional/arbitrage — formal enforcers; maintain institutional legitimacy through procedural theater; depend on powerful states for enforcement resources
 *   - International Civil Society & Human Rights Organizations: Organized/constrained — advocates for constraint reform; building complementarity pathways and universal jurisdiction doctrines; see sunset through norm evolution
 *   - Analytical Observer: Analytical/analytical — detects false naturalization of contingent institutional arrangements as inevitable features of anarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_law_enforcement_gatekeeping, 0.58).
domain_priors:suppression_score(international_law_enforcement_gatekeeping, 0.68).
domain_priors:theater_ratio(international_law_enforcement_gatekeeping, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_law_enforcement_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_law_enforcement_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(international_law_enforcement_gatekeeping, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_law_enforcement_gatekeeping, tangled_rope).
narrative_ontology:human_readable(international_law_enforcement_gatekeeping, "International Law Enforcement Gatekeeping").
narrative_ontology:topic_domain(international_law_enforcement_gatekeeping, "international_law/governance/institutional_power").

domain_priors:requires_active_enforcement(international_law_enforcement_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_law_enforcement_gatekeeping, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(international_law_enforcement_gatekeeping, dominant_military_powers).
narrative_ontology:constraint_beneficiary(international_law_enforcement_gatekeeping, institutional_legal_authorities).
narrative_ontology:constraint_victim(international_law_enforcement_gatekeeping, non_aligned_nations).
narrative_ontology:constraint_victim(international_law_enforcement_gatekeeping, weak_military_states).
narrative_ontology:constraint_victim(international_law_enforcement_gatekeeping, subaltern_legal_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAK STATE DEFENDANT (SNARE) — A non-aligned or militarily weak nation accused under international law has no meaningful exit. Enforcement is asymmetric: powerful states can veto prosecution (UN Security Council), ignore rulings, or selectively comply. Weak states face maximum suppression — international tribunals are available only when geopolitically expedient. No alternative legal system carries comparable legitimacy.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERMANENT SECURITY COUNCIL MEMBER (ROPE) — Experiences international law enforcement as coordination: uses the legal framework to constrain adversaries while maintaining veto power over own accountability. Can arbitrage between different legal forums (ICC, ICJ, ad-hoc tribunals) and diplomatic immunity provisions. Net beneficiary with full exit options.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL MIDDLE POWER (TANGLED ROPE) — Medium-sized states experience genuine coordination function (rules constrain aggressive neighbors, create deterrence) alongside asymmetric extraction (enforcement is selective, powerful states exempt themselves, legal costs are high). Exit is costly but possible — can form regional coalitions, withdraw from specific treaties, or pursue alternative legal mechanisms.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL CIVIL SOCIETY COALITION (SCAFFOLD) — NGOs, human rights organizations, and legal advocates see the gatekeeping system as a temporary coordination failure being addressed through normative pressure, treaty proliferation, and complementarity doctrines (domestic prosecution as alternative to international tribunals). Sunset logic: expanding jurisdictional bases, universal jurisdiction doctrine, and institutional capacity in the Global South are building alternative verification pathways.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ESTABLISHMENT (PITON) — Permanent institutions (UN, ICC, ICJ) maintain elaborate procedural theater around law enforcement (statute interpretation, evidentiary rules, preliminary examination processes) despite low functional enforcement capacity. The legal machinery persists through institutional inertia: replacing it would require consensus from powers that benefit from selective application. Theater ratio reflects the gap between professed legal impartiality and actual enforcement asymmetry.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, international law enforcement gatekeeping may appear as an immutable feature of anarchic state systems: without a supranational enforcer, legal rules depend on state consent, and powerful states will never subordinate to rules they did not draft. This perspective naturalizes the constraint as a fundamental structural property. However, the base properties contradict mountain classification — the engine detects this as false naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_law_enforcement_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_law_enforcement_gatekeeping, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_law_enforcement_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_law_enforcement_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(international_law_enforcement_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The gatekeeping system extracts legitimacy and jurisdiction from the global legal commons. Weak states bear enforcement costs without corresponding voice in enforcement rules. However, extraction is not maximal because the system does provide some genuine coordination benefit (rules do deter some violations, alliances do prefer rule-based over purely power-based competition). The extractiveness increase from 0.32 to 0.58 reflects institutional creep — more authority accumulated with no corresponding enforcement symmetry. Suppression (0.68): High. Barriers to exiting or evading the system are substantial: withdrawal from treaties carries diplomatic costs, alternative legal systems lack comparable legitimacy, powerful states can block weak states from invoking their own legal claims. But suppression is not absolute — some states have withdrawn from treaties (US from Rome Statute, various nations from specific courts), coalition-building can create countervailing pressure, and the threat of non-compliance is available. Theater ratio (0.65): Moderate-high. International legal institutions perform substantial procedural theater — statute interpretation, evidentiary rules, preliminary examinations, admissibility challenges — that create an appearance of neutral adjudication while actual enforcement remains shaped by geopolitical interest. The theater has increased over time as institutional procedures have become more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full polarization between beneficiaries and victims. Permanent Council members see legitimate international coordination with justified exemptions (they provide enforcement resources, stable alliance structures, nuclear deterrence). Weak states see a rigged system where enforcement is weaponized against them. Regional middle powers see genuine coordination benefits (rules do constrain aggressive neighbors) alongside extraction (rules are selectively enforced based on geopolitical alignment). The International Legal Establishment sees its own procedures and jurisdictional architecture; the structural asymmetries are difficult to perceive from within institutional legitimacy claims. Civil society coalitions see a temporary problem being solved through norm expansion and complementarity. The analytical observer risks seeing anarchy as inevitable (mountain perspective) but the structural data reveals the specific institutional choices (veto mechanisms, immunity provisions, forum shopping) that create the asymmetry — these are contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position relative to the enforcement flow. Permanent Council members benefit from the veto mechanism and immunity provisions — they control enforcement without being subject to it (beneficiaries, arbitrage exit). Weak states cannot exit the international legal system (trapped exit) and face enforcement disproportionately (victims). Regional middle powers can form coalitions and pursue alternative forums (constrained exit) while still benefiting from rules that constrain neighbors. The International Legal Establishment maintains institutional power through the gatekeeping system regardless of enforcement outcomes (institutional/arbitrage). Civil society has organized pressure mechanisms but lacks direct veto (organized/constrained). The analytical observer's d value reflects the observer position's structural relationship to the extraction flow — positioned to see the asymmetry from outside any particular power bloc.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between genuine coordination (rules do deter some violations, alliances prefer stability) and extractive asymmetry (enforcement is selective, powerful states are exempt). The constraint is correctly classified as Tangled Rope: it has a real coordination function (international rules do provide stability benefits) AND a genuine asymmetric extraction component (enforcement is selective and excludes powerful states). The mandatrophy would arise if analysts tried to argue that international law is either pure coordination (rope — false; enforcement is selective) or pure extraction (snare — false; rules do provide deterrence). The tangled rope classification holds both truths: the system genuinely coordinates state behavior AND it extracts disproportionately from weak states. The measurement trajectory (extractiveness increasing from 0.32 to 0.58 over 45 years) shows institutional creep — more authority accumulated without corresponding enforcement symmetry, suggesting that the tension between coordination and extraction is shifting over time toward extraction dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_mechanism,
    'Is selective enforcement of international law a deliberate extractive strategy or an inevitable consequence of decentralized enforcement against sovereign states?',
    'Counterfactual analysis: would enforcement patterns change if power distributions shifted? Historical analysis of cases where enforcement was symmetric vs asymmetric; correlation between enforcement selectivity and geopolitical interest.',
    'If deliberate: constraint classifies as Snare for victims, Rope for beneficiaries (current view). If inevitable: constraint may approach Mountain for all perspectives (structural feature of anarchy). If hybrid: Tangled Rope confirmed with clearer understanding of enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_mechanism, empirical, 'Whether selective enforcement is deliberate extraction or structural inevitability').

omega_variable(
    complementarity_pathway_effectiveness,
    'Does the complementarity principle (ICC defers to domestic prosecution) genuinely distribute enforcement capacity to weaker states, or does it create a new gatekeeping layer where powerful states'' proxies control proxy enforcement?',
    'Analysis of complementarity decisions: are cases deferred to states with weak institutional capacity? Tracking of proxy prosecution outcomes; comparison of conviction rates in complementarity vs direct ICC cases.',
    'If distributive: scaffold perspective is correct — the system is evolving toward decentralized enforcement. If new gatekeeping: the constraint perpetuates through delegation, not elimination, and the sunset timeline extends. If mixed: partial sunset with residual extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_pathway_effectiveness, empirical, 'Whether complementarity doctrine enables genuine decentralized enforcement or creates new gatekeeping').

omega_variable(
    universal_jurisdiction_viability,
    'Can universal jurisdiction doctrine (allowing third-country prosecution of international crimes) actually function as an exit route for weak states, or does it remain a boutique remedy available only to wealthy nations with robust legal systems?',
    'Case tracking: which nations successfully invoke universal jurisdiction against whom? Analysis of prosecution rates by defendant power level; assessment of whether universal jurisdiction creates chilling effects on powerful state officials.',
    'If genuinely available: weak states have a real exit option, suppression decreases, constraint shifts toward Tangled Rope from more perspectives. If boutique: universal jurisdiction is aspirational cover for the existing gatekeeping, suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_jurisdiction_viability, empirical, 'Whether universal jurisdiction functions as viable exit route').

omega_variable(
    treaty_withdrawal_costs,
    'What are the actual diplomatic, economic, and security costs for a weak state withdrawing from international legal frameworks? Are these costs prohibitive (suppression gate), surmountable (constrained), or navigable (mobile)?',
    'Historical case analysis: states that have withdrawn or threatened withdrawal (ICJ cases, Rome Statute, bilateral immunity agreements); measurement of subsequent isolation, sanctions, or alliance disruption; comparison of costs across power levels.',
    'If prohibitive: suppression increases, trapped classification spreads. If surmountable: exit_options upgrade to constrained, chi decreases. If navigable: mobile exit becomes available, chi approaches Rope from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_withdrawal_costs, empirical, 'Actual costs of international legal treaty withdrawal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_law_enforcement_gatekeeping, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intl_law_tr_t0, international_law_enforcement_gatekeeping, theater_ratio, 0, 0.42).
narrative_ontology:measurement(intl_law_tr_t15, international_law_enforcement_gatekeeping, theater_ratio, 15, 0.55).
narrative_ontology:measurement(intl_law_tr_t30, international_law_enforcement_gatekeeping, theater_ratio, 30, 0.65).
narrative_ontology:measurement(intl_law_tr_t45, international_law_enforcement_gatekeeping, theater_ratio, 45, 0.68).

% Extraction over time
narrative_ontology:measurement(intl_law_be_t0, international_law_enforcement_gatekeeping, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(intl_law_be_t15, international_law_enforcement_gatekeeping, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(intl_law_be_t30, international_law_enforcement_gatekeeping, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(intl_law_be_t45, international_law_enforcement_gatekeeping, base_extractiveness, 45, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_law_enforcement_gatekeeping, enforcement_mechanism).
narrative_ontology:affects_constraint(international_law_enforcement_gatekeeping, security_council_veto_power).
narrative_ontology:affects_constraint(international_law_enforcement_gatekeeping, diplomatic_immunity_provisions).
narrative_ontology:affects_constraint(international_law_enforcement_gatekeeping, treaty_forum_shopping).

% DUAL FORMULATION NOTE:
% International law enforcement gatekeeping is decomposed into three structurally distinct constraints: (1) the veto mechanism itself (institutional power distribution), (2) diplomatic immunity provisions (exemption architecture), and (3) forum shopping (procedural escape routes). Each has its own extractiveness value and classification. This story addresses the meta-constraint — how the gatekeeping system as a whole functions across all three components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_law_enforcement_gatekeeping, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
