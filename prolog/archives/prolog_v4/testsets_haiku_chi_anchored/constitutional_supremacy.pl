% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_supremacy, []).

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
 *   constraint_id: constitutional_supremacy
 *   human_readable: The Supremacy of Written Constitutions and Judicial Review
 *   domain: legal/political
 *
 * SUMMARY:
 *   The doctrine of constitutional supremacy — articulated in Marbury v.
 *   Madison (1803) and entrenched across most modern democracies — creates a
 *   structural constraint that combines coordination and extraction. On one
 *   hand, it solves a genuine collective action problem: without judicial
 *   review, legislatures could amend or nullify the constitution at will,
 *   making stable long-term rights protections impossible. On the other hand,
 *   it transfers substantial sovereign power from elected representatives to
 *   appointed judges, who can indefinitely block legislation the public
 *   majority desires. The constraint exhibits all six DR types depending on
 *   perspective. For voters whose preferences are nullified, it appears as a
 *   snare (trapped extraction). For legislatures, it is tangled rope (both
 *   enabling and constraining). For judges, it is rope (coordination through
 *   institutional authority). For constitutional reformers, it is tangled
 *   rope (sunset logic would require amendment). For originalist interpretive
 *   practice, it is piton (performative ritual). For the analytical observer
 *   at civilizational scope, it risks appearing as a mountain (natural law of
 *   governance), but this masks the contingent institutional choices that
 *   underpin the system. The theater ratio has increased from 0.25 (early
 *   Marbury period, when judges genuinely believed they were merely reading
 *   the text) to 0.58 (modern practice, where judicial discretion is visible
 *   in living constitutionalism and policy preferences drive outcomes). The
 *   extractiveness has increased correspondingly, from 0.28 to 0.52, as the
 *   judiciary's power to nullify legislation has expanded and the
 *   supermajority barriers to amendment have made legislative correction
 *   increasingly difficult.
 *
 * KEY AGENTS:
 *   - Judiciary: Primary beneficiary (institutional/arbitrage) — consolidates power to nullify legislation; sets agenda through constitutional interpretation
 *   - Legislative Body: Primary victim (moderate/constrained) — bears extraction through nullification; also benefits from constraint against its own tyranny
 *   - Disenfranchised Majority: Secondary victim (powerless/trapped) — cannot exit system when preferred legislation is struck down; bears full cost of judicial veto
 *   - Constitutional Reform Movements: Secondary beneficiary (organized/constrained) — benefit from constitutional rights protection but face supermajority barriers to amendment
 *   - Originalist Interpretive Practice: Institutional actor (institutional/arbitrage) — maintains performative ritual that masks judicial discretion; sees own practice as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_supremacy, 0.52).
domain_priors:suppression_score(constitutional_supremacy, 0.65).
domain_priors:theater_ratio(constitutional_supremacy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_supremacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_supremacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_supremacy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_supremacy, tangled_rope).
narrative_ontology:human_readable(constitutional_supremacy, "The Supremacy of Written Constitutions and Judicial Review").
narrative_ontology:topic_domain(constitutional_supremacy, "legal/political").

domain_priors:requires_active_enforcement(constitutional_supremacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_supremacy, judicial_institutional_power).
narrative_ontology:constraint_beneficiary(constitutional_supremacy, constitutional_consistency_mechanism).
narrative_ontology:constraint_victim(constitutional_supremacy, legislative_sovereignty).
narrative_ontology:constraint_victim(constitutional_supremacy, democratic_majority_will).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED MAJORITY (SNARE) — Voters whose preferred legislative outcomes are struck down by courts cannot exit the constitutional constraint system. No alternative sovereign exists. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(constitutional_supremacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATIVE BODY (TANGLED ROPE) — Parliament/Congress benefits from constitutional constraint (prevents tyranny, enables stable negotiation). But also bears extraction: judicial nullification prevents legislative will. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(constitutional_supremacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIARY (ROPE) — Benefits from supremacy doctrine via institutional power consolidation and agenda-setting authority. Experiences constraint as coordination: judicial review enables stable rule of law by preventing legislative overreach. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(constitutional_supremacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENTS (TANGLED ROPE) — Organized actors (amendment coalitions, social movements) benefit from constitutional supremacy (protection of fundamental rights) but are constrained by supermajority requirements for amendment (typically 2/3 or 3/5). d≈0.58, f(d)≈0.78, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(constitutional_supremacy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ORIGINALIST INTERPRETIVE RITUAL (PITON) — Judicial originalism (adherence to 'original public meaning') is substantially performative: judges select which historical sources count, which framers' intent matters, how to weight competing founders' views. The ritual persists through institutional inertia (judges maintain the fantasy of constraint-free interpretation). theater_ratio=0.58 reflects moderate performativity. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(constitutional_supremacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of constitutional limit on legislature appears necessary to any stable political system: checks and balances, separation of powers, and judicial review emerge across diverse constitutional traditions as structural invariants. However, the base properties (ε=0.52, suppression=0.65, theater=0.58) contradict the mountain classification. This reveals the false summit: what appears to be a natural law (all stable systems need judicial review) is actually a contingent institutional arrangement (many democracies function without strong constitutional courts).
constraint_indexing:constraint_classification(constitutional_supremacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_supremacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_supremacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_supremacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_supremacy, TR),
    TR >= 0.70.

:- end_tests(constitutional_supremacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The judiciary's power to nullify legislation is substantial and has expanded over 220 years. However, it is not maximal extraction (which would be >0.70) because legislatures retain significant agenda-setting power and can respond to nullification through redrafting, constitutional amendment (however difficult), or alternative mechanisms. The increase from 0.28 to 0.52 over the interval reflects the judiciary's expanding power as judicial review has matured from an exceptional veto (early Marbury) to routine legislative correction (modern practice). Suppression (0.65): Moderate-high. Barriers to legislative correction of judicial nullification are substantial: supermajority requirements for amendment (2/3 or 3/5) are mathematically difficult to achieve in polarized systems; alternative mechanisms (sunset clauses, delegation to agencies) are rarely written into foundational law; political will to amend a constitution is weak. However, suppression is not total: amendment is possible, court composition changes through appointment, and overruling of prior decisions occurs. Theater ratio (0.58): Moderate. Judicial opinion-writing contains substantial performativity: originalists claim to be discovering historical meaning while actually choosing which sources count; living constitutionalists appeal to vague principles; all judges present discretionary choices as inevitable textual readings. However, the theater is not total (0.70+) because modern scholarship explicitly debates judicial methodology and power, reducing the illusion that interpretation is constraint-free.
 *
 * PERSPECTIVAL GAP:
 *   Constitutional supremacy exhibits a wide perspectival gap across multiple dimensions. The judiciary sees coordination (stable rule of law through judicial review); the legislature sees extraction (nullification of preferred policy). The disenfranchised voter sees a snare (no exit); the constitutional reformer sees a scaffold with difficult sunset mechanics (amendment as the path forward). The originalist judge sees a mountain (bound by historical text); the analytical observer sees a false summit (the text's meaning is judge-created). This gap is wider than in many constraints because constitutional supremacy involves power transfer between institutional actors (judiciary vs legislature) and between elites (judges) and masses (voting public). The constraint's legitimacy derives entirely from perspectival agreement: if the judiciary sees coordination and the legislature agrees, the system functions; if the legislature sees extraction and the voting public agrees, pressure for constitutional amendment builds.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. The institutional power to nullify legislation with minimal risk of reversal positions the judiciary as the clear beneficiary. Arbitrage exit means the judiciary can play multiple roles (defender of rights, check on legislature, guardian of original meaning) to maximize legitimacy. Legislative Body: Victim + constrained → d≈0.68, f(d)≈1.02. Significant extraction. Legislatures face supermajority barriers to amendment but retain some policy-making power; the constraint is real but not total. Constrained exit reflects that legislative response is possible but difficult (rewrite nullified laws, form amendment coalitions). Disenfranchised Majority: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Voters whose preferences are nullified have no exit from the political system and no institutional mechanism to overturn judicial nullification except through the nearly-impossible amendment process. Constitutional Reform Movements: Beneficiary + constrained → d≈0.58, f(d)≈0.78. Mixed extraction. Reform movements benefit from constitutional rights protection but are constrained by amendment supermajorities. The constraint enables their coordination while impeding their ability to change rules. Originalist Judges: Institutional + arbitrage → d≈0.12, f(d)≈-0.05. Minimal effective extraction in piton perspective — judges maintain the fantasy of constraint through originalist methodology while actually exercising discretion. Analytical Observer: Analytical → d≈0.72, f(d)≈1.15. Mountain classification would apply if constitutional supremacy were structurally necessary, but the empirical base properties and the existence of stable democracies without strong constitutional courts contradict this.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA PERSPECTIVAL DECOMPOSITION: The mandatrophy (apparent contradiction between judicial review as both a coordination mechanism and an extraction mechanism) is resolved by recognizing that different agents experience the same constraint differently. The judiciary legitimately coordinates through constitutional constraint (protecting rights, preventing legislative tyranny). The legislature genuinely experiences extraction (nullification of preferred policy). The disenfranchised voter experiences snare-level extraction (no exit from the system). The constitutional reformer experiences a mixed constraint (rights protection + amendment barriers). No single classification is false — each is the structural reality from that position. The system's legitimacy depends on the perspectival agreement holding: if the judiciary's coordination narrative becomes obviously false (if judges are transparently pursuing policy preferences rather than law), then all perspectives shift downward (toward snare) and the system loses legitimacy. The theater ratio increase from 0.25 to 0.58 reflects precisely this erosion of the coordination narrative as originalism's performativity becomes visible. The ultimate mandatrophy resolution requires asking: which perspective's experience is most fundamental? The answer depends on normative priors (Is judicial power concentrated in unelected officials? Is majority rule overridden?). From a deferential realism standpoint, the system exhibits tangled rope properties across most perspectives, with snare properties for the powerless voter and rope properties for the judiciary — the configuration is stable only if perspectival agreement holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_basis,
    'On what normative basis do courts derive authority to nullify legislation passed by democratically elected representatives?',
    'Analysis of judicial reasoning across constitutional traditions; identification of whether legitimacy derives from written text, living constitution principles, or unstated conventions; cross-national comparison of judicial power claims',
    'If legitimacy is textual: supremacy is rope (coordination mechanism). If legitimacy is judge-created: supremacy is snare (extraction of legislative power). If legitimacy is conventional: supremacy is piton (theater masking power transfer).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_legitimacy_basis, conceptual, 'Normative basis for judicial authority to nullify legislation').

omega_variable(
    countermajoritarian_paradox,
    'How can unelected judges exercising veto power over elected legislatures be reconciled with democratic sovereignty?',
    'Empirical study of judicial nullification patterns: correlation between constitutional rulings and public opinion; comparison of policy outcomes under strong vs weak judicial review; historical analysis of democratic collapse vs strengthening under different judicial power regimes',
    'If courts protect minority rights that majorities would suppress: snare classification becomes rope (coordination). If courts block majority preferences: snare classification is confirmed. If courts split the difference: tangled rope (true classification) is revealed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_paradox, empirical, 'Reconciliation of judicial veto with democratic legitimacy').

omega_variable(
    amendment_supermajority_lock_in,
    'Do supermajority requirements for constitutional amendment (2/3 or 3/5 votes) create irreversible power transfers to the judiciary by making text changes practically impossible?',
    'Historical analysis of amendment success/failure rates; comparison of amendment accessibility across constitutional traditions; modeling of when amendment becomes mathematically impossible given political polarization',
    'If amendment is genuinely accessible: constitutional supremacy remains tangled rope (mixed coordination/extraction, reversible). If amendment becomes locked in: supremacy becomes snare (extraction is permanent, no exit for legislative body).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_supermajority_lock_in, empirical, 'Whether supermajority amendment requirements lock in judicial power').

omega_variable(
    structural_necessity_of_judicial_review,
    'Is some form of judicial constraint on legislature structurally necessary to any stable democracy, or is judicial review a contingent institutional choice?',
    'Comparative institutional analysis of stable democracies with and without strong constitutional courts; identification of alternative mechanisms for legislative constraint (supermajority requirements, legislative sunset clauses, delegation to independent agencies); modeling of stability conditions',
    'If structurally necessary: mountain classification might be correct (moved to ε≤0.25). If contingent: false summit confirmed — supremacy is institutional arrangement, not natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_necessity_of_judicial_review, conceptual, 'Whether judicial review is structurally necessary or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_supremacy, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(consti_tr_t0, constitutional_supremacy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(consti_tr_t110, constitutional_supremacy, theater_ratio, 110, 0.45).
narrative_ontology:measurement(consti_tr_t220, constitutional_supremacy, theater_ratio, 220, 0.58).

% Extraction over time
narrative_ontology:measurement(consti_be_t0, constitutional_supremacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(consti_be_t110, constitutional_supremacy, base_extractiveness, 110, 0.4).
narrative_ontology:measurement(consti_be_t220, constitutional_supremacy, base_extractiveness, 220, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_supremacy, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_supremacy, legislative_supermajority_requirement).
narrative_ontology:affects_constraint(constitutional_supremacy, amendment_accessibility).
narrative_ontology:affects_constraint(constitutional_supremacy, judicial_discretion_in_interpretation).

% DUAL FORMULATION NOTE:
% Constitutional supremacy decomposes into three interdependent constraints: (1) the foundational rule that written constitution supersedes statute (ε≈0.08, Mountain if truly unavoidable), (2) the institutional mechanism of judicial review that enforces supremacy (ε≈0.52, Tangled Rope — this story), and (3) the amendment supermajority requirement that prevents easy reversal (ε≈0.61, Snare for legislatures). The current story addresses the middle constraint — judicial review as a coordination mechanism that has accrued extraction properties over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_supremacy, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
