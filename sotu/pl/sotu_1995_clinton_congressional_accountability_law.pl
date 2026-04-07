% ============================================================================
% CONSTRAINT STORY: sotu_1995_clinton_congressional_accountability_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1995_clinton_congressional_accountability_law, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1995_clinton_congressional_accountability_law
 *   human_readable: Extension of Private-Sector Labor and Regulatory Laws to Congress (1995 Clinton)
 *   domain: governance/legislative_accountability
 *
 * SUMMARY:
 *   In his 1995 State of the Union address, President Clinton referenced
 *   recently enacted legislation extending private-sector employment and
 *   regulatory laws to Congress itself, eliminating the legislative branch's
 *   prior categorical exemption from labor law, workplace safety regulation,
 *   civil rights enforcement, and environmental compliance. This constraint
 *   exemplifies how legal alignment — making an institution subject to the
 *   same rules it imposes on others — creates a hybrid
 *   coordination-extraction dynamic. The baseline empirical claim is
 *   straightforward: Congress gains horizontal accountability and loses
 *   vertical privilege. But the structural consequences are multivalent. For
 *   Congressional staff, the law is protective (Rope) — extending legal
 *   remedies for discrimination, harassment, unsafe conditions. For Congress
 *   as an institution, it is restrictive (Snare) — creating compliance burden
 *   and litigation exposure. For Congressional leadership setting regulatory
 *   policy, it is mixed (Tangled Rope) — they coordinate on unified legal
 *   framework while also constraining their own exemptions. For public
 *   accountability mechanisms and the rule-of-law principle, it is
 *   foundational (Rope to Mountain depending on perspective). The
 *   constraint's extractiveness has risen over time (0.18 → 0.38) as
 *   compliance infrastructure has matured and litigation has increased, while
 *   theater ratio has also risen (0.22 → 0.48) as Congress develops formal
 *   compliance procedures that may substitute for substantive commitment to
 *   workplace equality. The constraint is neither purely coercive nor purely
 *   coordinating — it is generative of both goods (staff protection, legal
 *   alignment) and bads (compliance burden, institutional constraint).
 *
 * KEY AGENTS:
 *   - Congressional Staff Members: Primary beneficiary (powerless/constrained) — gain legal protection from workplace discrimination, harassment, and unsafe conditions; experience the constraint as beneficial legal alignment
 *   - Congressional Institution (Collective): Primary victim (institutional/trapped) — loses categorical exemption from employment law, faces compliance burden and litigation exposure; cannot exit without legislative action to repeal
 *   - Congressional Leadership: Powerful institutional actor (powerful/mobile) — retains power to set rules while now also binding themselves; can exit through repeal but choose not to (at least superficially)
 *   - Executive Branch / Oversight Mechanisms: Institutional beneficiary (institutional/arbitrage) — gains equal legal framework without hierarchical control; benefits from transparent uniform compliance
 *   - Organized Labor / Civil Rights Organizations: Organized beneficiary (organized/constrained) — gain enforcement mechanisms and extended protections; experience constraint on informal advocacy tactics
 *   - Democratic Transparency / Anti-Privilege Norm: Analytical beneficiary (analytical/analytical) — constraint supports norm transition toward equal legal standing; sunset implicit rather than explicit
 *   - Rule of Law Principle: Civilizational principle (analytical/analytical) — constraint embodies logical necessity of equal standing in rule-of-law system; risks false summit detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1995_clinton_congressional_accountability_law, 0.38).
domain_priors:suppression_score(sotu_1995_clinton_congressional_accountability_law, 0.32).
domain_priors:theater_ratio(sotu_1995_clinton_congressional_accountability_law, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1995_clinton_congressional_accountability_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1995_clinton_congressional_accountability_law, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(sotu_1995_clinton_congressional_accountability_law, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sotu_1995_clinton_congressional_accountability_law, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(sotu_1995_clinton_congressional_accountability_law, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1995_clinton_congressional_accountability_law, tangled_rope).
narrative_ontology:human_readable(sotu_1995_clinton_congressional_accountability_law, "Extension of Private-Sector Labor and Regulatory Laws to Congress (1995 Clinton)").
narrative_ontology:topic_domain(sotu_1995_clinton_congressional_accountability_law, "governance/legislative_accountability").

domain_priors:requires_active_enforcement(sotu_1995_clinton_congressional_accountability_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_congressional_accountability_law, congressional_staff_employees).
narrative_ontology:constraint_beneficiary(sotu_1995_clinton_congressional_accountability_law, public_accountability_mechanisms).
narrative_ontology:constraint_victim(sotu_1995_clinton_congressional_accountability_law, congressional_legislative_capacity).
narrative_ontology:constraint_victim(sotu_1995_clinton_congressional_accountability_law, institutional_congressional_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONGRESSIONAL STAFF MEMBER (ROPE) — Prior to the law, staff faced workplace conditions exempt from OSHA, ADA, Civil Rights Act protections. The law establishes equal legal standing and protection from workplace discrimination, harassment, and unsafe conditions. Experiences the constraint as coordination mechanism: legal harmonization removes arbitrary privilege and establishes predictable rules. Constrained exit (cannot easily change employers without significant career cost) but clearly benefits from legal protection. Sees this as constraint on Congressional power that benefits them.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL INSTITUTION / COLLECTIVE (SNARE) — The legislative branch loses its categorical exemption from employment law, environmental law, workplace safety regulation, and civil rights enforcement. Faces compliance burden, litigation risk, and operational constraints that previously did not apply. Cannot exit or negotiate — the law is binding and retroactive. Experiences maximum extraction: bears full cost of compliance infrastructure while powerless to exempt itself. The institution is trapped by its own legislative authority being turned back on itself.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS AS REGULATORY SETTER (TANGLED ROPE) — Congressional leadership (committee chairs, party leadership, floor managers) retains significant power: they set the rules for others yet now also bind themselves. This is genuine hybrid — they coordinate on unified legal framework (reducing regulatory arbitrage and creating predictability) while also losing the asymmetric advantage they previously held. High power and mobile exit options (can vote to repeal) but choose not to, partly because repeal would signal hypocrisy. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EXECUTIVE BRANCH OVERSIGHT / PUBLIC ACCOUNTABILITY MECHANISMS (ROPE) — The constraint enables equal legal accountability without hierarchical control: Congress and the executive branch now operate under the same legal framework rather than the executive holding power to investigate or discipline Congress separately. Benefits from transparent, uniform legal compliance rather than political prosecution risk. Experiences as pure coordination: establishes common rules that increase predictability and reduce arbitrary power asymmetries. No suppression, no extraction — genuine mutual coordination benefit.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED LABOR / CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — These groups benefit from extended legal protections and enforcement mechanisms (can now sue Congress for violations) and coordinated labor standards that apply uniformly. But also experience constraint: they gain enforcement rights but lose informal advocacy access to Congress (the personal relationships and informal pressure tactics that worked when Congress was exempt now face legal liability from both sides). Constrained exit (the law is binding) but dual benefit and burden structure. Moderately extractive on balance.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DEMOCRATIC TRANSPARENCY / ANTI-PRIVILEGE NORM (SCAFFOLD) — From the analytical observer perspective, this constraint is temporary support for a cultural norm transition: moving away from institutional exemptions toward equal legal standing. The constraint has low theater (OSHA violations, ADA accommodations, payroll compliance are objective, measurable, enforceable facts, not performative compliance). Suppression is moderate but declining as the norm becomes internalized. The sunset is implicit rather than explicit: as the culture shifts toward 'no one is above the law,' the constraint's enforcement becomes less necessary — violations become unthinkable rather than merely illegal. This is how norms change: constrain behavior until the constraint becomes redundant. Scaffold structure: high coordination function (establishes equal legal standing), declining suppression, eventual obsolescence as the norm matures.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 7: RULE OF LAW / EQUAL LEGAL STANDING (CIVILIZATIONAL) — From a civilizational perspective, this constraint embodies an immutable principle: in a rule-of-law system, no institution can exempt itself from the laws it enforces on others. This is a logical necessity, not a contingent policy choice. The law is not extractive in principle because it merely realizes what must be true in any functioning rule-of-law system: equal standing before law. However, this perspective risks false summit detection — the principle is valid but the law is a human artifact that requires active enforcement and has real distributional consequences (Congress bears compliance costs, staff members gain protections). The mountain classification may naturalize what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1995_clinton_congressional_accountability_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1995_clinton_congressional_accountability_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sotu_1995_clinton_congressional_accountability_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from Congress (compliance burden, reduced exemption privilege) and benefits staff (legal protections). The 0.38 value reflects that this is not maximal extraction — Congress retains substantial power (can legislate, can seek carve-outs, can shift compliance costs), and the benefits are genuine (staff protections are not merely performative). Baseline measurement (0.18) represents the year of enactment when compliance infrastructure was minimal; current measurement (0.38) reflects accumulated litigation costs and formalized compliance procedures. The plateau at t=7 (0.36) suggests that initial learning and adaptation has stabilized the extraction level. Suppression (0.32): Moderate. Congress is not trapped — it retains power to repeal or modify the law. But suppression exists because exit carries political costs (signal of hypocrisy or commitment to hypocrisy). Theater ratio (0.48): Moderate-low. The constraint is substantive — OSHA violations, ADA accommodations, payroll compliance are objective and measurable, not performative. But rising theater (0.22 → 0.48) suggests that formalized compliance procedures and documentation are substituting for substantive workplace culture change. Some agencies may show compliance on paper while maintaining informal hierarchies and discrimination.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the staff perspective (legal protection is transparent good) and Congress perspective (compliance burden is transparent bad). These are not merely different valuations of the same fact — they are asymmetric extraction. Staff gain; Congress bears cost. But the gap is bridged by two mechanisms: (1) Congress retains power to repeal, so suppression is moderate not maximal, and (2) the legal framework achieves something Congress itself says it values (equal standing, no exemptions), so extraction is embedded in a coordination narrative. The gap between leadership (Tangled Rope, mobile exit, mixed benefits) and institution (Snare, trapped, pure extraction) reveals that power and agency do not distribute uniformly within the institution. The gap between analytical perspectives (Scaffold, Mountain) and lived experiences reveals the oracle gap (Theorem 4): the analyst's comprehensive view risks naturalizing as universal what is actually contingent and distributional.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by (1) beneficiary/victim status, (2) exit options, and (3) power level. Staff are beneficiaries with constrained exit → low d → experience protective coordination. Congress is victim with trapped exit → high d → experience extractive constraint. Leadership is victim with mobile exit → moderate d → can theoretically escape but choose not to. Executive branch is beneficiary with arbitrage exit → low d → experience mutual coordination. The chi formula then scales extractiveness by f(d) and scope σ(S): staff see low chi (negative f(d) for beneficiary with constrained exit), Congress sees high chi (positive f(d) for victim with trapped exit). At national scope σ(1.0), no amplification. The directionality chain is unambiguous given the beneficiary/victim declarations. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mandatrophy by maintaining genuine dual structure — both coordination function (equal legal standing, unified framework) and asymmetric extraction (compliance burden on Congress, protection for staff). The constraint is Tangled Rope at the aggregate level: ε = 0.38 (moderate extraction), suppression = 0.32 (moderate asymmetry), χ ~ 0.40-0.50 depending on perspective (moderate effective extraction). The constraint clearly coordinates on legal alignment (reduces regulatory arbitrage, establishes predictable rules) while also extracting from Congress (compliance costs, privilege loss). Mandatrophy is resolved because the classification does not collapse to either pure coordination (Rope) or pure extraction (Snare) — it holds both simultaneously. The perspectival diversity (6-7 types across different positions) confirms that the constraint is structurally complex enough to warrant hybrid classification. The rising extractiveness (0.18 → 0.38) and theater ratio (0.22 → 0.48) over time suggest that the initial coordination narrative (we're all under the same law now) is gradually being eclipsed by the extraction narrative (Congress bears accumulating compliance costs while benefits accrue to staff and external actors). This is a classic sign of rent-seeking layering onto coordination — the underlying coordination becomes increasingly obscured by extraction mechanisms. The measurement plateau (extract_t7 ≈ 0.36) suggests that the constraint has reached a new equilibrium where extraction is stable but not accelerating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_cost_allocation_ambiguity,
    'Are the compliance costs (HR infrastructure, legal liability insurance, OSHA remediation, ADA accommodation) genuine transaction costs of legal alignment, or extractive overhead that concentrates on Congress while beneficiaries (staff, public) gain protections without bearing costs?',
    'Comparative cost analysis: compare Congressional compliance burden to private-sector firms of similar size; measure whether cost allocation follows proportional benefit distribution',
    'If genuine coordination cost: Tangled Rope classification holds. If extractive overhead: reclassify to higher ε Snare from Congressional perspective. If unequally borne: reclassify toward Tangled Rope with high suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_allocation_ambiguity, empirical, 'Whether compliance costs represent coordination overhead or extractive burden').

omega_variable(
    institutional_exemption_legitimacy,
    'Was Congress''s prior exemption from employment law a justified separation-of-powers principle or an indefensible privilege?',
    'Jurisprudential analysis of separation of powers doctrine; comparative review of exemptions in other democratic systems; empirical study of whether prior exemption enabled legislative independence or merely avoided accountability',
    'If exemption was justified: constraint is extractive violation of proper institutional separation. If exemption was unjustified: constraint is merely rule-of-law alignment. This determines whether the law is Snare (unjust) or Rope (just coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_exemption_legitimacy, conceptual, 'Whether prior Congressional exemption was institutionally justified').

omega_variable(
    legislative_function_preservation,
    'Does compliance burden (administrative time, legal resource allocation) materially impair Congressional legislative capacity, or is the burden manageable within existing institutional resources?',
    'Empirical measurement: track legislative output (bills introduced, hearings held, committee meetings) before and after law implementation; measure staff time allocation to compliance vs legislative functions',
    'If burden materially impairs capacity: suppression increases, classification shifts toward Snare. If burden is manageable: suppression decreases, Tangled Rope classification holds. If burden improves legislative quality (e.g., by reducing time on harassment litigation): classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_function_preservation, empirical, 'Impact of compliance burden on legislative function').

omega_variable(
    exemption_reversal_threshold,
    'What political conditions would trigger Congressional action to exempt itself again, or modify the law to reduce compliance burden?',
    'Political monitoring: track voting patterns on amendments, repeal efforts, exemption carve-outs; measure Congressional satisfaction metrics; document formal repeal/modification proposals',
    'If threshold is low (easily triggered by modest burden): constraint is maintained by external enforcement rather than internalized commitment — higher suppression. If threshold is high (Congress chooses constraint despite burden): internalized norm — lower suppression, Rope classification more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_reversal_threshold, empirical, 'Political threshold for exemption reversal').

omega_variable(
    false_summit_natural_law,
    'Is the ''rule of law requires equal standing'' principle a genuine natural law of democratic governance, or a constructed institutional choice that benefits some agents (staff, public accountability advocates) while burdening others (Congress)?',
    'Comparative institutional analysis: examine democratic systems with different approaches to legislative exemptions; philosophical analysis of rule-of-law requirements; empirical assessment of whether equal legal standing is necessary for democratic function or merely one design choice among several',
    'If natural law: mountain classification correct. If constructed: false summit — reclassify to Tangled Rope or Snare depending on ε and suppression. If contingent design choice: shifts the narrative from ''inevitable principle'' to ''deliberate institutional constraint with real distributional effects.''',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether equal legal standing is a natural law or constructed institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1995_clinton_congressional_accountability_law, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_baseline, sotu_1995_clinton_congressional_accountability_law, theater_ratio, 0, 0.22).
narrative_ontology:measurement(theater_t2_mid, sotu_1995_clinton_congressional_accountability_law, theater_ratio, 2, 0.38).
narrative_ontology:measurement(theater_t5_current, sotu_1995_clinton_congressional_accountability_law, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(extract_t0_baseline, sotu_1995_clinton_congressional_accountability_law, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(extract_t2_mid, sotu_1995_clinton_congressional_accountability_law, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(extract_t5_current, sotu_1995_clinton_congressional_accountability_law, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(extract_t7_plateau, sotu_1995_clinton_congressional_accountability_law, base_extractiveness, 7, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1995_clinton_congressional_accountability_law, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1995_clinton_congressional_accountability_law, congressional_staffing_quality_retention).
narrative_ontology:affects_constraint(sotu_1995_clinton_congressional_accountability_law, legislative_regulatory_consistency).
narrative_ontology:affects_constraint(sotu_1995_clinton_congressional_accountability_law, executive_legislative_balance_of_power).

% DUAL FORMULATION NOTE:
% This constraint is downstream of broader equal legal standing principles but represents a distinct structural arrangement: the application of existing private-sector law to a previously exempt institution. The constraint family would include: (1) base principle (equal standing) — mountain; (2) legislative exemption pre-1995 — piton (theatrical compliance with exemption); (3) post-1995 application — tangled rope. Each has different ε and different perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
