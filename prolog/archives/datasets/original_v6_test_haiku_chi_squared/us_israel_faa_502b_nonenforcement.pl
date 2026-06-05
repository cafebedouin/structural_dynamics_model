% ============================================================================
% CONSTRAINT STORY: us_israel_faa_502b_nonenforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_israel_faa_502b_nonenforcement, []).

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
 *   constraint_id: us_israel_faa_502b_nonenforcement
 *   human_readable: Non-enforcement of US Foreign Assistance Act Section 502B for Israel
 *   domain: geopolitical/human_rights_enforcement
 *
 * SUMMARY:
 *   Section 502B of the US Foreign Assistance Act (1976) establishes a
 *   statutory prohibition on military and security assistance to any country
 *   that the Secretary of State determines has engaged in a 'consistent
 *   pattern of gross violations of internationally recognized human rights.'
 *   The statute mandates suspension of assistance unless the President
 *   certifies to Congress that 'extraordinary circumstances' exist. For over
 *   four decades, this prohibition has been systematically non-enforced for
 *   Israel, despite documented allegations of settlement expansion,
 *   administrative detention, and civilian casualties in occupation and
 *   security operations. This non-enforcement functions as a structural
 *   constraint at multiple scales: (1) it extracts compliance costs from
 *   Palestinian civilians and international human rights institutions while
 *   extracting legitimacy rents for the Israeli security establishment; (2)
 *   it coordinates US-Israel strategic alignment while suppressing
 *   congressional accountability; (3) it performs a democratic ritual (formal
 *   human rights reviews) whose outcomes are predetermined; (4) it
 *   naturalizes contingent geopolitical choices as structural
 *   inevitabilities. The constraint exhibits the full spectrum of DR types
 *   depending on the observer's structural position.
 *
 * KEY AGENTS:
 *   - Israeli Security Establishment: Primary beneficiary (institutional/arbitrage) — receives unconditional military aid enabling long-term occupation and settlement security operations; derives arbitrage from US strategic interest in regional stability anchor
 *   - Palestinian Civilian Population: Primary victim (powerless/trapped) — subject to security operations funded by protected aid stream; no formal standing in US legislative process; cannot exit occupation structure
 *   - US Executive Branch: Co-beneficiary (institutional/arbitrage) — experiences non-enforcement as coordination mechanism maintaining regional alignment without domestic political cost of formal accountability; derives arbitrage from framing conditionality as strategically damaging
 *   - US Congress / Oversight Actors: Structurally constrained (organized/constrained) — nominally authorized to enforce Section 502B but face concentrated domestic lobbying, executive resistance, and reputational risk; derive political cover by avoiding enforcement votes
 *   - International Human Rights Bodies: Secondary victim (moderate/constrained) — constrained by diplomatic pressure and resource limits; benefit from documentation access but face retaliation risk; caught between mandate and survival
 *   - State Department Human Rights Bureau: Performative actor (institutional/arbitrage) — produces formal human rights assessments and Section 502B memoranda that are overridden by national security waivers; maintains ritual despite decoupling from outcomes
 *   - Realist International Relations Community: Analytical observer — risks naturalizing contingent power structure (US needs Israel as regional ally) as structural inevitability (great-power competition requires tolerating violations)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, 0.58).
domain_priors:suppression_score(us_israel_faa_502b_nonenforcement, 0.72).
domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_israel_faa_502b_nonenforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_israel_faa_502b_nonenforcement, tangled_rope).
narrative_ontology:human_readable(us_israel_faa_502b_nonenforcement, "Non-enforcement of US Foreign Assistance Act Section 502B for Israel").
narrative_ontology:topic_domain(us_israel_faa_502b_nonenforcement, "geopolitical/human_rights_enforcement").

domain_priors:requires_active_enforcement(us_israel_faa_502b_nonenforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_israel_faa_502b_nonenforcement, defense_contractors).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, palestinian_civilian_population).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, international_human_rights_regime).
narrative_ontology:constraint_victim(us_israel_faa_502b_nonenforcement, congressional_oversight_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS (SNARE) — Trapped within occupation and settlement expansion funded by unconditional US military aid. No exit option; no formal standing in US legislative process. Bears full cost of non-enforcement (security operations enabled by protected weapon systems). d≈0.94, f(d)≈1.41, σ=0.9 → χ≈0.74.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INT'L HUMAN RIGHTS BODIES (TANGLED ROPE) — Constrained by diplomatic pressure and resource limitations; benefit from detailed reporting access and institutional legitimacy that Section 502B enforcement would provide. Caught between documentation mandate and retaliation risk. d≈0.68, f(d)≈1.02, σ=1.1 → χ≈0.65.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ISRAELI SECURITY ESTABLISHMENT (ROPE) — Primary beneficiary. Experiences non-enforcement as coordination: uninterrupted weapons supply enables strategic planning and operational capability. Derives arbitrage from geopolitical leverage (US strategic interest in Middle East stability). d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary (negative effective extraction).
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US EXECUTIVE BRANCH (ROPE) — Primary beneficiary. Experiences non-enforcement as coordination mechanism solving Middle East alignment problem: unconditional support maintains Israel as regional military anchor without domestic political cost of formal accountability. Derives arbitrage from claim that conditioning aid would fracture bilateral relationship. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONGRESS / OVERSIGHT ACTORS (TANGLED ROPE) — Organized but structurally constrained. Nominally authorized to enforce Section 502B (coordination function), but face concentrated domestic lobbying, executive non-cooperation, and reputational cost (labeled anti-Israel). Derive some benefit from political cover (can claim support for Israel while avoiding enforcement votes). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE DEPT HUMAN RIGHTS REVIEW (PITON) — Performative compliance mechanism. State Department issues human rights reports and Section 502B justification memoranda, but the ritual has become decoupled from aid decisions. Theater ratio=0.68: detailed human rights assessments are produced; waivers and security memoranda override them without visible contradiction. Institutional inertia maintains the review process despite known non-enforcement. d≈0.12, f(d)≈-0.04, σ=1.0 → χ≈-0.003.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN) — Frames non-enforcement as immutable feature of great-power competition: in a multipolar world, strategic allies are tolerated human rights violators because geopolitical stability requires it. This perspective naturalizes contingent power structure as structural law. ε would be misclassified as ≤0.25 under this lens; actual ε=0.58 indicates this is a false summit — the arrangement is contingent institutional choice, not geopolitical necessity.
constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_israel_faa_502b_nonenforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_israel_faa_502b_nonenforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_israel_faa_502b_nonenforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_israel_faa_502b_nonenforcement, TR),
    TR >= 0.70.

:- end_tests(us_israel_faa_502b_nonenforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately-high. The non-enforcement generates significant asymmetric benefits: Israeli security establishment captures unconditional access to advanced weapon systems without meaningful accountability; Palestinians bear security costs without representation in the mechanism producing aid decisions. However, extractiveness is not maximal (>0.70) because (1) the constraint maintains some democratic appearance (human rights reviews are conducted), (2) it operates through institutional channels rather than pure coercion, and (3) some coalition-building cost is borne by beneficiaries (sustained lobbying and narrative management required). Suppression (0.72): High. Formal barriers to enforcement include: executive national security certification authority (can override Section 502B via waiver); classification of human rights assessments (limiting public scrutiny); concentrated domestic lobbying advantage (AIPAC, organized Jewish American support) vs dispersed cost-bearing (Palestinian civilians lack domestic political voice); media narrative control framing conditionality as antisemitic. These barriers are substantial but not absolute — Congress retains formal power and public opinion shows growing support for conditionality. Theater ratio (0.68): High. State Department produces detailed annual human rights reports on Israel and prepares Section 502B certification memoranda, but these documents systematically reach predetermined conclusions (non-violation findings) that contradict independent human rights assessments. The review process has become ritualized — it performs accountability without delivering it. Theater has increased over the interval as documentation of human rights concerns has grown while certification findings remain unchanged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival divergence in the corpus. Palestinian civilians classify this as a snare: they are trapped within a system they cannot exit or influence, bearing extraction costs with no remedy. Congressional oversight actors see a tangled rope: they have nominal authority to enforce but face structural constraints (executive power, lobbying concentration, reputational risk) that make enforcement costly. The Israeli security establishment sees a rope: unconditional aid enables strategic coordination without accountability burden. The US executive sees a rope: non-enforcement solves the regional stability problem while avoiding domestic political cost. International human rights bodies see a tangled rope: they derive legitimacy from documenting violations but constrained by diplomatic pressure from enforcement. The State Department human rights bureau sees a piton: they maintain the review ritual despite knowing its predetermined outcome. The realist analyst risks seeing a mountain: viewing non-enforcement as immutable feature of great-power competition. The engine's false summit detector reveals this last perspective as naturalizing what is actually a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian civilians: Victim + trapped → d≈0.94, f(d)≈1.41. Maximum extraction directionality. These agents bear the cost of operations funded by protected aid; they have no formal standing in the US system that governs the aid; they cannot exit the occupation structure. Israeli security establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Minimum extraction directionality (net beneficiary). They receive unconditional access to advanced weapons; they can exit the constraint (and claim strategic necessity) if it becomes politically costly. US executive: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Near-minimum. They derive geopolitical stability benefit and can rationalize non-enforcement through national security framing. Congress: Victim + constrained → d≈0.55, f(d)≈0.75. Moderate extraction. Nominally authorized to enforce but face substantial costs (executive non-cooperation, domestic lobbying, reputational damage) to exercise that authority. They are partially trapped (cannot easily override executive) but have some exit (can vote to enforce despite costs). International human rights bodies: Victim + constrained → d≈0.68, f(d)≈1.02. They derive institutional legitimacy from documentation but constrained by retaliation risk and power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy (the apparent incoherence of 'forced coordination extraction') by revealing that non-enforcement combines genuine coordination function (maintaining US-Israel strategic alignment, solving Middle East region-balancing problem) with genuine extraction (Palestinians bear uncompensated cost; congressional authority is suppressed; international human rights norms are undermined). The tangled rope classification is structurally correct: the constraint simultaneously (1) coordinates US and Israeli interests through information sharing and resource flows (coordination), (2) requires active suppression of the statutory enforcement mechanism via executive certification authority and lobbying concentration (extraction enforcement), and (3) generates asymmetric benefits (Israeli security + US strategic positioning) and asymmetric costs (Palestinian security operations + congressional oversight degradation). The perspectival gap between the beneficiary view (rope) and victim view (snare) does not resolve into a single type — it reveals that different agents genuinely experience different constraints. The constraint is not 'really' a rope or 'really' a snare; it is tangled: coordination and extraction are simultaneously structural. This is the diagnostic signature of mandatrophy resolution via indexical analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_enforcement_feasibility,
    'Would formal Section 502B enforcement (conditioning aid on specific human rights reforms) actually modify Israeli security conduct, or is military strategy path-dependent regardless of aid conditionality?',
    'Comparative analysis of other US aid conditions (Egypt, Morocco, Philippines) and their behavioral impact; game-theoretic modeling of Israeli strategic options if aid were conditioned',
    'If feasible: non-enforcement represents pure extraction/political choice. If infeasible: constraint reframes as coordination problem (US protecting strategic asset through diplomatic fiction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_enforcement_feasibility, empirical, 'Whether enforcement would change Israeli security behavior').

omega_variable(
    definition_consistency_violation,
    'Does the State Department''s annual human rights certification for Israel meet the statutory definition of ''consistent pattern of gross violations'' in any objective reading of the available evidence?',
    'Legal analysis by independent human rights attorneys; comparison of Israel''s classification against State Department standards applied to other countries; longitudinal consistency in terminology across administrations',
    'If objective violation evident: non-enforcement is explicit legal breach masquerading as policy discretion. If definition ambiguous: enforcement becomes political question, not legal question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_consistency_violation, conceptual, 'Statutory definition compliance for Section 502B').

omega_variable(
    domestic_coalition_mutability,
    'Is congressional non-enforcement driven by genuine popular support for unconditional aid, or by concentrated lobbying advantage and media narrative control?',
    'Public opinion polling on conditional vs unconditional aid; campaign contribution analysis; media coverage pattern analysis; voting patterns on Section 502B amendments',
    'If support genuine: constraint reflects authentic democratic preference (reframes from snare to rope). If driven by concentration: constraint is pure extraction masked by democratic appearance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_coalition_mutability, empirical, 'Whether congressional non-enforcement reflects popular will').

omega_variable(
    strategic_substitution_alternative,
    'Could the US achieve equivalent Middle East regional stability through conditional aid, security partnerships with other actors, or multilateral frameworks without unconditional Israel support?',
    'Strategic alternatives analysis; game-theoretic modeling of Middle East alignment under conditional aid scenario; retrospective analysis of periods when US leverage was higher',
    'If alternatives viable: non-enforcement is choice, not necessity. If genuine unique monopoly: constraint reframes toward structural inevitability (mountain-adjacent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_substitution_alternative, empirical, 'Viability of strategic alternatives to unconditional support').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_israel_faa_502b_nonenforcement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(faa502b_tr_t0, us_israel_faa_502b_nonenforcement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(faa502b_tr_t15, us_israel_faa_502b_nonenforcement, theater_ratio, 15, 0.52).
narrative_ontology:measurement(faa502b_tr_t30, us_israel_faa_502b_nonenforcement, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(faa502b_be_t0, us_israel_faa_502b_nonenforcement, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(faa502b_be_t15, us_israel_faa_502b_nonenforcement, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(faa502b_be_t30, us_israel_faa_502b_nonenforcement, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_israel_faa_502b_nonenforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, us_human_rights_certification_regime).
narrative_ontology:affects_constraint(us_israel_faa_502b_nonenforcement, israel_occupation_settlement_legitimacy).

% DUAL FORMULATION NOTE:
% Non-enforcement of Section 502B operates at the intersection of three structurally distinct claims: (1) the statutory prohibition itself (whether it is binding as written), (2) the interpretation of 'consistent pattern' (whether evidence meets threshold), and (3) the presidential waiver authority (whether it can override statutory prohibition). These are separate constraints in the family; this story models the meta-constraint (the de facto decision to not enforce), which is downstream of interpretation disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_israel_faa_502b_nonenforcement, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
