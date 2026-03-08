% ============================================================================
% CONSTRAINT STORY: arbitrary_enforcement_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arbitrary_enforcement_extraction, []).

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
 *   constraint_id: arbitrary_enforcement_extraction
 *   human_readable: Arbitrary Enforcement Extraction
 *   domain: authority_dynamics/enforcement_systems
 *
 * SUMMARY:
 *   Arbitrary enforcement extraction occurs when enforcement systems combine
 *   high suppression (limited exit options for subjects) with selective or
 *   unpredictable application of rules. The structural signature is chi sign
 *   inversion: enforcers experience negative effective extraction (the system
 *   subsidizes them) while subjects experience chi > 1.0 (extraction exceeds
 *   the base rate due to unpredictability costs). This constraint
 *   demonstrates maximum indexical divergence — the same institutional
 *   arrangement appears as coordination (rope) from the enforcer position and
 *   pure extraction (snare) from the subject position. The arbitrariness is
 *   not incidental but structural: selective enforcement allows the
 *   institution to extract compliance, resources, and deference without
 *   bearing the cost of universal application. Subjects respond with
 *   satisficing behavior — over-compliance to create safety margins — which
 *   imposes costs far exceeding what consistent enforcement would require.
 *   The constraint has degraded over the interval: theater_ratio has
 *   increased from 0.40 to 0.65 as enforcement has become more performative
 *   (ritualized compliance checks, documentation theater) while actual
 *   rule-of-law has eroded. Base extractiveness has increased from 0.58 to
 *   0.78 as selective targeting has intensified and exit options have been
 *   suppressed.
 *
 * KEY AGENTS:
 *   - Trapped Subjects: Primary victims (powerless/trapped) — face maximum extraction with no exit; arbitrary enforcement forces satisficing over-compliance
 *   - Institutional Enforcers: Primary beneficiaries (institutional/arbitrage) — experience negative chi; selective enforcement provides discretionary power and resource access
 *   - Constrained Intermediaries: Mixed position (moderate/constrained) — benefit when they can invoke enforcement, bear extraction when targeted; exit costly but possible
 *   - Targeted Populations: Secondary victims (moderate/constrained at local scope) — systematically selected for disproportionate enforcement; exit possible but costly
 *   - Reform Coalition: Organized agents (organized/mobile) — building alternative rule-of-law structures with generational sunset hypothesis
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing arbitrary enforcement as inherent to authority systems (false summit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arbitrary_enforcement_extraction, 0.78).
domain_priors:suppression_score(arbitrary_enforcement_extraction, 0.88).
domain_priors:theater_ratio(arbitrary_enforcement_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arbitrary_enforcement_extraction, extractiveness, 0.78).
narrative_ontology:constraint_metric(arbitrary_enforcement_extraction, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(arbitrary_enforcement_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arbitrary_enforcement_extraction, snare).
narrative_ontology:human_readable(arbitrary_enforcement_extraction, "Arbitrary Enforcement Extraction").
narrative_ontology:topic_domain(arbitrary_enforcement_extraction, "authority_dynamics/enforcement_systems").

domain_priors:requires_active_enforcement(arbitrary_enforcement_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arbitrary_enforcement_extraction, institutional_enforcers).
narrative_ontology:constraint_beneficiary(arbitrary_enforcement_extraction, enforcement_apparatus).
narrative_ontology:constraint_victim(arbitrary_enforcement_extraction, trapped_subjects).
narrative_ontology:constraint_victim(arbitrary_enforcement_extraction, targeted_populations).
narrative_ontology:constraint_victim(arbitrary_enforcement_extraction, compliance_bearing_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED SUBJECT (SNARE) — Faces maximum extraction with no exit. Arbitrary enforcement means compliance offers no protection while non-compliance guarantees punishment. The unpredictability itself is the extraction mechanism: subjects must over-comply (satisficing behavior) to create safety margins against arbitrary application, bearing costs far exceeding what consistent enforcement would require. Suppression is near-total — geographic, economic, or legal barriers prevent exit, and the enforcement apparatus actively suppresses alternatives.
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INSTITUTIONAL ENFORCER (ROPE) — Experiences negative effective extraction. The enforcement system provides institutional power, resource access, and discretionary authority. Arbitrariness is not a bug but a feature: selective enforcement allows the institution to reward allies, punish enemies, and extract compliance without bearing the cost of universal application. The enforcer sees coordination: the system 'works' by giving them the tools to manage their domain. Chi sign inversion — what the subject experiences as maximum extraction, the enforcer experiences as subsidy.
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSTRAINED INTERMEDIARY (TANGLED ROPE) — Agents with partial power (local officials, community leaders, small business owners) experience both extraction and coordination. They benefit from the system when they can invoke enforcement selectively to manage competitors or secure compliance, but they also bear extraction when they become targets. Exit is costly but possible — relocation, career change, or shifting to informal economy. The arbitrariness creates genuine coordination problems (unpredictable costs) layered with extraction (selective targeting based on political alignment or bribe payment).
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: TARGETED POPULATION (SNARE) — Moderate power agents who are systematically targeted by selective enforcement experience snare classification even with constrained rather than trapped exit. The selectivity concentrates extraction: enforcement is applied disproportionately to this group while others face minimal scrutiny. Exit is structurally possible but costly (relocation, identity concealment, economic loss). The immediate time horizon reflects that each enforcement interaction is a crisis — the biographical view might show tangled rope as some coordination exists, but moment-to-moment the experience is pure extraction.
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized agents working toward rule-of-law reforms see the arbitrary enforcement system as a temporary problem with a sunset. Civil society organizations, legal reform movements, and accountability mechanisms are building alternative structures: transparent enforcement criteria, judicial review, complaint mechanisms, and documentation systems. The coalition has mobility (can operate across jurisdictions, build international pressure, shift resources) and sees a generational path to replacing arbitrary enforcement with rule-bound systems. Effective extraction is low because the coalition has agency and a theory of change.
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT) — From a civilizational perspective, some observers naturalize arbitrary enforcement as an inherent feature of authority systems: 'power is always discretionary,' 'enforcement is always selective,' 'law is politics by other means.' This perspective treats the constraint as immutable — a law of political physics. However, the structural data contradicts this: the high suppression (0.88) and extractiveness (0.78) are not inherent to enforcement systems but to THIS configuration. Rule-of-law systems with transparent criteria, judicial review, and accountability mechanisms have dramatically lower extraction. The mountain classification is a false summit — naturalizing a contingent institutional arrangement as a universal law.
constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arbitrary_enforcement_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arbitrary_enforcement_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arbitrary_enforcement_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arbitrary_enforcement_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The base extraction rate reflects direct costs (fines, bribes, compliance expenditure) plus the satisficing premium — subjects over-comply to create safety margins against unpredictable enforcement. The unpredictability itself is an extraction mechanism: it forces subjects to bear costs that consistent enforcement would not impose. Suppression (0.88): Very high. Exit options are severely limited by geographic barriers (enforcement is territorial), economic dependency (subjects cannot afford relocation), legal barriers (enforcement apparatus controls movement or licensing), and active suppression of alternatives (informal economy is criminalized, mutual aid is disrupted). Theater ratio (0.65): Moderate-high and increasing. Enforcement has become increasingly performative: ritualized compliance checks that do not improve actual rule adherence, documentation requirements that serve no functional purpose, and visible enforcement actions that signal power rather than address violations. The theater serves the extraction: it makes the enforcement apparatus appear rule-bound while maintaining arbitrary discretion. Measurements show degradation: theater has increased from 0.40 to 0.65 as the system has shifted from functional enforcement to performative control. Extractiveness has increased from 0.58 to 0.78 as selective targeting has intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum indexical divergence. The institutional enforcer sees rope — the system coordinates their domain management and provides tools for control. The trapped subject sees snare — pure extraction with no exit and no benefit. The constrained intermediary sees tangled_rope — mixed coordination and extraction depending on whether they are invoking or targeted by enforcement. The reform coalition sees scaffold — a temporary problem being solved by rule-of-law reforms. The analytical observer risks seeing mountain — naturalizing arbitrary enforcement as inherent to authority. The gap is not a measurement error but the structural reality: the same institutional arrangement produces radically different experiences depending on the agent's position in the power geometry. The chi sign inversion (enforcer negative, subject > 1.0) is the quantitative signature of this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi sign inversion is the diagnostic signature of this constraint. Institutional enforcers are beneficiaries with arbitrage exit options — they derive d ≈ 0.05, producing f(d) ≈ -0.12, yielding negative chi (the system subsidizes them). Trapped subjects are victims with no exit — they derive d ≈ 0.95, producing f(d) ≈ 1.42, yielding chi > 1.0 when combined with scope modifier and satisficing premium. The structural asymmetry is extreme: what flows toward the enforcer (discretionary power, resource access, immunity from rules) flows away from the subject (compliance costs, unpredictability burden, suppressed alternatives). Constrained intermediaries occupy the middle: they are sometimes beneficiaries (when they invoke enforcement) and sometimes victims (when they are targeted), producing d ≈ 0.55 and moderate chi. The reform coalition, despite being organized, experiences low chi because they have exit options (mobile) and operate at generational time horizons where the constraint appears temporary. The analytical observer's mountain classification derives from naturalizing the constraint, not from low experienced extraction — it is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The high extractiveness (0.78) and extreme perspectival gap require explicit mandatrophy analysis to prevent mislabeling. The constraint is NOT pure extraction from all perspectives — it is a snare from the subject position and rope from the enforcer position. The classification 'snare' reflects the claimed_type (the subject's structural reality) but the full presheaf includes the enforcer's rope and the intermediary's tangled_rope. The mandatrophy is resolved by recognizing that the chi sign inversion is the constraint's defining feature: extraction is not evenly distributed but concentrated on trapped agents while benefiting institutional agents. The system is extractive in aggregate (high base ε) but the extraction flow is directional, not universal. The reform coalition's scaffold perspective is structurally real if the sunset hypothesis is credible (omega variable: reform_sunset_credibility). The analytical observer's mountain is a false summit — the constraint is not immutable but contingent on institutional arrangements that suppress exit and enable selective enforcement. Rule-of-law systems with transparent criteria and judicial review have dramatically lower extraction, proving the constraint is not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_selectivity_threshold,
    'At what level of enforcement selectivity does the system transition from ''discretion within rules'' to ''arbitrary extraction''?',
    'Quantitative analysis of enforcement patterns: variance in application rates across demographically similar populations; correlation between enforcement targeting and political/economic factors unrelated to rule violation; comparison of stated criteria vs actual application',
    'If threshold is low (selectivity > 20% variance): many discretionary systems misclassified as arbitrary. If threshold is high (selectivity > 80% variance): extractive systems evade detection by maintaining minimal formal compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_threshold, empirical, 'Selectivity threshold distinguishing discretion from arbitrary extraction').

omega_variable(
    satisficing_cost_measurement,
    'How do we measure the excess compliance cost imposed by unpredictability vs the cost of compliance under consistent enforcement?',
    'Behavioral economics analysis: comparison of compliance expenditure in arbitrary vs rule-bound systems controlling for rule stringency; measurement of safety-margin over-compliance; time-series analysis of compliance costs before and after enforcement regime changes',
    'If satisficing costs are low: extractiveness estimate is too high. If satisficing costs are high: subjects bear hidden extraction not captured in direct enforcement interactions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(satisficing_cost_measurement, empirical, 'Measurement of excess compliance costs from unpredictability').

omega_variable(
    reform_sunset_credibility,
    'Is the reform coalition''s sunset hypothesis structurally credible, or is it aspirational framing that masks a stable equilibrium?',
    'Historical analysis of enforcement reform trajectories: success rates of rule-of-law transitions; identification of structural preconditions (independent judiciary, free press, civil society capacity); assessment of whether those preconditions exist in this case',
    'If sunset is credible: scaffold classification confirmed, and the constraint is genuinely temporary. If sunset is aspirational: the organized perspective should reclassify as tangled_rope (coordination around reform activity that itself becomes institutionalized without changing the underlying enforcement system).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_credibility, conceptual, 'Credibility of rule-of-law reform sunset hypothesis').

omega_variable(
    chi_sign_inversion_mechanism,
    'Is the chi sign inversion (enforcer experiences negative extraction, subject experiences chi > 1.0) a stable structural feature or an artifact of incomplete accounting?',
    'Full cost accounting for enforcers: institutional maintenance costs, corruption overhead, legitimacy erosion, long-term career risk from regime change. If these costs are included, does enforcer chi remain negative?',
    'If inversion is stable: the structural asymmetry is real and the system is a pure transfer mechanism. If inversion is artifact: enforcers bear hidden costs (reputational, long-term risk) that reduce their net benefit, and the system may be less extractive than it appears from immediate transactions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chi_sign_inversion_mechanism, empirical, 'Stability of chi sign inversion between enforcer and subject').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arbitrary_enforcement_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arb_enf_tr_t0, arbitrary_enforcement_extraction, theater_ratio, 0, 0.4).
narrative_ontology:measurement(arb_enf_tr_t3, arbitrary_enforcement_extraction, theater_ratio, 3, 0.52).
narrative_ontology:measurement(arb_enf_tr_t6, arbitrary_enforcement_extraction, theater_ratio, 6, 0.6).
narrative_ontology:measurement(arb_enf_tr_t10, arbitrary_enforcement_extraction, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(arb_enf_be_t0, arbitrary_enforcement_extraction, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(arb_enf_be_t3, arbitrary_enforcement_extraction, base_extractiveness, 3, 0.66).
narrative_ontology:measurement(arb_enf_be_t6, arbitrary_enforcement_extraction, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(arb_enf_be_t10, arbitrary_enforcement_extraction, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arbitrary_enforcement_extraction, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of environmental_instability_as_constraint (which creates the conditions where arbitrary enforcement can persist — subjects cannot coordinate resistance when environmental instability fragments collective action) and exclusionary_coordination_asymmetry (which establishes the institutional power differential that enables selective enforcement). The arbitrary enforcement constraint has its own extractiveness (0.78) reflecting the satisficing premium and suppression costs, distinct from the upstream constraints' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
