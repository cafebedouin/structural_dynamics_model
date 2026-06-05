% ============================================================================
% CONSTRAINT STORY: juvenile_underclass_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_juvenile_underclass_2026, []).

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
 *   constraint_id: juvenile_underclass_2026
 *   human_readable: The Minor Underclass Structural Constraint
 *   domain: social/political
 *
 * SUMMARY:
 *   Minors are defined by a complete absence of formal political agency and
 *   economic self-determination. They cannot vote, hold office, enter binding
 *   contracts, own property independently, choose residence, decline parental
 *   direction, or consent to their own treatment by institutions. This
 *   structural underclass status is universal across jurisdictions and has
 *   remained nearly invariant for centuries despite significant variation in
 *   child welfare outcomes. The constraint creates a permanent extraction
 *   mechanism: parents, institutions, and the state capture control over
 *   children's time, labor, property, and decisions, justified by
 *   developmental rhetoric but exceeding what developmental science requires.
 *   The constraint exhibits all six DR types from different perspectives,
 *   revealing a fundamental tension between developmental protection
 *   (genuine) and institutional control for adult benefit (excessive). The
 *   theater_ratio (0.64) reflects that much of the legal apparatus protecting
 *   minors is substantially performative: age-of-majority thresholds do not
 *   align with actual competence; parental authority persists despite
 *   variation in parental capacity; child welfare systems combine genuine
 *   protection with institutional coercion. The constraint's 10-year
 *   measurement interval shows slight increases in extractiveness (0.52→0.58)
 *   and theater_ratio (0.58→0.64), consistent with increasing bureaucratic
 *   control and performative protection mechanisms even as child welfare
 *   advocacy grows.
 *
 * KEY AGENTS:
 *   - Children/Minors: Primary victim (powerless/trapped) — zero agency, complete dependence, no exit until majority
 *   - Parents/Legal Guardians: Primary beneficiary (institutional/arbitrage) — exclusive decision rights, economic benefit from child labor and co-residence, cultural authority
 *   - State Institutional Authority: Institutional beneficiary (institutional/constrained) — control over compulsory education, healthcare decisions, institutional placement; secondary extraction through judicial, educational, and correctional systems
 *   - Child Welfare System: Mixed actor (moderate/constrained) — coordination function (abuse prevention, basic welfare) but also extraction mechanism (institutionalization, behavioral control, state custody)
 *   - Children's Rights Movement: Organized reformer (organized/constrained) — seeking graduated agency expansion; sees sunset path through rights legislation and development thresholds
 *   - Educational/Correctional Institutions: Institutional actors (institutional/arbitrage) — benefit from compulsory attendance and behavioral control; perform custodial extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional control as developmental necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(juvenile_underclass_2026, 0.58).
domain_priors:suppression_score(juvenile_underclass_2026, 0.78).
domain_priors:theater_ratio(juvenile_underclass_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(juvenile_underclass_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(juvenile_underclass_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(juvenile_underclass_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(juvenile_underclass_2026, snare).
narrative_ontology:human_readable(juvenile_underclass_2026, "The Minor Underclass Structural Constraint").
narrative_ontology:topic_domain(juvenile_underclass_2026, "social/political").

domain_priors:requires_active_enforcement(juvenile_underclass_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, legal_guardians).
narrative_ontology:constraint_beneficiary(juvenile_underclass_2026, institutional_authority).
narrative_ontology:constraint_victim(juvenile_underclass_2026, children_minors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHILD SUBJECT (SNARE) — Minors have zero formal political agency, cannot enter contracts, cannot own property independently, cannot choose residence, cannot refuse parental/state direction. Exit is impossible until age of majority. d≈0.98, f(d)≈1.41, σ=1.2 → χ≈0.98. Maximal extraction with zero alternatives.
constraint_indexing:constraint_classification(juvenile_underclass_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CUSTODIAL AUTHORITY (ROPE) — Parents/guardians experience the minor constraint as a coordination mechanism: legitimate authority to guide development, allocate resources, make decisions in the child's interest. Beneficiary from legal framework that grants exclusive decision rights. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.08. Net beneficiary.
constraint_indexing:constraint_classification(juvenile_underclass_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE CHILD WELFARE SYSTEM (TANGLED ROPE) — State institutions tasked with protecting minors while also controlling them. System has coordination function (ensure basic welfare, prevent abuse) but also extraction mechanism (institutionalization, state custody without consent, behavioral control). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55. Mixed coordination and coercion.
constraint_indexing:constraint_classification(juvenile_underclass_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CHILDREN'S RIGHTS MOVEMENT (SCAFFOLD) — Organized advocates treating the minor constraint as a temporary institutional error with a sunset path. The movement seeks expansion of child agency through graduated legal rights (voice in educational decisions, bodily autonomy protections, economic self-determination thresholds). d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.35. Lower extraction because the movement has institutional momentum and sees a clear exit pathway through rights expansion.
constraint_indexing:constraint_classification(juvenile_underclass_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE AGE-OF-MAJORITY LEGAL FICTION (PITON) — The legal threshold at which minors transition to adults (18 or 21) is substantially performative. Cognitive development, economic capacity, and decision-making capability vary widely within age cohorts and do not align cleanly with statutory boundaries. The age-of-majority persists through institutional inertia despite poor functional alignment with actual maturity. theater_ratio=0.64 reflects this: the ritual of legal transition masks ongoing economic dependence, cognitive development variance, and contingent life circumstances. χ low because the piton is decaying.
constraint_indexing:constraint_classification(juvenile_underclass_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / DEVELOPMENTAL NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some dependency is intrinsic to human development: children require years of biological and cognitive maturation before acquiring full agency. The constraint appears to be a natural law of human ontogeny. However, the structural data (ε=0.58, suppression=0.78, theater=0.64, beneficiaries + victims declared) contradicts mountain classification. This is a FALSE SUMMIT: developmental necessity is being conflated with institutional extraction. The degree of suppression, duration of powerlessness, and legal/economic mechanisms of control exceed what developmental science requires.
constraint_indexing:constraint_classification(juvenile_underclass_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(juvenile_underclass_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(juvenile_underclass_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(juvenile_underclass_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(juvenile_underclass_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(juvenile_underclass_2026, TR),
    TR >= 0.70.

:- end_tests(juvenile_underclass_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from minors: unpaid household labor, unpaid educational labor (homework, credential-building for parental status), foregone earning capacity, control over decision-making and bodily autonomy. The 0.58 value reflects that some extraction is partially justified by genuine developmental costs (education, healthcare, supervision), but the magnitude of control far exceeds developmental necessity. The extraction is not maximal (0.70+) because some constraints are genuinely protective and some minors have partial agency in some domains. Suppression (0.78): High. Minors have severely limited alternatives: they cannot work independently in most jurisdictions until age 14-16; cannot leave parental custody; cannot refuse schooling, medical treatment, or parental direction; cannot own property or enter contracts. The suppression is not maximal (0.90+) because minors can sometimes signal preferences that influence decisions, and some jurisdictions allow earlier emancipation. Theater ratio (0.64): Moderate-high. The age-of-majority legal fiction is substantially performative: a 17-year-old who manages employment, finances, and rational decision-making becomes magically competent on their 18th birthday regardless of actual development. Parent-child relationships are legally treated as uniform despite enormous variation in parental capacity, child maturity, and relationship dynamics. Child welfare systems perform protection rituals (custody hearings, home visits) with variable actual protective effect. The theater reflects institutionalization of control mechanisms that persist despite poor functional alignment with actual development or welfare outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The child experiences it as pure extraction (Snare): zero agency, zero alternatives, zero exit until adulthood. The parent experiences it as coordination (Rope): legitimate authority to guide development. The welfare system experiences it as mixed (Tangled Rope): genuine protection function but also institutional control. The children's rights movement experiences it as a reformable temporary constraint (Scaffold): graduated agency expansion is feasible and desirable. The legal system experiences its own apparatus as degraded (Piton): age-of-majority thresholds do not align with competence. The analytical observer risks seeing it as natural law (Mountain): developmental dependency seems inherent to human ontogeny. This perspectival range reveals the constraint's central paradox: it conflates genuine developmental dependency with institutional extraction. Developmental necessity and institutional control are separable — the constraint could be reformed to preserve protection while expanding agency — but the current system treats them as inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Child/minor: Victim + trapped → d≈0.98, f(d)≈1.41. Maximum extraction and zero exit. Parent/guardian: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; can exit relationship if necessary (emancipation, adult children) but holds control while minor is dependent. State institutional authority: Beneficiary + constrained → d≈0.25, f(d)≈0.10. Benefits from control and compulsory systems but faces political/legal constraints on exercise (rights movements, international conventions). Child welfare system: Victim + constrained → d≈0.60, f(d)≈0.85. Genuinely constrained by mandate to protect and also control; extraction mechanism but not pure extraction. Children's rights movement: Organized + constrained → d≈0.45, f(d)≈0.48. Lower extraction because movement has institutional momentum and clear exit path. Educational institutions: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary from compulsory attendance and behavioral control. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is false summit — naturalizes contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE: This constraint resolves mandatrophy by distinguishing developmental necessity from institutional extraction. The developmental natural law (mountain) perspective is INCORRECT — developmental dependency does NOT require the current global suppression (0.78) or extractiveness (0.58) levels. Cross-national and historical evidence shows that graduated agency expansion (early property rights, work permit autonomy, voice in education, bodily integrity) produces equal or better developmental outcomes than maximum suppression. Therefore, the mountain classification from the analytical observer is a FALSE SUMMIT: it naturalizes what is actually a contingent institutional arrangement. The constraint is a SNARE from the child's perspective (pure extraction, zero alternatives) and a ROPE from the beneficiary's perspective (coordination of legitimate authority). The TANGLED ROPE classification for the welfare system is correct: it has genuine coordination (protection) and genuine extraction (control). The SCAFFOLD classification for the children's rights movement is correct: graduated agency is viable and has a clear sunset path. The PITON classification for the legal age-of-majority fiction is correct: theater_ratio of 0.64 indicates performative ritual without functional alignment to actual competence. The SNARE is the dominant classification for the global constraint, with significant potential for reformation to TANGLED ROPE (if suppression and extraction are reduced while maintaining genuine protection) or SCAFFOLD (if graduated agency expansion is implemented with sunset timelines).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developmental_capacity_variance,
    'At what point(s) do children acquire genuine decision-making capacity across cognitive, economic, and political domains, and do these align with statutory age thresholds?',
    'Longitudinal developmental psychology studies; comparison of statutory age-of-majority against demonstrated competence in financial decisions, informed consent, bodily autonomy, and political reasoning across populations',
    'If capacity maturation is highly variable within age cohorts: current age-of-majority system is misclassified as natural law. If capacity aligns with developmental stages detectable before statutory age: the constraint could be reformulated with graduated rights (earlier agency in some domains, later in others). This would shift the constraint from Snare toward Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developmental_capacity_variance, empirical, 'Alignment between developmental capacity milestones and statutory age thresholds').

omega_variable(
    institutional_extraction_necessity,
    'How much of the suppression (0.78) is required for legitimate developmental protection versus institutional control for adult benefit?',
    'Cross-national comparison: countries/regions with graduated child agency (early property rights, work permit autonomy, educational choice, bodily integrity laws) versus those with total minor control; outcome measures on child welfare, abuse rates, adult competence, and economic mobility',
    'If high-suppression regimes show no better developmental outcomes: the extraction is institutional rent-seeking, not developmental necessity. Suppression could drop to 0.40-0.50 without harming children. If low-suppression systems show worse outcomes: some level of suppression is developmentally justified, but the current global average (0.78) may still be excessive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_necessity, empirical, 'Necessity of current suppression levels for child welfare outcomes').

omega_variable(
    economic_extraction_magnitude,
    'What fraction of child labor, unpaid household labor, and foregone earning capacity represents extractive benefit to parents/institutions versus what represents co-residence and development costs?',
    'Labor economics analysis: imputed value of child household labor; comparison against per-capita developmental support costs; analysis of parental income gains attributable to child labor versus child development investment; cross-generational mobility studies',
    'If extraction is substantial and uncompensated: extractiveness should increase toward 0.65+. If labor is genuinely developmental (skill-building, responsibility-learning with proportional support): extractiveness should decrease toward 0.35-0.45. This directly affects classification between Tangled Rope and Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_extraction_magnitude, empirical, 'Quantification of extractive benefit from child labor and dependence').

omega_variable(
    consent_counterfactual,
    'If minors had full political and economic agency from birth, which existing restrictions would persist on developmental or safety grounds versus which would be eliminated?',
    'Expert consensus among developmental psychologists, economists, and ethicists; case-based reasoning from natural experiments (early emancipation, child-as-adult legal status); counterfactual policy design',
    'If genuine developmental restrictions are small (10-20% of current constraint): the constraint is primarily extractive (Snare confirmed). If genuine restrictions are large (50%+): the constraint is hybrid coordination/extraction (Tangled Rope/Scaffold). This determines whether the constraint is fundamentally unjust or partially justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_counterfactual, conceptual, 'Counterfactual scope of developmentally justified restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(juvenile_underclass_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juv_tr_t0, juvenile_underclass_2026, theater_ratio, 0, 0.58).
narrative_ontology:measurement(juv_tr_t5, juvenile_underclass_2026, theater_ratio, 5, 0.61).
narrative_ontology:measurement(juv_tr_t10, juvenile_underclass_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(juv_be_t0, juvenile_underclass_2026, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(juv_be_t5, juvenile_underclass_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(juv_be_t10, juvenile_underclass_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(juvenile_underclass_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(juvenile_underclass_2026, compulsory_education_2026).
narrative_ontology:affects_constraint(juvenile_underclass_2026, parental_property_rights_2026).
narrative_ontology:affects_constraint(juvenile_underclass_2026, adolescent_labor_market_2026).
narrative_ontology:affects_constraint(juvenile_underclass_2026, institutional_custody_2026).

% DUAL FORMULATION NOTE:
% The minor underclass constraint is upstream of several derivative institutional mechanisms that extract value: compulsory education (enforces time allocation), parental property rights (enables economic control), adolescent labor restrictions (prevent economic self-determination), and institutional custody (state extraction through welfare/correctional systems). Each downstream constraint has its own ε and perspectives, but all depend on the parent constraint: the fundamental lack of child agency. The constraint family models how developmental necessity (real) is conflated with institutional extraction (excessive) across multiple domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(juvenile_underclass_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
