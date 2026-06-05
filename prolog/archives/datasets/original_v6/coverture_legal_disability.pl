% ============================================================================
% CONSTRAINT STORY: coverture_legal_disability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coverture_legal_disability, []).

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
 *   constraint_id: coverture_legal_disability
 *   human_readable: Coverture Legal Disability in Anglo-American Common Law
 *   domain: legal/property_rights/family_law
 *
 * SUMMARY:
 *   Coverture (or 'coverture by marriage') was the legal doctrine in
 *   Anglo-American common law that rendered a married woman a legal nonentity
 *   upon marriage, her legal existence 'covered' by the husband's. Under
 *   coverture, a wife could not own property in her own name, sign contracts
 *   independently, sue or be sued, claim her own earnings, or exercise
 *   independent legal agency. The constraint exhibits stark perspectival
 *   variation: married women experience it as a pure snare (trapped by law
 *   with no exit); married men experience it as legitimate family
 *   coordination (rope); the common law legal system maintains it
 *   performatively as institutional inertia (piton); and an analytical
 *   observer might risk naturalizing it as law-of-nature family structure
 *   (false summit mountain). The constraint's theater_ratio increases over
 *   the measurement interval (0.42 to 0.55) as the legal system becomes
 *   increasingly required to perform justifications for coverture that its
 *   own doctrine no longer permits—a diagnostic sign of institutional
 *   decline. Extractiveness decreases slightly (0.72 to 0.68) as alternative
 *   legal mechanisms (marriage contracts, women's property acts, equity
 *   courts) begin carving out exceptions, reducing the constraint's scope.
 *   The constraint is not a mountain: it is entirely institutional,
 *   contingent to Anglo-American common law (absent in civil law systems),
 *   and actively enforced through property law and contract doctrine—not a
 *   natural necessity.
 *
 * KEY AGENTS:
 *   - Married Women: Primary victims (powerless/trapped) — legally ceases to exist; cannot own property, control earnings, or exercise independent legal agency; bears full cost of legal disability with no structural exit option
 *   - Married Men: Primary beneficiaries (institutional/arbitrage) — gain exclusive control over wife's property and earnings; experience constraint as family governance rather than extraction; can exit marriage and retain all assets
 *   - Widows and Separate Traders: Secondary agents (moderate/constrained) — experience constraint as tangled rope; legal framework provides some protections (dower, widow's portion, merchant licenses) alongside extraction (limited scope, complex procedure)
 *   - Common Law Legal System: Institutional actor (institutional/arbitrage) — maintains constraint through routine enforcement; increasingly recognizes dysfunction; maintains through institutional inertia rather than active policy
 *   - Parliament and Legal Reformers: Organized agents (organized/constrained) — by 19th century, push for married women's property acts; face resistance from beneficiary interests; constrained by political power of male property owners but with growing agency to change law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coverture_legal_disability, 0.68).
domain_priors:suppression_score(coverture_legal_disability, 0.78).
domain_priors:theater_ratio(coverture_legal_disability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coverture_legal_disability, extractiveness, 0.68).
narrative_ontology:constraint_metric(coverture_legal_disability, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(coverture_legal_disability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coverture_legal_disability, snare).
narrative_ontology:human_readable(coverture_legal_disability, "Coverture Legal Disability in Anglo-American Common Law").
narrative_ontology:topic_domain(coverture_legal_disability, "legal/property_rights/family_law").

domain_priors:requires_active_enforcement(coverture_legal_disability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coverture_legal_disability, married_husbands).
narrative_ontology:constraint_victim(coverture_legal_disability, married_women).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARRIED WOMAN UNDER COVERTURE (SNARE) — Legally ceases to exist as an independent person upon marriage. Trapped by law, social expectation, economic dependency, and lack of legal recourse. Cannot own property, sign contracts, sue or be sued independently, or control her own earnings. Bears full extraction cost with no structural exit option. Suppression is maximal: the legal disability prevents escape through alternative institutional arrangements.
constraint_indexing:constraint_classification(coverture_legal_disability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARRIED MAN UNDER COVERTURE (ROPE) — Gains exclusive control over wife's property, earnings, and legal agency. Experiences the constraint as legitimate marital governance and family coordination. Net beneficiary with maximal arbitrage options — can exit marriage and retain all accumulated property. Interprets the constraint as natural marital order, not extraction.
constraint_indexing:constraint_classification(coverture_legal_disability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WIDOW OR SEPARATE TRADER (TANGLED ROPE) — Women outside the direct coverture marriage (widows with dower rights, or feme soles with merchant licenses) experience the constraint as both coordination and extraction. Legal framework provides some property protections (dower, widow's portion) while simultaneously extracting via limited scope and complicated procedure. Constrained by legal complexity and limited alternative channels, but with some genuine agency and some coordination benefit (inheritance predictability, widow support norms).
constraint_indexing:constraint_classification(coverture_legal_disability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMON LAW LEGAL SYSTEM (PITON) — By the 19th century, coverture persists as institutional inertia despite growing recognition of its injustice. The legal machinery (courts, judges, property registries) maintains the disability through routine enforcement, but the system's own elite (judges, legal scholars, married men with professional ambitions) increasingly acknowledge its dysfunction for economic coordination. The constraint is maintained performatively — judges enforce it as written law while privately supporting reform. Theater ratio high: the system expends energy on legal fictions (a man representing his wife's interests 'as her agent') rather than direct enforcement.
constraint_indexing:constraint_classification(coverture_legal_disability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a deep historical or biological perspective, some coverture structure (male headship, female economic dependency during child-rearing years) could be naturalized as inherent to human family biology and reproduction. This perspective risks seeing the constraint as emerging naturally from reproductive asymmetry. However, the structural data falsifies this: the legal disability is a contingent Anglo-American common law innovation (not present in civil law systems or earlier Germanic law), requires active enforcement through contract and property law, and suppression is entirely institutional—not biological. This is a false summit: naturalization of legal invention.
constraint_indexing:constraint_classification(coverture_legal_disability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coverture_legal_disability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coverture_legal_disability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coverture_legal_disability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coverture_legal_disability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coverture_legal_disability, TR),
    TR >= 0.70.

:- end_tests(coverture_legal_disability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The husband gains exclusive control over the wife's property, earnings, and legal agency. This is direct, unambiguous extraction of economic value and legal personhood. The value is not higher (e.g., 0.85) because the constraint exists within a framework of family obligation—husbands are legally required to provide for wives' maintenance, and courts recognize some equitable interests (equity courts begin creating exceptions). Extractiveness declines slightly over the interval as alternative legal mechanisms (marriage contracts, women's property exceptions) reduce coverture's effective scope. Suppression (0.78): Very high. Legal barriers (a woman cannot contract or sue independently), economic dependency (she cannot earn her own living through law), and social barriers (no legitimate alternative to marriage; reputation destroyed if she leaves) combine to create near-total suppression of exit options. Suppression is structural, not merely internalized—the law itself prevents escape. Theater ratio (0.55): Moderate and increasing. Early in the period, coverture is enforced straightforwardly as written law—no performance needed, simply denial of legal standing. By the 19th century, as critiques mount, the legal system must increasingly perform justifications: judges write decisions explaining how a man represents his wife's interests 'as her agent,' develop legal fictions to accommodate women's actual economic participation (trusts, marriage contracts), and create equity court exceptions. The rising theater ratio (0.42→0.55) reflects the constraint becoming harder to justify directly—it is maintained through institutional ritual and legal fictions rather than straightforward application.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The married woman sees a snare: the constraint is unchangeable from within her position, extraction is maximal, suppression is total. The married man sees rope: the constraint solves the coordination problem of family property and enables him to act as a unified legal entity representing household interests. The widow or merchant woman sees tangled rope: she benefits from some legal protections while suffering legal disability. The common law system sees piton: it maintains the constraint through performative justification (legal fictions, equity court exceptions) even as its own doctrine becomes indefensible. The analytical observer risks seeing mountain: naturalizing the constraint as an inherent feature of family structure. The perspectival gaps expose the constraint's nature: what is 'natural family order' from the beneficiary's position is 'extractive legal disability' from the victim's position. The gap is not epistemic disagreement—it is structural asymmetry in who benefits and who pays.
 *
 * DIRECTIONALITY LOGIC:
 *   Married women's directionality is near d=1.0 (full target): they are victims of the constraint with trapped exit options and powerless structural position. They derive d from victim status + trapped exit, yielding maximum f(d)≈1.42. Married men's directionality is near d=0.0 (full beneficiary): they gain exclusive control with arbitrage options (can exit marriage and retain property) and institutional power. They derive d from beneficiary status + arbitrage exit, yielding negative or near-zero f(d)≈-0.12. Widows and separate traders occupy a middle position: they are partly victims (constrained by legal disability) and partly beneficiaries (dower protections, merchant licenses provide some agency). Their directionality d≈0.50–0.65 reflects this mixed position. The common law system as institutional beneficiary has d≈0.15 (institutional + arbitrage, like the married man's structural position but with the power atom shifted). The analysis confirms: high d for trapped victims → high f(d) → high effective extraction; low d for beneficiaries with exit → low/negative f(d) → little felt extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: Coverture is a pure-extraction constraint, not a coordination mechanism or temporary scaffold. The mandatrophy is resolved by showing that any 'coordination function' (family property management, unified legal representation) is subordinate to and masks the primary extraction mechanism (appropriation of the wife's legal personhood and economic agency). The constraint requires active enforcement (through property law, contract law, court decisions denying women legal standing). It persists despite alternative mechanisms that could coordinate family property without legal disability (marriage contracts, trusts, civil law approaches without coverture). The constraint's theater_ratio increase reveals institutional decline: as coverture becomes harder to justify directly, the legal system invests energy in fictions and exceptions rather than admitting straightforward extraction. The snare classification explains why legal reform faced such resistance from beneficiaries (married men, property-owning interests) even as the constraint's functional necessity disappeared. By the 19th century, coverture was maintaining itself through institutional inertia and beneficiary resistance, not through any surviving coordination need—a diagnostic signature of snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'To what extent is the married woman''s acceptance of coverture rooted in identity fusion (''I am a wife, that is my complete identity'') versus external legal barriers?',
    'Analysis of women''s testimonies, resistance patterns, and identity-work in legal records. Observation of women''s behavior after coverture is removed or avoided: do they immediately exercise independent agency, or does internalized subordination persist?',
    'If primarily identity-locked: reclassify exit_options from ''trapped'' to ''identity_locked'' at biographical time horizon, which changes the classification to ''rope'' (agent sees changeable constraint). If primarily trapped: snare classification confirmed (agent sees unchangeable constraint). Identity lock explains why legal reform alone (removing the law) does not immediately liberate women''s agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Extent of identity fusion in married women''s acceptance of coverture').

omega_variable(
    male_coalition_consciousness,
    'Did male beneficiaries of coverture perceive themselves as a coherent extractive coalition, or was the constraint maintained through distributed individual preference and institutional inertia?',
    'Analysis of legislative debates, legal arguments, and male political organizing around coverture reform. If men organized explicitly to oppose reform, the snare was actively maintained; if reform was resisted passively through institutional weight, the classification leans toward piton.',
    'If active coalition: snare classification confirmed (extraction requires suppression of alternatives, which requires active enforcement by beneficiaries). If passive institutional weight: reclassify as piton-leaning (constraint maintained by inertia, not conscious extraction mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(male_coalition_consciousness, empirical, 'Whether male beneficiaries organized as conscious extractive coalition').

omega_variable(
    coverture_suppression_mechanism,
    'Is the high suppression (0.78) structural (legal barriers + economic dependency + lack of alternative institutions) or partly internalized (women''s socialization into acceptance of subordination)?',
    'Post-coverture reform analysis: suppression should drop sharply if legal and economic barriers are removed. Persistence of internalized suppression after barrier removal indicates cognitive/identity mechanism was significant. Comparison with cross-cultural data: do societies without legal coverture show similar female economic subordination if socialization is identical?',
    'If primarily structural: suppression accurately measured at 0.78, and removal of legal disability alone suffices for liberation. If partly internalized: measured suppression (0.78) includes institutional cost + internalized cost, and liberation requires both legal reform and identity-work. This affects interpretation of the snare mechanism: is it purely extractive institution, or does it succeed partly through cognitive capture?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coverture_suppression_mechanism, empirical, 'Structural versus internalized components of coverture suppression').

omega_variable(
    coverture_coordination_function,
    'Did coverture solve any genuine coordination problem in household resource management, or was any ''coordination function'' purely post-hoc rationalization for the extraction?',
    'Historical comparison: did common-law countries with coverture show superior household economic outcomes vs. civil-law countries without it? Analysis of legal alternatives (marriage contracts, property trusts) that achieved coordination without legal disability. If coordination benefits are separable from extraction, a tangled rope classification might be more accurate than snare.',
    'If coordination function is real and separable: reclassify from snare to tangled_rope (genuine coordination + asymmetric extraction). If coordination function is illusory or achieved by alternative means without disability: snare classification confirmed (pure extraction masked by coordination narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverture_coordination_function, empirical, 'Whether coverture provided genuine coordination benefits beyond extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coverture_legal_disability, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cov_tr_t0, coverture_legal_disability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cov_tr_t2, coverture_legal_disability, theater_ratio, 2, 0.48).
narrative_ontology:measurement(cov_tr_t4, coverture_legal_disability, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(cov_be_t0, coverture_legal_disability, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(cov_be_t2, coverture_legal_disability, base_extractiveness, 2, 0.7).
narrative_ontology:measurement(cov_be_t4, coverture_legal_disability, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coverture_legal_disability, resource_allocation).
narrative_ontology:affects_constraint(coverture_legal_disability, married_womens_property_disability).
narrative_ontology:affects_constraint(coverture_legal_disability, coverture_debt_liability).
narrative_ontology:affects_constraint(coverture_legal_disability, womens_contractual_incapacity).

% DUAL FORMULATION NOTE:
% Coverture is a legal doctrine that encompasses multiple distinct constraints: property disability, contractual incapacity, and debt liability each have separate extractive mechanisms. The present story models coverture as a unified snare; decomposition into separate stories by mechanism would yield different ε values for each mechanism (property disability ε≈0.72, contractual incapacity ε≈0.65, debt liability ε≈0.55) reflecting different degrees of structural necessity vs. pure extraction. The unified story captures the systemic integration of these mechanisms into a single legal disability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
