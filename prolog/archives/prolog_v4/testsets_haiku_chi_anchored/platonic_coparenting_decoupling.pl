% ============================================================================
% CONSTRAINT STORY: platonic_coparenting_decoupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platonic_coparenting_decoupling, []).

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
 *   constraint_id: platonic_coparenting_decoupling
 *   human_readable: The Platonic Co-Parenting Modularization
 *   domain: social/familial
 *
 * SUMMARY:
 *   The platonic co-parenting modularization represents the structural
 *   decoupling of parental partnership from romantic partnership in
 *   contemporary kinship systems. Historically, romantic commitment, property
 *   transfer, legal custody, and child-rearing were unified in the
 *   legal-institutional bundle of marriage. The modularization — where
 *   individuals co-parent without romance, maintain romance without parental
 *   obligation, or structure parenting outside marriage — exposes this bundle
 *   as contingent rather than natural. The constraint exhibits all six DR
 *   types from different observational positions. From the child's
 *   perspective, the decoupling creates legal vulnerability and institutional
 *   non-recognition (snare). From the platonic co-parent's perspective, it
 *   offers romantic autonomy but legal ambiguity (tangled rope). From the
 *   romantic pair's perspective, it solves the problem of wanting romance
 *   without forced parenting (rope). From the kinship norm's perspective, the
 *   old bundle persists through institutional inertia and legal default
 *   despite widespread functional decoupling (piton). From the family law
 *   reform movement's perspective, it is a mixed coordination-extraction
 *   problem requiring active legal pluralism (tangled rope). From progressive
 *   jurisdictions, it is a temporary problem being solved through legal
 *   alternatives (scaffold). From the civilizational analytical view, it
 *   risks being naturalized as a law of pair-bonding rather than recognized
 *   as a contingent institutional arrangement (false mountain). The theater
 *   ratio (0.58) reflects the gap between actual modular family practice
 *   (widespread, functional, increasingly normalized) and regulatory fiction
 *   (legal systems still assume romantic-parental unity for hospital
 *   visitation, inheritance, school decision authority). The theater has
 *   increased over the interval as modular practice has outpaced legal
 *   recognition.
 *
 * KEY AGENTS:
 *   - Children in modular arrangements (powerless/trapped) — dependent on non-unified fiduciary authority, lack legal recourse
 *   - Platonic co-parents (moderate/constrained) — benefit from romantic autonomy but face legal vulnerability and institutional non-recognition
 *   - Romantic partnership pairs (moderate/mobile) — benefit from decoupling; can separate romance from parenting without institutional disruption
 *   - Nuclear family kinship norm (institutional/arbitrage) — persists through legal default and institutional inertia despite functional obsolescence
 *   - Family law reform movement (organized/constrained) — pushing for legal pluralism; see both coordination function and extraction mechanism
 *   - Progressive legal jurisdictions (organized/mobile) — building alternative frameworks (domestic partnership, parenting agreements) with institutional closure
 *   - Analytical observer (analytical/analytical) — risks naturalizing contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platonic_coparenting_decoupling, 0.38).
domain_priors:suppression_score(platonic_coparenting_decoupling, 0.52).
domain_priors:theater_ratio(platonic_coparenting_decoupling, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, extractiveness, 0.38).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(platonic_coparenting_decoupling, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platonic_coparenting_decoupling, tangled_rope).
narrative_ontology:human_readable(platonic_coparenting_decoupling, "The Platonic Co-Parenting Modularization").
narrative_ontology:topic_domain(platonic_coparenting_decoupling, "social/familial").

domain_priors:requires_active_enforcement(platonic_coparenting_decoupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, parenting_partners_with_romantic_autonomy).
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, children_accessing_stable_caregiving).
narrative_ontology:constraint_beneficiary(platonic_coparenting_decoupling, individual_choice_maximizers).
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, normative_romantic_pair_structure).
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, kinship_institutional_recognition).
narrative_ontology:constraint_victim(platonic_coparenting_decoupling, legal_custody_and_inheritance_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHILD (SNARE) — Dependent on coordination between non-romantic co-parents with no unified fiduciary authority. Lacks legal recourse when co-parents diverge on healthcare, education, or inheritance. Trapped by lack of guardian alternatives and dependency. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52. The modularization extracts clarity cost and institutional protection from the child.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PLATONIC CO-PARENT AS VICTIM (TANGLED ROPE) — Constrained by legal ambiguity (no custody rights, no inheritance standing, no hospital visitation default). Experiences both coordination benefit (shared parenting, emotional autonomy) and asymmetric extraction (legal vulnerability, social stigma, institutional non-recognition). Requires active enforcement of informal agreements. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ROMANTIC PARTNERSHIP (ROPE) — Benefits from decoupling: can maintain romantic partnership without parental obligation, or separate parenting from romance without institutional disruption. Mobile: can form/dissolve romantic bond independently. Coordination function: the modularization solves the problem of 'what if we want to co-parent without romance?' d≈0.35, f(d)≈0.30, σ=0.9 → χ≈0.10. Low effective extraction; this perspective experiences primarily coordination benefit.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: NUCLEAR FAMILY KINSHIP NORM (PITON) — Legal marriage as unified parental-romantic-property-inheritance bundle persists through institutional inertia despite functional decoupling. Court systems, hospital policies, inheritance defaults all assume the romantic-parental unity. The norm is largely performative: many families function as modular (unmarried co-parents, blended families, polyamorous structures) but maintain legal fiction of romantic unity for institutional access. theater_ratio=0.58 reflects the gap between actual modular practice and the regulatory fiction of unified pairing. The norm is degraded but maintained.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FAMILY LAW REFORM MOVEMENT (TANGLED ROPE) — Organized agents (LGBTQ+ advocacy, child welfare groups, family law scholars) recognize both coordination function (decoupling solves real problems) and extraction mechanism (institutional non-recognition creates legal vulnerability). Constrained by political resistance to norm change. Active enforcement: pushing for legal pluralism (domestic partnerships, parenting agreements, guardianship decoupling). d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18. Sees tangled rope; pushing toward scaffold through sunset of old norms.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PROGRESSIVE LEGAL PLURALISM (SCAFFOLD) — Some jurisdictions (California, Canada, Scandinavia) are building alternative legal frameworks: domestic partnership registers that decouple romance from parental custody, parenting agreements enforceable without marriage, hospital visitation by parental intent rather than marriage. This is temporary coordination with a sunset: as legal pluralism matures, the old nuclear-family bundle loses institutional force. d≈0.30, f(d)≈0.15, σ=0.9 → χ≈0.02. Very low effective extraction because the pathway is genuinely mobile and the alternatives are crystallizing.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (FALSE SUMMIT) — A tempting but incorrect analytical frame: 'The romantic-parental bundle is natural because reproduction requires pair-bonding.' This perspective would classify the constraint as Mountain — an immutable fact of human pair reproduction. However, the structural data (ε=0.38, suppression=0.52, theater=0.58, requires active enforcement, beneficiaries and victims exist) contradicts the mountain classification. The false summit reveals that the bundle is a contingent institutional arrangement, not a law of nature. Many cultures and historical periods have decoupled romance from parenting (co-wives sharing parental duty, community child-rearing, formal guardianship outside marriage). The 'natural' frame naturalizes a contingent Western legal structure.
constraint_indexing:constraint_classification(platonic_coparenting_decoupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platonic_coparenting_decoupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platonic_coparenting_decoupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(platonic_coparenting_decoupling, TR),
    TR >= 0.70.

:- end_tests(platonic_coparenting_decoupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The modularization creates extraction primarily through institutional non-recognition and legal default rules that advantage traditional romantic-pair structure. The extraction is not severe because: (1) wealthy families can contract around barriers, (2) alternative legal frameworks are emerging in progressive jurisdictions, (3) informal modular arrangements function without high coercion. But extraction exists because: (1) hospital visitation, inheritance, school authority default to spouse/legal parent, (2) children in modular arrangements lose legal protections, (3) platonic co-parents lack custody standing, (4) tax and property law assume marital unity. Suppression (0.52): Moderate-high. Significant barriers include legal default rules (who has custody if co-parent dies? who decides medical care?), social stigma (is platonic co-parenting 'real' family?), institutional friction (school forms assume two married parents), and economic barriers (legal contracting costs). But suppression is not total — informal modular arrangements persist and function, alternative frameworks exist in some jurisdictions, and norms are shifting. Theater ratio (0.58): Moderate. Family law regulatory fiction (marriage as unified parental-romantic-property bundle) is increasingly performative. Many families function as modular without legal formalization, many legal marriages are actually modular in practice, and the gap between 'assumed' and 'actual' structure has grown. The theater has increased as modular practice has normalized but law has not adjusted.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. The child sees institutional extraction (snare) — legal vulnerability, loss of guardian protections. The platonic co-parent sees mixed coordination and extraction (tangled rope) — romantic autonomy but legal ambiguity. The romantic pair sees coordination (rope) — their problem is solved by decoupling. The kinship norm sees its own degradation (piton) — the performative legal fiction persists but is increasingly misaligned with actual practice. The reform movement sees a solvable problem requiring active legal restructuring (tangled rope). Progressive jurisdictions see a temporary problem being overcome through legal pluralism (scaffold). The civilizational observer risks seeing a natural law (mountain — pair-bonding requires romance) but the structural data reveals this as a false summit. The perspectival gaps are largest between children (trapped victims), romantic pairs (mobile beneficiaries), and institutional norms (institutional arbitrage). These actors experience the same constraint as having opposite classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Children in modular arrangements: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit options; dependent on coordination between non-unified authorities. Platonic co-parents: Victim (legal status) + constrained → d≈0.68, f(d)≈1.02. High extraction through legal non-recognition; constrained by institutional barriers but not completely trapped (can formalize via contract or move to progressive jurisdiction). Romantic pairs: Beneficiary (decoupling solves their problem) + mobile → d≈0.35, f(d)≈0.30. Low extraction; they have exit options and benefit from the modularization. Nuclear family norm: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification derives from theater_ratio gate, not from high chi. The norm itself is the beneficiary of institutional inertia. Family law reform movement: Organized + constrained → d≈0.45, f(d)≈0.48. See both coordination and extraction; constrained by political resistance but have agency through advocacy. Progressive jurisdictions: Organized + mobile → d≈0.30, f(d)≈0.15. Low effective extraction because they are actively building alternatives. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False natural law classification (mountain) is revealed by structural data contradiction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by demonstrating that the 'coordination vs extraction' framing is perspectival. For romantic pairs, modularization is pure coordination (rope) — it solves a collective action problem (how to separate romance from parenting). For children and platonic co-parents, the same institutional arrangement functions as extraction (snare and tangled rope) — it creates legal vulnerability and asymmetric institutional recognition. The resolution is not to pick 'the right' classification but to recognize that the same structural arrangement produces both coordination benefit and extraction cost, distributed across different agent positions. The family law reform movement's tangled_rope perspective is the mandatrophy-resolving frame: it acknowledges both the coordination function (decoupling solves real problems) and the extraction mechanism (institutional non-recognition creates legal vulnerability). The scaffold perspective (progressive jurisdictions building legal alternatives) shows a pathway out: as legal pluralism matures and alternatives crystallize, the extraction mechanism weakens. The piton perspective (kinship norm) reveals institutional inertia: the old bundle persists not because it solves problems but because legal and institutional systems are slow to update. The false mountain perspective (natural law of pair-bonding) is the key mandatrophy error: it would naturalize the bundle as inevitable, preventing recognition of its contingency and the need for legal reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_custody_convergence_speed,
    'How quickly will family law jurisdictions converge on decoupled parental-romantic frameworks, and will convergence happen through legal reform or through norm drift outpacing law?',
    'Longitudinal tracking of family law reforms across jurisdictions; correlation between legal pluralism and custody dispute outcomes; comparison of informal vs formal decoupling adoption rates',
    'If law leads: scaffold timeline is 10-15 years; institutional clarity improves. If norm leads: legal ambiguity persists 20+ years; extraction via institutional gaps remains high. If bifurcation persists: wealthy families navigate modular structures through contracts; poor families trapped by legal defaults.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_custody_convergence_speed, empirical, 'Timeline for legal custody framework convergence').

omega_variable(
    child_welfare_outcome_modulation,
    'Do children in legally-formalized modular co-parenting arrangements have demonstrably better welfare outcomes than children in unmarried co-parenting without legal framework, controlling for parental conflict and economic stability?',
    'Prospective longitudinal cohort study comparing child outcomes in: (1) married traditional, (2) unmarried modular with legal agreement, (3) unmarried modular without legal framework, (4) blended families; controlling for parental conflict, economic resources, and parental education',
    'If legal formalization improves outcomes: scaffold is functional, extraction mechanism is real institutional gap. If outcomes independent of legal status: legal framework is theater; the coordination problem is actually social/emotional, not institutional. If outcomes worsen with formalization: explicit framing damages something implicit cooperation achieves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(child_welfare_outcome_modulation, empirical, 'Whether legal formalization improves child welfare in modular arrangements').

omega_variable(
    romantic_autonomy_stability_tradeoff,
    'Does decoupling romantic partnership from parental partnership increase romantic relationship instability, and if so, what is the welfare impact on co-parenting stability?',
    'Comparison of romantic relationship dissolution rates in traditional vs modular arrangements; impact of romantic dissolution on co-parenting continuity; longitudinal tracking of emotional stability in children across romantic transitions',
    'If instability increases: the coordination benefit (romantic autonomy) extracts stability cost; classification shifts toward snare for children. If stability unchanged: decoupling actually reduces conflict-driven family dissolution. If stability improves: decoupling enables healthier separations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(romantic_autonomy_stability_tradeoff, empirical, 'Whether romantic decoupling affects co-parenting stability').

omega_variable(
    institutional_barrier_fungibility,
    'Are the institutional barriers to modular co-parenting (hospital visitation, school decision-making, inheritance default, tax filing) fungible — can they be overcome through individual contracts and workarounds — or do they represent systematic legal asymmetry that requires structural reform?',
    'Audit of actual contract enforceability for modular co-parenting agreements across jurisdictions; case law analysis of dispute outcomes when contracts conflict with statutory defaults; comparison of enforcement costs (legal fees, dispute time) for contracted vs statutory arrangements',
    'If fungible: wealthy families solve modularization through legal contracting; constraint is snare only for poor families. Theater ratio decreases (more explicit than assumed). If structural: universal legal reform is necessary; piton classification is stable until reform happens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_barrier_fungibility, empirical, 'Whether institutional barriers to modular parenting can be overcome contractually').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platonic_coparenting_decoupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platonic_coparenting_decoupling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(plat_tr_t10, platonic_coparenting_decoupling, theater_ratio, 10, 0.52).
narrative_ontology:measurement(plat_tr_t20, platonic_coparenting_decoupling, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platonic_coparenting_decoupling, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(plat_be_t10, platonic_coparenting_decoupling, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(plat_be_t20, platonic_coparenting_decoupling, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platonic_coparenting_decoupling, enforcement_mechanism).
narrative_ontology:affects_constraint(platonic_coparenting_decoupling, family_law_pluralism).
narrative_ontology:affects_constraint(platonic_coparenting_decoupling, kinship_institutional_recognition).
narrative_ontology:affects_constraint(platonic_coparenting_decoupling, child_custody_default_rules).

% DUAL FORMULATION NOTE:
% Platonic co-parenting modularization is structurally downstream of the romantic-parental bundle assumption in family law. The upstream constraint (bundle assumption in legal code) has ε≈0.12 (mountain-like legal default); the modularization (decoupling in practice) has ε≈0.38 (tangled rope). The family law pluralism constraint is downstream — it represents the institutional pathway to resolving the extraction mechanism by formalizing alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platonic_coparenting_decoupling, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
