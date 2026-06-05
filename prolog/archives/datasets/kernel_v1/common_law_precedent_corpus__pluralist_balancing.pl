% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__pluralist_balancing, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Common Law Precedent Corpus — Pluralist Balancing Reading
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   The common law precedent corpus embodies a fundamental tension between
 *   the stability function of precedent (providing predictable legal
 *   frameworks) and the adaptation function (allowing doctrine to evolve as
 *   society and jurisprudence develop). The pluralist balancing reading
 *   asserts that precedent weight should vary by domain and context —
 *   constitutional law precedents may be overruled more readily than property
 *   law precedents; family law domains permit more equitable exception-making
 *   than contract law; antitrust doctrine adapts faster than tort doctrine.
 *   This reading instantiates ONE of three major jurisprudential positions
 *   about how precedent should constrain courts. The strict stare decisis
 *   reading demands categorical precedent hierarchy (prior precedent binds
 *   unless explicitly overruled). The evolutionary framework reading treats
 *   precedent as a resource for doctrinal evolution, not a constraint. The
 *   pluralist balancing reading claims that legitimate jurisprudence requires
 *   case-by-case calibration of precedent weight according to domain-specific
 *   values and factual context. This constraint generates a tangled-rope
 *   structure: the precedent corpus coordinates judicial decision-making by
 *   providing a common framework, but the context-dependent weighting of that
 *   framework creates systematic unpredictability for litigants, especially
 *   those without institutional knowledge of domain norms. Extractiveness has
 *   increased from 0.38 to 0.52 over the measurement interval as legal
 *   complexity has grown (more precedent to navigate) and domain
 *   specialization has deepened (more context-dependent variance), raising
 *   the cost for one-shot and marginalized litigants to navigate precedent
 *   hierarchies.
 *
 * KEY AGENTS:
 *   - The Supreme Court: Institutional beneficiary (institutional/arbitrage) — preserves flexibility to intervene in any domain while maintaining the constraint of precedent; benefits from pluralism that permits selective constraint
 *   - Circuit Courts: Moderate constrained beneficiary (moderate/constrained) — coordinate with Supreme Court and within their domain; constrained by Supreme precedent but benefit from domain discretion
 *   - One-Shot Litigants / Marginalized Claims: Primary victims (powerless/trapped) — lack institutional knowledge of domain precedent hierarchies; face unpredictable costs when precedent weight varies by context
 *   - Repeat Players / Large Law Firms: Organized beneficiaries (organized/constrained) — understand domain norms and precedent signaling; coordinate with courts through precedent strategy
 *   - The Legal Academy: Institutional maintainer (institutional/arbitrage) — produces scholarship systematizing precedent hierarchies (largely performative); benefits from the appearance of coherence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (pluralist balancing) as inherent features of law itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.48).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Common Law Precedent Corpus — Pluralist Balancing Reading").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'd70109bb-651a-40d6-bceb-fd28027452c4').
narrative_ontology:cs_kernel_codification('d70109bb-651a-40d6-bceb-fd28027452c4', formalized).
narrative_ontology:cs_authority_grounding('d70109bb-651a-40d6-bceb-fd28027452c4', lineage).
narrative_ontology:cs_interpretation_layer_present('d70109bb-651a-40d6-bceb-fd28027452c4').
narrative_ontology:cs_reading_relation('d70109bb-651a-40d6-bceb-fd28027452c4', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('d70109bb-651a-40d6-bceb-fd28027452c4', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_axiom('d70109bb-651a-40d6-bceb-fd28027452c4', foundational, precedent_weight_domain_dependent).
narrative_ontology:cs_axiom_status(precedent_weight_domain_dependent, holdable).
narrative_ontology:cs_axiom_grounding('d70109bb-651a-40d6-bceb-fd28027452c4', precedent_weight_domain_dependent, instrumental).
narrative_ontology:cs_axiom('d70109bb-651a-40d6-bceb-fd28027452c4', foundational, domain_norms_judicially_cognizable).
narrative_ontology:cs_axiom_status(domain_norms_judicially_cognizable, holdable).
narrative_ontology:cs_axiom_grounding('d70109bb-651a-40d6-bceb-fd28027452c4', domain_norms_judicially_cognizable, conventional).
narrative_ontology:cs_reference_frame('d70109bb-651a-40d6-bceb-fd28027452c4', adaptive_jurisprudence_within_constraint).
narrative_ontology:cs_drift_state('d70109bb-651a-40d6-bceb-fd28027452c4', contemporary_legal_complexity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d70109bb-651a-40d6-bceb-fd28027452c4', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, institutional_courts).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, established_precedent_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants_seeking_doctrinal_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, marginalized_legal_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITIGANT SEEKING DOCTRINAL CHANGE (SNARE) — Trapped within a jurisprudential context where precedent weight is context-dependent and opaque. Must navigate unpredictable domain-switching costs. No clear hierarchy of what makes precedent binding in their domain. Experiences maximum extraction through doctrinal uncertainty and inability to predict which precedents will constrain their case.
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIRCUIT COURT JUDGE (TANGLED ROPE) — Constrained by Supreme Court precedent but benefits from discretion in domain-specific interpretation. Must balance Supreme Court binding authority against circuit-level precedent weight and domain norms. Genuine coordination function (precedent provides framework for coherent jurisprudence) alongside asymmetric extraction (judges extract authority from precedent ambiguity). Significant agency but also significant constraints.
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUPREME COURT INSTITUTION (ROPE) — Benefits from precedent pluralism that preserves flexibility while maintaining the appearance of constraint. Can cite circuit inconsistency as justification for intervention. Experiences the precedent corpus as a coordination tool that enables adaptive jurisprudence while preserving institutional authority. Net beneficiary with significant arbitrage capacity.
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED ADVOCACY GROUPS / REPEAT PLAYERS (TANGLED ROPE) — Repeat players in appellate litigation benefit from understanding precedent hierarchy and domain norms that novice litigants do not. They coordinate with judges through precedent signaling and strategic domain choice. Both benefits (predictability for those who understand the system) and extraction (system opaque to those without institutional knowledge).
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL ACADEMY (PITON) — Maintains the fiction of coherent precedent hierarchy while scholarship documents ongoing doctrinal incoherence. Theater ratio high: law reviews publish volumes on precedent systematization, citation analysis, and hierarchical architecture, but the actual system operates on domain-specific context. The academy's interpretive labor is substantially performative — it systematizes what cannot be systematized. Inertia-driven: legal education persists in teaching precedent as a coherent system because the alternative (teaching jurisprudential pluralism directly) would undermine the authority structure the academy depends on.
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, precedent weighting by context is inherent to any legal system that must balance stability and adaptation. No body of rules can mechanically determine which precedents bind in unforeseen contexts. This is a structural limit of law itself — apparent incoherence is actually rational response to incommensurable constraints. However, the structural data reveals this as a false summit: the pluralism is not immutable but contingent on institutional choices (Supreme Court restraint, circuit autonomy allocation, and precedent-stare-decisis doctrine itself).
constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_law_precedent_corpus__pluralist_balancing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, TR),
    TR >= 0.70.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The pluralist balancing approach creates systematic unpredictability about which precedents will bind in a given domain. One-shot litigants and those seeking doctrinal change bear the cost of navigating this opacity. Repeat players and institutional actors extract value by understanding domain norms that are not formally codified. The extractiveness is not as high as a pure snare (0.66+) because some predictability exists within domains, and the Supreme Court does provide hierarchical constraint at the top level. Suppression (0.48): Moderate. The mechanism maintaining this constraint is not primarily coercive (formal barriers to precedent change) but rather institutional — the precedent corpus is embedded in legal training, judicial practice, and appellate procedure. Litigants cannot avoid precedent; they must navigate it. The suppression is not low because domain-dependent variance genuinely constrains what arguments courts will hear. Theater ratio (0.58): Moderate-high. Legal scholarship on precedent systematization is substantial (treatises, Restatements, academic articles on precedent doctrine) but the actual system operates on domain-specific context that scholarship does not fully capture. Judges perform the role of precedent-follower even when precedent weight is genuinely context-dependent. The theater has increased as legal complexity has grown — more scholarly apparatus needed to maintain the appearance of systematic precedent hierarchy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. The Supreme Court sees the precedent corpus as a coordination tool enabling adaptive jurisprudence (rope — they benefit from pluralism). Circuit courts see mixed coordination and constraint (tangled rope — they coordinate with Supreme Court and within domain but face unpredictability at domain boundaries). Repeat players see profitable adaptation (rope — they understand domain norms and exploit precedent variance). One-shot litigants see entrapment in an opaque system (snare — precedent weight varies unpredictably, and they lack institutional knowledge to navigate it). The legal academy sees a systematizable hierarchy that scholarship proves cannot be systematized (piton — performative scholarship maintaining inertial commitment to precedent coherence). The civilizational analytical observer risks seeing precedent pluralism as inherent to law's structure (mountain) rather than a contingent institutional choice about how much flexibility courts should retain.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position and exit options. The Supreme Court (beneficiary + arbitrage) derives low d (0.15) — they experience negative effective extraction, benefiting from the system's flexibility. Circuit courts (moderate power + constrained exit) derive moderate d (0.55) — they are both constrained by Supreme precedent and benefit from domain discretion. One-shot litigants (powerless + trapped exit, victims of unpredictability) derive high d (0.90) — they bear maximum experienced extraction. Repeat players (organized + constrained exit, but beneficiaries of domain knowledge) derive moderate d (0.40) — they extract value from the system but cannot fully escape its constraints. The legal academy (institutional + arbitrage) derives low d (0.20) — they benefit from being interpreters of the precedent corpus. The sigmoid function f(d) then modulates effective extraction chi based on these directionality values: beneficiaries experience damped chi; victims experience amplified chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The pluralist balancing reading resolves mandatrophy by distinguishing itself from strict stare decisis on the basis of whether precedent weight should be fixed or context-dependent. The reading claims that coherent jurisprudence requires context-dependent calibration — the same precedent may bind tightly in one domain (property law) and loosely in another (constitutional law) because the domains have different stability/adaptation tradeoffs. This is not a claim that precedent doesn't bind; it is a claim about how binding force should be calibrated. The false summit at the analytical/civilizational level reveals that pluralist balancing can naturalize what is actually a contingent institutional choice: the Supreme Court's institutional position depends on precedent being weighty enough to constrain lower courts, but not so weighty that the Supreme Court cannot adapt doctrine. Pluralist balancing serves that institutional interest by making precedent weight context-dependent — rigid enough to constrain below, flexible enough above.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_contestation,
    'What criteria determine which domain norms apply when a case sits at the boundary between multiple doctrinal domains (e.g., takings law vs. environmental law, criminal procedure vs. constitutional speech)?',
    'Empirical analysis of Supreme Court domain-assignment choices in boundary cases; tracking of which court (circuit vs. Supreme) reassigns domain classification on appeal',
    'If Supreme Court controls domain assignment: precedent weight is centralized and extractiveness increases (litigants cannot predict domain authority). If circuits retain domain assignment: precedent weight remains distributed and litigants face higher unpredictability costs but circuits retain more agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_contestation, empirical, 'Domain-boundary criteria for precedent-weight allocation').

omega_variable(
    circuit_precedent_binding_force,
    'Do circuit-level precedents bind as tightly as Supreme Court precedents within their domain, or is circuit precedent context-dependent in a way that permits lower courts to distinguish based on fact-pattern variation?',
    'Citation analysis of lower-court distinguishing moves; tracking of precedent abandonment at circuit level without Supreme Court intervention; surveying of circuit-court self-perception of precedent-binding force',
    'If circuit precedent binds tightly: precedent corpus is more rigid (moves toward snare for those seeking change). If circuit precedent is routinely distinguished: precedent corpus is more fluid and extractiveness is higher (unpredictability cost outweighs constraint benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(circuit_precedent_binding_force, empirical, 'Circuit precedent binding force relative to Supreme Court precedent').

omega_variable(
    pluralist_balancing_as_naturalization,
    'Is pluralist balancing of precedent weight a genuine jurisprudential principle, or is it a cover story for institutional arrangements that benefit repeat players and courts while imposing unpredictability costs on novice litigants?',
    'Comparative success rates of repeat-player vs. one-shot litigants; analysis of precedent-distinguishing moves by judge experience level and party-side institutional status; qualitative interviews with appellate litigators about predictability costs',
    'If genuine principle: constraint is rope-like (provides legitimate coordination). If cover story: constraint is snare-like (extraction hidden behind doctrinal neutrality). If mixed: constraint remains tangled-rope but with higher confidence in the asymmetric extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralist_balancing_as_naturalization, empirical, 'Whether pluralist balancing masks extractive institutional arrangements').

omega_variable(
    reading_vs_strict_stare_decisis_boundary,
    'What structural feature of THIS reading (pluralist balancing) distinguishes it from the strict-stare-decisis reading? Is the distinction that this reading permits domain-dependent variance, while strict stare decisis demands hierarchy?',
    'Textual analysis of jurisprudential commitments: strict stare decisis = precedent binds categorically unless explicitly overruled; pluralist balancing = precedent binds context-dependently based on domain norms and factual variation.',
    'If the readings coexist: different courts and judges instantiate each simultaneously (coexists_with relation). If pluralist balancing logically requires rejecting the strict-stare-decisis core commitment: the readings foreclose one another (forecloses relation). If pluralist balancing creates institutional conditions that make strict stare decisis harder to practice: the readings influence one another (influences relation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_strict_stare_decisis_boundary, conceptual, 'Structural relationship between pluralist-balancing and strict-stare-decisis readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prec_bal_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.45).
narrative_ontology:measurement(prec_bal_tr_t5, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 5, 0.52).
narrative_ontology:measurement(prec_bal_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(prec_bal_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prec_bal_be_t5, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(prec_bal_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prec_bal_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(prec_bal_su_t5, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 5, 0.44).
narrative_ontology:measurement(prec_bal_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_vs_rule_constraint).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, appellate_gatekeeping_authority).

% DUAL FORMULATION NOTE:
% The common law precedent corpus constraint family decomposes into three structurally distinct readings with different extractiveness values and beneficiary/victim structures. Strict stare decisis (ε≈0.35, Rope) treats precedent as fixed coordination. Pluralist balancing (ε≈0.52, Tangled Rope) treats precedent weight as domain-dependent, creating unpredictability. Evolutionary framework (ε≈0.25, Scaffold) treats precedent as adaptive resource. Each reading has its own perspectives and derives different directionality values from the same agents depending on the reading's framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
