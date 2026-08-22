% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_law_precedent_corpus__pluralist_balancing
 *   human_readable: Pluralist Precedent Balancing in Common Law
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   The pluralist balancing reading of the common law precedent corpus holds
 *   that precedent weight should vary by doctrinal domain and factual
 *   context, with courts balancing stability against adaptation case by case.
 *   This replaced the classical formalist regime (strict_stare_decisis) in
 *   the late 19th/early 20th century as new regulatory domains made rigid
 *   precedent unworkable. The framework coordinates judicial decision-making
 *   across a vast, differentiated legal system — but it also creates a
 *   multi-tier extraction structure: repeat players (institutional litigants,
 *   specialist bar, academy) navigate the domain-specific balancing tests and
 *   precedent hierarchies for professional advantage, while non-repeat
 *   players (pro se litigants, small claimants, domain-crossing parties) face
 *   unpredictable costs and opaque rules. The claimed_type is tangled_rope
 *   because the framework performs a genuine coordination function
 *   (domain-sensitive doctrinal evolution) while simultaneously extracting
 *   navigational rents from those least equipped to pay. The engine will
 *   compute per-seat types from the structural data below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.48).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.35).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Precedent Balancing in Common Law").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, '0e826569-98fa-4c9b-8b79-25e3d86f4879').
narrative_ontology:cs_kernel_codification('0e826569-98fa-4c9b-8b79-25e3d86f4879', distributed).
narrative_ontology:cs_authority_grounding('0e826569-98fa-4c9b-8b79-25e3d86f4879', practice).
narrative_ontology:cs_interpretation_layer_present('0e826569-98fa-4c9b-8b79-25e3d86f4879').
narrative_ontology:cs_reading_relation('0e826569-98fa-4c9b-8b79-25e3d86f4879', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('0e826569-98fa-4c9b-8b79-25e3d86f4879', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('0e826569-98fa-4c9b-8b79-25e3d86f4879', foundational, precedent_weight_domain_relative).
narrative_ontology:cs_axiom_status(precedent_weight_domain_relative, holdable).
narrative_ontology:cs_axiom_grounding('0e826569-98fa-4c9b-8b79-25e3d86f4879', precedent_weight_domain_relative, conventional).
narrative_ontology:cs_axiom('0e826569-98fa-4c9b-8b79-25e3d86f4879', foundational, balancing_test_legitimacy).
narrative_ontology:cs_axiom_status(balancing_test_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0e826569-98fa-4c9b-8b79-25e3d86f4879', balancing_test_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('0e826569-98fa-4c9b-8b79-25e3d86f4879', classical_formalist_stare_decisis).
narrative_ontology:cs_drift_state('0e826569-98fa-4c9b-8b79-25e3d86f4879', contemporary_pluralist_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e826569-98fa-4c9b-8b79-25e3d86f4879', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, institutional_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, legal_academy).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, small_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, domain_crossing_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_bar_specialists).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, contextual_adjudication_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds authority to articulate and apply balancing tests that determine precedent weight in each domain. Writes the opinions that create the multi-tier framework itself. Career advancement and institutional legitimacy depend on the framework's perceived coherence. Can exit by moving to private practice, academia, or senior status — but the framework they administer is their primary professional product.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Repeat players (government agencies, large corporations, civil rights organizations) with specialized appellate practices. They invest in mastering the domain-specific balancing tests and precedent hierarchies. Their expertise becomes a barrier to entry for competitors. They can forum-shop and select favorable domains, treating the framework as a navigable terrain rather than a constraint.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, institutional_litigants, beneficiary,
    organized, biographical, mobile, national).

% Produces the doctrinal categories, balancing frameworks, and domain taxonomies that the judiciary adopts. Career capital (tenure, citations, influence) accrues from mapping and refining the pluralist structure. They can exit to practice, policy work, or adjacent disciplines — but their professional identity is constituted through the framework's complexity.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_academy, beneficiary,
    organized, generational, arbitrage, national).

% Unrepresented parties facing a legal system where the applicable rule depends on which domain their claim falls into, which balancing test governs that domain, and how the court weighs competing precedents. They cannot afford counsel to navigate the multi-tier framework. No realistic exit — they are in court because they must be, and the framework's opacity is a direct barrier to their participation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, pro_se_litigants, payer,
    powerless, immediate, trapped, local).

% Individuals and small businesses with meritorious claims but limited resources. The cost of litigating through a framework where precedent weight shifts by domain and context exceeds the value of many claims. They can sometimes exit by settling, using small-claims procedures, or abandoning claims — but each exit path represents a loss of legal protection the framework was supposed to provide.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, small_claimants, payer,
    powerless, biographical, constrained, regional).

% Litigants whose claims span multiple doctrinal domains (e.g., administrative law + constitutional law, or IP + antitrust). They face unpredictable switching costs: the precedent hierarchy, balancing test, and weight assigned to analogous cases differ across domains. Their counsel must master multiple frameworks simultaneously. Exit means restructuring the claim to fit a single domain — often sacrificing the strongest legal theory.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, domain_crossing_parties, payer,
    moderate, biographical, constrained, national).

% Elite appellate practitioners who shape the framework through strategic litigation and amicus practice. They benefit from the framework's complexity (it creates demand for their specialization) and influence its evolution (their briefs propose the balancing tests courts adopt). Can exit to the bench, academia, or high-end commercial practice — but their market position depends on the framework's continued opacity to outsiders.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_bar_specialists, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__pluralist_balancing, appellate_bar_specialists, agenda_setter).

% Scholars, journalists, and reform advocates who study the framework from outside the adjudicative process. They do not bear its costs or collect its rents. Their exit is costless — they can shift attention to other systems. Their role is to map the framework's actual operation against its claimed coordination function.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured mechanism for the common law to preserve stability (through precedent) while permitting adaptation (through domain-sensitive balancing) without requiring legislative intervention for every doctrinal adjustment. The multi-tier hierarchy (binding precedent, persuasive precedent, distinguishable precedent, overruled precedent) coordinates judicial decision-making across thousands of judges and millions of cases.
% TRANSFER_FUNCTION: Moves decisional authority and predictive certainty from litigants (especially non-repeat players) to the appellate judiciary and the organized bar. The framework transfers the cost of doctrinal navigation onto parties who must decipher which precedents control in which domains under which balancing tests. Repeat players capture the value of that navigation expertise; non-repeat players pay the cost of opacity.
% ABSENT_VOICES: Pro se litigants, small claimants, and domain-crossing parties are structurally excluded from the framework's design and refinement. They would object to the unpredictability of precedent weight across domains, the cost of mastering multiple balancing tests, and the lack of a default rule that protects unsophisticated parties. They are absent because the framework's complexity makes their effective participation impossible without representation they cannot afford.
% DISAPPEARANCE_RATIONALE: If pluralist balancing vanished overnight, the common law would lose its primary mechanism for domain-sensitive adaptation. Courts would default to either rigid stare decisis (freezing doctrine) or unguided discretion (unpredictable outcomes). The organized bar would lose its specialization premium. Pro se litigants might face simpler but more rigid rules. The entire appellate ecosystem — briefing practices, law clerk training, judicial opinion-writing norms — would reorganize around whatever replacement regime emerged.
% FOUNDING_PROBLEM: Classical stare decisis proved too rigid for a growing, diversifying legal system: it could not accommodate new domains (administrative law, constitutional rights, complex commercial regulation) without either breaking precedent or declaring novel exceptions that undermined the system's legitimacy. Pluralist balancing emerged to let courts tailor precedent weight to domain context while maintaining a veneer of systematic coherence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Friedman, Horwitz) document the late-19th/early-20th century crisis of classical formalism that motivated balancing frameworks. The appellate judiciary and organized bar attest the problem remains live — new domains (digital privacy, algorithmic governance, climate liability) still require context-sensitive precedent weight. Critics (Scalia, formalist scholars) argue the founding problem was solved by the administrative state and statutory specificity, making judicial balancing an obsolete workaround that now creates more unpredictability than it resolves.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__pluralist_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__pluralist_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects the framework's dual character: it coordinates a complex legal system (genuine function) but the coordination mechanism — domain-specific balancing tests, multi-tier precedent hierarchies, case-by-case weight assignment — creates a navigational premium that repeat players capture and non-repeat players pay. The metric has risen from ~0.15 (1880) to 0.48 (2024) as doctrinal domains proliferated and balancing tests multiplied. Suppression (0.35) is moderate: the framework does not formally bar alternatives (parties can argue for different domain classifications, different balancing weights), but the expertise barrier to effective participation is high. Theater ratio (0.22) reflects that much of the balancing rhetoric performs coherence while the actual work is done by domain conventions and specialist intuition. Accessibility collapse (0.55) is partial: alternatives exist (statutory codification, administrative rulemaking, bright-line rules) but the common law's gravitational pull keeps most disputes inside the framework. Resistance (0.45) is measurable: formalist critiques, originalist movements, and access-to-justice reform efforts all push against the framework's opacity.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, the framework is a rope — it solves the genuine coordination problem of adapting precedent to new domains without legislative micromanagement. From the pro se litigant's seat, it is a snare — the coordination story is cover for a system that extracts navigational rents from the unrepresented. From the institutional litigant's seat, it is a tangled rope — they benefit from the coordination function (predictable domain-specific rules for their repeat matters) but also extract from the framework's complexity (their expertise is a barrier to competitors). The engine computes this seat divergence from the declared power, exit, and role structure; the authored claimed_type (tangled_rope) represents the structural whole, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary sits near the beneficiary end (d ≈ 0.15): they administer the framework, their legitimacy depends on its perceived coherence, and they have arbitrage-grade exit (senior status, private practice, academia). Institutional litigants and the specialist bar are near-symmetric (d ≈ 0.45–0.55): they pay navigational costs but capture the returns of expertise. The legal academy benefits structurally (d ≈ 0.30) — their professional capital is built on mapping the framework's complexity. Pro se litigants are full targets (d ≈ 0.95): trapped, powerless, facing the framework's full opacity with no exit. Small claimants are near-targets (d ≈ 0.85): constrained exit (settlement, abandonment) but the cost of navigation exceeds claim value. Domain-crossing parties sit at d ≈ 0.70: moderate power but high switching costs across domain-specific frameworks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classical stare decisis's rigidity in a diversifying legal system) is contested: the judiciary and bar say it remains live (new domains keep emerging); formalist critics say it was solved by the administrative state and statutory specificity, making judicial balancing an obsolete workaround. The framework persists with active enforcement (judicial opinions, bar discipline, law school curricula) but its coordination function has atrophied in domains where statutory/regulatory law has displaced common law evolution. The mandatrophy is partial: the framework is a tangled rope where the coordination function remains live in constitutional and administrative law but has become largely extractive in mature commercial and tort domains where the balancing tests serve mainly to validate specialist expertise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the domain-specific balancing structure structurally necessary for common law adaptation, or does it primarily serve as a complexity barrier that extracts navigational rents?',
    'Natural experiment: compare doctrinal stability and access-to-justice outcomes in domains that have adopted bright-line rules or statutory codification versus those retaining pluralist balancing. If outcomes are equivalent or better in codified domains, the balancing structure is extractive overhead.',
    'If the balancing structure is shown to be unnecessary for coordination, the constraint reclassifies toward snare (extraction without coordination function). If necessary, it remains tangled_rope with a genuine coordination core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the framework''s complexity is a coordination necessity or an extraction mechanism.').

omega_variable(
    precedent_hierarchy_extractiveness,
    'Does the multi-tier precedent hierarchy (binding/persuasive/distinguishable/overruled) create a measurable extraction gradient across litigant types?',
    'Empirical study of litigation outcomes and costs stratified by party type (repeat vs. non-repeat) and domain (high vs. low precedent density). Measure whether non-repeat players'' loss rate correlates with precedent hierarchy depth.',
    'A strong correlation would confirm that the hierarchy''s complexity is an extraction mechanism targeting non-repeat players. A weak correlation would support the coordination thesis that the hierarchy provides genuine guidance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_hierarchy_extractiveness, empirical, 'Whether the precedent hierarchy''s structural complexity extracts from non-repeat players.').

omega_variable(
    kernel_reading_identity,
    'Is the pluralist_balancing reading a distinct structural constraint, or merely a rhetorical gloss on the evolutionary_framework reading''s actual operation?',
    'Compare the precedent weight assigned in actual opinions citing pluralist balancing frameworks versus those citing evolutionary frameworks. If the distributions are statistically indistinguishable, the readings are not structurally distinct constraints.',
    'If not distinct, this constraint story should merge with evolutionary_framework; if distinct, the kernel decomposition is validated and the structural delta (multi-tier extractiveness, domain-switching costs) is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether pluralist_balancing and evolutionary_framework instantiate genuinely different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_pb_tr_t1880, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(clpc_pb_tr_t1910, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(clpc_pb_tr_t1940, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1940, 0.12).
narrative_ontology:measurement(clpc_pb_tr_t1970, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 1970, 0.16).
narrative_ontology:measurement(clpc_pb_tr_t2000, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(clpc_pb_tr_t2024, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(clpc_pb_be_t1880, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(clpc_pb_be_t1910, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1910, 0.22).
narrative_ontology:measurement(clpc_pb_be_t1940, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1940, 0.31).
narrative_ontology:measurement(clpc_pb_be_t1970, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(clpc_pb_be_t2000, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(clpc_pb_be_t2024, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(clpc_pb_su_t1880, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1880, 0.1).
narrative_ontology:measurement(clpc_pb_su_t1910, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1910, 0.15).
narrative_ontology:measurement(clpc_pb_su_t1940, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1940, 0.22).
narrative_ontology:measurement(clpc_pb_su_t1970, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 1970, 0.28).
narrative_ontology:measurement(clpc_pb_su_t2000, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(clpc_pb_su_t2024, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel. The strict_stare_decisis reading (ε ≈ 0.15, claimed mountain/rope) treats precedent as a near-natural-law constraint on judicial power. The evolutionary_framework reading (ε ≈ 0.35, claimed rope/scaffold) treats precedent as an adaptive resource. This pluralist_balancing reading (ε ≈ 0.48, claimed tangled_rope) treats the multi-tier hierarchy itself as a coordination-extraction hybrid. The three readings share the kernel (the precedent corpus) but instantiate different constraints with different ε, different beneficiary/victim structures, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, institutional, 0.15).
constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, organized, 0.45).
constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, powerless, 0.95).
constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, moderate, 0.7).
constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, powerful, 0.5).
constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
