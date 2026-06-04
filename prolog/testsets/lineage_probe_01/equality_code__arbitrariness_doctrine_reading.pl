% ============================================================================
% CONSTRAINT STORY: equality_code__arbitrariness_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_code__arbitrariness_doctrine_reading, []).

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
 *   constraint_id: equality_code__arbitrariness_doctrine_reading
 *   human_readable: Arbitrariness Doctrine Reading of Article 14: Equality as Caprice Suppression
 *   domain: legal/constitutional/doctrine
 *
 * SUMMARY:
 *   This constraint captures the Royappa widening of Article 14's equality
 *   guarantee: from a doctrine requiring classification-based justification
 *   (where state action is permitted if it fits a rational line-drawing test)
 *   to a general prohibition on capriciousness (where state action must be
 *   rational regardless of classification scheme). The reading instantiates a
 *   specific jurisprudential move—from classificatory equality to substantive
 *   anti-arbitrariness—and in doing so redefines which agents benefit from
 *   Article 14 protection (anyone facing capricious state action, not just
 *   members of suspect classes) and which agents bear extraction (the entire
 *   administrative apparatus, not just those making classifications). The
 *   constraint exhibits a tangled structure: it coordinates protection by
 *   universalizing equality's reach, but it extracts from administrative
 *   discretion by forcing rationalization of all state action. This reading
 *   coexists with the classification test reading in actual
 *   jurisprudence—courts deploy both, in different contexts—but the
 *   arbitrariness reading represents a doctrinal expansion that shifts the
 *   equilibrium toward broader protection and narrower state margin of
 *   appreciation.
 *
 * KEY AGENTS:
 *   - Citizens facing capricious state action (institutional/trapped): powerless before arbitrary administrative decisions; benefit from the doctrine but face suppression through uncertainty about what counts as arbitrary
 *   - Marginalized groups outside suspect classification categories (moderate/constrained): primary beneficiaries of expansion; gain standing where classification test fails but face resource barriers to enforcement
 *   - Constitutional Court as institutional actor (institutional/arbitrage): architect and arbiter; benefits from jurisdictional expansion and interpretive authority; experiences doctrine as primarily coordinative
 *   - State administrative apparatus (powerful/constrained): bears direct extraction through narrowed discretionary scope and increased justification burdens; must rationalize decisions previously made within margin of appreciation
 *   - Civil rights advocacy coalition (organized/mobile): net beneficiary; gains legal tools and justiciability expansion without bearing suppression costs
 *   - Analytical observer (analytical/analytical): sees the doctrine as a tangled coordination-extraction hybrid with variable enforcement intensity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_code__arbitrariness_doctrine_reading, 0.52).
domain_priors:suppression_score(equality_code__arbitrariness_doctrine_reading, 0.65).
domain_priors:theater_ratio(equality_code__arbitrariness_doctrine_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_code__arbitrariness_doctrine_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equality_code__arbitrariness_doctrine_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equality_code__arbitrariness_doctrine_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_code__arbitrariness_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(equality_code__arbitrariness_doctrine_reading, "Arbitrariness Doctrine Reading of Article 14: Equality as Caprice Suppression").
narrative_ontology:topic_domain(equality_code__arbitrariness_doctrine_reading, "legal/constitutional/doctrine").

domain_priors:requires_active_enforcement(equality_code__arbitrariness_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_code__arbitrariness_doctrine_reading, '6b65c88a-869f-4ab4-a42f-5293b1703c15').
narrative_ontology:cs_kernel_codification('6b65c88a-869f-4ab4-a42f-5293b1703c15', formalized).
narrative_ontology:cs_authority_grounding('6b65c88a-869f-4ab4-a42f-5293b1703c15', lineage).
narrative_ontology:cs_interpretation_layer_present('6b65c88a-869f-4ab4-a42f-5293b1703c15').
narrative_ontology:cs_reading_relation('6b65c88a-869f-4ab4-a42f-5293b1703c15', equality_code__classification_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b65c88a-869f-4ab4-a42f-5293b1703c15', equality_code__horizontal_reach_question_reading, influences).
narrative_ontology:cs_axiom('6b65c88a-869f-4ab4-a42f-5293b1703c15', foundational, arbitrariness_violates_equality_fundamentally).
narrative_ontology:cs_axiom_status(arbitrariness_violates_equality_fundamentally, holdable).
narrative_ontology:cs_axiom_grounding('6b65c88a-869f-4ab4-a42f-5293b1703c15', arbitrariness_violates_equality_fundamentally, deontological).
narrative_ontology:cs_axiom('6b65c88a-869f-4ab4-a42f-5293b1703c15', foundational, equality_guarantee_extends_beyond_classification_schemes).
narrative_ontology:cs_axiom_status(equality_guarantee_extends_beyond_classification_schemes, holdable).
narrative_ontology:cs_axiom_grounding('6b65c88a-869f-4ab4-a42f-5293b1703c15', equality_guarantee_extends_beyond_classification_schemes, deontological).
narrative_ontology:cs_reference_frame('6b65c88a-869f-4ab4-a42f-5293b1703c15', equality_as_rational_state_action).
narrative_ontology:cs_drift_state('6b65c88a-869f-4ab4-a42f-5293b1703c15', contemporary_post_royappa, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6b65c88a-869f-4ab4-a42f-5293b1703c15', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(equality_code__arbitrariness_doctrine_reading, equality_code).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_code__arbitrariness_doctrine_reading, citizens_facing_capricious_state_action).
narrative_ontology:constraint_beneficiary(equality_code__arbitrariness_doctrine_reading, marginalized_groups_without_classification_protection).
narrative_ontology:constraint_victim(equality_code__arbitrariness_doctrine_reading, administrative_discretion_scope).
narrative_ontology:constraint_victim(equality_code__arbitrariness_doctrine_reading, state_apparatus_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS CITIZEN (SNARE) — A citizen denied a benefit, excluded from a service, or subjected to state action with no rational pattern faces the full force of arbitrariness with no exit. Cannot challenge the action under the old classification test if no suspect class or fundamental right is implicated. Trapped between the state's discretion and Article 14's silence. Maximum extraction of dignity; suppression via lack of recourse.
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED GROUP (TANGLED ROPE) — Groups that do not fit into suspect classifications (caste, religion, gender) but face systematic capricious exclusion benefit from arbitrariness doctrine — it gives them standing where classification test fails. But they remain constrained by the need to prove arbitrariness in each case, resource burdens of litigation, and discretionary powers of lower courts interpreting the doctrine. Genuine coordination (the doctrine coordinates protection across multiple exclusion mechanisms) alongside significant extraction (burden of proof, resource barriers).
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (ROPE) — The Supreme Court as institutional actor benefits from the arbitrariness doctrine: it expands the court's jurisdictional reach (every administrative action becomes reviewable, not just classifications), increases docket control (discretionary review of what counts as arbitrary), and consolidates interpretive authority (the court alone defines the boundaries of permissible state discretion). The court experiences the doctrine as primarily coordinative: clarifying the scope of Article 14's guarantee. Net beneficiary with arbitrage options (can refine the doctrine, adjust its enforcement, or back away from overreach).
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE ADMINISTRATIVE APPARATUS (SNARE) — Government agencies with delegated discretionary powers face a narrowed mandate: every exercise of discretion is now potentially challengeable as arbitrary. The apparatus loses flexibility, faces increased litigation exposure, must develop explicit criteria for decisions previously made within margin of appreciation. Suppression is high (must rationalize every decision), extraction is significant (defensive burdens, compliance costs), but power status is 'powerful' because the state retains formal authority even under constraint. The constraint extracts not freedom but rationalization — forced speech / forced justification.
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS COALITION (ROPE) — Advocacy organizations, public interest litigators, and reform networks benefit substantially from arbitrariness doctrine without bearing extraction costs. The doctrine gives them legal tools (arbitrariness challenge) for cases that would fail under classification doctrine, expands justiciability, and builds enforcement infrastructure (strategic litigation, precedent accumulation). Coordinated benefit — no offsetting suppression for this actor. Mobile exit options (can pursue litigation, legislative reform, or administrative advocacy simultaneously).
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scale, the arbitrariness doctrine is a reading of Article 14 that disciplines discretionary state power through a general anti-caprice principle. The reading coordinates protection by universalizing the equality guarantee beyond specific classifications, but it extracts from the discretionary state (reduced flexibility, increased justification burden) and from those who would benefit from margin-of-appreciation reasoning (executives, police, bureaucrats). The doctrine's extractiveness is modulated by the court's enforcement intensity — high suppression when courts aggressively police arbitrariness, lower when courts defer to executive judgment on rationality. At civilizational scale, the doctrine appears as a general principle (coordinate) that carries enforcement costs (extract).
constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_code__arbitrariness_doctrine_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equality_code__arbitrariness_doctrine_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_code__arbitrariness_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_code__arbitrariness_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The doctrine suppresses administrative discretion by requiring rationality justification for all state action, expanding the scope of reviewable decisions far beyond classification cases. This extraction is genuine (the state loses flexibility) but not maximal (some margin of appreciation remains, courts apply deferential review, and rational-basis justifications are often accepted). The trajectory in measurements shows extractiveness rising from 0.35 to 0.52 over the interval, reflecting doctrinal enforcement tightening as lower courts internalize the Royappa principle. Suppression (0.65): High. The doctrine suppresses alternative framings (margin of appreciation reasoning, executive deference, discretionary judgment) by universalizing the anti-caprice norm. Suppression is not total because the classification test reading remains available as an alternative framework in contested cases, and because courts retain de facto control over enforcement intensity. Theater ratio (0.48): Low-moderate. The arbitrariness doctrine has relatively low performative content compared to classification test doctrine. Rather than asking courts to identify suspect classifications and articulate rational nexus (which can be largely formulaic), arbitrariness doctrine requires genuine engagement with whether a decision has a rational basis. The doctrine is more function than theater—courts cannot simply apply a template; they must assess actual administrative reasoning. This contrasts with classification-test reasoning, which can become routinized. The measurements show theater ratio stable around 0.50, indicating the doctrine maintains substantive rather than performative character across the interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a doctrinal reading shift produces different experienced classifications across institutional positions. The arbitrariness doctrine widens protection (benefits those previously outside classification doctrine) while narrowing administrative flexibility (extracts from the state). The classification test reading narrows to those who fit classifications but leaves those outside unsupported. The horizontal reach question reading asks whether either applies to private power—clubs, employers, platforms. These are genuinely distinct readings: they answer different questions (what equality protects, whose discretion is constrained, whose power is expanded), and they cannot all be simultaneously true in a single case's outcome, though they coexist as live positions in the jurisprudence. The perspectival gap reveals why: the readings coordinate on different axes (classification vs arbitrariness, state vs private) and produce different victim sets.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural relationship to the extraction flow. Citizens facing capriciousness have d approaching 1.0 (full targets of state caprice) but cannot exercise exit (trapped). Marginalized groups have high d (targets of arbitrary state action) but partial exit through litigation (constrained). The state apparatus has low d (nominally beneficiary of discretionary power, but that benefit is now constrained by the doctrine) but constrained exit (must operate under the new standard, cannot revert to pre-Royappa discretion). The court has d approaching 0.0 (beneficiary of jurisdictional and interpretive expansion) with arbitrage exit (can refine or narrow enforcement). The civil rights coalition has low d (clear beneficiary) with mobile exit (can switch strategies). The canonical directionality derivation chain produces these values from the beneficiary/victim declarations and exit options, modulated by the power atom. No overrides are needed—the structural data is internally consistent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not yet resolve mandatrophy—extractiveness is 0.52, below the 0.70 threshold. However, the structure shows the characteristic mandatrophy tension: the doctrine simultaneously coordinates (universalizes equality protection, extends Article 14's reach) and extracts (suppresses administrative discretion, narrows margin of appreciation, increases justification burdens). The tangled_rope classification reflects this: the doctrine is not pure coordination (it suppresses), not pure extraction (it protects), but hybrid. If enforcement intensity increases and extractiveness rises above 0.70, mandatrophy resolution would require assessing whether the suppression of administrative discretion is justified by the coordination benefit of universalized protection. The omega variable about enforcement resource capacity is key: if courts cannot police arbitrariness at scale, the constraint will degrade toward piton (theater-heavy, low function). If courts enforce aggressively, suppression rises and mandatrophy emerges as a genuine question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arbitrariness_threshold_ambiguity,
    'What constitutes arbitrary action? Does it require proof of malice, irrationality, or merely the absence of a rational basis?',
    'Jurisprudential analysis of lower court decisions invoking arbitrariness; comparison of dismissal rates for arbitrariness claims vs success rates; identification of implicit standards courts apply',
    'If threshold is high (requires manifest malice): doctrine provides little protection beyond classification test. If threshold is low (absence of stated reason suffices): doctrine dramatically expands Article 14''s reach and suppresses administrative discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrariness_threshold_ambiguity, conceptual, 'Definitional boundary of arbitrary action').

omega_variable(
    classification_test_versus_arbitrariness_doctrine_foreclosure,
    'Does the arbitrariness doctrine reading logically foreclose the classification test reading within a single constitutional framework?',
    'Doctrinal analysis of whether both readings can coexist as live interpretations within Indian constitutional law. Can a court apply classification-test reasoning in some cases and arbitrariness doctrine in others, or does one reading entail rejection of the other''s core premise?',
    'If readings foreclose: the contest is zero-sum (one reading''s victory is the other''s defeat in any single court''s jurisprudence). If readings coexist: the contest is about jurisdiction (classification test handles some cases, arbitrariness doctrine handles others). This determines whether the readings represent genuine alternatives or a stratified hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_test_versus_arbitrariness_doctrine_foreclosure, conceptual, 'Logical relationship between classification test and arbitrariness doctrine within unified constitutional framework').

omega_variable(
    enforcement_resource_capacity,
    'Do courts have the institutional capacity to police arbitrariness across the full range of administrative discretion (personnel decisions, regulatory choices, discretionary benefits allocation)?',
    'Empirical analysis of court docket capacity, opinion volume, reversal rates for arbitrariness claims at different court levels, and administrative adaptability to judicial review intensity',
    'If capacity is low: the doctrine will ossify (courts will de facto narrow scope through deferential review standards). If capacity is adequate: enforcement intensity will remain high and suppression of administrative discretion will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_resource_capacity, empirical, 'Institutional capacity for arbitrariness enforcement across administrative domains').

omega_variable(
    reading_kernel_ambiguity,
    'Is Article 14 a kernel that grounds itself in a foundational normative commitment to rationality in state action, or does the arbitrariness reading itself *construct* that commitment retroactively?',
    'Historical analysis of Article 14''s textual origins (constitutional drafting records), pre-Royappa jurisprudence on state action and rationality, and the degree to which Royappa invoked existing text vs imported new principle',
    'If Article 14 already contained the arbitrariness principle: reading is interpretive recovery. If Royappa constructed it: reading is creative expansion justified by doctrinal coherence but not by original textual commitment. This affects the reading''s legitimacy status under commitment-system analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether arbitrariness principle inheres in Article 14 or was constructed by this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_code__arbitrariness_doctrine_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eqarb_tr_t0, equality_code__arbitrariness_doctrine_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(eqarb_tr_t5, equality_code__arbitrariness_doctrine_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(eqarb_tr_t10, equality_code__arbitrariness_doctrine_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(eqarb_be_t0, equality_code__arbitrariness_doctrine_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eqarb_be_t5, equality_code__arbitrariness_doctrine_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(eqarb_be_t10, equality_code__arbitrariness_doctrine_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(eqarb_su_t0, equality_code__arbitrariness_doctrine_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(eqarb_su_t5, equality_code__arbitrariness_doctrine_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(eqarb_su_t10, equality_code__arbitrariness_doctrine_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_code__arbitrariness_doctrine_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_code__arbitrariness_doctrine_reading, equality_code__classification_test_reading).
narrative_ontology:affects_constraint(equality_code__arbitrariness_doctrine_reading, equality_code__horizontal_reach_question_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equality_code kernel. The classification_test_reading instantiates a narrower interpretation (equality depends on rational line-drawing); this reading instantiates a broader interpretation (equality means absence of caprice). The horizontal_reach_question_reading asks whether either applies to private power. All three stories share the same kernel text (Article 14) but instantiate different structural constraints with different ε values, different beneficiary/victim sets, and different spatial/temporal scopes. The network linkage reflects doctrinal kinship—each reading influences the others' authority by either supporting or contesting its core claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
