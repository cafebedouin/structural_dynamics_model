% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__pluralist_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Pluralist Balancing Approach to Precedent Weight
 *   domain: legal/theoretical/constitutional
 *
 * SUMMARY:
 *   The pluralist balancing reading of the common law precedent corpus holds
 *   that precedent weight should vary by legal domain (e.g., commercial law
 *   vs. criminal procedure) and factual context, with courts balancing
 *   stability against adaptation case by case. This approach emerged mid-20th
 *   century as a response to formalist stare decisis that blocked regulatory
 *   innovation. It claims to coordinate legal stability with adaptive
 *   justice, but its operation extracts predictive certainty from litigants
 *   and lower courts, concentrating interpretive authority in appellate
 *   judges. The constraint is actively enforced through the judicial
 *   hierarchy: lower courts that miscalibrate the balance face reversal;
 *   litigants who cannot predict the balance face higher costs. The claimed
 *   type is tangled_rope — genuine coordination function (stability +
 *   adaptation) with asymmetric extraction (appellate discretion paid for by
 *   litigant uncertainty).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__pluralist_balancing, 0.55).
domain_priors:suppression_score(common_law_precedent_corpus__pluralist_balancing, 0.45).
domain_priors:theater_ratio(common_law_precedent_corpus__pluralist_balancing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(common_law_precedent_corpus__pluralist_balancing, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__pluralist_balancing, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__pluralist_balancing, "Pluralist Balancing Approach to Precedent Weight").
narrative_ontology:topic_domain(common_law_precedent_corpus__pluralist_balancing, "legal/theoretical/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__pluralist_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__pluralist_balancing, 'c8f34c65-e3e6-40d7-8371-82a02a9b8f20').
narrative_ontology:cs_kernel_codification('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', fixed_text).
narrative_ontology:cs_authority_grounding('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', lineage).
narrative_ontology:cs_interpretation_layer_present('c8f34c65-e3e6-40d7-8371-82a02a9b8f20').
narrative_ontology:cs_reading_relation('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_axiom('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', foundational, precedent_weight_is_domain_and_context_dependent).
narrative_ontology:cs_axiom_status(precedent_weight_is_domain_and_context_dependent, holdable).
narrative_ontology:cs_axiom_grounding('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', precedent_weight_is_domain_and_context_dependent, conventional).
narrative_ontology:cs_reference_frame('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', classical_stare_decisis_framework).
narrative_ontology:cs_drift_state('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', contemporary_balancing_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c8f34c65-e3e6-40d7-8371-82a02a9b8f20', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, appellate_judges).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__pluralist_balancing, supreme_court_justices).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__pluralist_balancing, lower_court_judges).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, judicial_discretion_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__pluralist_balancing, contextual_precedent_weight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit at the apex of the judicial hierarchy; their balancing decisions set the precedent weights that bind all lower courts. They exercise near-final interpretive authority over when precedent yields to adaptation. Their institutional position is secure (life tenure), and they can choose which cases to hear, giving them arbitrage-grade exit from any single doctrinal line.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, supreme_court_justices, agenda_setter,
    institutional, generational, arbitrage, national).

% Apply and calibrate precedent weights in the bulk of contested cases. They possess significant discretion in how they characterize precedent (controlling vs. distinguishable) and which balancing factors to emphasize. Their exit is constrained by promotion aspirations, circuit norms, and the risk of reversal; they cannot easily leave the hierarchical role without career cost.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, appellate_judges, agenda_setter,
    powerful, biographical, constrained, national).

% Must apply balancing tests articulated by higher courts without clear rules, facing reversal risk if their balance differs from the appellate consensus. They bear the cognitive and institutional cost of unpredictability: each case requires fresh balancing, opinions are longer, and the threat of summary reversal disciplines compliance. Their exit options are limited to senior status or leaving the bench.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, lower_court_judges, payer,
    organized, biographical, constrained, national).

% Face unpredictable litigation costs and outcomes because precedent weight shifts with judicial assessments of domain and context. Repeat players (corporations, government) can invest in predictive modeling; one-shot litigants (individuals, small entities) cannot. They are trapped in the forum: no alternative dispute resolution can bind precedent, and forum-shopping is constrained by jurisdictional rules.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, litigants, payer,
    moderate, biographical, trapped, national).

% Analyze, critique, and theorize the balancing framework from outside the adjudicative hierarchy. They influence the constraint indirectly through judicial citations, amicus briefs, and training future judges. They neither collect nor pay the extraction directly; their seat is the analytical reference frame.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Could override judicial balancing by statute but rarely does so for constitutional precedent; for statutory precedent, legislative correction is possible but politically costly. They are structurally excluded from the case-by-case balancing calculus — their voice enters only through blunt statutory commands, not the nuanced weight calibration courts perform.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__pluralist_balancing, legislature, excluded,
    institutional, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal stability and predictability while allowing adaptation to novel contexts; solves the problem of rigid rules failing to fit new cases by giving courts a calibrated discretion to weigh precedent against competing values case by case.
% TRANSFER_FUNCTION: Moves interpretive authority and predictive certainty from litigants and lower courts to appellate judges; litigants bear the costs of unpredictable precedent application (longer briefing, higher fees, settlement pressure), while appellate courts collect the institutional capital of discretionary authority.
% ABSENT_VOICES: Future litigants in novel domains whose cases have not yet reached appellate courts; marginalized communities whose legal interests are not represented in the existing precedent corpus; their interests are not represented in the balancing calculus because the calculus only operates on cases that have already been litigated to the appellate level.
% DISAPPEARANCE_RATIONALE: If pluralist balancing vanished overnight, courts would adopt either strict stare decisis (increasing rigidity, reducing adaptive capacity) or an evolutionary framework (increasing fluidity, reducing stability); the legal system's predictability-adaptability equilibrium would shift dramatically, altering litigation strategy, settlement dynamics, and the institutional role of appellate courts.
% FOUNDING_PROBLEM: How to maintain legal stability while preventing precedent from becoming an obstacle to justice in novel or changing circumstances — the post-Lochner crisis of formalist precedent that could not accommodate regulatory innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., G. Edward White, Laura Kalman) and judicial biographies confirm this was the explicit animating problem of the 1930s–1960s shift toward balancing tests; the problem remains live per contemporary judicial opinions (e.g., Roberts Court balancing in Fourth Amendment, First Amendment cases) and scholarly debate (e.g., Strauss, Schauer, Fallon on common-law constitutionalism).
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__pluralist_balancing, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__pluralist_balancing, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__pluralist_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__pluralist_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__pluralist_balancing, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.55) reflects the multi-tier cost structure: supreme court justices extract maximal discretion; appellate judges extract case-management authority; lower judges and litigants pay in unpredictability. Suppression (0.45) is moderate — the hierarchy enforces compliance but alternatives exist (legislative override, constitutional amendment, academic critique). Theater ratio (0.35) captures performative adherence to 'balancing' language while outcomes often track ideological priors. Accessibility collapse (0.50) is middling: litigants can sometimes predict outcomes via doctrinal patterns, but domain-switching costs are high. Resistance (0.40) comes from formalist judges, originalist scholars, and litigant advocacy — but the balancing framework remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the supreme court seat, pluralist balancing is genuine coordination — the Court calibrates law to society's evolving needs. From the litigant seat, the same structure is extraction — they pay for judicial discretion with uncertainty and cost. From the lower court seat, it is enforced coordination — they must replicate the balance or be reversed. The engine computes this divergence from the declared structural data; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court justices are full beneficiaries (d ≈ 0.05): life tenure, certiorari control, final say on balance. Appellate judges are partial beneficiaries (d ≈ 0.30): they gain discretion but bear reversal risk. Lower court judges are payers (d ≈ 0.75): constrained by hierarchy, must guess the balance. Litigants are full targets (d ≈ 0.90): trapped in forum, bear costs of unpredictability, no exit. Legislature is excluded but powerful — directionality override applied (see directionality_overrides). Legal scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (formalist precedent blocking adaptation) remains live — new domains (digital privacy, AI liability, climate regulation) continually test precedent's fit. But the mandate has partially atrophied: balancing tests have proliferated into multi-factor inquiries that increase unpredictability without proportional adaptive gain. The constraint is not a snare because the coordination function (preventing doctrinal rigidity) is real and acknowledged by non-beneficiaries (e.g., formalist judges who accept balancing in some domains). It is not a rope because extraction is asymmetric and enforcement is hierarchical. Tangled rope is the structurally honest classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the pluralist_balancing reading represent a genuinely distinct constraint from its siblings, or a parameterization along a single continuum of precedent flexibility?',
    'Empirical analysis of judicial opinions: if judges self-identified as pluralists produce systematically different outcomes than strict or evolutionary judges on the same case types, the readings are distinct constraints. If they merely use different rhetoric for the same balancing, they are one constraint.',
    'If distinct, each reading gets its own ε and classification. If a continuum, the kernel is a single constraint with a flexibility parameter — violating ε-invariance unless decomposed by domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the three declared readings are structurally distinct constraints or rhetorical variants of one constraint.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Do appellate judges genuinely benefit from balancing discretion, or is the discretion a burden they would shed if stable rules existed?',
    'Judicial survey data on workload, reversal anxiety, and doctrinal preferences; comparison of opinion length and citation patterns in balancing vs. rule-based domains.',
    'If discretion is a burden, the beneficiary declaration is wrong — the constraint may be a piton (inertial maintenance) rather than a tangled rope. If discretion is valued, the extraction flows to judges as institutional capital.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether the declared beneficiaries (appellate judges) actually capture the constraint''s gains or merely administer it.').

omega_variable(
    domain_switching_cost_measurement,
    'Can the ''unpredictable domain-switching costs'' for litigants be quantified, or are they an unmeasurable feature of the balancing framework?',
    'Empirical legal studies measuring settlement rates, appeal rates, and legal spend variance across doctrinal domains with different balancing intensities (e.g., Fourth Amendment vs. commercial contract precedent).',
    'If measurable and high, extraction is confirmed. If unmeasurable or low, the victim declaration may overstate extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_switching_cost_measurement, empirical, 'Quantifiability of the multi-tier extractiveness claim.').

omega_variable(
    suppression_mechanism_internalized,
    'Is lower court compliance with balancing tests driven by structural hierarchy (reversal threat) or internalized professional norms (judicial role conception)?',
    'Natural experiment: compare compliance rates in domains with strong vs. weak appellate review; survey judges on whether they follow balancing tests because they must or because they believe it is the judicial role.',
    'If internalized, suppression is higher than structural measures suggest — judges carry the constraint with them. If structural, suppression is contingent on hierarchy and could decay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression mechanism in the judicial hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__pluralist_balancing, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 10, 0.25).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 20, 0.28).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 30, 0.31).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 40, 0.33).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__pluralist_balancing, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__pluralist_balancing, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__pluralist_balancing, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__pluralist_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__pluralist_balancing, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__pluralist_balancing, common_law_precedent_corpus__evolutionary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the common_law_precedent_corpus kernel. The strict_stare_decisis reading claims precedent binds as a backward constraint (lower extractiveness, higher suppression). The evolutionary_framework reading claims contemporary norms permit reinterpretation (higher extractiveness, lower suppression). The pluralist_balancing reading claims case-by-case domain-sensitive calibration (medium extractiveness, medium suppression). All three readings share the same precedent corpus referent but instantiate different constraints with different ε, different stakeholder structures, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__pluralist_balancing, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
