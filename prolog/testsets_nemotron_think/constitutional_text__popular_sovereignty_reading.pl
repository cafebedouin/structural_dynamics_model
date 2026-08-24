% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Authority Derived from Popular Constituent Power
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the popular_sovereignty_reading of the
 *   constitutional_text kernel. The reading asserts that constitutional
 *   authority derives exclusively from the constituent power of the demos,
 *   making both courts and legislature subordinate to extra-institutional
 *   democratic expression through amendment, convention, or revolution. The
 *   structural delta from sibling readings is that popular mobilization
 *   enters as meta-authority rather than as mere political pressure; the
 *   beneficiary is democratic participation itself; the victims are
 *   institutional stability and the epistemic authority of courts and
 *   legislature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.45).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.3).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, mountain).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Authority Derived from Popular Constituent Power").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:emerges_naturally(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '021bd2a5-7dc2-406a-a4a7-10d70452074c').
narrative_ontology:cs_kernel_codification('021bd2a5-7dc2-406a-a4a7-10d70452074c', fixed_text).
narrative_ontology:cs_authority_grounding('021bd2a5-7dc2-406a-a4a7-10d70452074c', lineage).
narrative_ontology:cs_interpretation_layer_present('021bd2a5-7dc2-406a-a4a7-10d70452074c').
narrative_ontology:cs_reading_relation('021bd2a5-7dc2-406a-a4a7-10d70452074c', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('021bd2a5-7dc2-406a-a4a7-10d70452074c', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('021bd2a5-7dc2-406a-a4a7-10d70452074c', foundational, constituent_power_vests_in_people).
narrative_ontology:cs_axiom_status(constituent_power_vests_in_people, holdable).
narrative_ontology:cs_axiom_grounding('021bd2a5-7dc2-406a-a4a7-10d70452074c', constituent_power_vests_in_people, deontological).
narrative_ontology:cs_axiom('021bd2a5-7dc2-406a-a4a7-10d70452074c', secondary, extra_institutional_interpretation_legitimate).
narrative_ontology:cs_axiom_status(extra_institutional_interpretation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('021bd2a5-7dc2-406a-a4a7-10d70452074c', extra_institutional_interpretation_legitimate, deontological).
narrative_ontology:cs_reference_frame('021bd2a5-7dc2-406a-a4a7-10d70452074c', original_constituent_act).
narrative_ontology:cs_drift_state('021bd2a5-7dc2-406a-a4a7-10d70452074c', contemporary_judicial_review_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('021bd2a5-7dc2-406a-a4a7-10d70452074c', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_people).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_supremacy).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_supremacy).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_expertise).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, incumbent_officials).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_vests_in_people).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, extra_institutional_interpretation_legitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold constituent power as the ultimate source of constitutional authority. Exercise interpretive authority through amendment processes, constitutional conventions, and revolutionary action when institutions betray the constitutional mandate. Cannot exit the polity without abandoning citizenship and the political community that constitutes their democratic agency.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_people, beneficiary,
    organized, generational, identity_locked, national).

% Exercise judicial review but are subordinated to popular sovereignty as the ultimate interpretive authority. Their decisions can be overturned by constitutional amendment or rejected by popular convention. Lose final say on constitutional meaning; their expertise-based authority is constrained by the requirement of ongoing popular consent.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, courts, payer,
    institutional, biographical, constrained, national).

% Enact ordinary legislation but cannot claim final constitutional authority. Subject to popular override through amendment, convention, or revolutionary repudiation. Legislative sovereignty is derivative, not original; the legislature's constitutional interpretations are provisional pending popular ratification or rejection.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Analyze and theorize the relationship between constituent power and constituted authorities. Provide intellectual resources for all three readings but hold no institutional power to enforce any interpretation. Their authority is persuasive, not constitutive.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% Administer the existing constitutional order under the current distribution of interpretive authority. Benefit from institutional stability but bear the cost of potential popular repudiation when their actions diverge from perceived popular will. Their agenda-setting power is constrained by the threat of extra-institutional mobilization.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, incumbent_officials, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, incumbent_officials, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legitimate authority by anchoring constitutional meaning in the ongoing consent of the governed rather than in the fiat of any constituted institution. Solves the problem of how a constitution can bind generations without becoming a dead hand of the past.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from courts and legislature to the people, exercised through formal amendment, constitutional convention, and the reserved right of revolution. The transfer is not of daily governance but of the meta-authority to determine what the constitution means.
% ABSENT_VOICES: Institutional actors who claim expertise-based authority as superior to popular will (technocrats, judicial supremacists, legislative supremacists); foreign courts and international tribunals that rely on judicial dialogue rather than popular ratification; future generations bound by present popular constitutional choices without having participated in them.
% DISAPPEARANCE_RATIONALE: If popular sovereignty vanished overnight, either judicial supremacy or legislative supremacy would become the operative theory of constitutional authority. Courts would become final arbiters with no popular check, or legislatures would become unconstrained by any higher popular will. The constitutional order would fundamentally rearrange from popular constitutionalism to institutional constitutionalism.
% FOUNDING_PROBLEM: The problem of legitimate authority: how can a constitution claim binding force on those who did not consent to it, and how can it adapt to changing circumstances without losing its authoritative character? The founding problem is the tension between constitutional fixity and democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists from Locke (consent of the governed) through Rousseau (general will) to Rawls (public reason) and contemporary popular constitutionalists (Ackerman, Kramer, Tushnet) attest the problem persists. Revolutionary traditions (American, French, post-colonial) and international law (self-determination as jus cogens) corroborate from outside the institutional beneficiaries. No major democratic tradition treats the founding problem as solved.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(constitutional_text__popular_sovereignty_reading),
    narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the transfer of ultimate interpretive authority from institutions to the people — institutions lose the final say they would otherwise claim. Suppression (0.30) is modest because the constraint enables rather than coerces; its persistence depends on popular mobilization capacity, not state enforcement. Theater ratio (0.15) is low: the coordination function (legitimate authority grounded in consent) is genuine, not performative. Accessibility collapse (0.65) is moderate-high: once popular sovereignty is accepted as the ground of legitimacy, alternative grounds (divine right, historical accident, institutional expertise alone) become difficult to sustain. Resistance (0.70) is high: courts and legislatures consistently resist ceding final interpretive authority, developing doctrines (judicial supremacy, parliamentary sovereignty) that insulate themselves from popular override.
 *
 * PERSPECTIVAL GAP:
 *   From the people's seat, the constraint is Mountain-like: a natural right of self-governance that emerges from the fact of collective agency. From courts' and legislature's seats, it operates as extraction: their developed expertise and institutional continuity are devalued relative to episodic popular will. The engine computes this divergence from the structural data; the claimed type (mountain) reflects the reading's self-understanding, not the institutional experience.
 *
 * DIRECTIONALITY LOGIC:
 *   The people (organized, identity_locked exit) are structural beneficiaries — the constraint subsidizes their democratic agency (d near 0.0). Courts and legislature (institutional, constrained exit) are structural payers — the constraint extracts their claim to final authority (d near 1.0). Incumbent officials sit in a dual position: they administer the current order (agenda_setter) but bear the cost of potential popular repudiation (payer). Constitutional scholars (analytical) observe from outside the extraction dynamic. The directionality derivation follows from beneficiary/victim declarations plus exit constraints: identity_locked citizens cannot exit the polity, making them full beneficiaries; institutional actors with constrained exit bear concentrated costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate authority) remains live — every generation faces the tension between constitutional fixity and democratic consent. The arrangement has not atrophied into piton because popular mobilization remains a live threat to institutional supremacy (witness amendment campaigns, convention calls, revolutionary movements). It is not a snare because the coordination function (legitimacy through consent) is genuine and beneficiaries (the people) are not a narrow interest group. The mandatrophy resolution is negative: the mandate has not outlived its function because the function (grounding authority in consent) is perpetual in democratic theory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the popular_sovereignty_reading a distinct constraint from its siblings, or a different measurement of the same constraint?',
    'Apply ε-invariance test: if measuring constitutional authority via popular mobilization yields different ε than measuring via judicial decisions or legislative acts, they are distinct constraints. The ε referent is the standing arrangement of constitutional authority; this reading assesses ε from the people''s seat.',
    'If distinct, each reading gets its own constraint story with independent ε, stakeholders, and classification. If same constraint, observable-dependent ε violates DP-001 and the stories must be merged with measurement-basis parameter (which the schema forbids).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel decomposition validity per ε-invariance principle').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is popular sovereignty a natural law of political legitimacy (Mountain) or a historically constructed claim that benefits democratic participants at the expense of institutional stability (Tangled Rope/Snare)?',
    'Historical-comparative analysis: do all political communities recognize popular sovereignty as inherent, or does it emerge in specific historical conditions? If the latter, the Mountain claim is a false summit.',
    'If Mountain: emerges_naturally=true stands, FSM may trigger due to beneficiaries. If Tangled Rope: requires_active_enforcement=true, beneficiaries and victims both declared, coordination function (legitimate authority) coexists with extraction (institutional subordination). If Snare: coordination story is cover for democratic tyranny or mob rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'False summit mountain detection for popular sovereignty claim').

omega_variable(
    enforcement_mechanism_absence,
    'How does popular sovereignty constrain courts and legislature without its own enforcement machinery, given that amendment/convention/revolution are rare and high-threshold?',
    'Analyze the shadow of the future: the threat of popular mobilization shapes institutional behavior even when not exercised. Measure compliance anticipation vs. actual invocation.',
    'If constraint operates only through latent threat, suppression is lower than measured; if institutions internalize popular sovereignty as normative constraint, suppression is near zero. Affects classification: Mountain vs. Rope vs. Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_mechanism_absence, empirical, 'How extra-institutional authority bites on constituted institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popular_sovereignty_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(popular_sovereignty_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(popular_sovereignty_tr_t100, constitutional_text__popular_sovereignty_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(popular_sovereignty_tr_t150, constitutional_text__popular_sovereignty_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(popular_sovereignty_tr_t200, constitutional_text__popular_sovereignty_reading, theater_ratio, 200, 0.14).
narrative_ontology:measurement(popular_sovereignty_tr_t250, constitutional_text__popular_sovereignty_reading, theater_ratio, 250, 0.15).

% Extraction over time
narrative_ontology:measurement(popular_sovereignty_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(popular_sovereignty_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(popular_sovereignty_be_t100, constitutional_text__popular_sovereignty_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(popular_sovereignty_be_t150, constitutional_text__popular_sovereignty_reading, base_extractiveness, 150, 0.4).
narrative_ontology:measurement(popular_sovereignty_be_t200, constitutional_text__popular_sovereignty_reading, base_extractiveness, 200, 0.43).
narrative_ontology:measurement(popular_sovereignty_be_t250, constitutional_text__popular_sovereignty_reading, base_extractiveness, 250, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(popular_sovereignty_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(popular_sovereignty_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(popular_sovereignty_su_t100, constitutional_text__popular_sovereignty_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement(popular_sovereignty_su_t150, constitutional_text__popular_sovereignty_reading, suppression_requirement, 150, 0.25).
narrative_ontology:measurement(popular_sovereignty_su_t200, constitutional_text__popular_sovereignty_reading, suppression_requirement, 200, 0.28).
narrative_ontology:measurement(popular_sovereignty_su_t250, constitutional_text__popular_sovereignty_reading, suppression_requirement, 250, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes 'constitutional authority' into three readings with divergent ε: judicial_supremacy (low ε for courts, high for legislature/people), legislative_sovereignty (low ε for legislature, high for courts/people), popular_sovereignty (low ε for people, high for courts/legislature). The ε values differ because each reading identifies a different meta-authority as the ground of legitimacy. The family is linked by the shared kernel (constitutional text) but each reading instantiates a different constraint on where ultimate interpretive authority resides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
