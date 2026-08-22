% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security-Maximization Reading of the Geneva Conventions (Operational-Necessity Suspension Doctrine)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story authors the security-maximization reading of the Geneva
 *   Conventions kernel: the position that the Conventions are peacetime
 *   aspirations properly suspended when a state faces irregular, asymmetric
 *   adversaries, because operational necessity in such conflicts outweighs
 *   the value of maintaining full POW status, habeas corpus, civilian
 *   immunity, and the ordinary torture prohibition's strict construction.
 *   This is one reading among three live readings of the same kernel text
 *   (the others are authored as separate constraints:
 *   humanitarian_ceiling_reading and conditional_reciprocity_reading). The ε
 *   authored here (0.81) is high because, from this reading's own operational
 *   lens, the arrangement concentrates broad discretionary power in the
 *   detaining state while withdrawing procedural protection from detained
 *   persons and theater civilians — the story's ε is about the standing
 *   arrangement as this reading itself describes it, not a hostile caricature
 *   and not the humanitarian-ceiling alternative.
 *
 * KEY AGENTS:
 *   - detaining_state_executive: agenda_setter (institutional/arbitrage) — designs and administers the suspension doctrine
 *   - military_and_intelligence_command: beneficiary/agenda_setter (institutional/arbitrage) — operational beneficiary of relaxed constraints
 *   - domestic_security_electorate: beneficiary (organized/constrained) — political beneficiary, insulated from direct costs
 *   - detained_unlawful_combatants: payer (powerless/trapped) — bears indefinite detention and coercive interrogation
 *   - civilian_populations_in_theater: payer (powerless/trapped) — bears degraded targeting protections
 *   - regular_armed_forces_reciprocity_pool: payer (organized/constrained) — bears the erosion of reciprocal POW protection
 *   - icrc_and_treaty_monitoring_bodies: excluded (institutional/constrained) — objects but lacks binding power
 *   - international_courts_and_tribunals: observer (institutional/analytical) — adjudicates after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.81).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.88).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security-Maximization Reading of the Geneva Conventions (Operational-Necessity Suspension Doctrine)").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'b1d5ef03-e8ff-4032-8759-e30000eb5178').
narrative_ontology:cs_kernel_codification('b1d5ef03-e8ff-4032-8759-e30000eb5178', fixed_text).
narrative_ontology:cs_authority_grounding('b1d5ef03-e8ff-4032-8759-e30000eb5178', extraction).
narrative_ontology:cs_interpretation_layer_present('b1d5ef03-e8ff-4032-8759-e30000eb5178').
narrative_ontology:cs_reading_relation('b1d5ef03-e8ff-4032-8759-e30000eb5178', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('b1d5ef03-e8ff-4032-8759-e30000eb5178', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('b1d5ef03-e8ff-4032-8759-e30000eb5178', foundational, necessity_supersedes_treaty_floor).
narrative_ontology:cs_axiom_status(necessity_supersedes_treaty_floor, holdable).
narrative_ontology:cs_axiom_grounding('b1d5ef03-e8ff-4032-8759-e30000eb5178', necessity_supersedes_treaty_floor, instrumental).
narrative_ontology:cs_axiom('b1d5ef03-e8ff-4032-8759-e30000eb5178', foundational, no_protection_is_non_derogable).
narrative_ontology:cs_axiom_status(no_protection_is_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('b1d5ef03-e8ff-4032-8759-e30000eb5178', no_protection_is_non_derogable, instrumental).
narrative_ontology:cs_reference_frame('b1d5ef03-e8ff-4032-8759-e30000eb5178', symmetric_interstate_combat_framework).
narrative_ontology:cs_drift_state('b1d5ef03-e8ff-4032-8759-e30000eb5178', post_9_11_asymmetric_conflict_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b1d5ef03-e8ff-4032-8759-e30000eb5178', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, detaining_state_executive).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_command).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, domestic_security_electorate).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_theater).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, regular_armed_forces_reciprocity_pool).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and administers the unlawful-combatant category, authorizes indefinite detention outside the POW framework, and sets interrogation policy under an operational-necessity rationale. Holds the discretion to expand or contract the suspension of protections and answers to no external adjudicator with binding power over its own designations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detaining_state_executive, agenda_setter,
    institutional, generational, arbitrage, global).

% Gains operational flexibility and intelligence yield from coercive interrogation, indefinite detention without habeas review, and permissive targeting standards around 'human shields.' Frames every relaxation as a battlefield necessity and controls the classification of detainees that determines which protections apply.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_command, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_command, agenda_setter).

% Receives the political and psychological benefit of a state that appears unconstrained in pursuing irregular adversaries. Bears none of the direct costs of detention or interrogation policy and largely does not observe them; support for the suspension doctrine is sustained by distance from its application.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_security_electorate, beneficiary,
    organized, biographical, constrained, national).

% Classified outside the POW category, denied habeas corpus and the presumption of protected status, held indefinitely without trial, and subject to interrogation techniques the reading defines as short of torture. Has no forum with binding authority over the state's own classification decision and no meaningful exit short of external political or judicial intervention.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_unlawful_combatants, payer,
    powerless, biographical, trapped, global).

% Bear degraded immunity from attack under an expanded acceptance of collateral damage and a 'human shields' doctrine that shifts responsibility for civilian casualties onto an adversary's tactics. Have no capacity to relocate the conflict or contest targeting determinations before harm occurs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_theater, payer,
    powerless, immediate, trapped, regional).

% The state's own uniformed personnel, whose treatment as POWs by adversaries depends on a shared convention baseline; when their own state narrows the category and the protections it extends to captured adversaries, the reciprocal expectation erodes, degrading the protection regular forces can expect if captured. They cannot individually exit this exposure — it is set by state policy above them.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, regular_armed_forces_reciprocity_pool, payer,
    organized, generational, constrained, global).

% Would assert that the Conventions' protections are non-derogable minimums, not suspendable aspirations, and would object to the unlawful-combatant category as a unilateral reclassification designed to exit the treaty regime's obligations. Has monitoring access in some theaters but no binding enforcement power over a state invoking necessity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, icrc_and_treaty_monitoring_bodies, excluded,
    institutional, generational, constrained, global).

% Evaluate after the fact whether the necessity-based suspensions were lawful, producing rulings and advisory opinions that can constrain future invocations but rarely reach the individuals harmed during the period of suspension.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, international_courts_and_tribunals, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, military_and_intelligence_command).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its own terms, the reading solves a real operational problem: irregular adversaries who do not wear uniforms, do not carry arms openly, and deliberately embed among civilians make the classic POW/civilian binary difficult to apply, and a state facing such an adversary claims it needs discretion to detain, interrogate, and target without the full peacetime apparatus in order to protect its forces and population.
% TRANSFER_FUNCTION: Moves the burden of protection away from detained persons and theater civilians and onto the state's unilateral discretion: legal certainty, procedural review, and the presumption of protected status are withdrawn from the governed population and concentrated as operational flexibility in the executive and military command.
% ABSENT_VOICES: Detained individuals have no forum with binding authority over their own classification; the ICRC and treaty bodies raise the humanitarian-ceiling objection but lack enforcement power against the invoking state; theater civilians harmed under the loosened targeting standard have no pre-harm voice in the necessity determination at all.
% DISAPPEARANCE_RATIONALE: If the security-maximization reading disappeared and states reverted to full POW/civilian binaries with binding external review, detention practices would shift toward charge-or-release timelines, interrogation techniques would be constrained by the torture prohibition without a permissive gloss, and targeting doctrine would tighten around civilian immunity — a substantial rearrangement of how asymmetric conflicts are conducted, not a cosmetic one.
% FOUNDING_PROBLEM: The reading was built to solve the practical difficulty of applying a treaty framework designed around symmetric, state-vs-state, uniformed warfare to conflicts against irregular, non-state, deliberately-concealed adversaries, where the state argues that full compliance would concede a decisive tactical advantage to an opponent who observes none of the same rules.
% FOUNDING_PROBLEM_CORROBORATION: Military and executive branch legal offices attest the founding problem remains live and cite ongoing irregular threats. Independent bodies outside the benefiting state apparatus — the ICRC, UN human rights mechanisms, and international tribunals reviewing detention and targeting cases — attest that the reading's scope has expanded well past the narrow classification difficulty it was framed to solve, now covering broad categories of detention and interrogation with limited nexus to the original operational problem; no corroboration exists from outside the detaining state's own institutions for the current scope of suspension.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.55 to 0.81) modeling doctrinal creep: a category initially framed narrowly around a specific classification difficulty (irregular combatants who conceal civilian status) expands over time to cover broader detention and interrogation practice, exactly the T17 accumulation pattern. Suppression is high and rising (0.65 to 0.88) because the doctrine's persistence depends on active resistance to external judicial and treaty-body review, not on voluntary participant assent. Theater ratio is moderate and rising (0.2 to 0.42): part of the apparatus (classification tribunals, review boards) performs due process without altering outcomes, and that performative share grows as external scrutiny increases pressure to appear compliant while substance is preserved.
 *
 * DIRECTIONALITY LOGIC:
 *   The detaining state executive and military command sit at the low-d, beneficiary end: they set the classification rules that determine who receives protection and collect the operational flexibility directly. The domestic security electorate is also a beneficiary but structurally distant from the mechanism's operation — it receives the political benefit without bearing or witnessing the cost, which is itself part of what sustains political support. Detained persons and theater civilians sit at the high-d, target end: trapped exit options, no binding forum, and the constraint's entire operation is organized around withdrawing exactly the protections that would otherwise apply to them. The regular-forces reciprocity pool occupies an intermediate but real payer position — they do not benefit from the classification discretion and instead bear a diffuse, delayed cost (degraded reciprocal treatment) that is easy to discount against the immediate operational gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine classification difficulty posed by irregular, non-uniformed adversaries — was real and narrow. This reading's tangled_rope classification (rather than pure snare) preserves that: there IS a coordination function (a workable operational framework for irregular conflict) bundled with the extraction (broad, expanding suspension of protection). Treating the whole doctrine as pure extraction would erase the genuine operational difficulty it responds to; treating it as pure coordination (rope) would erase the asymmetric cost concentrated on detained persons and civilians who have no voice in the necessity determination. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) is exactly the signal this framework is built to surface: the state's own institutions attest the founding problem is live, while independent monitoring bodies attest the doctrine's current scope has outrun that founding problem — a classic capture/creep pattern warranting the tangled_rope reading rather than either pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_determination_locus,
    'Who has final authority to determine that operational necessity exists — the detaining state alone, or some external reviewing body — and does the security-maximization reading''s self-certifying necessity standard collapse the distinction between a genuine emergency exception and unilateral treaty exit?',
    'Comparative review of cases where external courts or treaty bodies overturned a state''s own necessity determination versus cases where no external review occurred; a high rate of unreviewed self-certification would support the reading being structurally indistinguishable from unilateral exit dressed as interpretation.',
    'If necessity determinations are effectively unreviewable, the reading functions closer to a snare (self-serving discretion with no genuine external check) rather than a tangled_rope with a bounded coordination function; if external review is real and binding in practice, the coordination function is more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_determination_locus, empirical, 'Whether the state''s necessity determination is meaningfully reviewable or effectively self-certifying.').

omega_variable(
    scope_creep_vs_stable_doctrine,
    'Is the rising extractiveness trajectory authored here evidence of genuine doctrinal creep (a narrow exception expanding past its original justification) or does it instead reflect a stable doctrine whose application simply became more visible over time due to increased reporting and litigation?',
    'Track the ratio of detentions/interrogation techniques falling under the unlawful-combatant classification against the ratio that would have fallen under it at the doctrine''s founding moment, holding conflict intensity constant.',
    'Genuine creep supports the T17 accumulation reading and reinforces the tangled_rope classification tightening toward snare over time; stable-but-more-visible application would suggest the metrics should be flatter across the interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_creep_vs_stable_doctrine, empirical, 'Whether rising measured extraction reflects real expansion or improved visibility of a stable practice.').

omega_variable(
    committer_disagreement_locus,
    'This reading, the humanitarian_ceiling_reading, and the conditional_reciprocity_reading disagree specifically on whether ANY protection is non-derogable and whether adversary conduct is a legitimate conditioning variable — where exactly does the disagreement sit: is it a factual dispute about what the 1949 text requires, or a normative dispute about what a just law of war should require regardless of text?',
    'Textual and drafting-history analysis (was Common Article 3''s ''in all circumstances'' language intended as absolute, or as a floor conditioned on the drafters'' assumptions about symmetric interstate conflict) combined with tracking which reading each ratifying state''s domestic courts have adopted.',
    'If the disagreement is genuinely textual, one reading is more defensible as correct interpretation and the others are advocacy; if it is genuinely normative (the text is ambiguous and readers project their values onto it), all three readings remain equally defensible as readings and the kernel is irreducibly contested rather than resolvable by better textual scholarship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_disagreement_locus, conceptual, 'Whether the security-maximization/humanitarian-ceiling disagreement is a resolvable textual question or an irreducible normative one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t4, geneva_conventions_1949__security_maximization_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(gene_tr_t8, geneva_conventions_1949__security_maximization_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(gene_tr_t16, geneva_conventions_1949__security_maximization_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__security_maximization_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gene_be_t4, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(gene_be_t8, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(gene_be_t16, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 16, 0.77).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gene_su_t4, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 4, 0.71).
narrative_ontology:measurement(gene_su_t8, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(gene_su_t16, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 16, 0.84).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 24, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language label 'the Geneva Conventions govern asymmetric conflict' per the ε-invariance principle. The security_maximization_reading, humanitarian_ceiling_reading, and conditional_reciprocity_reading each author a different ε, a different beneficiary/victim structure, and a different claimed_type from the same kernel text, because measuring 'the Conventions' by the security-maximization observable versus the humanitarian-ceiling observable produces incompatible extraction values — this is the textbook two-constraints-not-one case the BGS decomposition models. The security_maximization_reading's axiom that no protection is non-derogable directly forecloses the humanitarian_ceiling_reading's foundational premise (some protections are absolute) within any single legal framework; it coexists with conditional_reciprocity_reading because both share a conditioning logic (this reading conditions on necessity, that one on reciprocity) without one framework being forced to reject the other's core claim outright.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
