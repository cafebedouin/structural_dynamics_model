% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_maximal, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Maximal Withdrawal Reading: Mandatory Full Territorial Retrocession
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) was adopted in the aftermath of the Six-Day
 *   War to establish a principle of territorial settlement: that territory
 *   taken by force cannot be legitimately retained. The resolution's text
 *   contains a famous ambiguity: the French version uses the definite article
 *   ('le retrait des forces armées des territoires occupés'—withdrawal from
 *   the occupied territories), while the English version uses the indefinite
 *   article ('withdrawal of armed forces from territories occupied in the
 *   recent conflict'—withdrawal from territories occupied). This linguistic
 *   difference has spawned two incompatible readings: the MAXIMAL WITHDRAWAL
 *   READING instantiated here holds that the definite article binds the
 *   occupier to full retrocession of ALL occupied territory without
 *   negotiation. The PARTIAL WITHDRAWAL READING holds that the indefinite
 *   article permits discretionary withdrawal and allows occupiers to retain
 *   strategically important territory if they accept new secure boundaries.
 *   The INTERPRETIVE AUTHORITY STRUCTURE reading contests not the scope of
 *   withdrawal but WHO has the authority to resolve the ambiguity—whether
 *   interpretation belongs to the ICJ, to the drafting states, or to
 *   customary practice. This story instantiates the maximal reading
 *   exclusively, treating it as a single ε-invariant constraint with its own
 *   beneficiary structure, enforcement mechanism, and type classification.
 *   The other readings are separate constraint stories, linked via the
 *   network field.
 *
 * KEY AGENTS:
 *   - Dispossessed territorial claimants: populations/states with legal standing to claim territories under occupation, beneficiaries of the maximal reading
 *   - Occupying state: powerful actor with military control of territory, incentive to adopt the partial reading, subject to the maximal reading if enforced
 *   - ICJ and international courts: institutional interpreters with authority to determine which reading becomes binding law
 *   - UN Security Council permanent members: drafters and institutional gatekeepers with divergent strategic interests
 *   - Drafting-state intent advocates: excluded voices arguing the definite article is a translation artifact, not an authorial mandate
 *   - Regional powers with strategic interests: payers under the maximal reading (their territorial gains are delegitimized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.72).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Maximal Withdrawal Reading: Mandatory Full Territorial Retrocession").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '787a633c-4c78-4f6d-99c8-8e72f1261f28').
narrative_ontology:cs_kernel_codification('787a633c-4c78-4f6d-99c8-8e72f1261f28', fixed_text).
narrative_ontology:cs_authority_grounding('787a633c-4c78-4f6d-99c8-8e72f1261f28', lineage).
narrative_ontology:cs_interpretation_layer_present('787a633c-4c78-4f6d-99c8-8e72f1261f28').
narrative_ontology:cs_reading_relation('787a633c-4c78-4f6d-99c8-8e72f1261f28', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('787a633c-4c78-4f6d-99c8-8e72f1261f28', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('787a633c-4c78-4f6d-99c8-8e72f1261f28', foundational, textual_primacy_territorial_mandate).
narrative_ontology:cs_axiom_status(textual_primacy_territorial_mandate, holdable).
narrative_ontology:cs_axiom_grounding('787a633c-4c78-4f6d-99c8-8e72f1261f28', textual_primacy_territorial_mandate, deontological).
narrative_ontology:cs_axiom('787a633c-4c78-4f6d-99c8-8e72f1261f28', foundational, full_retrocession_categorical_obligation).
narrative_ontology:cs_axiom_status(full_retrocession_categorical_obligation, holdable).
narrative_ontology:cs_axiom_grounding('787a633c-4c78-4f6d-99c8-8e72f1261f28', full_retrocession_categorical_obligation, conventional).
narrative_ontology:cs_reference_frame('787a633c-4c78-4f6d-99c8-8e72f1261f28', charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('787a633c-4c78-4f6d-99c8-8e72f1261f28', contemporary_occupation_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('787a633c-4c78-4f6d-99c8-8e72f1261f28', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council_permanent_members).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, regional_powers_with_strategic_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States or populations from which territory was occupied by force. Under this reading, UNSC 242 establishes their legal right to full restoration of occupied lands without negotiation or exchange. They cannot exit the constraint—its operation IS their enforcement mechanism. They depend entirely on the maximal reading's interpretation being adopted by the ICJ, occupying state, or enforcing parties.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants, beneficiary,
    moderate, generational, trapped, global).

% Controls the occupied territory and decides whether to recognize the maximal reading's binding force. Can resist interpretation through doctrinal argument (secure boundaries principle, strategic necessity), military deterrence, and diplomatic pressure. Faces the constraint as a mandatory obligation if the maximal reading prevails; their exit is constrained by international law compliance pressure and the cost of unilateral defection.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, agenda_setter,
    powerful, generational, constrained, global).

% Serve as the authoritative interpreters of UNSC 242's text under this reading. Their rulings establish whether the French definite article ('les territoires') creates a binding full-retrocession mandate or whether English indefinite phrasing permits discretionary withdrawal. Their interpretation is the mechanism by which the constraint transitions from text to enforceable rule.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_and_international_courts, observer,
    institutional, generational, analytical, universal).

% Drafted UNSC 242 and control its enforcement through Council authority. Under the maximal reading, they are bound to treat the text as creating a mandatory obligation they can invoke. Their interests diverge: some benefit from a maximal reading that restrains regional power-seekers; others (or their allies) pay through constraints on client-state territorial acquisitions. They retain significant exit through non-enforcement and reinterpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council_permanent_members, agenda_setter,
    institutional, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council_permanent_members, payer).

% Historians, legal scholars, and diplomatic records argue the drafters intended discretionary withdrawal ('territories' in English, not 'all territories'). Under the maximal reading their voice is structurally suppressed—textual originalism overrides intent-reconstruction as an interpretive method. They would object if admitted to the interpretation process but are excluded by the maximal reading's commitment to textual primacy over authorial intent.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, drafting_state_intent_advocates, excluded,
    institutional, generational, trapped, universal).

% States that have acquired territory through occupation and benefit from a permissive (partial) interpretation of UNSC 242. Under the maximal reading, they bear the cost of having their territorial acquisitions delegitimized as violations of an non-negotiable Charter principle. Their exit options are limited: they can resist interpretation through diplomatic channels, but compliance pressure from the UN system constrains unilateral refusal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, regional_powers_with_strategic_interests, payer,
    powerful, generational, constrained, regional).

% Scholarship, law review articles, and academic consensus form the epistemic background of interpretation. The maximal reading derives legitimacy from textual clarity arguments and Charter-grounded legal theory. The community's role is analytical—they articulate the structural basis for the maximal reading without directly enforcing it, but their consensus shapes institutional behavior.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_community, observer,
    organized, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding international legal norm preventing territorial acquisition by force: if withdrawal from ALL occupied territories is mandatory (not discretionary), states cannot profit from military conquest. The coordination function solves a collective-action problem: absent such a norm, regional powers race to acquire territory; with it, territorial status quo becomes a legal baseline that cannot be redrawn unilaterally.
% TRANSFER_FUNCTION: Transfers legal standing from occupying state to dispossessed claimants. Under the maximal reading, the constraint moves enforcement authority from the occupier (who controls the territory) to the international community and the claimant state (who hold the legal entitlement). It also moves diplomatic legitimacy: occupation becomes ipso facto illegitimate rather than a negotiable bargaining position.
% ABSENT_VOICES: Occupying states and their strategic allies are formally present in the UN system but structurally excluded from the interpretation process by the maximal reading's textual commitment: they would argue for the partial reading (discretionary withdrawal), secure boundaries principle, and deference to customary practice. Drafting-state intent advocates are also excluded: historians who say the drafters meant 'some territories' are suppressed by the maximal reading's method. Realist scholars arguing that law cannot override strategic necessity are absent from the consensus.
% DISAPPEARANCE_RATIONALE: If this reading of UNSC 242 disappeared—if the maximal withdrawal clause were no longer binding or became unenforceable—states currently held to full retrocession obligations would be free to negotiate partial withdrawals, retain strategic territories, and treat occupation as a bargaining position rather than a violation. Territorial settlement frameworks would shift from legal obligation to strategic negotiation. The legal basis for countless contemporary territorial disputes would evaporate.
% FOUNDING_PROBLEM: Post-WWII decolonization and Cold War regional conflicts generated a wave of territorial occupations. The founding problem was: how can the international community prevent states from treating military conquest as equivalent to legitimate land acquisition? How can it establish that territory taken by force cannot become territory legitimately held? UNSC 242 (1967) was adopted in the wake of the Six-Day War to codify a principle: occupation by force creates an obligation to withdraw, not a basis for territorial claim.
% FOUNDING_PROBLEM_CORROBORATION: The Security Council record and diplomatic correspondence from 1967 corroborate that the founding problem was live: states were acquiring territory by force and treating it as a negotiable bargaining asset. The ICJ, in advisory opinions and contentious cases, treats territorial integrity as a foundational principle of contemporary international law. Independent scholars specializing in the law of territorial acquisition confirm that prevention of conquest-based title is a central objective of modern jus cogens. Occupying states, however, contest this: they argue the founding problem is overstated and that strategic necessity and secure boundaries justify retention of some occupied territories.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The maximal reading is claimed as ROPE because it instantiates genuine coordination: it solves the problem of territorial acquisition by force by making such acquisition legally invalid. However, the metrics reveal substantial extractiveness (0.78) and suppression (0.72) because: (1) enforcement depends on the ICJ adopting this reading and then enforcing it against powerful occupying states, which creates a high suppression requirement—the reading must suppress alternative interpretations (the partial reading, the authority-structure reading) to remain binding; (2) extractiveness is high because the reading transfers legal standing entirely away from the occupier toward the claimant, a comprehensive reshaping of the occupier's legal position; (3) theater is moderate (0.42) because a portion of the constraint's operation is rhetorical—the maximal reading is invoked in diplomatic statements and academic literature with real strategic effect, but enforcement is episodic and often weak. The measurement series shows extractiveness rising from 0.55 (1967, just after adoption) to a plateau of 0.78 around 2000 (as the reading became settled doctrine in academic and judicial circles) and remaining stable through the projected endpoint. Suppression follows a similar trajectory, rising as the ICJ entrenches the textual interpretation method (suppressing intent-reconstruction arguments). Theater rises early and plateaus as the reading becomes institutionalized. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (occupying state) and the beneficiary seat (dispossessed claimants) will compute different types. The occupier, under the maximal reading, experiences a mandatory obligation to withdraw from all occupied territory—no negotiation, no secure boundaries exception, no strategic necessity override. From their seat, the constraint appears extractive (they lose territory) and suppressive (alternatives are foreclosed by the interpretation method). They will compute the constraint as Snare or Tangled Rope depending on whether they perceive any genuine coordination benefit (they might argue the general norm benefits them by preventing others from conquering THEIR territory, but the extraction cost is immediate and certain while the benefit is abstract and contingent). The beneficiary seat (claimant) experiences this as pure benefit: their legal standing is affirmed, their territorial entitlement is recognized, and their exit is impossible in the sense that they cannot relinquish their claim (they are structurally bound to the benefit). From their seat, the constraint is Rope or even appears Mountain-like (a binding legal principle that persists regardless of their preference). The engine's per-seat computation will capture this divergence; the claim/metric independence principle means we author the maximal reading's structure as Rope (genuine coordination) while expecting per-seat classifications to split between Rope (for beneficiaries) and Snare (for occupiers).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply across the stakeholder seats. For dispossessed claimants (beneficiary): d approaches 0.0—they benefit from the constraint's operation without running it, and their exit is structurally impossible (they cannot opt out of their territorial claim). For the occupying state (agenda_setter): d approaches 1.0 when the maximal reading prevails—the constraint extracts from them (requires them to abandon territory) and offers no reciprocal benefit; their exit is constrained by compliance pressure and the cost of violating international law. For the ICJ and courts (observer): d = 0.5 (neutral)—they neither benefit nor pay; they execute the constraint's interpretation. For the UN Security Council (agenda_setter + payer): d is heterogeneous—states whose allies are occupiers pay through the constraint's effect on their regional partners; states with no regional interests benefit from the general norm. This divergence is the engine's key output: the same textual constraint generates opposite classifications (benefit vs. extraction) depending on the stakeholder's power and position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent territorial acquisition by force) is structurally live in 2026, but its instantiation through UNSC 242 has undergone significant drift. The maximal reading was forcefully articulated during the Cold War and the immediate post-decolonization period (1967–1980s) when the international legal community treated the no-conquest principle as foundational. However, by the early 21st century, the founding problem persists but the constraint's practical effect has attenuated: occupying states increasingly reject the maximal reading's binding force; the ICJ itself has sometimes adopted softer language (acknowledging the partial reading's legitimacy in certain contexts); and enforcement has become episodic rather than systematic. This suggests a potential mandatrophy: the founding problem remains live, but the constraint's operation has become increasingly theatrical. The theater_ratio rising from 0.28 to 0.42 and plateauing suggests that much of UNSC 242's operation is now normative performance—states cite it diplomatically, academics invoke it, but actual compliance and enforcement are weak relative to the reading's stated scope. However, mandatrophy has not fully resolved yet: the maximal reading remains a live legal claim with ICJ support, and it does still function as a constraint (compliance pressure is real, even if episodic). The measurement trajectory shows the constraint drifting toward Piton territory (high theater, persistent extractiveness, weak enforcement) but not yet definitively classified as one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_primacy_vs_intent_reconstruction,
    'Does the maximal reading''s commitment to textual interpretation (French definite article ''les'') override the drafters'' documented intention to permit discretionary withdrawal?',
    'Jurisprudence from the ICJ on treaty interpretation methods (Vienna Convention Article 31–32): does the Court privilege the treaty''s plain text or the states'' contemporaneous intent? Which method does it apply and why?',
    'If intent-reconstruction is validated, the partial reading becomes structurally tenable and this reading loses its interpretive mandate. If textual primacy is sustained, the maximal reading is reinforced. This is the central methodological divide between the readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_primacy_vs_intent_reconstruction, conceptual, 'Whether treaty interpretation prioritizes text or authorial intent.').

omega_variable(
    enforcement_capacity_vs_occupier_noncompliance,
    'What is the actual enforcement architecture that makes the maximal withdrawal clause binding? If an occupying state simply refuses to comply, what mechanism compels withdrawal?',
    'Historical analysis of cases where UNSC 242 was invoked and compliance measured: are withdrawals enforced by Council military action, sanctions regimes, diplomatic isolation, or do they occur only when the occupier decides it is strategically convenient? What is the correlation between the maximal reading''s adoption and actual territorial withdrawal?',
    'If enforcement proves structurally absent (compliance occurs only when voluntary), the constraint reverts to a norm-expressing document without binding force—extractiveness would be lower, suppression would reveal itself as performative. If enforcement proves robust (sanctions, military action follow non-compliance), the constraint is genuinely binding. This determines whether the constraint is Rope (real coordination) or Piton (theatrical compliance theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_occupier_noncompliance, empirical, 'Whether the maximal reading produces enforceable withdrawal or remains rhetorical.').

omega_variable(
    reading_family_coherence,
    'This constraint is one reading of a contested kernel (unsc_242_withdrawal_clause). The kernel is the Charter Article 2(4) territorial integrity principle itself. Do the three declared readings (maximal, partial, authority_structure) exhaust the structurally distinct interpretations, or are there other readings that branch differently?',
    'Systematic survey of legal scholarship and state practice: enumerate all live positions on UNSC 242''s scope and map them to the three declared readings. Identify any position that is structurally orthogonal to this set.',
    'If there are unmapped readings, the constraint family is incomplete and the inferred_coupling_protocol will misroute edges between this constraint and siblings. If the three readings exhaust the structural space, the constraint family is complete and the coupling model is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_coherence, conceptual, 'Whether the three kernel readings are exhaustive or whether other structurally distinct readings exist.').

omega_variable(
    dispossessed_claimant_standing,
    'Under the maximal reading, who has standing to invoke the withdrawal clause? Is it only the original occupier''s prior sovereign, or does it extend to populations, ethnic groups, or successor states that claim the territory?',
    'Analysis of UNSC practice and ICJ cases: which entities have been recognized as legitimate claimants in territorial disputes? What criteria determine standing?',
    'If standing is narrowly defined (only recognized prior sovereigns), the beneficiary set is small and enforcement is straightforward. If standing is broadly defined (populations, successor states, ethnic groups all have claims), the constraint becomes vastly more complex and potentially incoherent—multiple overlapping claims on the same territory undermine the no-conquest principle. The extractiveness of the constraint depends on who counts as a beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispossessed_claimant_standing, empirical, 'Whether the maximal reading''s beneficiary class is clearly defined or inherently contested.').

omega_variable(
    kernel_reading_vs_sibling_foreclosure,
    'Does the maximal reading logically foreclose the partial reading, or do both remain live interpretive options within international law?',
    'Logical analysis of the two readings'' foundational axioms: if the maximal reading is true (ALL withdrawal is mandatory, definite article controls), can the partial reading be true (SOME withdrawal is discretionary, indefinite article permits flexibility) within the same legal framework? Or do the axioms contradict each other such that only one can hold at any given time?',
    'If they foreclose each other, the constraint landscape is binary—one reading or the other must prevail for the system to be coherent. If they coexist, the system is ambiguous and enforcement depends on which reading the adjudicator adopts. This affects whether the constraint is interpreted as settled law or as perpetually contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_sibling_foreclosure, conceptual, 'Whether the maximal and partial readings logically exclude each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_max_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(unsc_242_max_tr_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(unsc_242_max_tr_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(unsc_242_max_tr_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(unsc_242_max_tr_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(unsc_242_max_tr_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(unsc_242_max_tr_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement(unsc_242_max_tr_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 58, 0.42).

% Extraction over time
narrative_ontology:measurement(unsc_242_max_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(unsc_242_max_be_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(unsc_242_max_be_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(unsc_242_max_be_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(unsc_242_max_be_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(unsc_242_max_be_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement(unsc_242_max_be_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 48, 0.78).
narrative_ontology:measurement(unsc_242_max_be_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 58, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unsc_242_max_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(unsc_242_max_su_t8, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(unsc_242_max_su_t16, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(unsc_242_max_su_t24, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(unsc_242_max_su_t32, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(unsc_242_max_su_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(unsc_242_max_su_t48, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 48, 0.72).
narrative_ontology:measurement(unsc_242_max_su_t58, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 58, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% UNSC Resolution 242 is a single legal text that instantiates three structurally distinct constraints under different readings. The maximal reading (this story) treats the text as binding full retrocession of all occupied territories (definite article primary). The partial reading treats the text as permitting discretionary withdrawal under secure boundaries doctrine (indefinite article primary). The authority structure reading contests who has the interpretive authority to resolve between them. These are not measurements of the same constraint; they are different constraints unified by a shared kernel. The readings do not coexist within a single framework—the maximal reading foreclosed by the partial reading's adoption in practice, or they coexist as competing live positions held by different parties in ongoing dispute. See constraint family structure in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
