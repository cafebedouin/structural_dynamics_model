% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as State-Militia-Authority Guarantee (Collective Right Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story generates the collective-right reading of the Second Amendment
 *   kernel: the claim that the amendment's operative guarantee runs to states
 *   maintaining organized militias, not to individuals possessing firearms
 *   independent of militia service. This was the dominant lower-federal-court
 *   reading for much of the twentieth century (drawing on United States v.
 *   Miller, 1939) and remains the position of the Heller dissent (Stevens,
 *   J., joined by Souter, Ginsburg, Breyer, JJ.) but was rejected as the
 *   controlling reading by the Heller majority in 2008 and McDonald v.
 *   Chicago in 2010. Extraction is low under this reading because its
 *   institutional scope is narrow — it allocates authority between state and
 *   federal governments over an organized militia structure that has been
 *   functionally superseded by the National Guard, rather than adjudicating a
 *   live individual-rights conflict. This story does not describe the
 *   individual_right_reading or civic_right_reading constraints, which are
 *   separate files with their own ε, beneficiary/victim structure, and
 *   classification, linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - state_governments: institutional beneficiary retaining militia-organizing authority
 *   - organized_militias: the historical entity (now the National Guard) the clause is read to protect
 *   - federal_regulatory_authority: institutional beneficiary gaining regulatory latitude
 *   - individual_gun_owners: excluded from the reading's protective scope entirely
 *   - federal_judiciary: agenda-setter whose adoption or rejection of this reading determines its doctrinal force
 *   - constitutional_historians: analytical observers of drafting history and militia statutes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as State-Militia-Authority Guarantee (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, 'b3bbf9c7-64df-48be-a2be-95c87086234c').
narrative_ontology:cs_kernel_codification('b3bbf9c7-64df-48be-a2be-95c87086234c', fixed_text).
narrative_ontology:cs_authority_grounding('b3bbf9c7-64df-48be-a2be-95c87086234c', lineage).
narrative_ontology:cs_interpretation_layer_present('b3bbf9c7-64df-48be-a2be-95c87086234c').
narrative_ontology:cs_reading_relation('b3bbf9c7-64df-48be-a2be-95c87086234c', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b3bbf9c7-64df-48be-a2be-95c87086234c', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('b3bbf9c7-64df-48be-a2be-95c87086234c', foundational, operative_clause_subordinate_to_militia_purpose).
narrative_ontology:cs_axiom_status(operative_clause_subordinate_to_militia_purpose, holdable).
narrative_ontology:cs_axiom_grounding('b3bbf9c7-64df-48be-a2be-95c87086234c', operative_clause_subordinate_to_militia_purpose, conventional).
narrative_ontology:cs_axiom('b3bbf9c7-64df-48be-a2be-95c87086234c', foundational, no_free_standing_individual_arms_right_exists).
narrative_ontology:cs_axiom_status(no_free_standing_individual_arms_right_exists, overridden).
narrative_ontology:cs_axiom_grounding('b3bbf9c7-64df-48be-a2be-95c87086234c', no_free_standing_individual_arms_right_exists, conventional).
narrative_ontology:cs_reference_frame('b3bbf9c7-64df-48be-a2be-95c87086234c', founding_era_militia_federalism).
narrative_ontology:cs_drift_state('b3bbf9c7-64df-48be-a2be-95c87086234c', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('b3bbf9c7-64df-48be-a2be-95c87086234c', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, gun_control_advocacy_organizations).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_balance_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, state_police_power_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states retain robust constitutional footing to organize, arm, and regulate militias (historically the ancestor of the National Guard) without federal preemption of that specific function, and correspondingly wide latitude to regulate private firearms possession since the amendment is not read to constrain them on that separate question.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% The historical civic-militia structure (today largely subsumed into the National Guard system) is the entity the clause is read to actually protect: its continued existence and the states' authority to arm and organize it, not an individual entitlement running to private persons outside that structure.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, generational, constrained, national).

% Congress and federal agencies gain interpretive room to legislate broadly on firearms (background checks, manufacturing standards, possession restrictions) because the amendment is read as addressing the state/federal militia relationship rather than erecting an individual constitutional shield against such regulation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_regulatory_authority, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, federal_regulatory_authority, agenda_setter).

% Under this reading, individual persons unconnected to organized militia service have no Second Amendment claim at all — their possession of firearms is a matter left to ordinary legislative and state constitutional processes, not a federally guaranteed personal right. They are structurally outside the reading's protective scope and would object that this erases what they consider the amendment's plain historical meaning.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, excluded,
    moderate, biographical, constrained, national).

% Groups favoring firearms regulation gain a doctrinal foothold: if the amendment protects only state militia authority, sweeping federal and state firearms regulation faces no individual-rights constitutional obstacle, making legislative victories more durable against judicial reversal on Second Amendment grounds.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_control_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Courts adopting this reading (the pre-Heller consensus in several circuits, and the losing position in District of Columbia v. Heller, 2008) treat firearms regulation challenges under rational-basis-like scrutiny rather than heightened individual-rights review, substantially narrowing the amendment's justiciable content.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Examine founding-era militia statutes, the amendment's drafting history, and state constitutional analogues to assess whether the operative clause ('a well regulated Militia') or the prefatory/individual-right clause ('the right of the people') controls the amendment's scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the founding-era federalism question of who controls organized armed force below the federal standing army: this reading holds the amendment coordinates state authority to maintain militias against federal disarmament, not an individual entitlement against state or federal regulation generally.
% TRANSFER_FUNCTION: Under this reading nothing is extracted from individuals because no individual right exists to be burdened; what the reading transfers is interpretive authority — away from courts adjudicating individual firearms claims and toward legislatures (state and federal) setting firearms policy through ordinary political processes.
% ABSENT_VOICES: Individual gun owners asserting a personal constitutional right are excluded from this reading's protective scope entirely; they are present in the political debate but structurally absent from the doctrinal category this reading recognizes. The Heller majority (5-4, 2008) explicitly rejected this reading as the controlling one, though four justices retained it in dissent.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live doctrinal option (as it substantially did after Heller and McDonald v. Chicago), the immediate practical world does not rearrange because it was already the minority post-2008 position, but state and federal firearms regulation lose a categorical shield against individual-rights challenges, and gun control advocacy groups lose a doctrinal argument they still deploy in scope-of-right and level-of-scrutiny arguments even within an individual-right framework.
% FOUNDING_PROBLEM: The founding generation feared a federal standing army could disarm state militias and consolidate coercive power in the national government; the amendment was proposed to guarantee that states could maintain armed, organized militias as a counterweight.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and constitutional scholars outside both the gun-rights and gun-control advocacy camps broadly agree the organized state militia the amendment describes has been legally and functionally superseded by the National Guard under the Militia Act of 1903 and subsequent federalization statutes, and that no state today relies on an independent unorganized militia for defense against federal overreach — the specific founding-era problem is widely treated as historically resolved rather than live, even by scholars who differ sharply on what follows doctrinally from that fact.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at 2025) because this reading, if controlling, imposes no burden on private persons at all — it simply declines to recognize an individual constitutional claim, leaving firearms regulation to ordinary majoritarian processes rather than constraining any party through the amendment itself. Suppression is moderate (0.28): the reading itself suppresses nothing, but its rejection by the Heller majority means it now operates mainly as a dissenting/minority position whose adoption would require overturning precedent, which some readers experience as suppressed rather than merely unadopted. The extraction bump around 2008 reflects the Heller litigation moment itself, when the reading's doctrinal fate was being actively contested and briefed with high stakes; it recedes afterward as the reading settles into minority/historical status rather than active contestation. Accessibility collapse is moderate (0.35): this reading remains available to scholars, dissenting justices, and state legislatures as an interpretive resource even though it lost at the Supreme Court.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments, organized militias (as institutionally succeeded by the National Guard), and federal regulatory authority are beneficiaries because this reading either preserves their institutional prerogatives or expands their regulatory latitude. Individual gun owners are excluded rather than victimized in the extractive sense — the reading does not take something from them so much as decline to recognize a claim they assert exists; the schema classifies them as excluded rather than victims because no resource or right is being actively extracted through enforcement, only a doctrinal recognition being withheld.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal disarmament of state militias as a tool of consolidated coercive power — is corroborated as dead by military historians outside both advocacy camps: the unorganized militia this reading protects has been legally superseded by the federalized National Guard for over a century. This creates the diagnostic tension the six-questions battery is built to surface: a reading whose founding problem is dead but which some parties (state governments seeking regulatory latitude, gun control advocates) still invoke for its present-day doctrinal utility rather than its original militia-federalism purpose. That mismatch — dead founding problem, still-live doctrinal invocation — is exactly the signal the founding_problem_status/disappearance_verdict cross-check is designed to catch, and it is flagged here as 'contested' precisely because the reading's practical utility today is decoupled from the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_controlling_weight,
    'Does the prefatory clause (''A well regulated Militia...'') limit and define the scope of the operative clause (''the right of the people...''), or does the operative clause stand as an independent grant that the prefatory clause merely explains one purpose of?',
    'Resolution would require either a definitive historical-linguistic consensus on 18th-century constitutional drafting conventions (unlikely to be reached) or a Supreme Court reversal of Heller''s holding that the prefatory clause does not limit the operative clause — the latter is a live possibility given the closeness of the 2008 vote (5-4) and subsequent changes in Court composition.',
    'If the prefatory-controls reading becomes doctrinally dominant again, extractiveness for this reading would rise sharply as it moved from historical/minority position to actively enforced controlling law, displacing the individual_right_reading''s current beneficiary set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_controlling_weight, conceptual, 'Whether the militia clause grammatically and substantively limits the arms-bearing clause.').

omega_variable(
    militia_act_supersession_effect,
    'Does the federalization of the militia under the Militia Act of 1903 and the Dick Act''s successors functionally moot the collective-right reading''s protective object, since no independent state militia meaningfully exists anymore?',
    'Legal-historical analysis of whether National Guard federalization fully absorbed the constitutional ''militia'' category, versus arguments that unorganized state militias retain residual constitutional status under state constitutions and statutes.',
    'If the militia category is fully superseded, this reading''s beneficiary set (state_governments, organized_militias) may be largely notional rather than operative, supporting the founding_problem_status of ''dead'' authored above; if residual state militia authority survives, the beneficiary set retains live institutional stakes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_act_supersession_effect, empirical, 'Whether National Guard federalization eliminated the constitutional object this reading protects.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the selection of collective_right_reading as one of three parallel constraint stories itself neutral, or does treating all three readings as structurally symmetric obscure that one reading (individual_right_reading) is currently controlling constitutional law while the other two are minority or historical positions?',
    'Track doctrinal status over time via case law citation analysis — the network edges among the three sibling stories could be weighted or annotated by controlling-authority status at any given date rather than treated as timelessly symmetric.',
    'If asymmetry in controlling status is not surfaced, downstream analysis might treat the three readings as equally live when only one currently governs enforceable law, which could misrepresent present extraction distribution across the kernel family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether presenting three sibling readings as parallel constraints obscures their unequal current doctrinal authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__collective_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__collective_right_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_scope__collective_right_reading, theater_ratio, 2025, 0.15).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.05).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__collective_right_reading, base_extractiveness, 1900, 0.06).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.08).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__collective_right_reading, base_extractiveness, 1980, 0.1).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_scope__collective_right_reading, base_extractiveness, 2025, 0.18).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_scope__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.05).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% This story, individual_right_reading, and civic_right_reading form a three-member constraint family decomposing the natural-language label 'the Second Amendment' per the ε-invariance principle. Each reading assigns a different beneficiary/victim structure and a different ε to what colloquial usage treats as one constraint: this reading (collective_right) has the lowest ε because it recognizes no individual claim to burden; individual_right_reading has higher ε because it creates an enforceable barrier against firearms regulation that regulators experience as extraction on their policy authority; civic_right_reading sits between, conditioning the individual right on militia-connection criteria that themselves require enforcement. All three link to each other bidirectionally as siblings in the same kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
