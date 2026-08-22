% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary Reading: State Cannot Compel Medical Intervention
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story represents the 'bodily_autonomy_primary' reading of
 *   the contested 'vaccine_mandate_balance' kernel. The reading holds that
 *   individual consent to medical intervention is inviolable — the state
 *   cannot compel vaccination regardless of collective benefit, disease
 *   severity, or transmission dynamics. The kernel is the question of whether
 *   and when state vaccine mandates are legitimate; this reading instantiates
 *   one specific constraint: an absolute bar on compulsion. The sibling
 *   readings are 'public_health_primary' (collective protection supersedes
 *   individual consent when voluntary compliance fails) and
 *   'proportionality_reading' (mandates permissible only under strict
 *   proportionality thresholds with robust exemptions). This story authors
 *   ONLY the bodily_autonomy_primary constraint — its ε, beneficiaries,
 *   victims, and structure are assessed by this reading's own lights, not
 *   averaged across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.85).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.92).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Bodily Autonomy Primary Reading: State Cannot Compel Medical Intervention").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf').
narrative_ontology:cs_kernel_codification('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', distributed).
narrative_ontology:cs_authority_grounding('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', diffuse_epistemic).
narrative_ontology:cs_reading_relation('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', foundational, bodily_autonomy_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', bodily_autonomy_absolute, deontological).
narrative_ontology:cs_axiom('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', foundational, state_compulsion_never_justified_by_collective_benefit).
narrative_ontology:cs_axiom_status(state_compulsion_never_justified_by_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', state_compulsion_never_justified_by_collective_benefit, deontological).
narrative_ontology:cs_reference_frame('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', pre_bioethics_state_medical_power).
narrative_ontology:cs_drift_state('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4d9d6b1a-1e13-4305-bb86-a0b91ebe7caf', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, liberty_advocacy_organizations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, individual_refusers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, public_health_infrastructure).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to impose vaccine mandates to achieve herd immunity and protect vulnerable populations. Under this reading, its mandate power is structurally blocked — it cannot compel medical intervention regardless of collective benefit. The authority bears the cost of foregone policy tools and must rely on voluntary measures.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, state_public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who would be compelled to vaccinate under a mandate regime. Under this reading, they are the primary victims when mandates are imposed — they bear the direct coercive burden of forced medical intervention. Their exit options are minimal: compliance, penalties, or social exclusion.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced, payer,
    powerless, biographical, trapped, national).

% Individuals who refuse vaccination on grounds of conscience, bodily autonomy, or medical skepticism. Under this reading, they benefit from the absolute barrier against state compulsion — their refusal carries legal weight. They can exercise exit through exemption claims or jurisdictional movement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, individual_refusers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals who cannot be vaccinated or for whom vaccines are less effective, and who face elevated risk when community transmission is high. Under this reading, they are NOT treated as victims — their elevated risk is framed as inherent to liberty, not as a cost imposed by the constraint. They have limited exit (cannot easily avoid exposure) but no structural voice in this reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_exposed, excluded,
    powerless, biographical, constrained, national).

% Civil liberties groups, bodily autonomy advocates, and legal organizations that litigate against mandates. They benefit institutionally from the absolute autonomy framing — it provides clear legal tools, fundraising narratives, and organizational mission coherence. They have high exit options (can shift jurisdiction, forum, strategy).
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, liberty_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).

% The epidemiological surveillance, outbreak response, and vaccination delivery systems that would use mandates as a tool. Under this reading, they bear the cost of a constrained toolkit — longer outbreaks, higher resource expenditure on voluntary campaigns, and inability to deploy the most effective intervention. They cannot exit their mandate to protect population health.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_infrastructure, payer,
    institutional, generational, analytical, national).

% Adjudicates challenges to mandates under this reading. Provides the authoritative interpretation that gives the constraint its force. Neither collects rents nor pays costs directly — its role is to certify the boundary.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, constitutional_court, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function under this reading — the constraint is framed as a negative right (freedom from compulsion) that blocks a collective action tool rather than enabling one. Any coordination (voluntary vaccination) occurs despite the constraint, not because of it.
% TRANSFER_FUNCTION: Transfers the cost of disease burden from the unvaccinated-coerced (who would bear compulsion) to the immunocompromised-exposed (who bear elevated infection risk) and to public health infrastructure (which bears higher containment costs). The state authority transfers its policy power to the individual refuser's veto.
% ABSENT_VOICES: Immunocompromised-exposed individuals and their caregivers — they would object to the framing that their elevated risk is 'inherent to liberty' rather than a cost of the autonomy absolute. They are structurally excluded from the beneficiary/victim calculus of this reading. Also absent: future cohorts who face resurgent diseases that mandates could have prevented.
% DISAPPEARANCE_RATIONALE: If this absolute autonomy constraint vanished overnight, states could impose mandates when voluntary compliance fails. The unvaccinated-coerced would move from protected to targeted; immunocompromised-exposed would gain a structural shield; public health infrastructure would regain its most potent tool; liberty organizations would lose their absolute legal anchor. The epidemiological and legal landscape would reorganize.
% FOUNDING_PROBLEM: Historical experience of state-compelled medical interventions (eugenics, forced sterilization, unethical experimentation) created a founding commitment to bodily integrity as a non-derogable right. The arrangement was built to prevent the state from ever again treating bodies as instruments of collective policy.
% FOUNDING_PROBLEM_CORROBORATION: The Nuremberg Code and subsequent bioethics frameworks (Belmont Report, UNESCO Declaration) corroborate the founding problem from outside the benefiting parties — they were crafted by the international scientific and ethical community in response to state abuses, not by liberty advocacy organizations. However, the status is contested: public health authorities argue the founding problem (state abuse) is addressed by procedural safeguards (proportionality, exemptions), not by an absolute bar, and that the absolute bar now creates a new abuse (preventing effective protection of the vulnerable).
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.85) is high because the constraint's operation — when mandates are imposed — extracts bodily integrity and medical autonomy from the unvaccinated_coerced through coercive mechanisms (fines, exclusion, force). Suppression (0.92) is very high because the constraint's persistence depends on actively blocking the state's primary epidemic control tool and suppressing the policy alternative (mandates) through constitutional litigation, legislative bans, and judicial review. Theater ratio (0.15) is low because the constraint's enforcement (court injunctions, statutory bars) is functionally real, not performative — it actually prevents mandates. Accessibility collapse (0.78) is high because once the absolute autonomy principle is accepted, the mandate alternative is structurally foreclosed — no proportionality calculus can overcome it. Resistance (0.68) is substantial because public health authorities, epidemiologists, and vulnerable populations actively contest the constraint's dominance.
 *
 * PERSPECTIVAL GAP:
 *   The unvaccinated_coerced seat experiences this as a snare (pure extraction via compulsion) when mandates exist; the liberty_advocacy_organizations seat experiences it as a mountain (inviolable right). The engine computes this divergence from the structural data — the same constraint reads as extraction from the target seat and as coordination from the beneficiary seat. The immunocompromised_exposed seat is analytically invisible in this reading (excluded, not victim) but would be a primary victim in the public_health_primary reading — this cross-reading divergence is the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The unvaccinated_coerced are full targets (d ≈ 1.0) — they bear the coercive extraction when mandates are imposed. The immunocompromised_exposed are NOT victims under this reading — their risk is framed as the price of liberty, not a cost of the constraint. The state_public_health_authority and public_health_infrastructure are payers — they bear the cost of a constrained toolkit. Individual_refusers and liberty_advocacy_organizations are beneficiaries — they collect the protective shield against compulsion. The constitutional_court is the analytical observer. Exit options differentiate: unvaccinated_coerced are trapped (no exit from compulsion if mandate exists); immunocompromised_exposed are constrained (cannot easily avoid exposure); individual_refusers are mobile (can claim exemptions, move jurisdictions); liberty_organizations have arbitrage-grade exit (forum shopping, strategy shifts).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state abuse of medical compulsion) is contested as live vs. addressed-by-safeguards. The constraint persists as an absolute bar even where proportionality safeguards exist — this suggests mandatrophy (the arrangement outlives its precise function). However, liberty advocates argue the founding problem remains live because safeguards are routinely eroded in emergencies. The classification as snare (not mountain) captures that the constraint's current operation extracts from the coerced — it is not a neutral natural law but an active bar with victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the ''vaccine_mandate_balance'' kernel a single commitment with multiple readings, or are these structurally distinct constraints that merely share a label?',
    'Apply the ε-invariance test: if the public_health_primary reading and bodily_autonomy_primary reading author substantially different ε values for the same observable (mandate imposition), they are distinct constraints. Document the ε gap and the structural divergence in beneficiary/victim sets.',
    'If distinct constraints, they must be separate JSON files linked by network.affects_constraints. If one kernel with readings, the committer frame (cs_structure) captures the relationship. The current authoring treats them as one kernel with readings per the kernel context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel framing correctly captures the structural relationship between the three positions.').

omega_variable(
    immunocompromised_victim_status,
    'Does the immunocompromised_exposed group''s exclusion from victim status in this reading reflect a genuine structural judgment, or is it a framing artifact that obscures their extraction?',
    'Compare the immunocompromised_exposed situation across all three readings. If they are victims in public_health_primary and payers in proportionality_reading but excluded in bodily_autonomy_primary, the exclusion is reading-relative — a structural artifact of the autonomy absolute. Their elevated risk is invariant; only the classification changes.',
    'If the exclusion is a framing artifact, this reading systematically misclassifies a victim group, inflating its own coherence. The engine''s cross-reading comparison would reveal the displacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immunocompromised_victim_status, conceptual, 'Whether the immunocompromised_exposed are genuinely not victims or merely rendered invisible by the reading''s beneficiary structure.').

omega_variable(
    mandate_imposition_counterfactual,
    'The base extractiveness of 0.85 assumes mandate imposition — but under this reading, mandates are blocked. Is ε measured for the counterfactual (if mandates were imposed) or for the actual state (mandates blocked)?',
    'Per the ε-referent rule for kernel-reading stories: ε''s referent is the standing arrangement under contest (the mandate regime), assessed by this reading''s lights. This reading sees the mandate regime as the threat; its ε reflects the extraction that WOULD occur if the constraint failed. The constraint''s current success (blocking mandates) does not lower its ε — ε is the threat magnitude.',
    'Clarifies that high ε does not imply the constraint is currently extracting; it implies the constraint is the barrier against a high-extraction arrangement. This is consistent with the snare classification: the constraint is the snare''s JAWS (the thing that would extract if not blocked), not the snare itself. Reconsider whether ''snare'' is the right claimed_type or whether the constraint is the anti-snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_imposition_counterfactual, conceptual, 'Whether ε refers to the mandate regime (threat) or the autonomy constraint (barrier) — critical for classification coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.05).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 10, 0.08).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 30, 0.12).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 40, 0.14).
narrative_ontology:measurement(vacc_tr_t50, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(vacc_be_t50, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(vacc_su_t50, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 50, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the vaccine_mandate_balance kernel family. The three readings instantiate distinct constraints with different ε values, beneficiary/victim structures, and claimed types. bodily_autonomy_primary: ε=0.85, victims={unvaccinated_coerced}, claimed=snare. public_health_primary: ε=0.35 (mandate as coordination), victims={immunocompromised_exposed}, claimed=tangled_rope. proportionality_reading: ε=0.55, victims={unvaccinated_coerced, immunocompromised_exposed under weak exemptions}, claimed=tangled_rope. The kernel contest is the structural divergence across these three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, powerless, 0.95).
constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, moderate, 0.15).
constraint_indexing:directionality_override(vaccine_mandate_balance__bodily_autonomy_primary, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
