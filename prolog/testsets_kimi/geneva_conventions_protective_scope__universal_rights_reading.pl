% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Universal Protective Floor (Universal Rights Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story instantiates the universal_rights_reading of the
 *   geneva_conventions_protective_scope kernel. Under this reading, Common
 *   Article 3 of the Geneva Conventions, supplemented by international human
 *   rights law, creates a universal floor of protection applicable to all
 *   persons affected by armed conflict without distinction based on combatant
 *   status. This reading expands the beneficiary set to include civilian
 *   populations and non-state armed group members while treating state
 *   military operational flexibility as the primary cost-bearing victim. The
 *   structural delta from sibling readings is precisely this expanded
 *   protected class and the elevated extraction on state targeting and
 *   detention practices. The constraint is claimed as tangled_rope because it
 *   possesses a genuine coordination functionâpreventing a legal vacuum of
 *   protectionâwhile asymmetrically extracting operational discretion from
 *   state military apparatuses that must actively enforce the norms against
 *   themselves and their allies.
 *
 * KEY AGENTS:
 *   - state_parties (agenda_setter/institutional/constrained): Ratified the conventions and administer the treaty regime through domestic military law and doctrine, while bearing operational restrictions.
 *   - state_military_apparatus (payer/institutional/constrained): Conducts targeting, detention, and interrogation operations subject to universal legal constraints; bears the direct operational costs of compliance and legal exposure.
 *   - civilian_populations (beneficiary/powerless/trapped): Receive protective guarantees against direct attack and indiscriminate violence; lack exit from conflict zones.
 *   - non_state_armed_groups (beneficiary/moderate/constrained): Benefit from protective status regardless of combatant status; subject to reciprocal restrictions but protected from summary execution and torture.
 *   - international_judicial_bodies (observer/institutional/analytical): Interpret and apply the universal floor through international criminal tribunals and human rights courts.
 *   - state_sovereignty_absolutists (excluded/organized/analytical): Argue for unrestricted state military discretion and reject human rights law applicability in armed conflict; excluded from the interpretive mainstream of this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Universal Protective Floor (Universal Rights Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '966237be-01ab-4696-b882-f9d5a3e2495a').
narrative_ontology:cs_kernel_codification('966237be-01ab-4696-b882-f9d5a3e2495a', formalized).
narrative_ontology:cs_authority_grounding('966237be-01ab-4696-b882-f9d5a3e2495a', lineage).
narrative_ontology:cs_interpretation_layer_present('966237be-01ab-4696-b882-f9d5a3e2495a').
narrative_ontology:cs_reading_relation('966237be-01ab-4696-b882-f9d5a3e2495a', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('966237be-01ab-4696-b882-f9d5a3e2495a', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('966237be-01ab-4696-b882-f9d5a3e2495a', foundational, universal_protective_floor).
narrative_ontology:cs_axiom_status(universal_protective_floor, holdable).
narrative_ontology:cs_axiom_grounding('966237be-01ab-4696-b882-f9d5a3e2495a', universal_protective_floor, deontological).
narrative_ontology:cs_axiom('966237be-01ab-4696-b882-f9d5a3e2495a', foundational, human_rights_non_displacement).
narrative_ontology:cs_axiom_status(human_rights_non_displacement, holdable).
narrative_ontology:cs_axiom_grounding('966237be-01ab-4696-b882-f9d5a3e2495a', human_rights_non_displacement, conventional).
narrative_ontology:cs_reference_frame('966237be-01ab-4696-b882-f9d5a3e2495a', universal_protective_floor).
narrative_ontology:cs_drift_state('966237be-01ab-4696-b882-f9d5a3e2495a', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('966237be-01ab-4696-b882-f9d5a3e2495a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and ratified the Geneva Conventions and Additional Protocols; maintain the treaty framework through domestic implementation, military manuals, and diplomatic pressure. Bear the operational costs of restricted targeting, detention, and interrogation standards across all conflict types, while remaining bound by treaty withdrawal clauses that are politically prohibitive to exercise.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_parties, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, state_parties, payer).

% Conducts targeting, detention, and interrogation operations subject to universal legal constraints regardless of conflict classification. Bears direct operational costs through restricted rules of engagement, mandatory detainee treatment standards, and exposure to international criminal prosecution for violations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_apparatus, payer,
    institutional, biographical, constrained, global).

% Receive protective legal guarantees against direct attack, indiscriminate violence, and displacement in armed conflict. Their physical exit from conflict zones is often blocked by geography, economic immobility, or siege conditions, making the legal constraint their primary protective resource.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% Benefit from protective status under Common Article 3 and human rights law regardless of whether they meet traditional combatant criteria. Subject to reciprocal restrictions on methods of warfare, but protected from summary execution, torture, and disappearance if captured by state forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    moderate, biographical, constrained, regional).

% Interpret and apply the universal protective floor through international criminal tribunals, the International Court of Justice, and regional human rights courts. Their jurisprudence actively enforces the constraint against states and non-state actors but they do not set the underlying treaty agenda.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_judicial_bodies, observer,
    institutional, generational, analytical, global).

% Argue that international humanitarian law should not constrain state military operations against non-state actors and that human rights treaties are inapplicable to extraterritorial battlefield conduct. Prominent in certain state military legal advisories and political discourse but structurally marginalized in human rights tribunal jurisprudence under this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_sovereignty_absolutists, excluded,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a minimum standard of humane treatment for all persons affected by armed conflict, preventing a legal vacuum where no protections apply and ensuring that basic dignity guarantees are universally upheld regardless of the combatant status of the protected.
% TRANSFER_FUNCTION: Transfers operational discretion and tactical flexibility from state military apparatuses to protected persons and international supervisory bodies, by restricting targeting authority, mandating detention conditions, and subjecting state interrogation methods to external legal scrutiny.
% ABSENT_VOICES: State sovereignty absolutists and advocates for unilateral military necessity argue that humanitarian law should not constrain state operations against non-state actors and that human rights treaties are inapplicable to extraterritorial battlefield conduct; they are structurally marginalized in human rights tribunal jurisprudence but remain prominent in state military legal advisories.
% DISAPPEARANCE_RATIONALE: If the universal protective floor vanished overnight, states would regain unrestricted targeting and detention authority over non-state actors and civilians in conflict zones; international criminal jurisdiction would lose its foundational norms; and the architecture of international humanitarian and human rights law would fragment into conflict-type specific regimes without a minimum guarantee.
% FOUNDING_PROBLEM: The absence of legal protections for non-state armed group members and civilians in irregular or asymmetric conflicts, leading to summary execution, torture, and disappearance of captured persons outside the reach of either peacetime human rights law or traditional prisoner-of-war regimes.
% FOUNDING_PROBLEM_CORROBORATION: International criminal tribunals and UN human rights mechanisms attest that the problem remains live in contemporary asymmetric and counterterrorism conflicts. State military legal advisories and some constitutional courts attest that the founding problem is partially solved for traditional interstate wars but has expanded into contested domains; corroboration is split between beneficiary-aligned human rights institutions and payer-aligned state apparatuses, with no fully independent neutral party.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored as moderate-high because the universal floor substantially constrains state military operations, restricting targeting, mandating detention standards, and prohibiting certain interrogation methods. Suppression (0.58) reflects the active enforcement machinery of international criminal tribunals, universal jurisdiction prosecutions, and human rights litigation that suppresses the alternative of unregulated military discretion. Theater ratio (0.42) acknowledges growing performative complianceâstates rhetorically endorsing the universal floor while evading it through territorial and classification maneuvers in counterterrorism operations. Accessibility collapse (0.75) is high because once the universal floor is accepted, the state-centric and lawless alternatives collapse as legitimate options. Resistance (0.45) reflects persistent state non-compliance, reservations, and counterterrorism doctrines that narrow the floor's application.
 *
 * PERSPECTIVAL GAP:
 *   The state military apparatus experiences the constraint as extractive restriction on operational necessity, while civilian populations and non-state armed groups experience it as protective coordination. The agenda-setting state parties experience dual positioning: they authored and enforce the constraint (coordination) while their own military operations are restricted by it (extraction). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and non-state armed groups are structural beneficiaries (low directionality) because the constraint subsidizes their protection. The state military apparatus is the structural victim (high directionality) because the constraint extracts operational flexibility. State parties as agenda-setters sit near the beneficiary end for authority and legitimacy but their military apparatus sits near the target end for extraction; the dual-position is captured by separate stakeholder seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the universal protective floor as pure extraction (snare) by requiring the genuine coordination functionâpreventing legal vacuum and summary violence against non-combatants. It prevents mislabeling it as pure coordination (rope) by requiring identification of the asymmetric victim (state military flexibility). The temporal measurements show extraction spiking during the War on Terror (t=12) and remaining elevated, indicating the constraint's coordination function is genuine but its extractive load on state operations is structurally persistent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lex_specialis_vs_complementarity,
    'Does international humanitarian law operate as lex specialis displacing human rights law in armed conflict, or do the two bodies of law complementarily apply to create a universal protective floor?',
    'International judicial decisions (ICJ, ECtHR) on the interplay between IHL and IHRL; state practice and opinio juris regarding concurrent applicability.',
    'If lex specialis displaces human rights law, the universal reading''s floor collapses and effective extraction on state military operations drops substantially; if complementary, the reading holds and extraction remains elevated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lex_specialis_vs_complementarity, conceptual, 'Ambiguity in the legal relationship between IHL and human rights law').

omega_variable(
    state_compliance_gap,
    'To what extent does state military practice actually conform to the universal protective floor, and does persistent non-compliance represent reduced extractiveness or institutional failure?',
    'Empirical monitoring of detention, targeting, and interrogation practices across conflict zones; ICRC confidential reports and international criminal tribunal indictments.',
    'Persistent non-compliance with negligible cost to states would indicate the constraint is more theatrical than extractive (theater_ratio higher, effective extraction lower); effective enforcement would confirm the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_gap, empirical, 'Gap between legal obligation and actual state military practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t6, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(gene_tr_t18, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gene_be_t6, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(gene_be_t18, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(gene_su_t6, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(gene_su_t18, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Geneva protections' conflates three structurally distinct readings of the same treaty kernel. This decomposition separates the universal rights reading (universal floor for all persons), the state-centric reading (limited to Article 4 combatants), and the hybrid proportionality reading (scaled by conflict type). Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family via mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
