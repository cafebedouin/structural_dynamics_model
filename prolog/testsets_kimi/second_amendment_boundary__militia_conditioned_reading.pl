% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Reading
 *   domain: constitutional/law/political
 *
 * SUMMARY:
 *   This constraint instantiates the militia-conditioned reading of the
 *   second_amendment_boundary kernel. Under this reading, the prefatory
 *   clause ('A well regulated Militia, being necessary to the security of a
 *   free State') operates as a scope limit on the operative clause ('the
 *   right of the people to keep and bear Arms, shall not be infringed'),
 *   bounding the right to collective-defense contexts and permitting
 *   comprehensive state regulation. The constraint has historically enabled
 *   extensive firearms regulation in the United States. Its structural
 *   beneficiaries are state regulators and public-safety coalitions; its
 *   victims are individual gun owners whose possession is restricted. The
 *   kernel conflates at least three structurally distinct readings; this file
 *   models only the militia-conditioned reading, linked to its siblings via
 *   network edges.
 *
 * KEY AGENTS:
 *   - Militia-reading judiciary (agenda_setter/institutional/constrained): Administers the constitutional boundary by upholding regulations against individual-right challenges.
 *   - State regulators (beneficiary/institutional/constrained): Collect expanded constitutional authority to legislate comprehensive firearms restrictions.
 *   - Public safety coalition (beneficiary/organized/mobile): Benefits from a constitutional framework that defers to legislative majorities on public safety.
 *   - Restricted gun owners (payer/moderate/identity_locked): Bear the costs of possession restrictions; exit is constrained by legal geography and identity-fused ownership culture.
 *   - Individual-right advocates (excluded/organized/constrained): Structurally excluded from interpretive venues where the militia reading dominates.
 *   - Constitutional scholars (observer/analytical/analytical): Analytical seat tracking interpretive drift without direct stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.35).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.35).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional/law/political").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '9314c6a9-5a1e-421b-adfd-0b8373e5fc3d').
narrative_ontology:cs_kernel_codification('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', fixed_text).
narrative_ontology:cs_authority_grounding('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', lineage).
narrative_ontology:cs_interpretation_layer_present('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d').
narrative_ontology:cs_reading_relation('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', second_amendment_boundary__insurrectionist_reading, influences).
narrative_ontology:cs_axiom('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', foundational, prefatory_clause_operative_limit).
narrative_ontology:cs_axiom_status(prefatory_clause_operative_limit, holdable).
narrative_ontology:cs_axiom_grounding('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', prefatory_clause_operative_limit, conventional).
narrative_ontology:cs_axiom('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', foundational, collective_defense_preeminent_purpose).
narrative_ontology:cs_axiom_status(collective_defense_preeminent_purpose, holdable).
narrative_ontology:cs_axiom_grounding('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', collective_defense_preeminent_purpose, conventional).
narrative_ontology:cs_reference_frame('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', well_regulated_militia_sovereignty).
narrative_ontology:cs_drift_state('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', post_heller_bruen_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9314c6a9-5a1e-421b-adfd-0b8373e5fc3d', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_regulators).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_coalition).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_security_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and state judges who interpret the Second Amendment through the militia-conditioned lens, upholding firearms regulations against individual-right challenges. They administer the constitutional boundary by treating the prefatory clause as an operative scope limit on the operative clause.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, militia_reading_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Exercise expanded constitutional authority to enact comprehensive firearms regulations â background check regimes, assault-weapon bans, possession licensing â under the cover of the militia-conditioned reading. Their regulatory capacity depends on courts continuing to treat the prefatory clause as a meaningful scope limit.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_regulators, beneficiary,
    institutional, generational, constrained, national).

% Advocate for stringent gun-control measures and democratic majoritarian control over firearms policy. Benefit from a constitutional framework that defers to legislative judgment on public safety, avoiding a categorical judicial barrier to regulation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_coalition, beneficiary,
    organized, generational, mobile, national).

% Bear the direct costs of firearms restrictions enabled by the militia reading â denied purchase permits, banned weapon categories, registration burdens, and carry prohibitions. For collectors and self-defense claimants, gun ownership is often fused with personal identity, rural practice, or security self-concept; regulatory restriction is experienced as structural denial of a constitutionally expected liberty.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners, payer,
    moderate, biographical, identity_locked, national).

% Advance the competing individual-right reading of the Second Amendment. Structurally excluded from jurisdictions where the militia reading dominates constitutional interpretation; their legal arguments are treated as foreclosed by the prefatory clause's asserted scope limit, and their preferred policies are presumptively unconstitutional in those venues.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_right_advocates, excluded,
    organized, generational, constrained, national).

% Track and analyze the interpretive drift between readings across the twentieth and twenty-first centuries. Neither collect rents nor bear regulatory costs; their work describes the structural contest over constitutional meaning without having a direct stake in the firearms-policy outcome.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables democratic majorities and state legislatures to coordinate firearms policy for collective security without facing a categorical constitutional veto. It solves the collective-action problem of regulating armed violence by assigning regulatory authority to the state rather than treating private possession as an individually held veto against regulation.
% TRANSFER_FUNCTION: Transfers constitutional authority over firearms from the individual possessor to the state, and transfers compliance costs, possession restrictions, and licensing burdens from individual firearms owners to the public-safety framework administered by state regulators.
% ABSENT_VOICES: Individual-right advocates and affected gun owners in high-regulation jurisdictions are structurally excluded where the militia reading dominates; their constitutional claims are treated as resolved by the prefatory clause, and their objections are rarely admitted into the interpretive conversation as live legal questions.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned reading vanished overnight, existing comprehensive firearms regulations would lose their primary constitutional defense in federal courts, state legislatures would face strict-scrutiny barriers to many statutory schemes, and the firearms-policy landscape would shift toward an individual-right default â the constitutional world would rearrange around a different boundary.
% FOUNDING_PROBLEM: The arrangement was built to solve the founding-era problem of organizing collective defense through state militias while allowing state governments to regulate armed populations for public safety, without the constitutional barrier of an individually enforceable right to private arms.
% FOUNDING_PROBLEM_CORROBORATION: Historians attesting to the founding-era militia organization concern corroborate the origin from outside the beneficiary set; however, independent constitutional scholars and gun-rights advocates dispute that this eighteenth-century collective-defense problem remains live today, arguing it has been superseded by modern military and policing structures. State regulators assert the problem is still live, but this self-interested claim is contested by corroborating sources outside the benefiting parties.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness was high at interval start (0.75 in the Miller-era orthodoxy) because the reading enabled broad state regulation with minimal individual-right resistance; it decayed to 0.35 by interval end as Heller (2008) and Bruen (2022) displaced the reading's functional authority. Theater_ratio rose monotonically from 0.15 to 0.55 because the reading's persistence shifted from functional constitutional governance to performative dissents, academic defense, and ritual legislative citation without controlling judicial outcomes. Suppression_requirement tracked the same decay: from 0.70 when the individual-right reading was largely foreclosed in federal courts, down to 0.35 as the rival reading became dominant. Accessibility_collapse is low (0.25) because the individual-right alternative is now legally dominant and highly visible. Resistance is high (0.80) because the reading faces sustained opposition from a mobilized gun-rights movement and a hostile Supreme Court majority. The claim (tangled_rope) and metrics are authored independently: the metrics describe the constraint's weakening but still extractive end-state.
 *
 * PERSPECTIVAL GAP:
 *   State regulators and public-safety coalitions experience the constraint as enabling legitimate democratic coordination over public safety; from their seats the militia reading is a constitutional permission slip for majoritarian regulation. Restricted gun owners experience the same structure as asymmetric extraction of liberty â their possession is subject to comprehensive democratic veto. The engine computes this divergence from the beneficiary/victim declarations and exit asymmetry: beneficiaries are institutionally anchored and mobile, while payers are identity-locked into a regulatory geography that treats their claims as constitutionally subordinate.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulators and the public-safety coalition are structural beneficiaries of the constraint (low d); their regulatory capacity and policy preferences are subsidized by the reading. Restricted gun owners are structural victims (high d); their liberty is the direct target of the extraction. The individual-right advocates are excluded from the interpretive frame but are structurally targeted by the reading's suppressive force. The judiciary administering the reading sits near the agenda-setter/beneficiary boundary with constrained exit because their institutional role is bound to the interpretive framework they enforce.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve the collective-defense problem of organizing state militias and regulating armed populations in a republic without a standing army. By the mid-twentieth century, that founding problem was dead: the unorganized militia had atrophied, the National Guard was federalized, and modern policing replaced local militia functions. Yet the reading persisted as a general regulatory enabler, with state regulators continuing to invoke it to justify firearms restrictions decoupled from any actual militia purpose. This mandatrophy â mandate outliving function â is exactly what the Tangled Rope classification captures: a coordination story (public safety through democratic regulation) layered onto an atrophied founding rationale (militia readiness), with the extraction (liberty restriction) persisting through active judicial enforcement. The Heller and Bruen decisions can be read as mandatrophy recognition by the Court, explicitly rejecting the militia reading because its coordination story had become performative cover for asymmetric regulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operative_status,
    'Does the prefatory clause of the Second Amendment operate as a grammatical and legal scope limit on the operative clause, or merely as a non-binding statement of purpose?',
    'Supreme Court jurisprudential settlement or constitutional amendment explicitly clarifying the grammatical relationship between the two clauses.',
    'If the prefatory clause is merely purposive, the militia-conditioned reading collapses as a constitutional constraint and the kernel resolves toward the individual-right reading; if it is scope-limiting, the individual-right reading is structurally foreclosed as a matter of constitutional text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_operative_status, conceptual, 'Core textual ambiguity driving the kernel contest between readings.').

omega_variable(
    militia_regulatory_coupling,
    'Is modern firearms regulation actually coupled to the maintenance of a well-regulated militia, or has the militia concept become functionally decoupled from the regulatory practice the reading enables?',
    'Empirical analysis of state National Guard statutes, unorganized-militia legal frameworks, and the legislative history of contemporary gun-control statutes to test whether regulatory means are tied to militia ends.',
    'If decoupled, the coordination story weakens and the constraint''s extraction (liberty restriction) may exceed its coordinative justification, pushing the computed type toward snare-like asymmetry; if coupled, the tangled-rope structure remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_regulatory_coupling, empirical, 'Whether the constraint''s coordination function remains structurally tied to its original collective-defense purpose.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the militia-conditioned reading logically foreclose the individual-right reading within a single constitutional framework, or can both readings coexist as live judicial options?',
    'Comparative analysis of judicial opinions attempting to reconcile both readings; identification of any single opinion that simultaneously treats the prefatory clause as both operative and non-operative.',
    'If foreclosure is real, the kernel is a zero-sum textual contest; if coexistence is possible, the classification of the relationship should shift from forecloses to coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Structural relationship between this reading and its primary sibling within the same commitment framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t10, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(seco_tr_t30, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 40, 0.5).
narrative_ontology:measurement(seco_tr_t50, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(seco_be_t10, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(seco_be_t30, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(seco_be_t50, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(seco_su_t10, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(seco_su_t30, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(seco_su_t50, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 50, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_boundary kernel. The natural-language label 'Second Amendment' conflates at least three structurally distinct constraints: the individual right reading (low extraction, liberty-protective), the insurrectionist reading (high extraction, anti-state), and this militia-conditioned reading (moderate-to-high extraction, state-coordinative). Each has distinct beneficiary/victim structures, enforcement patterns, and empirical status. They are modeled as separate stories linked by network edges, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
