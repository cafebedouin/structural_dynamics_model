% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional law / political theory / firearms policy
 *
 * SUMMARY:
 *   This constraint instantiates the individual_right_reading of the
 *   contested second_amendment_boundary kernel. The natural-language label
 *   'Second Amendment' conflates three structurally distinct constraints.
 *   This reading holds that the operative clause establishes a pre-existing
 *   individual right to keep and bear arms, while the prefatory militia
 *   clause states a purpose without limiting the right's scope. The effect is
 *   to constitutionally shield private firearm possession and commerce from
 *   democratic regulation, creating a national floor below which state and
 *   local governments may not descend.
 *
 * KEY AGENTS:
 *   - individual_firearm_owners: Primary beneficiary (organized/mobile) â possess and acquire under constitutional shield.
 *   - firearms_industry: Primary beneficiary (powerful/arbitrage) â profits from constitutionally shielded market.
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â interprets and enforces the reading.
 *   - mass_shooting_victims, domestic_violence_victims, firearm_suicide_victims: Primary targets (powerless/trapped) â bear lethal and injury costs of unrestricted access.
 *   - state_governments: Payer (institutional/constrained) â blocked from exercising police power.
 *   - gun_control_advocacy_groups: Observer (organized/constrained) â structurally overruled in policy goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.65).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional law / political theory / firearms policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '16c82391-52ea-4e03-9a77-8979dfb2280d').
narrative_ontology:cs_kernel_codification('16c82391-52ea-4e03-9a77-8979dfb2280d', fixed_text).
narrative_ontology:cs_authority_grounding('16c82391-52ea-4e03-9a77-8979dfb2280d', lineage).
narrative_ontology:cs_interpretation_layer_present('16c82391-52ea-4e03-9a77-8979dfb2280d').
narrative_ontology:cs_reading_relation('16c82391-52ea-4e03-9a77-8979dfb2280d', second_amendment_boundary__militia_conditioned_reading, forecloses).
narrative_ontology:cs_reading_relation('16c82391-52ea-4e03-9a77-8979dfb2280d', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('16c82391-52ea-4e03-9a77-8979dfb2280d', foundational, operative_clause_establishes_independent_right).
narrative_ontology:cs_axiom_status(operative_clause_establishes_independent_right, holdable).
narrative_ontology:cs_axiom_grounding('16c82391-52ea-4e03-9a77-8979dfb2280d', operative_clause_establishes_independent_right, conventional).
narrative_ontology:cs_axiom('16c82391-52ea-4e03-9a77-8979dfb2280d', foundational, prefatory_clause_non_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_non_limiting, holdable).
narrative_ontology:cs_axiom_grounding('16c82391-52ea-4e03-9a77-8979dfb2280d', prefatory_clause_non_limiting, conventional).
narrative_ontology:cs_reference_frame('16c82391-52ea-4e03-9a77-8979dfb2280d', individual_self_defense_tradition).
narrative_ontology:cs_drift_state('16c82391-52ea-4e03-9a77-8979dfb2280d', post_heller_mcdonald_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16c82391-52ea-4e03-9a77-8979dfb2280d', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, individual_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, second_amendment_advocacy_orgs).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, firearm_suicide_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, state_governments).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, textual_originalism).
narrative_ontology:constraint_vindicates(second_amendment_boundary__individual_right_reading, individual_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess and acquire firearms under a constitutional shield that blocks or narrows state and federal regulatory barriers; benefit from lowered licensing, carry, and possession restrictions validated by federal courts.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, individual_firearm_owners, beneficiary,
    organized, generational, mobile, national).

% Manufactures, imports, and sells firearms and ammunition to a nationwide market that courts protect from many product-safety, capacity, and distribution regulations; derives concentrated profit from the constitutional barrier.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Derive membership, funding, and political influence from defending the individual-right reading; the constraint's doctrinal dominance is their organizational lifeline.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, second_amendment_advocacy_orgs, beneficiary,
    organized, generational, mobile, national).

% Interprets the Second Amendment to invalidate firearms regulations; controls the doctrinal test (text-history-tradition) that determines which laws survive and which are struck down.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Bear the lethal cost of unrestricted firearms access that this reading prevents legislatures from curtailing through capacity limits, waiting periods, or assault-weapon bans.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, national).

% Face elevated homicide risk when abusers retain lawful firearm access that this reading protects from restraining-order-based confiscation and comprehensive disarmament laws.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_victims, payer,
    powerless, immediate, trapped, national).

% Complete suicide using firearms kept accessible by markets this reading shields from regulatory reduction; the constraint blocks waiting periods and safe-storage mandates that reduce means.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, firearm_suicide_victims, payer,
    powerless, immediate, trapped, national).

% Exercise traditional police power to legislate public safety, then see those laws struck down or chilled by federal courts applying the individual-right reading; regulatory capacity is truncated by constitutional interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Campaign for regulatory restrictions that the individual-right reading categorically blocks or narrows; their democratic policy agenda is rendered unconstitutional regardless of electoral majority.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_control_advocacy_groups, observer,
    organized, generational, constrained, national).

narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, nationally applicable individual right to possess firearms, reducing legal uncertainty across jurisdictions and supplying a clear rule for courts and citizens.
% TRANSFER_FUNCTION: Moves regulatory authority from state legislatures and local governments to federal courts, and transfers physical risk from the protected owner class and industry to the general population, concentrating it on specific victim populations.
% ABSENT_VOICES: Survivors of gun violence and public health researchers are present in public discourse but structurally excluded from constitutional interpretation; their empirical findings do not alter the doctrinal framework. Comparative constitutional scholars and foreign jurisdictions with successful regulatory regimes are treated as irrelevant to textual meaning.
% DISAPPEARANCE_RATIONALE: If the individual-right reading vanished overnight, state legislatures would enact waiting periods, capacity limits, licensing regimes, and safe-storage laws within months; the firearms market would contract and restructure around new regulatory geography; the federal judiciary would lose this specific docket and doctrinal framework.
% FOUNDING_PROBLEM: Fear of centralized federal tyranny and the need for decentralized armed citizenry as a check on government; concern that a national standing army might enable disarmament and oppression.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians and legal scholars outside the beneficiary set attest the founding problem concerned federalism and militia control, not individual self-defense against crime. Gun control advocacy groups and comparative constitutional scholars attest the anti-tyranny premise is anachronistic in contemporary governance. Beneficiaries (originalist jurists, advocacy organizations) assert the problem is eternally live.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__individual_right_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the reading blocks regulatory interventions that would reduce mortality, effectively transferring physical risk from owners to victims. Suppression (0.55) is moderate: judicial supremacy suppresses legislative alternatives without physical coercion, but the Supremacy Clause and precedent create a hard structural ceiling. Theater ratio (0.32) reflects that originalist methodology is genuinely held but performs ideological work in selecting which history counts. Accessibility collapse (0.75) is high because once the reading is established, legislative alternatives are struck down as unconstitutional. Resistance (0.60) reflects sustained democratic and advocacy opposition. Measurements show modest drift upward over fifty years as the reading hardened from a novel doctrine into an entrenched interpretive regime.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (owners, industry, advocacy orgs) experience the constraint as constitutional liberty and market protection; the payer seats (victims, state governments) experience it as unregulated risk and truncated sovereignty. The federal judiciary experiences it as ordinary legal interpretation. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (individual_firearm_owners, firearms_industry, second_amendment_advocacy_orgs) receive low directionality: the constraint subsidizes their legal and economic position. Victims (mass_shooting_victims, domestic_violence_victims, firearm_suicide_victims) receive high directionality: they bear the costs of the constraint's operation with no exit. State_governments, though institutional, are declared victims with constrained exit, placing them near the target end. The federal_judiciary, as agenda_setter with analytical exit, sits near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â armed resistance to centralized tyranny â is contested as either dead or transformed beyond recognition, yet the arrangement has expanded into broad market shielding and self-defense doctrine. The R5 genealogy (founding_problem contested + disappearance_verdict world_rearranges) flags potential mandatrophy: the constraint persists beyond its original justification. Classifying as tangled_rope prevents mislabeling the genuine legal coordination (a uniform national right reducing regulatory chaos) as pure extraction, while the declared victim set and active enforcement prevent mislabeling it as benign rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the individual_right_reading of second_amendment_boundary. Sibling readings (militia_conditioned_reading, insurrectionist_reading) would alter the victim set, beneficiary set, and enforcement mechanism. Does the prefatory clause possess operative semantic force or is it purely hortatory?',
    'Historical-linguistic analysis of eighteenth-century usage of prefatory clauses in statutory texts; comparison with state constitutional analogues.',
    'If prefatory clauses routinely carried operative force in the founding era, the individual-right reading''s epsilon rises (more extractive, less coordination); if they were conventionally hortatory, the reading''s coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Location of structural disagreement between kernel readings: whether the militia clause limits scope.').

omega_variable(
    victim_causation_counterfactual,
    'Does the individual-right reading cause the victimization attributed to it, or would violence persist via black markets and substitute means under a militia-conditioned regulatory regime?',
    'Comparative criminological study of jurisdictions with strict regulatory regimes; natural experiments from state-level variation pre- and post-incorporation.',
    'If substitution effects are small, the victim set is genuinely paying through this constraint and extraction is as authored; if substitution is near-complete, the victim cost is lower and the constraint''s extractiveness should be revised downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_causation_counterfactual, empirical, 'Whether regulatory substitution would actually reduce victimization or merely displace it.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of legislative alternatives structural (judicial review and federal supremacy) or internalized (legislatures self-censoring because they believe the reading is historically inevitable)?',
    'Legislative-docket analysis: do state legislatures introduce and pass gun-control bills that are then struck down (structural), or do they fail to introduce them at all (internalized)?',
    'If internalized, effective suppression is higher than the structural measure suggests and state governments may function as unwitting enforcers of their own disempowerment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of regulatory alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_right_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sa_ind_right_tr_t10, second_amendment_boundary__individual_right_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(sa_ind_right_tr_t20, second_amendment_boundary__individual_right_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(sa_ind_right_tr_t30, second_amendment_boundary__individual_right_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(sa_ind_right_tr_t40, second_amendment_boundary__individual_right_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(sa_ind_right_tr_t50, second_amendment_boundary__individual_right_reading, theater_ratio, 50, 0.32).

% Extraction over time
narrative_ontology:measurement(sa_ind_right_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sa_ind_right_be_t10, second_amendment_boundary__individual_right_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sa_ind_right_be_t20, second_amendment_boundary__individual_right_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(sa_ind_right_be_t30, second_amendment_boundary__individual_right_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(sa_ind_right_be_t40, second_amendment_boundary__individual_right_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(sa_ind_right_be_t50, second_amendment_boundary__individual_right_reading, base_extractiveness, 50, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_boundary__individual_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, insurrectionist_reading).

% DUAL FORMULATION NOTE:
% The natural-language phrase 'Second Amendment' conflates three structurally distinct constraints. This file isolates the individual-right reading. Its siblings instantiate different epsilon values, different beneficiary/victim structures, and different enforcement logics. They form a constraint family linked by mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
