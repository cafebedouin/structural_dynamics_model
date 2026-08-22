% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective Right (State Militia Authority)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint embodies the 'collective right' reading of the Second
 *   Amendment: the right to keep and bear arms belongs to the states and
 *   their organized militias, not to private individuals. Under this
 *   interpretation, the Amendment protects state authority to regulate,
 *   condition, and restrict individual firearm ownership in service of
 *   militia readiness and public safety. The constraint operates through
 *   constitutional doctrine that vests regulatory authority in state
 *   legislatures and agencies rather than treating gun ownership as a
 *   fundamental individual entitlement. This is one of three structurally
 *   distinct readings of the same constitutional text (the kernel); it is
 *   authored as a clean, ε-invariant constraint for THIS reading only, not as
 *   an average across readings.
 *
 * KEY AGENTS:
 *   - State legislatures: institutional agenda-setters who gain broad regulatory power under this reading
 *   - Individual gun owners: moderate-power payers who bear licensing, permitting, and exclusion costs
 *   - State militia operators and public safety regulators: institutional beneficiaries who gain legitimized authority
 *   - Gun-rights advocates: excluded parties whose constitutional theory is treated as foreclosed
 *   - Interpreting courts: observers whose doctrinal choices determine which reading controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.31).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective Right (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '5056fc4b-3e28-4113-817d-520a7ed03d0a').
narrative_ontology:cs_kernel_codification('5056fc4b-3e28-4113-817d-520a7ed03d0a', fixed_text).
narrative_ontology:cs_authority_grounding('5056fc4b-3e28-4113-817d-520a7ed03d0a', lineage).
narrative_ontology:cs_interpretation_layer_present('5056fc4b-3e28-4113-817d-520a7ed03d0a').
narrative_ontology:cs_reading_relation('5056fc4b-3e28-4113-817d-520a7ed03d0a', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('5056fc4b-3e28-4113-817d-520a7ed03d0a', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('5056fc4b-3e28-4113-817d-520a7ed03d0a', foundational, second_amendment_protects_state_militia_authority).
narrative_ontology:cs_axiom_status(second_amendment_protects_state_militia_authority, holdable).
narrative_ontology:cs_axiom_grounding('5056fc4b-3e28-4113-817d-520a7ed03d0a', second_amendment_protects_state_militia_authority, empirically_contingent).
narrative_ontology:cs_axiom('5056fc4b-3e28-4113-817d-520a7ed03d0a', secondary, individual_firearm_possession_is_state_regulated_privilege).
narrative_ontology:cs_axiom_status(individual_firearm_possession_is_state_regulated_privilege, holdable).
narrative_ontology:cs_axiom_grounding('5056fc4b-3e28-4113-817d-520a7ed03d0a', individual_firearm_possession_is_state_regulated_privilege, conventional).
narrative_ontology:cs_reference_frame('5056fc4b-3e28-4113-817d-520a7ed03d0a', state_militia_regulatory_authority).
narrative_ontology:cs_drift_state('5056fc4b-3e28-4113-817d-520a7ed03d0a', contemporary_gun_rights_mobilization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5056fc4b-3e28-4113-817d-520a7ed03d0a', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militia_operators).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, public_safety_regulators).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_gun_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the authority to regulate firearm ownership, licensing, and use within their jurisdiction under this reading. They establish and enforce regulations on who may possess weapons, under what conditions, and for what purposes. They benefit from broad regulatory latitude and the power to condition individual rights on militia participation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% The National Guard and state military forces operate under the authority this reading secures. They coordinate firearm use at the institutional level and benefit from the clarity that the Second Amendment protects their prerogative, not individual citizen arsenals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_militia_operators, beneficiary,
    institutional, generational, analytical, national).

% Law enforcement, ATF, and public health agencies operate under regulatory authority this reading legitimizes. They justify licensing, background checks, and restrictions on the grounds that the Amendment protects collective militia capacity, not individual possession rights.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, public_safety_regulators, beneficiary,
    institutional, biographical, analytical, national).

% Under this reading, gun ownership is a privilege contingent on state-defined militia participation or licensing rather than a protected individual right. They bear regulatory costs (licensing, permitting, training requirements) and lose the claim to self-defense or sport-shooting as a standalone constitutional entitlement. Exit means forgoing firearms entirely or moving to a jurisdiction with a different constitutional reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Would argue for individual-right readings and would contest this constraint's legitimacy. They are not absent from the legal system but are systematically disadvantaged by a judicial interpretation that privileges collective authority over individual entitlement. Their constitutional theory is treated as foreclosed by the institutional reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocacy_organizations, excluded,
    organized, biographical, constrained, national).

% The tribunal charged with reading the Second Amendment and determining which interpretation controls. From this seat, the constraint is a specific reading codified into precedent, subject to reversal if the interpreting authority shifts its doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, supreme_court_or_interpreting_authority, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures state capacity to organize collective defense, train armed forces, and regulate firearms as tools of state security and militia preparation rather than private possession. The coordination problem solved is ensuring the state retains authority to organize armed force without competition from private arsenals organized outside institutional control.
% TRANSFER_FUNCTION: Transfers regulatory authority from individual gun owners to state bodies and their agencies. Individuals lose the claim to firearm ownership as a constitutional entitlement; states gain the power to condition, restrict, or deny ownership entirely based on their definition of militia participation and public safety.
% ABSENT_VOICES: Gun-rights advocates and individual owners who regard firearms ownership as a personal liberty unconnected to militia service are structurally disadvantaged by this reading. Their constitutional theory is treated as foreclosed. Individuals in the payer seat have no seat at the framing table — the reading is adjudicated by institutional interpreters (courts, legislatures), not by those whose rights are curtailed.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and were replaced by an individual-right reading, the world would rearrange substantially: federal and state gun regulations would become subject to heightened scrutiny, licensing and permitting schemes would face constitutional challenge, and individual gun owners would gain a recognized entitlement to firearm possession for lawful purposes. State regulatory power would shrink materially.
% FOUNDING_PROBLEM: Ensure that the young nation's state militias remained functional and under state control as a check on federal standing armies; prevent individual private arsenals from fragmenting or competing with collective state military organization.
% FOUNDING_PROBLEM_CORROBORATION: Historians and originalist scholars attest that the founding concern was militia function and state authority. The historical problem — fragmenting private gun ownership undermining state militia coherence in the 18th-century context — is no longer live in modern federal and state structures. Historians outside the gun-regulation advocacy community (e.g., academic historical societies) note that the practical concern addressed by the Amendment has been superseded by standing armies, professional forces, and the Second Amendment's non-role in modern militia structure.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.31 at interval end), not high, because the constraint's scope is narrowly institutional: it privileges state authority without creating obvious private-benefit monopolies. The beneficiaries are state agencies and legislatures, which are public bodies, not private extractors. Suppression is low-moderate (0.28) because the constraint operates primarily through law and interpretation, not through coercive enforcement against active large-scale resistance. Theater is very low (0.12) because there is no performative maintenance — the constraint's function (allocating regulatory authority) is its actual function; no gaps between stated and real purpose. Accessibility of alternatives is moderate (0.62): individuals retain the option to advocate for different readings or move to jurisdictions with different constitutional jurisprudence, but the dominant doctrine forecloses one structural path to the alternative. Resistance is high (0.78) because the individual-right reading commands substantial advocacy, litigation, and legislative pushback; the constraint faces real organized contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the state-authority seat, this reading is genuine coordination: it secures the state's power to organize collective defense and prevent fragmented private arsenals from undermining that organization. From the individual gun-owner seat, the same structure operates as regulatory extraction: they lose a claimed constitutional entitlement and gain only contingent permission. The engine computes different effective directionality from each seat based on the structural asymmetry: state agencies sit near the beneficiary end (d near 0.0), individuals near the target end (d near 1.0). The authored claim and the authored metrics are independent: the reading CLAIMS to be a rope (coordination without large net extraction) while the metrics describe a low but non-zero extraction and moderate resistance — that divergence is exactly the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and state militia operators are structural beneficiaries under this reading: they gain uncontested authority to condition and regulate. Their directionality is near 0.0 (full beneficiary). Individual gun owners are the targets: they lose the claim to a right and gain only contingent permission to own under state conditions. Their directionality is near 1.0 (full target), modulated downward slightly because exit options exist (legal advocacy, relocation, litigation) and the constraint is contested rather than naturalized. Gun-rights advocates sit as excluded: they have a claim on the conversation but no seat at the interpreting table, making their exit options trapped despite their organizational power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (militia coherence and state control) is dead: modern state structures, professional armed forces, and international military alliances render the 18th-century concern obsolete. Yet the constraint persists through institutional inertia — it remains doctrine because courts have repeatedly reaffirmed it, and legislatures have built regulatory structures on it. The persistence despite the dead founding problem is the signature of mandatrophy. The constraint is not sustained by those it extracts from (individuals have no incentive to keep it) nor by those it benefits most (state militias rely primarily on federal structure and professional organization, not on constitutional doctrine preventing private gun ownership). It persists because interpreting authorities (courts) have adopted it as precedent and because regulatory apparatus has accumulated around it. This fits the piton profile more than the rope profile — a constraint whose original function is gone but whose institutional frame remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_necessity_contestation,
    'Does modern state security actually depend on constitutional doctrine restricting individual gun ownership, or is the connection between the Amendment''s text and modern militia organization entirely severed?',
    'Historical and institutional analysis of how modern state armed forces operate and coordinate compared to the founding-era militia system. Comparative study of how other democracies organize collective defense without similar constitutional restrictions on individual ownership.',
    'If the connection is entirely severed, the constraint is mandatrophy-certified: a dead founding problem sustained purely by institutional inertia. If a residual coordination function remains, part of the extraction is coordination cost rather than pure rent. Either way, this omega determines whether the constraint is truly a piton or a degraded rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_necessity_contestation, empirical, 'Whether modern militia function depends on restricting individual gun ownership.').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does the collective-right reading logically foreclose the individual-right reading, or do both readings remain live constitutional options that compete across time and jurisdictional variation?',
    'Constitutional theory and doctrine analysis: examine whether accepting the collective-right interpretation as true entails the falsity of the individual-right interpretation, or whether both can coherently be held in different institutional contexts or at different historical moments.',
    'If the readings foreclose each other, this constraint should relate to its siblings via ''forecloses''. If they coexist as live options, the relation is ''coexists_with''. This determines the network structure of the kernel''s constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Logical compatibility of the collective-right reading with individual-right and civic-right alternatives.').

omega_variable(
    original_meaning_interpretive_authority,
    'Which interpretive framework has the stronger claim to represent the Amendment''s original public meaning: originalist reading of militia service prerequisites, or originalist reading of individual right to keep arms?',
    'Originalist scholarship and historical linguistic analysis of 18th-century usage and contemporaneous understanding. Examination of founding-era state constitutions and militia laws to assess which reading aligns with widespread practice.',
    'If the collective-right reading better captures original meaning, it gains legitimacy from originalist constitutional theory. If the individual-right reading does so, that reading''s credibility rises within the originalist camp, potentially shifting which reading the engine classifies as more natural or more entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_interpretive_authority, empirical, 'Which reading better represents the Amendment''s original public meaning and founding-era practice.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of individual gun-owner resistance structural (legal barriers, institutional capacity to enforce restrictions) or partly internalized (gun owners themselves come to accept the collective-right framing as legitimate doctrine)?',
    'Post-reversal suppression trajectory: if a subsequent ruling or legislative shift to the individual-right reading were to occur, would gun-owner mobilization and litigation rise sharply, suggesting suppression was structural? Or would cultural resistance remain weak, suggesting internalized acceptance of the collective-right doctrinal frame?',
    'If suppression is structural, the constraint''s effective extractiveness is the authored 0.31. If partly internalized, the constraint carries additional extractive force because individuals accept the limitation without active resistance. The distinction informs whether this constraint is better modeled as rope (coordination with modest administrative cost) or snare (extraction sustained by internalized acceptance of the framing).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether resistance suppression is structural or internalized in the gun-owner population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__collective_right_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__collective_right_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__collective_right_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__collective_right_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(seco_tr_t40, observed).
narrative_ontology:measurement(seco_tr_t50, second_amendment_scope__collective_right_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(seco_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__collective_right_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__collective_right_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__collective_right_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__collective_right_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement_basis(seco_be_t40, observed).
narrative_ontology:measurement(seco_be_t50, second_amendment_scope__collective_right_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(seco_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__collective_right_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__collective_right_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__collective_right_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement_basis(seco_su_t30, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__collective_right_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement_basis(seco_su_t40, observed).
narrative_ontology:measurement(seco_su_t50, second_amendment_scope__collective_right_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(seco_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment scope kernel decomposes into three structurally distinct constraints, one for each interpretive reading. The collective-right reading (this file) treats the Amendment as protecting state militia authority and regulatory capacity. The individual-right reading treats the Amendment as protecting individual ownership unconnected to militia service. The civic-right reading treats the Amendment as protecting individual ownership conditioned on civic militia participation. These readings share a kernel (the same constitutional text) but instantiate different constraints with different beneficiary/victim structures, different ε values, and different institutional relations. All three are linked through the network.affects_constraints field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
