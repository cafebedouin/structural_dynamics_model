% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment: Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This constraint story instantiates the civic right reading of the Second
 *   Amendment scope kernel: the view that the Amendment protects an
 *   individual right to keep and bear arms, but that right is structurally
 *   conditioned on participation in a well-regulated militia — the civic
 *   republican ideal of universal citizen-soldiery as the foundation of
 *   republican defense. The reading treats the 'well regulated Militia'
 *   clause as a substantive precondition, not a prefatory flourish. The
 *   constraint's extraction derives from gating the arms right behind militia
 *   eligibility and service; its suppression operates through legal
 *   disability for non-eligible persons and the civic republican frame that
 *   makes non-participation illegitimate. The measurement series tracks
 *   1791–2021: founding-era genuine coordination (low ε, low theater) decays
 *   as the militia system atrophies (rising ε, rising theater) while the
 *   conditioning structure persists without its functional basis. The
 *   claimed_type is tangled_rope: genuine coordination (republican defense)
 *   coexists with asymmetric extraction (exclusion of non-participants from
 *   the right), requiring active enforcement (militia regulation, eligibility
 *   determination).
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: Primary beneficiary (moderate/constrained) — receives conditional arms right tied to civic service obligation
 *   - state_militia_authority: Agenda setter (institutional/generational) — defines eligibility, organizes militia, regulates arms access for militia purposes
 *   - non_militia_eligible_individuals: Primary victim/payer (powerless/trapped) — legally disabled from arms right under this reading; bears cost of exclusion
 *   - constitutional_courts: Observer (analytical/analytical) — adjudicates scope disputes between readings
 *   - standing_army_proponents: Excluded (powerful/trapped) — benefit from militia obsolescence but structurally excluded from this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.48).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment: Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '15b9b781-e6db-48b4-8f50-989c96edf38a').
narrative_ontology:cs_kernel_codification('15b9b781-e6db-48b4-8f50-989c96edf38a', fixed_text).
narrative_ontology:cs_authority_grounding('15b9b781-e6db-48b4-8f50-989c96edf38a', lineage).
narrative_ontology:cs_interpretation_layer_present('15b9b781-e6db-48b4-8f50-989c96edf38a').
narrative_ontology:cs_reading_relation('15b9b781-e6db-48b4-8f50-989c96edf38a', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('15b9b781-e6db-48b4-8f50-989c96edf38a', second_amendment_scope__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('15b9b781-e6db-48b4-8f50-989c96edf38a', foundational, right_conditioned_on_militia_service).
narrative_ontology:cs_axiom_status(right_conditioned_on_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('15b9b781-e6db-48b4-8f50-989c96edf38a', right_conditioned_on_militia_service, conventional).
narrative_ontology:cs_axiom('15b9b781-e6db-48b4-8f50-989c96edf38a', secondary, civic_obligation_precondition).
narrative_ontology:cs_axiom_status(civic_obligation_precondition, holdable).
narrative_ontology:cs_axiom_grounding('15b9b781-e6db-48b4-8f50-989c96edf38a', civic_obligation_precondition, conventional).
narrative_ontology:cs_reference_frame('15b9b781-e6db-48b4-8f50-989c96edf38a', founding_militia_republicanism).
narrative_ontology:cs_drift_state('15b9b781-e6db-48b4-8f50-989c96edf38a', post_heller_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('15b9b781-e6db-48b4-8f50-989c96edf38a', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_eligible_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, state_militia_authority).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, republican_defense_without_standing_armies).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_virtue_through_universal_service).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutional right to keep and bear arms, but only insofar as they participate in or remain eligible for militia service. The right is real but gated: they must satisfy the civic condition (enrollment, training, availability) to claim it. Exit from the condition means exit from the right. Their power is moderate — they vote, serve, and litigate, but the condition is set by the state.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, biographical, constrained, national).

% Defines militia eligibility, organizes training and muster, regulates arms access for militia purposes, and adjudicates exemptions. Collects regulatory authority and the civic republican legitimacy of being the 'well regulated' actor. Can shift between organized militia (National Guard) and unorganized militia definitions to modulate the beneficiary class. Has arbitrage-grade exit: can professionalize defense and let the militia condition atrophy while retaining regulatory power.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, state_militia_authority, beneficiary).

% Categorically excluded from the Second Amendment right under this reading: by age, gender (historically), disability, conscientious objection, immigration status, or simply non-enrollment. Bear the full cost of disarmament without the conditional benefit. No exit from the legal disability — cannot 'join the militia' if the state defines eligibility to exclude them. Trapped by the civic republican frame that treats their exclusion as natural.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_eligible_individuals, payer,
    powerless, biographical, trapped, national).

% Adjudicate disputes between the three readings. Their institutional role is to declare which reading governs, but they are themselves constituted by the constitutional order the kernel structures. They do not collect or pay the extraction; they authorize its distribution. Their analytical seat sees the full structural divergence between seats.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, constitutional_courts, observer,
    analytical, generational, analytical, national).

% Benefit from the militia system's obsolescence — a professional standing army replaces the civic militia, concentrating defense authority in the executive. They are structurally excluded from this reading's framework because the reading's premise (militia as primary defense) contradicts their interest. Their exclusion is not accidental: the civic right reading was historically mobilized *against* standing armies. They are trapped in the sense that the constitutional text they would prefer (no militia condition) is foreclosed by this reading's logic.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, standing_army_proponents, excluded,
    powerful, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes republican defense through universal citizen-soldiery: distributes arms readiness across the polity, ties military capacity to civic virtue, and avoids standing armies by making every eligible citizen a potential defender. The coordination is genuine — it solves the collective action problem of defense provision without centralized coercion.
% TRANSFER_FUNCTION: Moves the legal right to bear arms from universal application to militia-eligible participants only; regulatory authority gains gatekeeping power over arms access, eligibility criteria, and exemption regimes. The transfer is from non-eligible individuals (who lose the right) to the state militia authority (which gains regulatory control) and militia-eligible citizens (who retain a conditional right).
% ABSENT_VOICES: Those excluded from militia eligibility by the state's definition — women (historically), conscientious objectors, non-citizens, the disabled, the elderly — who would claim an individual right unconnected to service. Also absent: standing army advocates who benefit from the militia system's decay but are structurally excluded from this reading's civic republican frame. Their absence is what makes the coordination function appear universal when it is actually gated.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, the Second Amendment would be read either as an unconditional individual right (individual_right_reading) or as a collective state authority (collective_right_reading). Arms regulation would restructure: either near-deregulation (individual right) or plenary state control (collective right). The militia-eligible class would lose their conditional right; the state would lose its militia-regulatory framework; non-eligible individuals would either gain or lose all arms claims depending on which sibling reading prevails.
% FOUNDING_PROBLEM: How to ensure republican defense without standing armies — tying arms bearing to civic virtue and collective security through universal militia service, making the citizenry itself the defense establishment.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era corroboration: Madison (Federalist 46), Hamilton (Federalist 29), 1792 Militia Act, state militia statutes — all attest the problem was standing army avoidance through universal militia. Contemporary corroboration outside beneficiaries: none. The militia system has atrophied; the National Guard is a federal reserve, not a universal citizen militia. No non-beneficiary actor attests the founding problem persists. Originalist scholars (beneficiaries of this reading) claim it is 'contested' but offer no functional militia system.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48 at interval end) reflects the service-gated right: militia-eligible citizens gain a valuable legal entitlement, but non-eligible persons are categorically excluded — a substantial asymmetric transfer. Suppression (0.55) is moderate: the constraint operates through legal disability (structural) reinforced by civic republican legitimacy (internalized). Theater ratio (0.38) rises over time as the militia system atrophies — the coordination function (distributed defense readiness) becomes performative while the gating structure persists. Accessibility collapse (0.62) is moderate-high: the textual conditioning on 'well regulated Militia' makes alternative readings (unconditional individual right) textually contested but not foreclosed. Resistance (0.45) reflects persistent challenge from individual_right_reading advocates. The claim/metric independence is maintained: the reading claims to be a rope (pure coordination of civic defense), but the metrics reveal substantial extraction from the excluded class.
 *
 * PERSPECTIVAL GAP:
 *   From the militia-eligible citizen's seat, the constraint appears as rope — a genuine coordination mechanism linking arms access to civic duty. From the non-eligible individual's seat, it appears as snare — categorical exclusion from a fundamental right justified by a defunct militia system. The state_militia_authority seat experiences it as scaffold — a transitional structure meant to obsolete itself as standing armies replace militia, but lacking a sunset clause. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens are structural beneficiaries (d ~ 0.25): they receive a conditional right, but the condition is a civic duty they would owe regardless. State militia authority is near-symmetric (d ~ 0.5): administers the system but gains regulatory gatekeeping power. Non-militia-eligible individuals are full targets (d ~ 0.9): categorically excluded from the right, with no exit from the legal disability (trapped). Standing army proponents are excluded (d ~ 0.7): their preferred arrangement (professional military) is structurally incompatible with this reading's premise. Constitutional courts are analytical (d ~ 0.0): observational seat. The civic republican frame amplifies suppression for non-eligible persons by making their exclusion seem like civic failure rather than legal injury.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — republican defense without standing armies — is dead (replaced by professional military). The constraint persists without its functional basis: the militia conditioning structure remains in constitutional text and doctrine (Heller's 'prefatory clause' dicta, state constitutional analogues) but the militia system it was built to serve has atrophied. This is mandatrophy: a coordination mechanism whose founding problem is gone, persisting as a gating structure that now primarily extracts (excludes non-participants) rather than coordinates. The theater_ratio rise (0.08→0.38) tracks this decay. The reading is not a piton because the agenda-setter (state) could reform it but faces prohibitive fixing_cost (constitutional amendment); it is not a snare because the coordination function was genuine and the exclusion is textually grounded, not pure cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the civic right reading a genuine historical reconstruction of founding-era understanding, or a modern synthesis attempting to mediate between individual and collective right positions?',
    'Comparative analysis of founding-era militia statutes, ratification debates, and early judicial commentary against the specific textual claims of this reading.',
    'If modern synthesis, the reading''s ε reflects contemporary policy preference rather than recovered original meaning; if historical, the service-conditioned right has stronger descriptive claim as the kernel''s authentic instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Historical authenticity vs. modern synthesis of the civic right reading').

omega_variable(
    militia_eligibility_boundary,
    'Who constitutes the ''militia-eligible'' class under this reading — all able-bodied citizens, organized militia members only, National Guard, or a politically determined subset?',
    'Statutory and judicial history of militia definitions from 1792 Militia Act through Dick Act (1903) to current 10 USC §246; tracking how eligibility expansions/contractions map to arms regulation.',
    'A narrow eligibility (organized militia only) makes this reading functionally equivalent to collective_right_reading; universal eligibility makes it a gated individual right. The boundary determines ε magnitude and victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_eligibility_boundary, empirical, 'Structural ambiguity in the militia-eligible beneficiary class definition').

omega_variable(
    service_condition_operationalization,
    'Does the militia service condition require active participation, mere enrollment availability, or citizenship-status-as-standing-obligation?',
    'Analysis of historical militia muster requirements, exemption regimes (clergy, conscientious objectors, wealth-based substitutes), and their modern analogs in selective service / national guard frameworks.',
    'Active service requirement makes the right highly conditional (high extraction from non-servers); enrollment-availability makes it near-universal (low extraction); standing obligation makes it a civic republican duty with symbolic arms right.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(service_condition_operationalization, conceptual, 'Whether the conditioning mechanism is active, latent, or symbolic').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to arms access for non-militia-eligible) or internalized (civic republican ideology making non-participation unthinkable)?',
    'Post-obsolescence suppression trajectory: if arms restrictions on non-militia persons persist after militia system atrophies, reclassify as partially internalized ideological suppression.',
    'If internalized, the constraint''s effective suppression exceeds structural measure — the civic republican frame continues extracting compliance after its enforcement machinery decays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the civic republican frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 230).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_civic_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sa_civic_tr_t46, second_amendment_scope__civic_right_reading, theater_ratio, 46, 0.12).
narrative_ontology:measurement(sa_civic_tr_t92, second_amendment_scope__civic_right_reading, theater_ratio, 92, 0.22).
narrative_ontology:measurement(sa_civic_tr_t138, second_amendment_scope__civic_right_reading, theater_ratio, 138, 0.31).
narrative_ontology:measurement(sa_civic_tr_t184, second_amendment_scope__civic_right_reading, theater_ratio, 184, 0.35).
narrative_ontology:measurement(sa_civic_tr_t230, second_amendment_scope__civic_right_reading, theater_ratio, 230, 0.38).

% Extraction over time
narrative_ontology:measurement(sa_civic_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sa_civic_be_t46, second_amendment_scope__civic_right_reading, base_extractiveness, 46, 0.22).
narrative_ontology:measurement(sa_civic_be_t92, second_amendment_scope__civic_right_reading, base_extractiveness, 92, 0.35).
narrative_ontology:measurement(sa_civic_be_t138, second_amendment_scope__civic_right_reading, base_extractiveness, 138, 0.42).
narrative_ontology:measurement(sa_civic_be_t184, second_amendment_scope__civic_right_reading, base_extractiveness, 184, 0.46).
narrative_ontology:measurement(sa_civic_be_t230, second_amendment_scope__civic_right_reading, base_extractiveness, 230, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(sa_civic_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sa_civic_su_t46, second_amendment_scope__civic_right_reading, suppression_requirement, 46, 0.5).
narrative_ontology:measurement(sa_civic_su_t92, second_amendment_scope__civic_right_reading, suppression_requirement, 92, 0.52).
narrative_ontology:measurement(sa_civic_su_t138, second_amendment_scope__civic_right_reading, suppression_requirement, 138, 0.55).
narrative_ontology:measurement(sa_civic_su_t184, second_amendment_scope__civic_right_reading, suppression_requirement, 184, 0.54).
narrative_ontology:measurement(sa_civic_su_t230, second_amendment_scope__civic_right_reading, suppression_requirement, 230, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_scope kernel decomposes into three readings with distinct ε and beneficiary/victim structures. This reading (civic_right_reading) has moderate ε (~0.48) with service-based gating; individual_right_reading has low ε (~0.15) with universal beneficiary set; collective_right_reading has higher ε (~0.65) with state as sole beneficiary. The upstream founding-era militia republicanism constrains all three; downstream, Heller-era jurisprudence creates authority_erosion pressure on this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
