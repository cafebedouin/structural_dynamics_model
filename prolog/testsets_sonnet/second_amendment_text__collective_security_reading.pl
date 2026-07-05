% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Collective Security (Militia-Conditioned) Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates ONE structurally distinct reading of the Second
 *   Amendment's text: the collective-security reading, which holds that the
 *   operative clause ('the right of the people to keep and bear Arms, shall
 *   not be infringed') is conditioned by the prefatory militia clause ('A
 *   well regulated Militia, being necessary to the security of a free
 *   State'), such that the right exists to preserve organized civic defense
 *   capacity and the state retains broad regulatory latitude over arms so
 *   long as it does not abolish that collective-defense function. This is NOT
 *   a story about the amendment generally — it is one reading among (at
 *   least) three structurally distinct constitutional claims sharing a single
 *   text (the kernel). The individual_right_reading treats the operative
 *   clause as an independent guarantee of personal self-defense unconditioned
 *   by militia status; the originalist_civic_virtue_reading treats 'the
 *   Militia' as coextensive with the whole armed citizenry rather than a
 *   state-organized body. Each reading has a different beneficiary/victim
 *   structure and a different epsilon: this reading's beneficiary is the
 *   state regulatory apparatus and its victim class is the individual owner
 *   disconnected from organized militia service, which is a materially
 *   different extraction profile than the individual-right reading (where the
 *   state regulatory apparatus would instead appear closer to a constrained
 *   actor).
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: agenda_setter (institutional/analytical) — administers licensing regimes this reading legitimizes
 *   - individual_gun_owners: primary payer (moderate/constrained) — bears compliance costs, loses unconditioned-right claim
 *   - organized_militia_successors_national_guard: beneficiary (institutional/analytical) — sole class whose arms-bearing is unambiguously protected under this reading
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates among competing readings of the same kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.42).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.38).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Collective Security (Militia-Conditioned) Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'ced31752-aa0d-4e7a-8445-6bce213a5f29').
narrative_ontology:cs_kernel_codification('ced31752-aa0d-4e7a-8445-6bce213a5f29', fixed_text).
narrative_ontology:cs_authority_grounding('ced31752-aa0d-4e7a-8445-6bce213a5f29', lineage).
narrative_ontology:cs_interpretation_layer_present('ced31752-aa0d-4e7a-8445-6bce213a5f29').
narrative_ontology:cs_reading_relation('ced31752-aa0d-4e7a-8445-6bce213a5f29', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('ced31752-aa0d-4e7a-8445-6bce213a5f29', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('ced31752-aa0d-4e7a-8445-6bce213a5f29', foundational, militia_clause_is_binding_condition).
narrative_ontology:cs_axiom_status(militia_clause_is_binding_condition, holdable).
narrative_ontology:cs_axiom_grounding('ced31752-aa0d-4e7a-8445-6bce213a5f29', militia_clause_is_binding_condition, conventional).
narrative_ontology:cs_axiom('ced31752-aa0d-4e7a-8445-6bce213a5f29', secondary, state_organized_militia_supersedes_universal_citizenry).
narrative_ontology:cs_axiom_status(state_organized_militia_supersedes_universal_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('ced31752-aa0d-4e7a-8445-6bce213a5f29', state_organized_militia_supersedes_universal_citizenry, empirically_contingent).
narrative_ontology:cs_reference_frame('ced31752-aa0d-4e7a-8445-6bce213a5f29', militia_conditioned_founding_settlement).
narrative_ontology:cs_drift_state('ced31752-aa0d-4e7a-8445-6bce213a5f29', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('ced31752-aa0d-4e7a-8445-6bce213a5f29', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_successors_national_guard).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, public_safety_constituencies).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unorganized_militia_advocates).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_retailers).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, police_power_supremacy_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, collective_rights_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures and executive agencies write and enforce licensing, permitting, registration, and use restrictions on firearms, justified as regulating the militia-context right rather than an unconditional individual entitlement. This reading gives the apparatus wide latitude to condition, tax, and restrict ownership as long as some rational relation to collective security or public order is asserted. It administers the enforcement machinery — background check systems, permit boards, licensing bureaucracies — and its authority under this reading is close to unreviewable so long as it does not abolish the right outright.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Modern organized state militias (National Guard units) are read as the constitutionally contemplated 'well regulated militia.' Under this reading their federally-chartered, state-administered status is treated as having absorbed and superseded the individual citizen-soldier function, giving them institutional legitimacy as the sole class whose arms-bearing is unambiguously protected.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, organized_militia_successors_national_guard, beneficiary,
    institutional, generational, analytical, national).

% Advocacy coalitions, municipal governments facing gun violence, and public health researchers benefit from a constitutional reading that permits robust regulation. They can lobby for licensing regimes, waiting periods, and possession restrictions without the doctrine itself being the primary obstacle; their exit option is political mobilization within ordinary lawmaking, which this reading leaves open.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, public_safety_constituencies, beneficiary,
    organized, biographical, mobile, national).

% Private individuals seeking to own or carry firearms outside any organized militia context bear the cost of this reading directly: their claim to a personal right is treated as derivative of, and conditioned on, a collective militia purpose that in practice no longer functions as citizen-soldier service. They must comply with licensing and permitting regimes whose legitimacy this reading affirmatively supplies. Exit is limited to relocating to friendlier jurisdictions or absorbing the compliance burden; the underlying constitutional claim to unconditional ownership is foreclosed by this reading's own logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Groups asserting that the 'unorganized militia' (all able-bodied citizens under statutes like 10 U.S.C. § 246) satisfies the militia condition find this reading unpersuasive on its own terms — the reading typically treats 'well regulated' as requiring actual state organization, not mere statutory membership. They have no doctrinal foothold within this reading and cannot exit the jurisdiction of federal constitutional interpretation.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, unorganized_militia_advocates, payer,
    powerless, biographical, trapped, national).

% Commercial sellers bear compliance costs from licensing and background-check infrastructure justified under this reading's collective-security rationale. They can relocate operations across state lines but cannot exit the federal constitutional framework; their business model is directly shaped by how much regulatory latitude this reading grants the state.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_retailers, payer,
    moderate, biographical, constrained, national).

% Appellate and supreme courts adjudicate between this reading and its individual-right sibling. They do not benefit from the constraint's operation but determine, through case law, which reading commands doctrinal authority at a given historical moment. Their choice among readings has direct downstream effect on every other stakeholder's exit options.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves the problem of channeling armed capacity through accountable, trained, state-organized bodies rather than dispersed private armament — reducing coordination failures and violence risk that come from unregulated private arms proliferation, and giving legislatures a stable doctrinal basis for public-safety regulation.
% TRANSFER_FUNCTION: Moves discretionary authority over who may lawfully own, carry, and use firearms from individual claimants to state licensing and permitting bureaucracies; correspondingly shifts compliance costs (fees, waiting periods, training requirements, denial risk) onto individual owners and retailers who fall outside the organized-militia frame.
% ABSENT_VOICES: Individual self-defense claimants and unorganized-militia advocates would object that this reading strips the personal right of practical content by conditioning it on an institutional context (organized militia service) that has been legislatively allowed to atrophy into the National Guard — they are present in litigation but structurally lose whenever this reading commands a majority.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned entirely in favor of an unconditioned individual right, existing licensing regimes, permit-to-purchase laws, and many possession restrictions would face renewed constitutional challenge; state regulatory apparatuses would lose their primary doctrinal shield, and firearms markets and retailer compliance obligations would shift substantially within a single litigation cycle.
% FOUNDING_PROBLEM: The founding-era problem was distrust of standing armies and reliance on citizen militias for collective defense and internal order; the constitutional text conditions the right on maintaining a 'well regulated Militia' as 'necessary to the security of a free State.'
% FOUNDING_PROBLEM_CORROBORATION: Military historians and several federal judges (including in pre-Heller circuit opinions) attest that the organized state militia the clause contemplated was functionally supplanted by the professionalized National Guard and federal standing military by the mid-20th century — a shift documented outside any advocacy group's litigation interest, in the legislative history of the Dick Act (1903) and National Defense Act (1916). Gun-rights organizations dispute that this supersession extinguishes the individual right; state regulatory bodies, who benefit from this reading, echo the historians' account, so the strongest independent corroboration comes from military-institutional historians rather than either interested party.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) reflecting that this reading, while doctrinally displacing an unconditioned individual claim, does not eliminate all private ownership — it primarily licenses and shapes rather than bans. Suppression sits lower-moderate (0.38) because compliance mechanisms (permits, waiting periods, background checks) constrain rather than criminalize possession outright, and substantial resistance (0.72) persists in the form of sustained litigation, legislative countermobilization, and cultural resistance from gun-rights constituencies, indicating the reading has never achieved uncontested settlement. Theater ratio (0.22) reflects that most regulatory activity under this reading performs a genuine (if contested) public-safety function rather than pure symbolic compliance, though some licensing bureaucracy exhibits performative rather than substantively protective characteristics. Accessibility collapse is moderate-low (0.35): this reading does not close off the individual-right reading as a live legal possibility, and the doctrinal contest remains active — it is not a settled mountain-like foreclosure.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulatory apparatus is the clear structural beneficiary: this reading supplies the doctrinal foundation for its licensing, permitting, and possession-restriction authority, and it faces essentially no exit from its own jurisdiction (it does not need one — it is the source of the constraint). Individual gun owners disconnected from organized militia service are the structural target: the reading conditions their claim on a collective-defense rationale that, as a matter of institutional history, has been effectively absorbed by the professionalized National Guard, leaving them without the doctrinal shield an unconditioned individual right would provide. Their exit options are geographically constrained (interstate variation in gun law stringency) rather than eliminated, which keeps directionality high-but-not-maximal rather than fully trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distrust of standing armies, reliance on organized militia for collective defense) is genealogically dead as a live operational need — professionalized militaries and the National Guard have supplanted the militia function this reading is textually anchored to. Yet the reading persists as active constitutional doctrine because it now serves a live, distinct function: providing the state a stable ground for firearms regulation independent of whether the original militia rationale still operates. This is precisely the mandatrophy pattern the R5 interview is designed to surface — the founding_problem_status is 'dead' while the disappearance_verdict is 'world_rearranges', flagging that the arrangement has been repurposed rather than sunset, without asserting that repurposing is illegitimate. The tension between a dead founding rationale and a live present function is exactly what the collective_security_reading was built to hold together, and is also its chief doctrinal vulnerability against the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_operative_or_prefatory,
    'Is the militia clause a limiting condition on the operative right (this reading), a now-satisfied historical justification that no longer constrains, or a description of the right''s purpose that never functioned as a legal condition at all (individual_right_reading)?',
    'This is a genealogical and interpretive question, not empirically resolvable by new evidence — it depends on which theory of constitutional interpretation (textualist, purposivist, or living-constitutionalist) is adopted, and possibly on further Supreme Court doctrine displacing or reaffirming Heller/McDonald.',
    'If courts settle definitively on the individual_right_reading (as post-2008 doctrine substantially has), this reading''s beneficiary structure (state regulatory apparatus as primary beneficiary) becomes doctrinally marginal, though it retains force in scholarly and dissenting-opinion contexts and could re-emerge with different court composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_operative_or_prefatory, conceptual, 'Whether the militia clause functions as a binding condition, a satisfied justification, or mere prefatory description.').

omega_variable(
    national_guard_militia_equivalence,
    'Does the modern National Guard genuinely satisfy the constitutional concept of ''the Militia,'' or is it structurally a federal military reserve that has extinguished, rather than fulfilled, the militia function the clause contemplated?',
    'Legal-historical analysis of the Dick Act (1903), National Defense Act (1916), and subsequent federalization statutes; comparison of Guard command structure (federal call-up authority) against founding-era militia''s state-and-citizen-controlled structure.',
    'If the Guard is judged a poor substitute (a federalized reserve, not a citizen militia), this reading''s beneficiary designation of ''organized_militia_successors_national_guard'' becomes contestable, and the founding_problem_status classification of ''dead'' would strengthen further, sharpening the mandatrophy diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_guard_militia_equivalence, empirical, 'Whether the National Guard structurally continues or extinguishes the founding-era militia concept.').

omega_variable(
    sibling_reading_epsilon_divergence,
    'Given that the individual_right_reading and originalist_civic_virtue_reading would assign the state regulatory apparatus a near-opposite directionality (constrained target rather than beneficiary), is the underlying constitutional text a single constraint with contested measurement, or genuinely three constraints sharing only a textual surface?',
    'Apply the epsilon-invariance test across all three sibling files: if epsilon and beneficiary/victim structure differ substantially and stably across readings (as authored here), the framework treats them as three constraints, not one under three lenses.',
    'Confirms the decomposition strategy used in this file family; if resolved the other way (treating it as one constraint with an observer-relative epsilon), the entire kernel/reading apparatus would need restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_divergence, conceptual, 'Whether the three declared readings constitute one constraint under contested measurement or three distinct constraints sharing a text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_text__collective_security_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_text__collective_security_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(seco_tr_t1934, second_amendment_text__collective_security_reading, theater_ratio, 1934, 0.15).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.28).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_text__collective_security_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_text__collective_security_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(seco_be_t1900, second_amendment_text__collective_security_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(seco_be_t1934, second_amendment_text__collective_security_reading, base_extractiveness, 1934, 0.3).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.32).
narrative_ontology:measurement(seco_be_t2024, second_amendment_text__collective_security_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_text__collective_security_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(seco_su_t1900, second_amendment_text__collective_security_reading, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(seco_su_t1934, second_amendment_text__collective_security_reading, suppression_requirement, 1934, 0.28).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.2).
narrative_ontology:measurement(seco_su_t2024, second_amendment_text__collective_security_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__collective_security_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'the Second Amendment' per the epsilon-invariance principle. individual_right_reading and originalist_civic_virtue_reading are separate files with their own epsilon, beneficiary/victim structure, and claimed_type. All three must be read together to understand the kernel contest; none is authoritative over the others within this framework — the engine's classification of each is independent, and courts (not this framework) adjudicate which reading commands legal force at a given time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
