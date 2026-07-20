% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective Right Reading (State Militia Authority)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the collective right reading of the Second
 *   Amendment: the constitutional provision is interpreted as protecting
 *   state authority to maintain militias, not as conferring any individual
 *   right to private firearm ownership. Under this reading, the federal
 *   judiciary enforces a federalism-based allocation of armed-force
 *   authority, state governments gain regulatory autonomy, and individual
 *   ownership claims are constitutionally excluded. The kernel is the Second
 *   Amendment text; this is one of three live readings (collective,
 *   individual, civic). The structural delta is low extractiveness because
 *   the constraint operates at the institutional level, transferring
 *   authority between governments rather than extracting concentrated
 *   resources from persons.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/constrained) â receives regulatory autonomy
 *   - organized_militias: Secondary beneficiary (organized/constrained) â institutional existence secured
 *   - federal_judiciary: Agenda setter (institutional/constrained) â interprets and enforces the reading
 *   - individual_firearm_claimants: Excluded party (organized/constrained) â bears absence of constitutional shield
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.22).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.35).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective Right Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '3362e5c5-16cc-4712-90d8-4d4990b57e06').
narrative_ontology:cs_kernel_codification('3362e5c5-16cc-4712-90d8-4d4990b57e06', fixed_text).
narrative_ontology:cs_authority_grounding('3362e5c5-16cc-4712-90d8-4d4990b57e06', lineage).
narrative_ontology:cs_interpretation_layer_present('3362e5c5-16cc-4712-90d8-4d4990b57e06').
narrative_ontology:cs_reading_relation('3362e5c5-16cc-4712-90d8-4d4990b57e06', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('3362e5c5-16cc-4712-90d8-4d4990b57e06', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('3362e5c5-16cc-4712-90d8-4d4990b57e06', foundational, state_militia_authority_exclusive).
narrative_ontology:cs_axiom_status(state_militia_authority_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('3362e5c5-16cc-4712-90d8-4d4990b57e06', state_militia_authority_exclusive, conventional).
narrative_ontology:cs_axiom('3362e5c5-16cc-4712-90d8-4d4990b57e06', foundational, individual_ownership_unprotected).
narrative_ontology:cs_axiom_status(individual_ownership_unprotected, holdable).
narrative_ontology:cs_axiom_grounding('3362e5c5-16cc-4712-90d8-4d4990b57e06', individual_ownership_unprotected, conventional).
narrative_ontology:cs_reference_frame('3362e5c5-16cc-4712-90d8-4d4990b57e06', state_militia_sovereignty_framework).
narrative_ontology:cs_drift_state('3362e5c5-16cc-4712-90d8-4d4990b57e06', post_heller_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3362e5c5-16cc-4712-90d8-4d4990b57e06', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militias).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise broad regulatory authority over firearms and militia organization within their borders because federal constitutional challenges based on individual right claims are rejected by courts under this reading. They do not set the constitutional interpretation but receive the autonomy it allocates.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Their institutional role as the constitutional locus of Second Amendment protection is secured against federal disarmament. They operate under state command structures and cannot unilaterally redefine their constitutional status.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militias, beneficiary,
    organized, generational, constrained, national).

% Interprets and enforces the Second Amendment as a federalism provision, rejecting individual ownership claims that lack militia nexus. Bound by textual methodology, precedent, and stare decisis; reversing the reading requires an overwhelming doctrinal shift.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Seek constitutional protection for private firearm possession and self-defense, but under this reading their claims are dismissed as outside the Amendment's scope. They remain subject to state and local regulation without a federal constitutional shield.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearm_claimants, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the vertical allocation of armed-force authority between the federal government and the states, preserving state capacity to maintain organized militias independent of federal monopoly over military power.
% TRANSFER_FUNCTION: Transfers regulatory autonomy over firearms and militia organization from the federal government to the states, while withholding from individuals a federal constitutional barrier against state regulation.
% ABSENT_VOICES: Individual self-defense advocates and individual-rights constitutional scholars, who would assert that private firearm ownership is constitutionally protected irrespective of militia service; their interpretive position is treated as doctrinally inadmissible under this reading.
% DISAPPEARANCE_RATIONALE: If this constitutional reading disappeared, federal courts would lose the doctrinal basis to reject individual right claims; state firearms regulations would face immediate federal constitutional challenge; the equilibrium of state regulatory autonomy would shift toward individual-right adjudication and potential federal pre-emption.
% FOUNDING_PROBLEM: The fear that a standing federal army would disarm state militias, leaving states defenseless against federal tyranny and unable to suppress insurrections or repel invasions.
% FOUNDING_PROBLEM_CORROBORATION: Historical historians attest the founding generation feared federal military supremacy, but legal scholars and military historians outside the state-sovereignty beneficiary set argue the modern National Guard framework and professional military have rendered the original militia-disarmament problem obsolete; no contemporary defense analyst outside the beneficiary set attests that federal disarmament of state militias is a present risk.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.22, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored low (0.22) because the constraint operates narrowly between governmental institutions; it does not collect a resource transfer from individuals but rather withholds a legal entitlement. Suppression is moderate (0.35) because the reading must be actively maintained by courts rejecting individual claims, yet the suppression is juridical rather than material. Theater is low (0.12) because the reading was historically the dominant doctrinal framework and is not primarily performative, though it has become more defensive post-Heller. Accessibility collapse is moderate (0.55): within the judicial system, individual-right alternatives are collapsed, but they remain live in political and scholarly discourse. Resistance is moderate-high (0.48) because a well-organized individual-rights movement consistently challenges the reading. The metric claim is rope because the arrangement's primary structural role is federal-state coordination of military authority, even though it asymmetrically disadvantages individual claimants.
 *
 * PERSPECTIVAL GAP:
 *   From the state-government seat, the constraint appears as a rope preserving federalism and state autonomy against federal overreach. From the individual-claimant seat, the same doctrinal structure appears as an exclusionary barrier that denies constitutional protection and leaves them exposed to state regulation. The engine computes this divergence from the identical structural data: the state beneficiary seat experiences low effective extraction, while the excluded individual seat experiences moderate effective extraction (amplified by scope and the absence of exit).
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and organized militias are the declared beneficiaries (low d, subsidized by the constraint's allocation of authority). The federal judiciary is the agenda setter (moderate d, administering the doctrine without being its primary beneficiary). Individual firearm claimants are excluded from protection; they are neither beneficiaries nor victims in the base properties, so their d derives from their structural position as organized but excluded actors, placing them nearer the target end than beneficiaries but not at full target because no concentrated extraction is directed at them.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating coordination function (preserving state militia capacity) from the cost it imposes (denial of individual constitutional shield). A snare classification would require concentrated beneficiaries capturing extracted resources and identifiable victims paying costs; here the beneficiary is diffuse state authority and the cost is diffuse legal exposure, so the low Îµ and institutional scope correctly resist snare classification. The framework captures that a rights-denial can be structurally extractive even when no party captures a monetary rent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the Second Amendment text logically support only this collective reading, or is the text structurally ambiguous enough to sustain the individual and civic readings as well?',
    'Historical-linguistic analysis of the text''s operative and prefatory clauses, supplemented by empirical study of original public meaning at ratification.',
    'If the text is logically determinate, this reading is either correct or incorrect as a matter of meaning; if ambiguous, the reading is one of multiple valid constructions and its authority depends on institutional choice rather than textual necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Textual ambiguity and kernel determinacy').

omega_variable(
    militia_obsolescence,
    'Has the institutional militia become obsolete as a military and policing instrument, rendering the collective right reading a historical anachronism?',
    'Comparative analysis of National Guard and state defense force deployment frequency, federal dependence on state military assets, and scholarly assessment of militia efficacy in modern warfare.',
    'If the militia is obsolete, the coordination function is hollow and the reading persists by doctrinal inertia; if still relevant, the coordination remains live and the reading retains functional justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_obsolescence, empirical, 'Whether the militia institution remains operationally relevant').

omega_variable(
    individual_cost_quantification,
    'Does the exclusion of individual ownership claims from constitutional protection impose a measurable cost on individuals, or merely leave regulation to the ordinary political process?',
    'Cross-jurisdictional comparison of state firearms regulation intensity and enforcement patterns under collective-right versus individual-right regimes.',
    'If measurable regulatory cost or criminal penalty differential exists, effective extraction may be higher than the narrow institutional framing suggests; if purely political, Îµ remains legitimately low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_cost_quantification, empirical, 'Whether exclusion from right protection translates to extractive cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sa_collective_tr_t16, second_amendment_scope__collective_right_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(sa_collective_tr_t32, second_amendment_scope__collective_right_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(sa_collective_tr_t48, second_amendment_scope__collective_right_reading, theater_ratio, 48, 0.15).
narrative_ontology:measurement(sa_collective_tr_t64, second_amendment_scope__collective_right_reading, theater_ratio, 64, 0.18).
narrative_ontology:measurement(sa_collective_tr_t80, second_amendment_scope__collective_right_reading, theater_ratio, 80, 0.2).

% Extraction over time
narrative_ontology:measurement(sa_collective_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sa_collective_be_t16, second_amendment_scope__collective_right_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(sa_collective_be_t32, second_amendment_scope__collective_right_reading, base_extractiveness, 32, 0.22).
narrative_ontology:measurement(sa_collective_be_t48, second_amendment_scope__collective_right_reading, base_extractiveness, 48, 0.22).
narrative_ontology:measurement(sa_collective_be_t64, second_amendment_scope__collective_right_reading, base_extractiveness, 64, 0.23).
narrative_ontology:measurement(sa_collective_be_t80, second_amendment_scope__collective_right_reading, base_extractiveness, 80, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sa_collective_su_t16, second_amendment_scope__collective_right_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(sa_collective_su_t32, second_amendment_scope__collective_right_reading, suppression_requirement, 32, 0.38).
narrative_ontology:measurement(sa_collective_su_t48, second_amendment_scope__collective_right_reading, suppression_requirement, 48, 0.35).
narrative_ontology:measurement(sa_collective_su_t64, second_amendment_scope__collective_right_reading, suppression_requirement, 64, 0.3).
narrative_ontology:measurement(sa_collective_su_t80, second_amendment_scope__collective_right_reading, suppression_requirement, 80, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the second_amendment_scope kernel, which decomposes into three structurally distinct claims: collective_right_reading (state militia authority), individual_right_reading (private ownership), and civic_right_reading (militia-conditioned individual right). Each reading has a different beneficiary/victim structure and Îµ profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
