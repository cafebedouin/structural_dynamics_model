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
 *   human_readable: Second Amendment Collective Right Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the collective right reading of the Second
 *   Amendment: the constitutional provision protects the authority of states
 *   to maintain and regulate militias, and does not secure an individual
 *   right to private firearms ownership. Under this reading, the amendment
 *   functions as a federalism provision allocating authority between state
 *   and federal governments regarding military organization. The kernel is
 *   the Second Amendment text; this reading competes with individual-right
 *   and civic-right readings. The authored metrics reflect a low-extraction
 *   institutional coordination arrangement, though the reading has faced
 *   substantial resistance and increasing interpretive drift toward the
 *   individual-right position.
 *
 * KEY AGENTS:
 *   - state_governments: Primary beneficiary (institutional/mobile) â retain regulatory autonomy and federalism shield
 *   - state_militia_institutions: Secondary beneficiary (organized/constrained) â protected from federal disarmament
 *   - individual_firearms_claimants: Excluded target (organized/constrained) â denied constitutional coverage under this reading
 *   - federal_judiciary: Agenda-setter (institutional/analytical) â administers interpretive framework
 *   - gun_rights_advocates: Excluded opposition (organized/constrained) â arguments treated as category errors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.22).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.4).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective Right Reading").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '3ac8b513-0c73-4ede-9369-eb2215ca1a98').
narrative_ontology:cs_kernel_codification('3ac8b513-0c73-4ede-9369-eb2215ca1a98', fixed_text).
narrative_ontology:cs_authority_grounding('3ac8b513-0c73-4ede-9369-eb2215ca1a98', lineage).
narrative_ontology:cs_interpretation_layer_present('3ac8b513-0c73-4ede-9369-eb2215ca1a98').
narrative_ontology:cs_reading_relation('3ac8b513-0c73-4ede-9369-eb2215ca1a98', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3ac8b513-0c73-4ede-9369-eb2215ca1a98', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('3ac8b513-0c73-4ede-9369-eb2215ca1a98', foundational, right_holder_is_state_militia_institution).
narrative_ontology:cs_axiom_status(right_holder_is_state_militia_institution, holdable).
narrative_ontology:cs_axiom_grounding('3ac8b513-0c73-4ede-9369-eb2215ca1a98', right_holder_is_state_militia_institution, conventional).
narrative_ontology:cs_axiom('3ac8b513-0c73-4ede-9369-eb2215ca1a98', foundational, individual_arms_bearing_not_constitutionally_shielded).
narrative_ontology:cs_axiom_status(individual_arms_bearing_not_constitutionally_shielded, holdable).
narrative_ontology:cs_axiom_grounding('3ac8b513-0c73-4ede-9369-eb2215ca1a98', individual_arms_bearing_not_constitutionally_shielded, conventional).
narrative_ontology:cs_reference_frame('3ac8b513-0c73-4ede-9369-eb2215ca1a98', state_militia_federalism).
narrative_ontology:cs_drift_state('3ac8b513-0c73-4ede-9369-eb2215ca1a98', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('3ac8b513-0c73-4ede-9369-eb2215ca1a98', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_militia_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain constitutional authority to organize, arm, and discipline militia forces without federal preemption; the Second Amendment operates as a federalism shield preserving state regulatory autonomy over military organization and firearms policy within their borders.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, mobile, national).

% National Guard and organized state militia units benefit from constitutional protection against federal disarmament or neglect; their funding and command structure remains partially state-directed under this reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_militia_institutions, beneficiary,
    organized, generational, constrained, national).

% Individuals seeking constitutional protection for private firearms possession are structurally excluded from the Second Amendment's coverage under this reading; their claims are adjudicated as falling outside the provision's scope, leaving them to seek protection under state law or other constitutional provisions.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearms_claimants, excluded,
    organized, biographical, constrained, national).

% Federal courts administer this reading through precedential interpretation, determining whether firearms regulations implicate state militia authority or exceed federal power; their doctrinal framework sets the boundaries of permissible federal legislation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocacy organizations advancing individual rights framings are excluded from the interpretive framework under this reading; their constitutional arguments are treated as category errors rather than competing claims within the amendment's scope.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents federal interference with state military organization by constitutionally entrenching state authority over militia arming and training; coordinates the federal-state boundary in military affairs.
% TRANSFER_FUNCTION: Moves constitutional protection and regulatory autonomy from individual firearms owners to state government and militia institutions, reallocating the locus of protected authority.
% ABSENT_VOICES: Individual firearms owners, gun rights litigation organizations, and advocates of armed self-defense as a personal right are structurally excluded; they would argue for individual incorporation but are positioned outside the amendment's beneficiary class under this reading.
% DISAPPEARANCE_RATIONALE: If the collective right reading vanished, states would lose a federalism shield against federal firearms regulation; judicial review standards would shift toward individual-rights or civic-rights frameworks, fundamentally rearranging the constitutional allocation of authority among federal government, states, and private actors.
% FOUNDING_PROBLEM: Founding-era fear that the new federal government might disarm state militias and rely on a standing federal army, thereby undermining state sovereignty, republican civic virtue, and the distributed military capacity thought necessary to resist tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and legal scholars outside the direct beneficiary set attest to the founding-era federalism concern. Military historians corroborate the historical transformation of state militias into the modern National Guard system and the rise of a permanent federal standing military, which substantially resolves the original problem. Gun rights advocates and some historians contest that the original federalism concern exhausts the amendment's meaning.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.22) because the reading allocates authority to state institutions rather than extracting resources from individuals. Suppression is moderate (0.40) because the reading must be judicially enforced against individual-rights challengers to maintain its boundary. Theater ratio rises over the interval (0.30 at endpoint) because defense of the reading became increasingly performative as the individual-right reading gained scholarly and judicial ground. Resistance is high (0.70) due to sustained opposition from gun rights advocates and the Supreme Court's eventual rejection in District of Columbia v. Heller. Accessibility collapse is moderate (0.45) because alternative readings remain intellectually and legally available.
 *
 * PERSPECTIVAL GAP:
 *   State governments experience the constraint as a protective constitutional shield preserving their regulatory autonomy. Individual firearms claimants experience it as a structural denial of constitutional protection â not active extraction but an exclusion from the amendment's coverage boundary. The federal judiciary experiences it as a federalism doctrine that organizes judicial review. These seats diverge because the constraint structurally allocates rights to one set of actors while explicitly withholding them from another.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and militia institutions are beneficiaries (directionality near 0.0) because the constraint subsidizes their regulatory authority against federal preemption. Individual claimants and gun rights advocates are structurally excluded from the beneficiary set; since no victim group is declared, their directionality derives from their exclusion â the constraint operates as a negative boundary on their claims rather than an active extraction mechanism. The federal judiciary sits near symmetric (directionality ~0.5) as it administers the framework without being a direct beneficiary or target of the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fear of federal disarmament of state militias â is unambiguously dead in the modern era of the National Guard and permanent federal standing military. The reading persists partly as interpretive inertia and partly as a live federalism principle in constitutional discourse. Classifying it as rope prevents mislabeling the allocation of authority as pure extraction, while the dead founding problem status and rising theater ratio signal mandatrophy risk: the coordination function has atrophied even if the doctrinal form persists through performative defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_ambiguity,
    'Does the historical and textual evidence from the founding era support the collective right reading as the original public meaning, or does it underdetermine the distinction between collective and individual rights?',
    'Comprehensive philological and archival analysis of ratification-era usage of ''bear arms'' and ''militia'' in public discourse, combined with systematic review of state constitutional analogues from the same period.',
    'If historical evidence underdetermines the reading, the constraint''s classification as rope rests on conventional coordination rather than textual necessity; if evidence strongly supports it, the constraint gains mountain-like immunity claims that would require FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_ambiguity, empirical, 'Historical evidentiary ambiguity surrounding original public meaning').

omega_variable(
    modern_militia_institutional_continuity,
    'Does the modern National Guard and state military bureaucracy satisfy the constitutional concept of ''militia'' such that the collective right reading remains institutionally anchored, or has the transformation of military organization rendered the reading functionally obsolete?',
    'Comparative institutional analysis of founding-era militia structure against modern National Guard command, funding, and deployment chains; assessment of whether the constitutional predicate still refers to an extant institution.',
    'If the modern National Guard is discontinuous with the constitutional militia, the reading''s beneficiary set loses institutional anchoring and the constraint drifts toward piton status; if continuous, the coordination function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_militia_institutional_continuity, empirical, 'Institutional continuity between constitutional militia and modern National Guard').

omega_variable(
    exclusion_as_extraction_boundary,
    'Does the structural exclusion of individuals from constitutional protection under this reading constitute a form of extraction, or merely the absence of a benefit that the text was never interpreted to confer?',
    'Comparative analysis across constitutional rights jurisprudence: when a provision is read to cover only institutional actors, is the exclusion of individuals a cost borne by them (extraction) or a neutral boundary (non-benefit)?',
    'If exclusion is extraction, the constraint would require victim declarations and directionality recalculation, potentially shifting classification toward tangled_rope; if non-benefit, the low epsilon and rope classification hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_as_extraction_boundary, conceptual, 'Whether exclusion from constitutional protection counts as extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__collective_right_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__collective_right_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(seco_tr_t60, second_amendment_scope__collective_right_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(seco_tr_t70, second_amendment_scope__collective_right_reading, theater_ratio, 70, 0.5).
narrative_ontology:measurement(seco_tr_t80, second_amendment_scope__collective_right_reading, theater_ratio, 80, 0.52).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__collective_right_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__collective_right_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(seco_be_t60, second_amendment_scope__collective_right_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(seco_be_t70, second_amendment_scope__collective_right_reading, base_extractiveness, 70, 0.25).
narrative_ontology:measurement(seco_be_t80, second_amendment_scope__collective_right_reading, base_extractiveness, 80, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__collective_right_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__collective_right_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(seco_su_t60, second_amendment_scope__collective_right_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(seco_su_t70, second_amendment_scope__collective_right_reading, suppression_requirement, 70, 0.65).
narrative_ontology:measurement(seco_su_t80, second_amendment_scope__collective_right_reading, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
