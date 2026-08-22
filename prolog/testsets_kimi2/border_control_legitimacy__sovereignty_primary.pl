% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Control Legitimacy
 *   domain: political/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_primary reading of the
 *   border_control_legitimacy kernel. The reading holds that state
 *   territorial sovereignty entails absolute discretion to exclude
 *   non-citizens, and that border control is constitutive of statehood itself
 *   rather than merely instrumental to it. Within this frame, excluded
 *   migrants are structurally subordinated; enforcement is justified as
 *   defense of an existential state prerogative; and human rights constraints
 *   are treated as external, contingent limits on an otherwise absolute
 *   authority. The constraint operates across the global state system, though
 *   it is administered nationally.
 *
 * KEY AGENTS:
 *   - state_governments: Primary agenda-setter (institutional/arbitrage) â administers border control as sovereignty expression and collects legitimating authority
 *   - border_enforcement_agencies: Secondary agenda-setter (institutional/constrained) â executes coercion justified as defense of statehood
 *   - citizen_populations: Primary beneficiary (organized/mobile) â receives membership privileges and territorial exclusivity without administering enforcement
 *   - excluded_migrants: Primary target (powerless/trapped) â bears exclusion, detention, and deportation costs under absolute discretion
 *   - asylum_seekers: Secondary target (powerless/trapped) â subordinated protection claims treated as discretionary admissions
 *   - human_rights_organizations: Analytical observer (organized/analytical) â contests the frame from outside the state apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.82).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.88).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Sovereignty-Primary Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '6721ebed-1cff-4f21-b77c-677828260535').
narrative_ontology:cs_kernel_codification('6721ebed-1cff-4f21-b77c-677828260535', fixed_text).
narrative_ontology:cs_authority_grounding('6721ebed-1cff-4f21-b77c-677828260535', lineage).
narrative_ontology:cs_interpretation_layer_present('6721ebed-1cff-4f21-b77c-677828260535').
narrative_ontology:cs_reading_relation('6721ebed-1cff-4f21-b77c-677828260535', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('6721ebed-1cff-4f21-b77c-677828260535', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('6721ebed-1cff-4f21-b77c-677828260535', foundational, absolute_territorial_discretion).
narrative_ontology:cs_axiom_status(absolute_territorial_discretion, holdable).
narrative_ontology:cs_axiom_grounding('6721ebed-1cff-4f21-b77c-677828260535', absolute_territorial_discretion, conventional).
narrative_ontology:cs_axiom('6721ebed-1cff-4f21-b77c-677828260535', secondary, human_rights_subordinate_to_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_subordinate_to_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('6721ebed-1cff-4f21-b77c-677828260535', human_rights_subordinate_to_sovereignty, conventional).
narrative_ontology:cs_reference_frame('6721ebed-1cff-4f21-b77c-677828260535', westphalian_sovereignty_absolutism).
narrative_ontology:cs_drift_state('6721ebed-1cff-4f21-b77c-677828260535', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6721ebed-1cff-4f21-b77c-677828260535', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_governments).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, territorial_integrity_as_statehood_prerequisite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer territorial border control as an expression of sovereignty. Set admission criteria, deploy enforcement apparatus, and justify exclusions as constitutive of statehood. Collect legitimacy and political support from citizen populations for defending territorial integrity.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Execute border control operations including detention, deportation, and interdiction. Their institutional existence and budget depend on the sovereignty-primary framing. Justify coercion as defense of statehood rather than as discretionary policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Receive the benefits of territorial sovereignty including perceived security, resource access, and membership privileges. Their political support legitimates the enforcement apparatus. They do not directly administer borders but benefit from the exclusion of non-citizens.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_populations, beneficiary,
    organized, generational, mobile, national).

% Subject to exclusion, detention, and deportation under the absolute discretion claim. Denied entry, separated from family, or pushed into dangerous routes. They bear the direct human cost of the sovereignty-primary arrangement but have no voice in its administration.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Seek protection from persecution but are treated as discretionary admissions under the sovereignty-primary frame. Their claims are subordinated to state prerogative, often resulting in denial, detention, or refoulement despite international protection obligations.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Document abuses, litigate against exclusionary practices, and advocate for migrant rights. They contest the sovereignty-primary reading from outside the state apparatus, framing border control as subject to human rights limits rather than constitutive of legitimate authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, state_governments).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the allocation of territorial membership and jurisdictional authority among states, establishing who may enter and remain within a defined territory and under what conditions.
% TRANSFER_FUNCTION: Moves freedom of movement, security, and opportunity from non-citizens to sovereign states and their citizen populations, through the mechanism of border exclusion, detention, and enforcement.
% ABSENT_VOICES: Excluded migrants and asylum seekers are structurally absent from sovereignty determinations; their perspective is treated as irrelevant to the constitutive authority of statehood. Human rights advocates are present in discourse but treated as external challengers rather than internal stakeholders with standing.
% DISAPPEARANCE_RATIONALE: If the absolute discretion claim vanished overnight, states would lose the primary justification for blanket exclusion; border practices would shift toward jurisdictional balancing or rights-based adjudication; migrant mobility would increase; and the enforcement apparatus would require entirely new legitimating frameworks.
% FOUNDING_PROBLEM: The need to establish orderly territorial jurisdiction and resolve competing claims over space, membership, and authority in a world of multiple political communities.
% FOUNDING_PROBLEM_CORROBORATION: Classical international law scholars from within the beneficiary-attested tradition corroborate that competing territorial claims were the founding problem. However, no source outside the benefiting parties corroborates that absolute discretion to exclude remains the necessary solution; human rights organizations and migration scholars outside the beneficiary set attest instead that the founding problem has evolved toward managed mobility and protection obligations, making the absolute-exclusion solution obsolete.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because absolute discretion to exclude transfers freedom of movement, security, and economic opportunity from non-citizens to the sovereign state system. Suppression is higher (0.88) because the constraint's persistence depends on actively excluding alternatives such as free movement, open borders, or protection-based admission; enforcement infrastructure has intensified over the interval. Theater ratio is moderate-high (0.48) because sovereignty performanceâborder walls, ritualized detention, patriotic rhetoricâhas become increasingly central to legitimation even as the functional coordination aspects erode. Accessibility collapse is high (0.78): within the sovereignty-primary frame, alternatives like open borders are rendered conceptually illegitimate and politically unthinkable. Resistance is moderate (0.55) because migrant movements and human rights frameworks contest the arrangement, but they are structurally disadvantaged against the state apparatus. The measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state governments, enforcement agencies) experience the constraint as constitutive legitimacy and necessary coordination; the payer seats (excluded migrants, asylum seekers) experience it as coercive extraction backed by violence. The engine computes divergent per-seat classifications from this structural asymmetry. The citizen beneficiary seat occupies a middle position, receiving coordination benefits while remaining largely blind to the extractive costs borne by the excluded.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and citizen populations sit at the beneficiary end of the directionality spectrum: the constraint subsidizes their territorial control and membership privileges. Border enforcement agencies are agenda-setters with constrained exit; their directionality is intermediate because their institutional survival is fused to the constraint but they do not personally bear its costs. Excluded migrants and asylum seekers sit at the full-target end: they are the agents from whom mobility and security are extracted, with trapped exit options that amplify effective extraction. Human rights organizations occupy an analytical seat with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint risks mandatrophy because its founding problemâcompeting territorial jurisdictions in a multistate worldâmay no longer require absolute exclusion as the solution. Transnational governance, regional free-movement regimes, and managed mobility frameworks suggest alternative coordinations. If the founding problem is dead or transformed but the arrangement persists, the constraint would drift from tangled_rope toward piton (theatrical maintenance of an atrophied function) or snare (pure extraction justified by obsolete legitimation). The founding_problem_status is contested precisely because beneficiary-attested sources insist the problem remains live in its original form, while outside observers attest evolution toward new coordination needs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_naturalness,
    'Is territorial sovereignty with absolute exclusion a constitutive feature of political order, or a historically contingent arrangement naturalized through repetition and power?',
    'Historical comparative analysis examining whether state functions persist in contexts where border control is weak or absent (pre-modern empires, contemporary Schengen zones, city-states).',
    'If contingent, the constraint''s claim to mountain-like status collapses and it reclassifies toward snare or tangled_rope; if constitutive, the high extraction may be treated as necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness, conceptual, 'Whether absolute border sovereignty is natural or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforcement infrastructure, legal barriers) or internalized (the belief among state actors and citizens that border closure is inseparable from statehood)?',
    'Observe suppression trajectory in jurisdictions where enforcement infrastructure is removed or relaxed; if political demand restores closure, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measures and the constraint''s persistence is more deeply anchored, complicating reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function of territorial jurisdiction be separated from the asymmetric extraction of absolute exclusion?',
    'Examine the jurisdictional_sovereignty and freedom_of_movement_primary readings of the same kernel; assess natural experiments in regional free-movement zones where jurisdiction persists without absolute exclusion.',
    'If separable, the constraint is a tangled_rope where extraction rides on coordination; if inseparable, it approaches snare status if contingent, or mountain status if genuinely constitutive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether jurisdiction and exclusion are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__sovereignty_primary, theater_ratio, 10, 0.32).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__sovereignty_primary, theater_ratio, 20, 0.4).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__sovereignty_primary, theater_ratio, 30, 0.45).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.47).
narrative_ontology:measurement(bord_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__sovereignty_primary, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__sovereignty_primary, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__sovereignty_primary, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(bord_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__sovereignty_primary, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__sovereignty_primary, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__sovereignty_primary, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(bord_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_control_legitimacy kernel. The kernel decomposes into three structurally distinct readings that share the referent (border control and territorial sovereignty) but author different epsilon values, beneficiary/victim structures, and normative foundations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
