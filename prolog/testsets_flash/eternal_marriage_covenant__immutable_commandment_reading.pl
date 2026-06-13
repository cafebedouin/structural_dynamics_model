% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: D&C 132: Immutable Divine Law of Polygamy for Exaltation (Immutable Commandment Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story analyzes the 'immutable commandment' reading of D&C
 *   132, which establishes polygamy as an eternal, unchangeable divine law
 *   necessary for the highest degree of exaltation. This reading posits that
 *   federal pressure to abandon polygamy creates a martyrdom constraint,
 *   where compliance with secular law is seen as apostasy from divine law,
 *   and no legitimate revision path for the doctrine exists. The constraint
 *   is claimed as a Snare due to its high extraction and suppression,
 *   particularly for women and children within polygamous unions, and the
 *   identity-locked nature of its adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.9).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.95).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "D&C 132: Immutable Divine Law of Polygamy for Exaltation (Immutable Commandment Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, '9756ea0e-7b99-4d92-bee6-327d4b84e100').
narrative_ontology:cs_kernel_codification('9756ea0e-7b99-4d92-bee6-327d4b84e100', fixed_text).
narrative_ontology:cs_authority_grounding('9756ea0e-7b99-4d92-bee6-327d4b84e100', lineage).
narrative_ontology:cs_interpretation_layer_present('9756ea0e-7b99-4d92-bee6-327d4b84e100').
narrative_ontology:cs_reading_relation('9756ea0e-7b99-4d92-bee6-327d4b84e100', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('9756ea0e-7b99-4d92-bee6-327d4b84e100', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('9756ea0e-7b99-4d92-bee6-327d4b84e100', foundational, polygamy_is_eternal_immutable_commandment).
narrative_ontology:cs_axiom_status(polygamy_is_eternal_immutable_commandment, holdable).
narrative_ontology:cs_axiom_grounding('9756ea0e-7b99-4d92-bee6-327d4b84e100', polygamy_is_eternal_immutable_commandment, theological).
narrative_ontology:cs_axiom('9756ea0e-7b99-4d92-bee6-327d4b84e100', foundational, exaltation_requires_obedience_to_plural_marriage).
narrative_ontology:cs_axiom_status(exaltation_requires_obedience_to_plural_marriage, holdable).
narrative_ontology:cs_axiom_grounding('9756ea0e-7b99-4d92-bee6-327d4b84e100', exaltation_requires_obedience_to_plural_marriage, theological).
narrative_ontology:cs_reference_frame('9756ea0e-7b99-4d92-bee6-327d4b84e100', original_revelation_immutable_law).
narrative_ontology:cs_drift_state('9756ea0e-7b99-4d92-bee6-327d4b84e100', federal_anti_polygamy_campaign, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('9756ea0e-7b99-4d92-bee6-327d4b84e100', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_authority).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, exalted_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, children_of_polygamous_unions).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, members_seeking_exaltation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, members_seeking_exaltation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces D&C 132 as an eternal, immutable commandment. Benefits from the hierarchical structure and control over access to exaltation. Their identity is fused with upholding this divine law, making exit unthinkable.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, patriarchal_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Bear the social, emotional, and legal costs of living in polygamous unions, often with limited autonomy and resources. Their spiritual salvation is tied to obedience to this commandment, creating an identity lock.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, polygamous_wives, payer,
    powerless, biographical, trapped, local).

% Born into a system where their family structure is deemed divine but is illegal and socially stigmatized. Their identity is deeply intertwined with their family and religious community, making exit from the system a profound personal crisis.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, children_of_polygamous_unions, payer,
    powerless, biographical, identity_locked, local).

% Believe that obedience to this commandment, even if not practiced in this life, is essential for eternal progression and exaltation. They pay through cognitive dissonance and potential social ostracization if they question the doctrine, but benefit from the promise of eternal family units.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, members_seeking_exaltation, payer,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, members_seeking_exaltation, beneficiary).

% Enforces anti-polygamy laws, creating a direct conflict with the religious practice. From this reading's perspective, federal pressure creates a martyrdom constraint, where compliance with secular law means apostasy from divine law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% The promised outcome of obedience to the immutable commandment: eternal family units and divine status. This is the ultimate, abstract beneficiary of the constraint, representing the theological justification.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, exalted_families, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(eternal_marriage_covenant__immutable_commandment_reading, exalted_families).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the eternal progression of families and individuals towards exaltation, as understood through the specific interpretation of D&C 132.
% TRANSFER_FUNCTION: Transfers spiritual authority, social status, and access to eternal blessings from individuals to the patriarchal authority, in exchange for obedience to the immutable divine law of polygamy.
% ABSENT_VOICES: Those who question the immutability of the doctrine, or who suffer under its practice but are silenced by the threat of spiritual condemnation, are absent. Their voices would challenge the divine mandate and the necessity of the practice for exaltation.
% DISAPPEARANCE_RATIONALE: If the immutable commandment reading of D&C 132 vanished, the entire theological framework for exaltation would collapse for adherents. Patriarchal authority would lose its divine mandate, and the spiritual identity of many members would be fundamentally altered, leading to a profound reorganization of religious and social structures.
% FOUNDING_PROBLEM: To establish the eternal nature of marriage and family, and to provide a path for the highest degree of exaltation in the afterlife, which was understood to require plural marriage.
% FOUNDING_PROBLEM_CORROBORATION: Adherents of this reading, particularly those in fundamentalist groups, attest that the founding problem of eternal progression and exaltation remains live and that D&C 132 provides the immutable divine solution. No corroborating sources outside the benefiting parties exist, as the claim is theological and internal to the belief system.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the doctrine demands profound personal sacrifice and adherence to a socially and legally condemned practice, with the promise of eternal rewards. Suppression is extremely high (0.95) due to the theological identity lock, the social pressure within the community, and the severe spiritual consequences of non-compliance (loss of exaltation). Resistance is high (0.8) from external forces (federal government) but low internally due to the identity lock. Accessibility collapse is high (0.9) as alternatives to this path to exaltation are seen as spiritually inferior or non-existent. Theater ratio is low (0.1) because the practice and belief are genuinely held and enforced, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of patriarchal authority, this is a divine Rope, coordinating eternal families. From the perspective of polygamous wives and children, it is a Snare, extracting profound costs under the guise of divine law. The federal government views it as a Snare that violates secular law and human rights. The engine's computation of per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Patriarchal authority is a clear beneficiary, as the constraint reinforces their power and control over spiritual matters. Polygamous wives and children are primary victims, bearing the direct costs. Members seeking exaltation are also victims, as they are compelled to adhere to a difficult doctrine, but they also benefit from the promise of eternal blessings, creating an identity lock. The federal government acts as an external agenda-setter, attempting to dismantle the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense for its adherents, as the 'founding problem' of eternal exaltation is considered perpetually 'live' and immutable. The classification as a Snare prevents mislabeling it as a Rope or Mountain, which would obscure the coercive and extractive elements inherent in this specific reading of the doctrine, particularly given the identity-locked nature of its victims and the external legal pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_human_interpretation,
    'Is D&C 132 an immutable divine law, or a human interpretation of revelation that could be revised?',
    'Theological re-evaluation by a recognized religious authority, or a shift in the interpretive tradition that allows for re-contextualization of historical revelation.',
    'If reclassified as a human interpretation, the constraint''s suppression and extractiveness would decrease, as the identity lock would weaken, potentially shifting its classification from Snare to Tangled Rope or even Rope if the coordination function became primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_vs_human_interpretation, conceptual, 'Ambiguity between divine mandate and human interpretive choice.').

omega_variable(
    exaltation_necessity_ambiguity,
    'Is polygamy truly a necessary condition for the highest degree of exaltation, or is this a theological claim that could be reinterpreted?',
    'A shift in core theological doctrine by the religious institution, or the emergence of alternative, equally valid paths to exaltation within the belief system.',
    'If polygamy is found not to be strictly necessary, the identity lock on members would weaken, reducing the constraint''s extractiveness and suppression, potentially leading to a reclassification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exaltation_necessity_ambiguity, conceptual, 'Theological necessity of polygamy for exaltation.').

omega_variable(
    federal_pressure_as_martyrdom,
    'Is federal pressure genuinely a martyrdom constraint, or is it a legitimate challenge to an extractive practice?',
    'Historical analysis of the motivations and impacts of federal anti-polygamy laws, and a re-evaluation of the ''persecution'' narrative by adherents.',
    'If federal pressure is seen as a legitimate challenge, the ''martyrdom'' framing (which reinforces the identity lock) would weaken, potentially reducing the perceived suppression and extractiveness for adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_pressure_as_martyrdom, empirical, 'Framing of external legal pressure as religious persecution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 1843, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1843, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1843, 0.1).
narrative_ontology:measurement(eter_tr_t1852, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1852, 0.1).
narrative_ontology:measurement(eter_tr_t1862, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(eter_tr_t1872, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1872, 0.1).
narrative_ontology:measurement(eter_tr_t1882, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1882, 0.1).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(eter_be_t1843, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1843, 0.7).
narrative_ontology:measurement(eter_be_t1852, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1852, 0.75).
narrative_ontology:measurement(eter_be_t1862, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1862, 0.8).
narrative_ontology:measurement(eter_be_t1872, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1872, 0.85).
narrative_ontology:measurement(eter_be_t1882, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1882, 0.88).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 1890, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1843, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1843, 0.7).
narrative_ontology:measurement(eter_su_t1852, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1852, 0.75).
narrative_ontology:measurement(eter_su_t1862, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1862, 0.8).
narrative_ontology:measurement(eter_su_t1872, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1872, 0.85).
narrative_ontology:measurement(eter_su_t1882, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1882, 0.9).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 1890, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
