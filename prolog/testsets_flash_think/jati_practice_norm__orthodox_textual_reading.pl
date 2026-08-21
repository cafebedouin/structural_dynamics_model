% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Scriptural Varna Framework (Orthodox Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes the jati system as understood through an
 *   orthodox textual reading, where social boundaries and occupational roles
 *   are rigidly derived from ancient scriptural varna frameworks. Deviation
 *   from these prescribed roles is considered ritual pollution, leading to
 *   severe social and economic consequences. This reading emphasizes the
 *   immutability and divine sanction of the hierarchy, which benefits
 *   upper-jati elites and religious authorities while trapping lower-jati and
 *   outcaste groups in highly extractive positions. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as snare (reflecting the structural
 *   reality of extraction and blocked mobility) while the authored metrics
 *   describe its operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Scriptural Varna Framework (Orthodox Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '8c43ce66-401c-42be-bf05-943f8c3b4c5b').
narrative_ontology:cs_kernel_codification('8c43ce66-401c-42be-bf05-943f8c3b4c5b', fixed_text).
narrative_ontology:cs_authority_grounding('8c43ce66-401c-42be-bf05-943f8c3b4c5b', lineage).
narrative_ontology:cs_interpretation_layer_present('8c43ce66-401c-42be-bf05-943f8c3b4c5b').
narrative_ontology:cs_reading_relation('8c43ce66-401c-42be-bf05-943f8c3b4c5b', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('8c43ce66-401c-42be-bf05-943f8c3b4c5b', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('8c43ce66-401c-42be-bf05-943f8c3b4c5b', foundational, varna_is_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('8c43ce66-401c-42be-bf05-943f8c3b4c5b', varna_is_divinely_ordained, theological).
narrative_ontology:cs_axiom('8c43ce66-401c-42be-bf05-943f8c3b4c5b', foundational, ritual_purity_hierarchy).
narrative_ontology:cs_axiom_status(ritual_purity_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('8c43ce66-401c-42be-bf05-943f8c3b4c5b', ritual_purity_hierarchy, deontological).
narrative_ontology:cs_reference_frame('8c43ce66-401c-42be-bf05-943f8c3b4c5b', ancient_scriptural_dharma).
narrative_ontology:cs_drift_state('8c43ce66-401c-42be-bf05-943f8c3b4c5b', contemporary_secular_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8c43ce66-401c-42be-bf05-943f8c3b4c5b', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_jati_elites).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, religious_authorities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_members).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, outcaste_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the social hierarchy, control significant resources, and actively enforce the norms of ritual purity and occupational segregation derived from scriptural interpretations. Their status and power are directly tied to the rigidity of the system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_jati_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret and transmit the scriptural varna framework, declaring ritual purity and pollution, thereby legitimizing and perpetuating the entire system. Their authority and social standing are derived from their role as custodians of this orthodox interpretation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, religious_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Are assigned specific, often stigmatized, occupations and face pervasive social discrimination. Their mobility is severely blocked, and they bear the social and economic costs of being deemed ritually impure for any deviation from prescribed roles. Exit is virtually impossible without severe social ostracization.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_members, payer,
    powerless, biographical, trapped, local).

% Exist outside the varna framework, facing extreme social exclusion, economic deprivation, and the highest degree of ritual stigmatization. They are forced into the most polluting occupations and have no recognized social standing within the orthodox system.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, outcaste_groups, payer,
    powerless, immediate, trapped, local).

% Actively challenge the scriptural basis and social implications of the jati system, advocating for equality and social justice. They face significant resistance from orthodox elements but work to create alternative social and legal frameworks.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reformers, observer,
    moderate, biographical, constrained, national).

% Study the historical evolution, textual interpretations, and contemporary manifestations of jati, providing critical analysis of its social and economic impacts. They operate from an external, academic perspective.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, anthropological_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order, assigning roles, duties, and ritual status based on birth, thereby maintaining social cohesion and ritual purity according to specific scriptural interpretations.
% TRANSFER_FUNCTION: Transfers social status, economic opportunity, and ritual purity from lower-jati and outcaste groups to upper-jati elites and religious authorities, while enforcing occupational segregation and limiting social mobility.
% ABSENT_VOICES: Those who would challenge the scriptural interpretation, the concept of birth-based hierarchy, or the very legitimacy of the varna framework are systematically marginalized, silenced, or excommunicated, preventing their perspectives from gaining any legitimate voice within the orthodox system.
% DISAPPEARANCE_RATIONALE: If the scriptural varna framework and its enforcement vanished overnight, the entire social, economic, and religious fabric of societies where this constraint is dominant would undergo a profound and chaotic reorganization. Traditional power structures would collapse, occupational roles would be redefined, and the concept of ritual purity would lose its enforcing power, leading to widespread social upheaval and the emergence of new forms of social organization.
% FOUNDING_PROBLEM: To maintain a divinely ordained social order, ensure ritual purity, and assign specialized labor roles within a complex society, as interpreted from ancient scriptures.
% FOUNDING_PROBLEM_CORROBORATION: Religious texts and their traditional interpreters, as well as upper-jati communities, attest that the problem of maintaining dharma and social order according to scriptural injunctions is still live. Social reformers and scholars, however, contest this, arguing the problem has shifted to one of maintaining an extractive hierarchy under the guise of tradition; legislative-hearing testimony and independent sociological analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) due to the systematic transfer of social status and economic opportunity from lower to upper jatis, enforced by the threat of ritual pollution and social ostracization. Suppression is also very high (0.90) because deviation is met with severe social penalties, blocked mobility, and the absence of legitimate alternatives. The theater ratio is low (0.10) as the enforcement of these norms is deeply embedded in social practice and genuinely functional for maintaining the hierarchy, rather than being merely performative. Accessibility collapse is high (0.90) because the system leaves virtually no viable alternatives for those trapped within it. Resistance is low (0.30) due to the overwhelming power of the enforcing authorities and the severe consequences of defiance. The measurement series reflect a period of increasing rigidity and enforcement, possibly in response to early reform movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of upper-jati elites and religious authorities, this constraint is a divinely ordained system for maintaining social order and ritual purity. From the perspective of lower-jati and outcaste groups, it is a deeply extractive and oppressive snare that denies them agency and opportunity. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper-jati elites and religious authorities are the primary beneficiaries and agenda-setters, deriving power, status, and economic advantage from the system's rigidity (d near 0.0). Lower-jati and outcaste groups are the primary targets, bearing the full costs of social exclusion, occupational immobility, and ritual stigmatization (d near 1.0). Social reformers and scholars act as observers, analyzing and challenging the system from external positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'Is the rigid varna framework an immutable scriptural command, or a historically contingent interpretation that has been selectively emphasized to maintain social hierarchy?',
    'Comparative textual analysis across different historical periods and interpretive traditions, alongside archaeological and sociological evidence of pre-textual social structures.',
    'If historically contingent, the constraint''s ''naturalness'' claim is undermined, reclassifying it more firmly as a constructed snare rather than a divinely ordained order. This would shift the burden of justification to the beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, conceptual, 'Ambiguity regarding the immutability vs. contingency of scriptural interpretation.').

omega_variable(
    social_function_vs_extraction,
    'Is the primary function of this constraint the maintenance of social order and ritual purity, or is it primarily a mechanism for the extraction of labor and status from lower jatis?',
    'Economic analysis of resource distribution and labor exploitation within the system, alongside sociological studies of social mobility and the impact of ''pollution'' on economic opportunity.',
    'If extraction is the primary function, the coordination story is revealed as cover, solidifying its classification as a snare and highlighting the victims'' trapped status. If genuine social order is the primary function, it might lean towards a tangled rope, but the high extractiveness would still be a major factor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_function_vs_extraction, empirical, 'Distinguishing genuine social function from extractive cover story.').

omega_variable(
    internalized_suppression_component,
    'To what extent is the observed suppression due to external enforcement (social ostracization, economic penalties) versus internalized belief in one''s varna/jati dharma and the inevitability of one''s social position?',
    'Longitudinal studies of individuals who have exited or challenged the system, examining the persistence of self-limiting beliefs and social stigma even after external barriers are reduced.',
    'If internalized suppression is a significant component, the effective suppression for targets is higher than external measures suggest, making exit even more difficult and amplifying the snare-like qualities. It also points to different intervention strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_component, empirical, 'Structural vs. internalized suppression mechanism in jati system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1900, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(jati_tr_t1920, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1920, 0.11).
narrative_ontology:measurement(jati_tr_t1940, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(jati_tr_t1960, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(jati_tr_t1980, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t1900, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1900, 0.75).
narrative_ontology:measurement(jati_be_t1920, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1920, 0.78).
narrative_ontology:measurement(jati_be_t1940, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1940, 0.81).
narrative_ontology:measurement(jati_be_t1960, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1960, 0.83).
narrative_ontology:measurement(jati_be_t1980, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1980, 0.84).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1900, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(jati_su_t1920, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1920, 0.83).
narrative_ontology:measurement(jati_su_t1940, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1940, 0.86).
narrative_ontology:measurement(jati_su_t1960, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1960, 0.88).
narrative_ontology:measurement(jati_su_t1980, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1980, 0.89).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
