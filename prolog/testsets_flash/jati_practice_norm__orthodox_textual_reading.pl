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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Framework
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint describes the operation of jati (caste) boundaries as
 *   derived from a fixed, scriptural varna framework, where deviation is
 *   considered ritual pollution. This 'orthodox textual reading' emphasizes
 *   the immutability of social hierarchy based on religious texts, assigning
 *   specific occupations and social statuses to different jatis. It is a
 *   Snare due to its high extraction from lower jatis and severe suppression
 *   of social mobility, actively enforced by religious and social
 *   authorities.
 *
 * KEY AGENTS:
 *   - upper_jati_elites: Primary beneficiary (institutional/arbitrage) — benefits from social hierarchy and labor allocation.
 *   - religious_authorities: Agenda setter (institutional/arbitrage) — interprets and enforces scriptural mandates, maintaining the system's legitimacy.
 *   - lower_jati_communities: Primary victim (powerless/trapped) — bears the brunt of social exclusion, economic exploitation, and blocked mobility.
 *   - individuals_seeking_social_mobility: Victim (powerless/identity_locked) — faces severe social and ritual penalties for attempting to deviate from ascribed roles.
 *   - social_reformers: Observer (organized/analytical) — actively resists the constraint and advocates for its dismantling.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.92).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Framework").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '072194cd-8fc9-4ab1-b719-0ca195d15bea').
narrative_ontology:cs_kernel_codification('072194cd-8fc9-4ab1-b719-0ca195d15bea', fixed_text).
narrative_ontology:cs_authority_grounding('072194cd-8fc9-4ab1-b719-0ca195d15bea', lineage).
narrative_ontology:cs_interpretation_layer_present('072194cd-8fc9-4ab1-b719-0ca195d15bea').
narrative_ontology:cs_reading_relation('072194cd-8fc9-4ab1-b719-0ca195d15bea', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('072194cd-8fc9-4ab1-b719-0ca195d15bea', jati_practice_norm__colonial_census_reading, coexists_with).
narrative_ontology:cs_axiom('072194cd-8fc9-4ab1-b719-0ca195d15bea', foundational, varna_is_divinely_ordained_and_immutable).
narrative_ontology:cs_axiom_status(varna_is_divinely_ordained_and_immutable, holdable).
narrative_ontology:cs_axiom_grounding('072194cd-8fc9-4ab1-b719-0ca195d15bea', varna_is_divinely_ordained_and_immutable, theological).
narrative_ontology:cs_axiom('072194cd-8fc9-4ab1-b719-0ca195d15bea', secondary, deviation_from_varna_is_ritual_pollution).
narrative_ontology:cs_axiom_status(deviation_from_varna_is_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('072194cd-8fc9-4ab1-b719-0ca195d15bea', deviation_from_varna_is_ritual_pollution, theological).
narrative_ontology:cs_reference_frame('072194cd-8fc9-4ab1-b719-0ca195d15bea', ancient_scriptural_varna_order).
narrative_ontology:cs_drift_state('072194cd-8fc9-4ab1-b719-0ca195d15bea', contemporary_secular_challenges, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('072194cd-8fc9-4ab1-b719-0ca195d15bea', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_jati_elites).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, religious_authorities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, individuals_seeking_social_mobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the social hierarchy, preferential access to resources, and the availability of cheap labor from lower jatis. They uphold the scriptural interpretation to maintain their status and power.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_jati_elites, beneficiary,
    institutional, generational, arbitrage, regional).

% Interpret and propagate the scriptural varna framework, legitimizing the jati system through religious doctrine. They enforce ritual purity norms and social sanctions against deviation, deriving significant authority and social capital from this role.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, religious_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).

% Bear the burden of social exclusion, economic exploitation, and ritual discrimination. They are assigned specific, often stigmatized, occupations and face severe barriers to social and economic advancement. Exit is severely constrained by social ostracism and lack of alternatives.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_communities, payer,
    powerless, generational, trapped, local).

% Face intense social pressure, ritual penalties, and economic hardship for attempting to move outside their ascribed jati roles. Their identity is deeply intertwined with their jati, making 'exit' a profound personal and social rupture.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, individuals_seeking_social_mobility, payer,
    powerless, biographical, identity_locked, local).

% Actively challenge the scriptural basis and practical enforcement of the jati system, advocating for equality and social justice. They document its extractive and suppressive effects and organize resistance movements.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reformers, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, upper_jati_elites).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social roles and labor division based on a religiously sanctioned hierarchy, providing a stable (though unequal) framework for community organization and ritual practice.
% TRANSFER_FUNCTION: Transfers social status, economic opportunity, and ritual purity from lower-jati communities to upper-jati elites and religious authorities, in exchange for a rigidly defined social order.
% ABSENT_VOICES: Historically, many lower-jati voices were systematically silenced or marginalized from religious and social discourse. If present and empowered, they would articulate the profound injustice and suffering caused by the system, challenging its scriptural legitimacy and demanding fundamental restructuring.
% DISAPPEARANCE_RATIONALE: If the scriptural varna framework and its enforcement vanished overnight, the social order would undergo massive upheaval. Traditional power structures would collapse, labor markets would reorganize, and individuals would seek new social and economic roles, leading to a profound rearrangement of society.
% FOUNDING_PROBLEM: The founding problem, from this reading's perspective, was to establish a divinely ordained, stable, and ritually pure social order that prevents chaos and ensures the proper functioning of society through a hierarchical division of labor and roles.
% FOUNDING_PROBLEM_CORROBORATION: Religious authorities and upper-jati elites continue to assert that the founding problem of maintaining social order and ritual purity is live, citing the dangers of social breakdown and moral decay if the system is abandoned. However, social reformers and lower-jati communities strongly contest this, arguing that the 'problem' is a justification for ongoing exploitation, and that the system itself is the source of social disorder and injustice. Independent sociological and historical analyses from outside the benefiting parties corroborate the contested nature of the founding problem's status, highlighting its function in maintaining power structures rather than solving a genuine collective problem for all.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).

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
 *   The extractiveness (0.85) is high because lower jatis are systematically denied access to resources, education, and higher-status occupations, while being assigned 'polluting' labor. Suppression (0.92) is severe, enforced through social ostracism, ritual penalties, and economic dependency, making exit or deviation extremely difficult. Accessibility collapse (0.95) is near-total, as alternatives to one's ascribed jati role are almost entirely foreclosed. Resistance (0.7) is significant, reflecting ongoing efforts by lower-jati communities and social reformers to challenge the system, but it faces entrenched power. Theater ratio (0.15) is low, as the constraint is actively and genuinely enforced, with little performative maintenance; its function is direct extraction and social control.
 *
 * PERSPECTIVAL GAP:
 *   Upper jati elites and religious authorities perceive this as a divinely ordained or natural social order, ensuring stability and ritual purity. Lower jati communities experience it as an oppressive system of exploitation and enforced immobility. The engine's classification as a Snare reflects the latter, despite the former's claims of naturalness or coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Upper jati elites and religious authorities are clear beneficiaries (d near 0.0) as they accrue social status, economic advantage, and power from the system's rigidity. Lower jati communities and individuals seeking mobility are clear targets (d near 1.0) due to severe restrictions and extraction. The constraint subsidizes the former by extracting from the latter.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from the orthodox textual reading, is to maintain a divinely ordained social order and ritual purity. This mandate is 'live' for its beneficiaries, but for victims, it functions as a cover for extraction. The high extractiveness and suppression, coupled with the contested founding problem status, prevent mislabeling this as a legitimate coordination mechanism. The classification as a Snare correctly identifies its primary function as extraction, not coordination, despite its claims to religious authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'Is the ''fixed scriptural varna framework'' an accurate and singular interpretation of religious texts, or one among multiple possible readings?',
    'Comparative textual analysis by independent religious scholars, historical sociological studies of pre-colonial jati fluidity.',
    'If it''s one interpretation, the constraint''s claim to naturalness (or divine mandate) is weakened, reclassifying it further towards a constructed snare. If it''s singular, the ''mountain'' aspect of its claimed origin gains credence, though its extractive function remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, conceptual, 'Ambiguity in the scriptural basis for fixed jati boundaries.').

omega_variable(
    jati_kernel_reading_identification,
    'This constraint is the ''orthodox_textual_reading'' of the ''jati_practice_norm'' kernel. How would its classification change if viewed through the ''localized_practice_reading'' or ''colonial_census_reading''?',
    'Analyze the ''localized_practice_reading'' (jati boundaries as fluid, negotiated coordination norms) and ''colonial_census_reading'' (jati reified by external administration) as separate constraint stories and compare their computed classifications.',
    'The ''localized_practice_reading'' would likely compute as a Rope or Tangled Rope with lower extraction and suppression, reflecting local negotiation. The ''colonial_census_reading'' would likely compute as a Snare or Tangled Rope, but with a different authority grounding (state administration vs. religious texts) and a different set of beneficiaries/victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jati_kernel_reading_identification, conceptual, 'This constraint is one reading of the jati_practice_norm kernel; other readings would yield different classifications.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (economic dependency, social ostracism) or internalized (belief in one''s destined place, fear of ritual pollution)?',
    'Post-migration studies of individuals who have exited traditional jati structures: if social and economic barriers are removed but self-limiting beliefs persist, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. This would make the snare more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for jati boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__orthodox_textual_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__orthodox_textual_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 30, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jati_practice_norm' kernel. This 'orthodox_textual_reading' emphasizes fixed scriptural varna, while 'localized_practice_reading' focuses on fluid local norms, and 'colonial_census_reading' on reification by external administration. Each is a distinct constraint with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
