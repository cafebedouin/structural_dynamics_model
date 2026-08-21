% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Kemalist Rupture: Latin Script as Secular Modernization
 *   domain: comparative_linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This constraint describes the Kemalist state's imposition of the Latin
 *   script in Turkey (1928) as a means to achieve secular modernization by
 *   deliberately severing ties with the Ottoman-Islamic past. From this
 *   reading's perspective, the rupture was a feature, not a bug, and the
 *   state's monopolization of the literacy apparatus was a necessary step.
 *   The high extractiveness and suppression reflect the coercive nature of
 *   this top-down reform and the profound costs imposed on those whose
 *   identity and livelihood were tied to the old script.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.85).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.92).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, snare).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Kemalist Rupture: Latin Script as Secular Modernization").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "comparative_linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '53df85d0-c7ba-4f33-b87c-d93a4a51153c').
narrative_ontology:cs_kernel_codification('53df85d0-c7ba-4f33-b87c-d93a4a51153c', formalized).
narrative_ontology:cs_authority_grounding('53df85d0-c7ba-4f33-b87c-d93a4a51153c', extraction).
narrative_ontology:cs_interpretation_layer_present('53df85d0-c7ba-4f33-b87c-d93a4a51153c').
narrative_ontology:cs_reading_relation('53df85d0-c7ba-4f33-b87c-d93a4a51153c', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('53df85d0-c7ba-4f33-b87c-d93a4a51153c', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('53df85d0-c7ba-4f33-b87c-d93a4a51153c', foundational, secular_modernity_requires_ottoman_rupture).
narrative_ontology:cs_axiom_status(secular_modernity_requires_ottoman_rupture, holdable).
narrative_ontology:cs_axiom_grounding('53df85d0-c7ba-4f33-b87c-d93a4a51153c', secular_modernity_requires_ottoman_rupture, instrumental).
narrative_ontology:cs_axiom('53df85d0-c7ba-4f33-b87c-d93a4a51153c', foundational, state_as_sole_arbiter_of_national_identity).
narrative_ontology:cs_axiom_status(state_as_sole_arbiter_of_national_identity, holdable).
narrative_ontology:cs_axiom_grounding('53df85d0-c7ba-4f33-b87c-d93a4a51153c', state_as_sole_arbiter_of_national_identity, conventional).
narrative_ontology:cs_reference_frame('53df85d0-c7ba-4f33-b87c-d93a4a51153c', new_secular_turkish_identity).
narrative_ontology:cs_drift_state('53df85d0-c7ba-4f33-b87c-d93a4a51153c', contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('53df85d0-c7ba-4f33-b87c-d93a4a51153c', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, turkish_republic_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_elite).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_educated_class).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary architect and enforcer of the script reform, viewing it as essential for national identity, secularism, and modernization. It monopolized the literacy apparatus and suppressed all alternatives.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, turkish_republic_state, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefited from the new script as it aligned with their Westernizing ideology and granted them privileged access to state power and modern education, effectively marginalizing the old guard.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_elite, beneficiary,
    powerful, biographical, mobile, national).

% Rendered functionally illiterate overnight, losing their social status, professional utility, and access to their cultural heritage. Their knowledge base became inaccessible, and they had no viable alternative but to attempt re-education or face irrelevance.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_educated_class, payer,
    powerless, immediate, trapped, national).

% Their entire body of religious texts and scholarly tradition was in Arabic script, making them a primary target of the rupture. Their authority and ability to transmit knowledge were severely curtailed, forcing a choice between abandoning their tradition or operating underground.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars, payer,
    powerless, generational, identity_locked, national).

% Already with low literacy rates, they faced a new barrier to accessing any written material, including religious texts and official documents. The reform further isolated them from the state's modernization project and their own cultural past.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_population, payer,
    powerless, biographical, constrained, local).

% Viewed the reform as a radical but necessary step towards Westernization and secularism, often overlooking the social costs and coercive aspects in favor of the stated modernization goals.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aimed to coordinate national identity around a new, secular, Western-oriented vision for Turkey, by creating a textual and cultural break with the Ottoman past and aligning with European modernity.
% TRANSFER_FUNCTION: Transferred cultural capital, political power, and access to education from the Ottoman-educated, religiously-oriented classes to the new secular, Western-educated elite, by making the former's knowledge base obsolete.
% ABSENT_VOICES: The vast majority of the population, particularly the rural and religiously conservative segments, had no voice in the decision. Their resistance was met with state suppression, not consultation. Ottoman intellectuals and religious leaders were actively silenced or marginalized.
% DISAPPEARANCE_RATIONALE: If the Latin script reform had not occurred, Turkey's national identity, educational system, and relationship with its Ottoman and Islamic heritage would be fundamentally different. The entire trajectory of Turkish modernization would have been altered, with profound implications for its political and cultural landscape.
% FOUNDING_PROBLEM: The Kemalist state perceived the Ottoman-Arabic script as a barrier to modernization, a symbol of religious conservatism, and an impediment to widespread literacy due to its complexity and poor fit for Turkish phonology.
% FOUNDING_PROBLEM_CORROBORATION: The Turkish Republic state and secular elites continue to assert the problem was live and the reform was essential for national progress. Historians and sociologists, from outside the benefiting parties, corroborate the perceived problem but often highlight the coercive methods and social rupture as significant costs.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the reform effectively devalued the cultural and intellectual capital of a significant portion of the population, transferring power and legitimacy to the new secular elite. Suppression is extremely high due to the state's active enforcement, including banning the old script, establishing 'People's Schools' for new literacy, and punishing non-compliance. Theater ratio is low because the state's stated goals of modernization and secularization were genuinely pursued, even if coercively. Resistance was significant but largely suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The Kemalist state and secular elite would perceive this as a necessary, even beneficial, 'rope' for national development, while the victims experienced it as a 'snare' that extracted their cultural heritage and social standing. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish Republic state and secular elite are clear beneficiaries, gaining political legitimacy and cultural dominance. The Ottoman-educated class, religious scholars, and the rural population are victims, suffering immediate and profound losses of status, access, and cultural continuity. The state's identity was locked into this rupture, making exit unthinkable for the agenda-setter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_literacy_impact,
    'Did the Latin script reform genuinely increase overall literacy rates and educational attainment in the long term, or did it primarily shift the basis of literacy without a net gain for the broader population?',
    'Longitudinal studies comparing literacy trends in Turkey with demographically similar countries that did not undergo radical script reform, controlling for other modernization factors.',
    'If literacy gains were marginal or primarily concentrated in the secular elite, it would weaken the ''modernization'' justification and strengthen the ''extraction of cultural capital'' argument. If widespread, it would lend more credence to the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_literacy_impact, empirical, 'Assessing the actual impact of the script reform on literacy and education beyond the initial disruption.').

omega_variable(
    cultural_rupture_necessity,
    'Was a complete textual rupture with the Ottoman-Islamic past truly necessary for secular modernization, or could modernization have proceeded with a more gradual or dual-script approach?',
    'Comparative historical analysis of other post-colonial or modernizing nations that adopted different approaches to script reform and cultural continuity.',
    'If alternatives existed, it would highlight the ''preference'' aspect of the Kemalist choice and further underscore the coercive nature of the imposed rupture, strengthening the ''snare'' classification. If no viable alternatives are found, it would partially legitimize the perceived necessity from the Kemalist perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_rupture_necessity, conceptual, 'Examining the historical necessity of the cultural rupture for achieving modernization goals.').

omega_variable(
    internalized_suppression_of_ottoman_identity,
    'To what extent did the state''s suppression of the Ottoman script and associated cultural practices lead to internalized suppression, where subsequent generations self-censored or devalued their own Ottoman heritage even after direct state enforcement lessened?',
    'Sociological and psychological studies of cultural memory, identity formation, and intergenerational transmission of historical narratives in post-reform Turkish society.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the target population carries the suppression with them, making ''exit'' from the new identity frame more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_ottoman_identity, empirical, 'Structural vs. internalized suppression mechanism regarding Ottoman cultural identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.05).
narrative_ontology:measurement(scri_tr_t1935, script_as_identity__kemalist_rupture_reading, theater_ratio, 1935, 0.08).
narrative_ontology:measurement(scri_tr_t1942, script_as_identity__kemalist_rupture_reading, theater_ratio, 1942, 0.1).
narrative_ontology:measurement(scri_tr_t1950, script_as_identity__kemalist_rupture_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.75).
narrative_ontology:measurement(scri_be_t1935, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1935, 0.82).
narrative_ontology:measurement(scri_be_t1942, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1942, 0.85).
narrative_ontology:measurement(scri_be_t1950, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.8).
narrative_ontology:measurement(scri_su_t1935, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1935, 0.9).
narrative_ontology:measurement(scri_su_t1942, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1942, 0.92).
narrative_ontology:measurement(scri_su_t1950, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1950, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'script_as_identity' kernel, focusing on the Kemalist state's use of Latin script to enforce a secular, modern Turkish identity by rupturing with the Ottoman-Islamic past. It is linked to sibling readings that emphasize continuity or phonetic utility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
