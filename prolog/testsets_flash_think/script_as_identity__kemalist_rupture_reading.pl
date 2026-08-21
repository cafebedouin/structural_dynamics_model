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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Kemalist Latin Script Adoption for Secular Rupture
 *   domain: political/cultural/linguistic
 *
 * SUMMARY:
 *   This constraint story instantiates the 'kemalist_rupture_reading' of the
 *   'script_as_identity' kernel. It describes the 1928 Turkish script reform,
 *   where the newly founded Kemalist state replaced the Ottoman Arabic script
 *   with a Latin-based alphabet. This act was a cornerstone of the secular
 *   modernization project, explicitly designed to sever cultural and
 *   historical ties with the Ottoman-Islamic past and reorient Turkey towards
 *   a Western, secular identity. The reading emphasizes the state's intent to
 *   create a clean break, viewing any 'transition cost' as a feature, not a
 *   bug, and monopolizing the new literacy apparatus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.85).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.9).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, snare).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Kemalist Latin Script Adoption for Secular Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/cultural/linguistic").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '69d37671-61e3-4da4-a294-2961d74028ad').
narrative_ontology:cs_kernel_codification('69d37671-61e3-4da4-a294-2961d74028ad', formalized).
narrative_ontology:cs_authority_grounding('69d37671-61e3-4da4-a294-2961d74028ad', extraction).
narrative_ontology:cs_interpretation_layer_present('69d37671-61e3-4da4-a294-2961d74028ad').
narrative_ontology:cs_reading_relation('69d37671-61e3-4da4-a294-2961d74028ad', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('69d37671-61e3-4da4-a294-2961d74028ad', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('69d37671-61e3-4da4-a294-2961d74028ad', foundational, secular_modernity_requires_rupture).
narrative_ontology:cs_axiom_status(secular_modernity_requires_rupture, holdable).
narrative_ontology:cs_axiom_grounding('69d37671-61e3-4da4-a294-2961d74028ad', secular_modernity_requires_rupture, instrumental).
narrative_ontology:cs_axiom('69d37671-61e3-4da4-a294-2961d74028ad', foundational, ottoman_script_impedes_progress).
narrative_ontology:cs_axiom_status(ottoman_script_impedes_progress, holdable).
narrative_ontology:cs_axiom_grounding('69d37671-61e3-4da4-a294-2961d74028ad', ottoman_script_impedes_progress, empirically_contingent).
narrative_ontology:cs_reference_frame('69d37671-61e3-4da4-a294-2961d74028ad', new_turkish_republic_identity).
narrative_ontology:cs_drift_state('69d37671-61e3-4da4-a294-2961d74028ad', contemporary_turkey, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('69d37671-61e3-4da4-a294-2961d74028ad', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_elites).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literati).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, traditional_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The new Turkish Republic, which unilaterally imposed the Latin script to forge a new national identity, sever ties with the Ottoman past, and facilitate modernization. It monopolized the literacy apparatus and enforced the change through law and education.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Intellectuals, bureaucrats, and urban populations who aligned with the Kemalist project. They gained cultural capital, political influence, and easier access to Western knowledge by embracing the new script and its associated secular identity.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_elites, beneficiary,
    powerful, generational, mobile, national).

% Scholars, poets, and writers whose expertise was in the Ottoman Arabic script. They lost their cultural capital, their ability to read historical texts, and their social standing overnight, becoming functionally illiterate in the new system.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literati, payer,
    powerless, biographical, trapped, national).

% Islamic clerics and educators who relied on Arabic script for religious texts and traditional education. The script change severely curtailed their authority, access to knowledge, and ability to transmit religious tradition, forcing a rupture with their identity.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars, payer,
    powerless, biographical, identity_locked, national).

% The general populace, particularly older generations and those in rural areas, who were forced to learn a new script. This severed their direct connection to written cultural heritage, including personal letters, historical documents, and religious texts, creating a generational literacy gap.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, traditional_public, payer,
    powerless, biographical, constrained, national).

% The historical and cultural legacy of the Ottoman Empire, which the Kemalist project actively sought to sever. Its voice, representing continuity and tradition, was excluded from the new national narrative.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_past, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(script_as_identity__kemalist_rupture_reading, ottoman_past).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_state).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify the new Turkish nation-state under a modern, secular identity, distinct from the Ottoman past, and to increase literacy with a phonetically simpler script for the Turkish language.
% TRANSFER_FUNCTION: Transfers cultural capital, historical continuity, and religious authority from traditional Ottoman institutions and literati to the new Kemalist state and secular elites, while also transferring the burden of re-literacy to the general public.
% ABSENT_VOICES: The Ottoman literati and religious scholars, whose cultural and religious authority was directly undermined, were actively suppressed or marginalized. The historical continuity itself, as a concept, was excluded from the new narrative, treated as an impediment rather than a resource.
% DISAPPEARANCE_RATIONALE: If the Latin script adoption and its associated rupture vanished, the modern Turkish state's foundational identity would be destabilized. This would lead to a profound re-evaluation of its relationship with its Ottoman and Islamic past, potentially altering its political, cultural, and educational trajectories, and requiring a massive effort to bridge the resulting historical and linguistic gaps.
% FOUNDING_PROBLEM: The Kemalist state perceived the Ottoman script (Arabic alphabet) as a barrier to modernization, a symbol of a stagnant past, and a hindrance to widespread literacy due to its complexity and poor phonetic fit for Turkish vowel harmony.
% FOUNDING_PROBLEM_CORROBORATION: The Kemalist state and its proponents assert the problem was live and the solution necessary for national progress and increased literacy. Critics (historians, cultural preservationists, religious groups) attest that the problem was overstated or that the solution created more significant problems (cultural amnesia, historical discontinuity), with independent academic studies often highlighting both benefits and severe costs. Corroboration for the 'problem solved' view comes from literacy rates and ease of learning the new script; corroboration for the 'problem created' view comes from studies on historical memory and cultural loss.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is very high (0.85) because the script change imposed immense cultural and intellectual costs on a large segment of the population, effectively rendering them illiterate in their own history and religious texts, while concentrating power and cultural capital in the hands of the new state and secular elites. Suppression is also very high (0.90) due to the state's swift and absolute enforcement, banning the old script and making the new one mandatory in all public and educational spheres. Theater ratio is low (0.10) because the reform was a genuinely functional, albeit coercive, act of state-building and identity formation, not merely performative maintenance of an atrophied function. Accessibility collapse is near total (0.95) for the old script, as the state effectively eliminated its public use. Resistance was present but largely overwhelmed by state power (0.70).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Kemalist state and secular elites, the script reform was a necessary and beneficial act of modernization and national liberation. From the perspective of the Ottoman literati, religious scholars, and traditional public, it was an act of cultural destruction, historical amnesia, and an imposition of foreign identity. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The Kemalist state and secular elites are clear beneficiaries, gaining control over national identity and cultural production. The Ottoman literati, religious scholars, and the traditional public are victims, bearing the direct costs of cultural disenfranchisement, loss of historical access, and forced re-education. The directionality for the state and elites is near 0.0 (full beneficiary), while for the victims it is near 1.0 (full target).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    script_as_identity_kernel_reading,
    'Is this constraint best understood as the ''kemalist_rupture_reading'' of the ''script_as_identity'' kernel, or does it conflate with a sibling reading?',
    'Analysis of primary sources from the period, focusing on official state rhetoric and the stated goals of the script reform, to confirm the explicit intent of rupture versus continuity or pure instrumentalism.',
    'If the rupture intent is less central than claimed, the extractiveness and suppression metrics might be re-evaluated downward, potentially shifting the classification towards a Tangled Rope or even a Rope if the coordination function (literacy) is re-emphasized over the rupture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(script_as_identity_kernel_reading, conceptual, 'Confirms the specific reading of the script_as_identity kernel.').

omega_variable(
    cultural_cost_vs_modernization_gain,
    'What is the true long-term balance between the cultural costs (historical amnesia, loss of traditional knowledge) and the modernization gains (increased literacy, access to Western knowledge) attributed to the script reform?',
    'Longitudinal sociological and historical studies, comparative analysis with other nations'' script reforms, and quantitative assessment of literacy rates versus cultural retention metrics over multiple generations.',
    'If cultural costs are found to significantly outweigh modernization gains, the extractiveness of the constraint would be further validated or even increased. If gains are found to be overwhelmingly positive, the ''snare'' classification might be challenged by a ''tangled_rope'' argument, though the high suppression would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_cost_vs_modernization_gain, empirical, 'Assesses the net impact of the script reform beyond initial state goals.').

omega_variable(
    internalized_suppression_of_ottoman_identity,
    'To what extent did the initial structural suppression of the Ottoman script and identity become internalized within the Turkish populace, persisting even after active enforcement waned?',
    'Psychological and sociological studies on national identity formation, educational curricula analysis, and public discourse analysis regarding the Ottoman past in contemporary Turkey.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the ''victims'' carry the suppression with them, making resistance to the Kemalist rupture more difficult even in later generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_ottoman_identity, empirical, 'Structural vs. internalized suppression mechanism for cultural identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 1958).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(scri_tr_t1938, script_as_identity__kemalist_rupture_reading, theater_ratio, 1938, 0.1).
narrative_ontology:measurement(scri_tr_t1948, script_as_identity__kemalist_rupture_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(scri_tr_t1958, script_as_identity__kemalist_rupture_reading, theater_ratio, 1958, 0.1).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.8).
narrative_ontology:measurement(scri_be_t1938, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1938, 0.85).
narrative_ontology:measurement(scri_be_t1948, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(scri_be_t1958, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1958, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.85).
narrative_ontology:measurement(scri_su_t1938, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1938, 0.9).
narrative_ontology:measurement(scri_su_t1948, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(scri_su_t1958, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1958, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
