% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Turkish Graphemic Substrate: Secular Nationalist Reading
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   This constraint story instantiates the 'secular nationalist' reading of
 *   the Turkish graphemic substrate kernel. It describes the Turkish script
 *   reform of 1928, which replaced the Ottoman Turkish alphabet (based on
 *   Arabic script) with a new Latin-based alphabet. This reform was a
 *   cornerstone of the Turkish Republic's project to create a modern, secular
 *   national identity, explicitly severing ties with the Ottoman-Islamic past
 *   and aligning with European modernity. The constraint was imposed rapidly
 *   and coercively by the state, leading to a generational rupture in
 *   literacy and cultural memory.
 *
 * KEY AGENTS:
 *   - Turkish State: Primary agenda-setter and enforcer (institutional/arbitrage)
 *   - Secular Elites: Primary beneficiaries (powerful/mobile)
 *   - Ottoman Literate Population: Primary victims (powerless/trapped)
 *   - Religious Scholars: Victims, identity-locked (powerless/identity_locked)
 *   - Cultural Conservatives: Victims, constrained (moderate/constrained)
 *   - European Modernity Advocates: Beneficiaries (organized/analytical)
 *   - Ottoman Continuity Advocates: Excluded (powerless/identity_locked)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.85).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.9).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Turkish Graphemic Substrate: Secular Nationalist Reading").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '99f4659a-a491-4199-997c-e6326a17d806').
narrative_ontology:cs_kernel_codification('99f4659a-a491-4199-997c-e6326a17d806', formalized).
narrative_ontology:cs_authority_grounding('99f4659a-a491-4199-997c-e6326a17d806', extraction).
narrative_ontology:cs_interpretation_layer_present('99f4659a-a491-4199-997c-e6326a17d806').
narrative_ontology:cs_reading_relation('99f4659a-a491-4199-997c-e6326a17d806', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('99f4659a-a491-4199-997c-e6326a17d806', turkish_graphemic_substrate__gradual_transition_reading, forecloses).
narrative_ontology:cs_axiom('99f4659a-a491-4199-997c-e6326a17d806', foundational, turkish_identity_is_secular_and_modern).
narrative_ontology:cs_axiom_status(turkish_identity_is_secular_and_modern, holdable).
narrative_ontology:cs_axiom_grounding('99f4659a-a491-4199-997c-e6326a17d806', turkish_identity_is_secular_and_modern, conventional).
narrative_ontology:cs_axiom('99f4659a-a491-4199-997c-e6326a17d806', foundational, latin_script_is_vehicle_for_progress).
narrative_ontology:cs_axiom_status(latin_script_is_vehicle_for_progress, holdable).
narrative_ontology:cs_axiom_grounding('99f4659a-a491-4199-997c-e6326a17d806', latin_script_is_vehicle_for_progress, instrumental).
narrative_ontology:cs_reference_frame('99f4659a-a491-4199-997c-e6326a17d806', republican_founding_principles).
narrative_ontology:cs_drift_state('99f4659a-a491-4199-997c-e6326a17d806', contemporary_islamic_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('99f4659a-a491-4199-997c-e6326a17d806', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, turkish_state).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_elites).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_modernity_advocates).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_population).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, cultural_conservatives).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary enforcer and architect of the script reform, which aimed to rapidly forge a new national identity distinct from the Ottoman past and align Turkey with European modernity. It directly benefited from the consolidation of state power and the secularization of public life.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, turkish_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Gained significant cultural, political, and social capital by promoting and embodying the new Latin-script-based, secular Turkish identity. They were the primary beneficiaries of the new cultural order.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_elites, beneficiary,
    powerful, biographical, mobile, national).

% Lost functional literacy overnight with the abrupt change from Arabic to Latin script. They were cut off from their written cultural heritage and faced significant barriers to participation in the new public sphere.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_population, payer,
    powerless, biographical, trapped, national).

% Their traditional knowledge base, primarily contained in Arabic script texts, became largely inaccessible to the general public, diminishing their authority and influence. Their professional identity was deeply tied to the old script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars, payer,
    powerless, generational, identity_locked, national).

% Resisted the script reform and the broader secularization efforts, viewing them as a rupture with cherished cultural and religious traditions. They were forced to comply but experienced a profound sense of loss and alienation.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, cultural_conservatives, payer,
    moderate, biographical, constrained, national).

% Saw the script reform as a crucial step in Turkey's alignment with European modernity and secular values. They provided ideological support and validation for the state's actions.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_modernity_advocates, beneficiary,
    organized, generational, analytical, global).

% Their perspective, emphasizing the continuity of Turkish identity with its Ottoman-Islamic past and the legitimacy of the Arabic script, was actively suppressed and marginalized from public discourse and policy-making.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_advocates, excluded,
    powerless, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, turkish_state).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly forge a new, unified national identity for the Turkish Republic, distinct from its Ottoman-Islamic past, facilitating modernization, secularization, and alignment with European cultural and political norms.
% TRANSFER_FUNCTION: Transfers cultural capital, political power, and linguistic access from the Ottoman-Islamic tradition and its associated elites to a new secular, Latin-script-based Turkish identity and its proponents, while simultaneously severing intergenerational access to historical texts.
% ABSENT_VOICES: Advocates for Ottoman continuity, religious scholars, and cultural conservatives who valued the Arabic script for its historical, religious, and aesthetic significance were actively silenced, marginalized, and excluded from the decision-making process. Their perspectives were deemed antithetical to the new national project.
% DISAPPEARANCE_RATIONALE: If the script reform and its underlying secular nationalist ideology vanished overnight, there would be a profound re-evaluation of Turkish identity, a resurgence of interest in Ottoman texts and history, and potentially a re-introduction of Arabic script elements into public life. This would fundamentally alter the cultural, educational, and political landscape of Turkey, leading to a significant reorganization of national self-perception and historical memory.
% FOUNDING_PROBLEM: The perceived backwardness, religious conservatism, and multi-ethnic, multi-script nature of the Ottoman Empire, which was seen as an impediment to the rapid modernization, secularization, and national unification of the new Turkish Republic along Western lines.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians, state institutions, and proponents of European alignment corroborate the founding problem as having been live and urgent, emphasizing the need for a radical break with the past. However, cultural conservatives, some religious groups, and advocates for Ottoman continuity argue that the problem was manufactured or exaggerated, and that the reform created more problems (e.g., historical illiteracy) than it solved, citing independent cultural analyses and historical scholarship from outside the benefiting parties.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.85) because the reform imposed a massive cost on the existing literate population, rendering their cultural heritage inaccessible and forcing a complete relearning of written language. Suppression is extremely high (0.90) due to the state's top-down, coercive enforcement, including legal prohibitions on the old script and a rapid, mandatory education campaign. Theater ratio is low (0.10) because the reform was a genuine, functional, and ideologically driven change, not merely performative maintenance of an atrophied function. Accessibility collapse is near total (0.95) as the old script became functionally obsolete for the vast majority of the population. Resistance was significant but ultimately overcome by state power (0.70).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Turkish state and secular elites, the script reform was a necessary and beneficial act of modernization and national liberation, a 'rope' coordinating a new, progressive identity. From the perspective of the Ottoman literate population, religious scholars, and cultural conservatives, it was a 'snare' that violently severed their connection to history, religion, and cultural identity, imposing immense costs and rendering them culturally disenfranchised. The engine's classification as 'tangled_rope' reflects this dual nature: a genuine (albeit coercive) coordination function for the state, coupled with severe extraction from a large segment of the population.
 *
 * DIRECTIONALITY LOGIC:
 *   The Turkish state and secular elites are clear beneficiaries, gaining political legitimacy and cultural dominance. European modernity advocates also benefit ideologically. The Ottoman literate population, religious scholars, and cultural conservatives are direct targets, bearing the costs of lost literacy, cultural rupture, and diminished influence. Ottoman continuity advocates are structurally excluded, their very position foreclosed by the constraint's foundational axioms.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to rapidly modernize and secularize Turkey, was largely achieved in its initial decades. However, the 'founding problem status' is now 'contested,' indicating that while the original problem may be 'dead' in the eyes of some, the constraint persists due to its embeddedness in national identity and institutional inertia. The high extractiveness and suppression, coupled with the contested status of the founding problem, prevent mislabeling this as a simple 'rope' or 'scaffold' (which would imply a temporary, mutually beneficial coordination). Instead, it highlights the ongoing costs of a coercive identity project.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_literacy_loss_impact,
    'What is the long-term, quantifiable impact of the script reform on intergenerational knowledge transfer and access to pre-republican Turkish cultural heritage?',
    'Longitudinal studies tracking literacy rates in Ottoman Turkish, content analysis of historical curricula, and surveys of public engagement with Ottoman-era texts.',
    'If the loss is severe and persistent, it strengthens the ''snare'' aspect by highlighting the irreversible cultural costs. If new mechanisms for accessing Ottoman heritage have emerged, it might slightly reduce the perceived ''accessibility_collapse'' over time, though not the initial rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_literacy_loss_impact, empirical, 'Quantifying the cultural cost of historical illiteracy.').

omega_variable(
    identity_construction_vs_imposition,
    'To what extent did the script reform genuinely foster a new, embraced national identity, versus merely imposing a state-mandated identity through coercive means?',
    'Sociolinguistic studies, analysis of popular culture and literature over time, and public opinion surveys on national identity and historical memory, particularly among younger generations.',
    'If the new identity is widely and genuinely embraced, it would strengthen the ''coordination'' aspect of the tangled rope. If it remains largely a state imposition, it would push the classification closer to a ''snare'' by emphasizing the coercive nature over any genuine collective benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_construction_vs_imposition, conceptual, 'Distinguishing genuine identity shift from state-imposed identity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal prohibitions, educational mandates) or internalized (a collective belief that the Ottoman past is ''backward'' and the Latin script is ''modern'')?',
    'Post-reform attitudinal surveys and analysis of public discourse in periods of political liberalization: if the rejection of the old script persists even when structural barriers are relaxed, it suggests internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target population carries the suppression with them. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural engineering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 1928, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t1928, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(turk_tr_t1948, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(turk_tr_t1968, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1968, 0.07).
narrative_ontology:measurement(turk_tr_t1988, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 1988, 0.08).
narrative_ontology:measurement(turk_tr_t2008, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 2008, 0.09).
narrative_ontology:measurement(turk_tr_t2023, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(turk_be_t1928, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1928, 0.75).
narrative_ontology:measurement(turk_be_t1948, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(turk_be_t1968, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1968, 0.83).
narrative_ontology:measurement(turk_be_t1988, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 1988, 0.85).
narrative_ontology:measurement(turk_be_t2008, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 2008, 0.84).
narrative_ontology:measurement(turk_be_t2023, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t1928, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1928, 0.8).
narrative_ontology:measurement(turk_su_t1948, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(turk_su_t1968, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1968, 0.9).
narrative_ontology:measurement(turk_su_t1988, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 1988, 0.89).
narrative_ontology:measurement(turk_su_t2008, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 2008, 0.87).
narrative_ontology:measurement(turk_su_t2023, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_education_system).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_historical_narrative).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_secularism_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
