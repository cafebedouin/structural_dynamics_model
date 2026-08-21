% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__modernist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__modernist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__modernist_reading
 *   human_readable: Orthographic Legitimacy: Modernist Reading (Rupture from Ottoman Past)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'modernist reading' of orthographic
 *   legitimacy, where the adoption of a new script (e.g., Latin alphabet in
 *   Turkey) is seen as a necessary rupture from an Ottoman/Islamic past to
 *   align with Western modernity and forge a new national identity. It is a
 *   state-driven project that actively suppresses the old script and its
 *   associated cultural forms, rendering traditional elites functionally
 *   illiterate and transferring cultural and political power to a new secular
 *   elite. The claimed type is Tangled Rope, reflecting the state's narrative
 *   of modernization and national coordination, while the metrics describe a
 *   highly extractive and suppressive reality for the affected populations.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: Primary agenda_setter (institutional/arbitrage) — enforces the new script, benefits from national identity shift.
 *   - secular_elites: Primary beneficiary (powerful/mobile) — gain social and political capital.
 *   - ottoman_literate_class: Primary payer (powerless/identity_locked) — rendered illiterate, lose status and access.
 *   - religious_scholars: Primary payer (powerless/identity_locked) — lose authority and cultural influence.
 *   - traditional_elites: Secondary payer (moderate/constrained) — lose influence and power base.
 *   - western_powers: Observer (institutional/analytical) — encourage modernization efforts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, 0.85).
domain_priors:suppression_score(orthographic_legitimacy_kernel__modernist_reading, 0.9).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__modernist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__modernist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__modernist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__modernist_reading, "Orthographic Legitimacy: Modernist Reading (Rupture from Ottoman Past)").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__modernist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__modernist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__modernist_reading, '957d7456-5dd9-4368-ac6c-7dfdb60b727e').
narrative_ontology:cs_kernel_codification('957d7456-5dd9-4368-ac6c-7dfdb60b727e', formalized).
narrative_ontology:cs_authority_grounding('957d7456-5dd9-4368-ac6c-7dfdb60b727e', extraction).
narrative_ontology:cs_interpretation_layer_present('957d7456-5dd9-4368-ac6c-7dfdb60b727e').
narrative_ontology:cs_reading_relation('957d7456-5dd9-4368-ac6c-7dfdb60b727e', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('957d7456-5dd9-4368-ac6c-7dfdb60b727e', orthographic_legitimacy_kernel__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('957d7456-5dd9-4368-ac6c-7dfdb60b727e', foundational, western_alignment_is_progress).
narrative_ontology:cs_axiom_status(western_alignment_is_progress, holdable).
narrative_ontology:cs_axiom_grounding('957d7456-5dd9-4368-ac6c-7dfdb60b727e', western_alignment_is_progress, conventional).
narrative_ontology:cs_axiom('957d7456-5dd9-4368-ac6c-7dfdb60b727e', foundational, ottoman_past_is_stagnation).
narrative_ontology:cs_axiom_status(ottoman_past_is_stagnation, holdable).
narrative_ontology:cs_axiom_grounding('957d7456-5dd9-4368-ac6c-7dfdb60b727e', ottoman_past_is_stagnation, conventional).
narrative_ontology:cs_reference_frame('957d7456-5dd9-4368-ac6c-7dfdb60b727e', secular_republican_ideal).
narrative_ontology:cs_drift_state('957d7456-5dd9-4368-ac6c-7dfdb60b727e', contemporary_postcolonial_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('957d7456-5dd9-4368-ac6c-7dfdb60b727e', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__modernist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__modernist_reading, secular_elites).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, religious_scholars).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__modernist_reading, traditional_elites).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, westernization_ideology).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, national_identity_construction).
narrative_ontology:constraint_vindicates(orthographic_legitimacy_kernel__modernist_reading, secularism_as_progress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces the new orthography, benefiting from the consolidation of a new national identity and the weakening of traditional power structures. It frames the change as essential for national progress and alignment with Western civilization.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain social, political, and economic capital by aligning with the new script and the state's modernist ideology. They become the new cultural gatekeepers and interpreters of national identity.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, secular_elites, beneficiary,
    powerful, biographical, mobile, national).

% Rendered functionally illiterate overnight, losing access to historical texts, social status, and professional opportunities tied to the old script. Their identity is deeply intertwined with the Ottoman cultural legacy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, ottoman_literate_class, payer,
    powerless, biographical, identity_locked, national).

% Lose their authority, their ability to read and interpret religious texts in their original form, and their cultural influence. The script change is a direct assault on their institutional power and the religious foundations of the old order.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, religious_scholars, payer,
    powerless, generational, identity_locked, national).

% Experience a significant erosion of their influence and power base, which was often tied to the old script, traditional education, and Ottoman institutions. They face pressure to adapt or become marginalized.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, traditional_elites, payer,
    moderate, biographical, constrained, national).

% Observe and often tacitly or explicitly encourage such modernization efforts, viewing them as a sign of alignment with European norms and a break from perceived 'backwardness.' They do not directly participate but their ideological influence is significant.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__modernist_reading, western_powers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__modernist_reading, modernizing_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__modernist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify the nation under a new, secular, and modern identity, facilitating communication with Western powers and streamlining state administration by adopting a Latin-based script.
% TRANSFER_FUNCTION: Transfers cultural capital, political power, and access to knowledge from traditional, Ottoman-aligned groups to the modernizing state apparatus and secular elites. It also transfers the burden of re-literacy onto the populace.
% ABSENT_VOICES: The vast majority of the population, particularly rural and less educated segments, who were largely excluded from the decision-making process but bore the immediate costs of illiteracy and cultural dislocation. Their voices would highlight the forced nature of the change and the loss of cultural heritage.
% DISAPPEARANCE_RATIONALE: If the modernist orthographic legitimacy and its enforcement vanished overnight, the national identity project would be fundamentally undermined. Traditional cultural forms and historical connections to the Ottoman past would likely reassert themselves, challenging the state's authority and leading to a profound reorganization of cultural and political life.
% FOUNDING_PROBLEM: To break from a perceived stagnant and religiously-dominated Ottoman past, align the new nation with European modernity, and forge a new, secular national identity distinct from its imperial predecessor.
% FOUNDING_PROBLEM_CORROBORATION: The modernizing state and its proponents attest that the Ottoman past was indeed stagnant and that the rupture was a necessary step for national progress. However, historians, cultural critics, and descendants of the affected traditional classes, from outside the benefiting parties, corroborate the problem of perceived stagnation but contest the necessity and methods of the rupture, highlighting the cultural loss, forced assimilation, and the imposition of a specific ideological vision.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__modernist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__modernist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__modernist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__modernist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__modernist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_legitimacy_kernel__modernist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_legitimacy_kernel__modernist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the script change imposes immense costs on traditional groups, forcing them to abandon their cultural heritage and re-learn literacy, while the state and secular elites consolidate power. Suppression is very high (0.90) due to active state enforcement, legal prohibitions on the old script, and the rapid overhaul of the education system. Theater ratio is low (0.10) because the script reform was a genuinely transformative and functional project for the modernizing state, not merely performative. Accessibility collapse is near total (0.95) for the old script, as it was removed from public life and education. Resistance is high (0.70) from those whose identities and livelihoods were tied to the old system, though often suppressed by state power.
 *
 * PERSPECTIVAL GAP:
 *   The modernizing state and secular elites perceive this constraint as a necessary and beneficial act of national coordination and progress, leading to a 'Rope' or 'Scaffold' classification from their seat. In contrast, the Ottoman literate class, religious scholars, and traditional elites experience it as a 'Snare' – a coercive act that extracts their cultural capital, renders them powerless, and suppresses their way of life. The engine's computation of per-seat types will reflect this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing state apparatus and secular elites are clear beneficiaries (low d) as they gain power, legitimacy, and cultural dominance. The Ottoman literate class, religious scholars, and traditional elites are targets (high d) as they bear the direct costs of illiteracy, loss of status, and cultural rupture. Their 'identity_locked' exit option further amplifies their directionality towards the target end, as their very self-concept is tied to the old script and tradition, making exit (i.e., embracing the new script) a form of self-abnegation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the orthographic reform as pure coordination (Rope) by highlighting the severe extraction and suppression involved. Conversely, it avoids labeling it as pure extraction (Snare) by acknowledging the genuine, albeit ideologically driven, coordination function of forging a new national identity. The 'Tangled Rope' classification captures this hybrid nature, where a coordination goal is achieved through highly asymmetric and coercive means.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modernity_definition_ambiguity,
    'Is ''Western/European modernity'' an objective, universal standard for national development, or a culturally specific construct whose adoption entails specific ideological and cultural costs?',
    'Comparative historical analysis of diverse modernization paths that did not involve such radical cultural ruptures, assessing their long-term outcomes and social costs.',
    'If modernity is a culturally specific construct, the ''modernist reading'' becomes a preference-driven choice with identifiable victims, rather than an inevitable path, potentially reclassifying the coordination function as a cover for ideological imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernity_definition_ambiguity, conceptual, 'Ambiguity in the definition and universality of ''modernity'' as a guiding principle for orthographic reform.').

omega_variable(
    identity_rupture_necessity,
    'Was a complete rupture from the Ottoman/Islamic past truly necessary for national identity formation, or could a more gradual evolution have achieved similar goals with less social and cultural cost?',
    'Counterfactual historical analysis and sociological studies of national identity formation in other contexts, evaluating the efficacy and necessity of such radical breaks.',
    'If the rupture was not strictly necessary, the high extraction and suppression become less justifiable as ''coordination costs,'' strengthening the Snare-like aspects of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_rupture_necessity, empirical, 'Whether the radical rupture from the past was a necessary condition for the desired national identity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal enforcement, educational system overhaul) or internalized (social pressure to conform to the new modern identity, self-censorship)?',
    'Longitudinal sociological studies tracking the persistence of old script knowledge and cultural practices in private spheres, and the psychological impact of the reform on individuals and communities over generations.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them even in the absence of overt enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in orthographic reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__modernist_reading, 1928, 1958).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1928, 0.15).
narrative_ontology:measurement(orth_tr_t1934, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1934, 0.12).
narrative_ontology:measurement(orth_tr_t1940, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(orth_tr_t1946, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1946, 0.1).
narrative_ontology:measurement(orth_tr_t1952, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1952, 0.1).
narrative_ontology:measurement(orth_tr_t1958, orthographic_legitimacy_kernel__modernist_reading, theater_ratio, 1958, 0.1).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1928, 0.75).
narrative_ontology:measurement(orth_be_t1934, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1934, 0.8).
narrative_ontology:measurement(orth_be_t1940, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1940, 0.83).
narrative_ontology:measurement(orth_be_t1946, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1946, 0.84).
narrative_ontology:measurement(orth_be_t1952, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1952, 0.85).
narrative_ontology:measurement(orth_be_t1958, orthographic_legitimacy_kernel__modernist_reading, base_extractiveness, 1958, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1928, 0.8).
narrative_ontology:measurement(orth_su_t1934, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1934, 0.85).
narrative_ontology:measurement(orth_su_t1940, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1940, 0.88).
narrative_ontology:measurement(orth_su_t1946, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1946, 0.9).
narrative_ontology:measurement(orth_su_t1952, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1952, 0.9).
narrative_ontology:measurement(orth_su_t1958, orthographic_legitimacy_kernel__modernist_reading, suppression_requirement, 1958, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__modernist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
