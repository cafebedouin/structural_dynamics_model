% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__rupture_reading, []).

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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Turkish Script Reform (Rupture Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Turkish script reform of 1928, replacing the Ottoman Turkish alphabet
 *   (based on Arabic script) with a new Latin-based alphabet, was a
 *   foundational act of the Turkish Republic. This 'rupture reading' frames
 *   the reform as a deliberate and highly extractive constraint designed to
 *   sever the new nation's ties to its Ottoman and Islamic past, thereby
 *   forging a new, secular national identity. It rendered the entire
 *   pre-reform literate population functionally illiterate in official
 *   contexts overnight, creating a profound cultural and historical
 *   discontinuity.
 *
 * KEY AGENTS:
 *   - republican_state_apparatus: Agenda setter (institutional/generational) — enforced the change
 *   - ottoman_literate_population: Primary target (powerless/biographical) — bore the extraction
 *   - islamic_scholars: Payer (moderate/biographical) — lost authority and access to texts
 *   - new_national_identity_proponents: Beneficiary (organized/generational) — gained a new cultural foundation
 *   - traditional_elites: Payer (powerful/biographical) — lost status and influence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.95).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.98).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Turkish Script Reform (Rupture Reading)").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '0e22c4a2-81c2-438b-80bf-018f73912f7d').
narrative_ontology:cs_kernel_codification('0e22c4a2-81c2-438b-80bf-018f73912f7d', formalized).
narrative_ontology:cs_authority_grounding('0e22c4a2-81c2-438b-80bf-018f73912f7d', extraction).
narrative_ontology:cs_interpretation_layer_present('0e22c4a2-81c2-438b-80bf-018f73912f7d').
narrative_ontology:cs_reading_relation('0e22c4a2-81c2-438b-80bf-018f73912f7d', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0e22c4a2-81c2-438b-80bf-018f73912f7d', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('0e22c4a2-81c2-438b-80bf-018f73912f7d', foundational, ottoman_past_is_burden).
narrative_ontology:cs_axiom_status(ottoman_past_is_burden, holdable).
narrative_ontology:cs_axiom_grounding('0e22c4a2-81c2-438b-80bf-018f73912f7d', ottoman_past_is_burden, conventional).
narrative_ontology:cs_axiom('0e22c4a2-81c2-438b-80bf-018f73912f7d', foundational, new_identity_requires_clean_break).
narrative_ontology:cs_axiom_status(new_identity_requires_clean_break, holdable).
narrative_ontology:cs_axiom_grounding('0e22c4a2-81c2-438b-80bf-018f73912f7d', new_identity_requires_clean_break, deontological).
narrative_ontology:cs_reference_frame('0e22c4a2-81c2-438b-80bf-018f73912f7d', ottoman_cultural_hegemony).
narrative_ontology:cs_drift_state('0e22c4a2-81c2-438b-80bf-018f73912f7d', post_reform_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0e22c4a2-81c2-438b-80bf-018f73912f7d', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, republican_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_national_identity_proponents).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, traditional_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, future_generations_of_turks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The newly formed Turkish Republic, led by Mustafa Kemal Atatürk, which conceived, legislated, and rigorously enforced the script reform. Its goal was to create a modern, secular nation-state distinct from the Ottoman Empire.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, republican_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The entire population literate in the Ottoman Arabic script before 1928. They were rendered functionally illiterate in the new official script overnight, losing access to public life, official documents, and their own written heritage.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_literate_population, payer,
    powerless, biographical, trapped, national).

% Custodians of religious and classical Ottoman texts written in Arabic script. The reform severely curtailed their authority, influence, and the accessibility of their knowledge base, as new generations could not read the old texts.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholars, payer,
    moderate, biographical, constrained, national).

% Intellectuals, politicians, and segments of the populace who actively supported the creation of a new, secular, Western-oriented Turkish national identity. The script reform was a key tool in achieving this cultural break.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_national_identity_proponents, beneficiary,
    organized, generational, mobile, national).

% Landowners, bureaucrats, and religious leaders whose power and status were tied to the Ottoman system and its cultural forms. The script reform undermined their social standing and cultural capital.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, traditional_elites, payer,
    powerful, biographical, constrained, national).

% Born into a system where the Latin script is the sole means of literacy, they are effectively severed from direct access to pre-reform history and literature, thus embodying the new national identity without the 'burden' of the old. Their identity is shaped by this rupture.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, future_generations_of_turks, beneficiary,
    powerless, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__rupture_reading, republican_state_apparatus).
narrative_ontology:fixing_cost_class(orthographic_kernel__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the creation of a new, unified national identity by establishing a clear break from the Ottoman past and aligning with Western orthographic standards.
% TRANSFER_FUNCTION: Transfers cultural capital, historical access, and social authority from the Ottoman/Islamic literate population and traditional elites to the republican state apparatus and proponents of the new national identity.
% ABSENT_VOICES: Any organized resistance from the Ottoman literate population was swiftly suppressed. Voices advocating for gradual reform, bilingualism, or preservation of the Arabic script were effectively silenced by state coercion and the rapid implementation of the new law.
% DISAPPEARANCE_RATIONALE: If the script reform had not occurred, or if its effects were reversed, the cultural, political, and educational landscape of Turkey would be fundamentally different. The connection to the Ottoman past would be direct, the national identity would be less secular and Western-oriented, and the state's legitimacy claims would rest on different foundations.
% FOUNDING_PROBLEM: The Ottoman script was seen as a barrier to mass literacy, technological modernization, and the formation of a distinct Turkish national identity, being too complex and too closely associated with the multi-ethnic, religious Ottoman Empire.
% FOUNDING_PROBLEM_CORROBORATION: While the republican state apparatus claimed the problem was live (modernization, literacy), independent historians and sociolinguists (outside the benefiting parties) largely corroborate that the primary problem solved by the 'rupture reading' was the political goal of severing ties with the past, which was achieved. The literacy and modernization problems could have been addressed by less extractive means, making the 'founding problem' as stated by the state largely a cover story for the deeper political rupture.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the reform effectively 'extracted' literacy and access to historical texts from an entire generation, creating a chasm between past and present. Suppression is near total (0.98) due to the immediate and comprehensive legal enforcement, including banning the old script in public and education. Theater ratio is very low (0.05) as the reform was a direct, functional act of state-building with minimal performative pretense; its intent was to achieve a real, if brutal, outcome. Accessibility collapse is near total (0.99) as the old script became unusable in official and public life. Resistance was high (0.85) but ultimately ineffective due to the state's coercive power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the republican state apparatus and proponents of the new national identity, the reform was a necessary, even liberating, act of modernization and nation-building. From the perspective of the Ottoman literate population, Islamic scholars, and traditional elites, it was a devastating act of cultural destruction and disenfranchisement. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The republican state apparatus and new national identity proponents are clear beneficiaries (d near 0.0) as the constraint directly served their nation-building agenda. The Ottoman literate population, Islamic scholars, and traditional elites are clear victims (d near 1.0) as they bore the direct costs of illiteracy, loss of cultural capital, and diminished social standing. The constraint was designed to extract from the old order to benefit the new.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'rupture reading' classifies this as a snare because its primary function was not coordination but the deliberate, coercive extraction of cultural continuity to achieve a political goal. The mandate was to create a new identity by severing the old; this mandate was actively pursued and enforced, not atrophied. The high extractiveness and suppression, coupled with active enforcement, prevent mislabeling it as a rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the Turkish script reform primarily a rupture from the past, a modernization effort, or a continuity of Turkish identity?',
    'Historical analysis of state archives, public discourse, and educational curricula from the reform era, focusing on explicit policy goals and public reception.',
    'If primarily a rupture, the constraint is a snare for cultural continuity. If modernization, it''s a tangled rope for technological adoption. If continuity, it''s a rope for linguistic identity. This story instantiates the ''rupture'' reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the primary intent and effect of the script reform.').

omega_variable(
    suppression_internalization,
    'To what extent has the suppression of the Ottoman script been internalized by subsequent generations, making alternatives unthinkable?',
    'Sociolinguistic studies on script preference, literacy rates in Ottoman Turkish, and attitudes towards historical texts among contemporary Turkish citizens.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the ''trapped'' exit option becomes ''identity_locked'' for many, reinforcing the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism for script change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 1938).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_kernel__rupture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orth_tr_t5, orthographic_kernel__rupture_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(orth_tr_t10, orthographic_kernel__rupture_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_kernel__rupture_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(orth_be_t5, orthographic_kernel__rupture_reading, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(orth_be_t10, orthographic_kernel__rupture_reading, base_extractiveness, 10, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_kernel__rupture_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(orth_su_t5, orthographic_kernel__rupture_reading, suppression_requirement, 5, 0.9).
narrative_ontology:measurement(orth_su_t10, orthographic_kernel__rupture_reading, suppression_requirement, 10, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, turkish_education_system_reform).
narrative_ontology:affects_constraint(orthographic_kernel__rupture_reading, secularization_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'orthographic_kernel' alongside 'continuity_reading' and 'modernization_reading'. Each reading represents a distinct structural claim about the script reform's primary function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
