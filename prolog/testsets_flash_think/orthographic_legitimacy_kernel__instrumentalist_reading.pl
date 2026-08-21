% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Orthographic Legitimacy: Instrumentalist Reading
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'instrumentalist' reading of orthographic
 *   legitimacy, where the choice and enforcement of a writing system are
 *   justified primarily by their utility in maximizing literacy rates and
 *   administrative efficiency. This reading views orthography as a pragmatic
 *   tool for state-building and social development, rather than an intrinsic
 *   cultural or religious marker. It is one reading of the broader
 *   'orthographic_legitimacy_kernel', which also includes 'modernist_reading'
 *   and 'continuity_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.55).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Orthographic Legitimacy: Instrumentalist Reading").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '84312c1e-651d-49bf-83d8-ab2caf6825f6').
narrative_ontology:cs_kernel_codification('84312c1e-651d-49bf-83d8-ab2caf6825f6', formalized).
narrative_ontology:cs_authority_grounding('84312c1e-651d-49bf-83d8-ab2caf6825f6', practice).
narrative_ontology:cs_interpretation_layer_present('84312c1e-651d-49bf-83d8-ab2caf6825f6').
narrative_ontology:cs_reading_relation('84312c1e-651d-49bf-83d8-ab2caf6825f6', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_reading_relation('84312c1e-651d-49bf-83d8-ab2caf6825f6', orthographic_legitimacy_kernel__continuity_reading, forecloses).
narrative_ontology:cs_axiom('84312c1e-651d-49bf-83d8-ab2caf6825f6', foundational, orthography_is_a_tool_for_social_engineering).
narrative_ontology:cs_axiom_status(orthography_is_a_tool_for_social_engineering, holdable).
narrative_ontology:cs_axiom_grounding('84312c1e-651d-49bf-83d8-ab2caf6825f6', orthography_is_a_tool_for_social_engineering, empirically_contingent).
narrative_ontology:cs_axiom('84312c1e-651d-49bf-83d8-ab2caf6825f6', foundational, state_legitimacy_tied_to_public_utility).
narrative_ontology:cs_axiom_status(state_legitimacy_tied_to_public_utility, holdable).
narrative_ontology:cs_axiom_grounding('84312c1e-651d-49bf-83d8-ab2caf6825f6', state_legitimacy_tied_to_public_utility, instrumental).
narrative_ontology:cs_reference_frame('84312c1e-651d-49bf-83d8-ab2caf6825f6', mass_literacy_and_efficient_administration).
narrative_ontology:cs_drift_state('84312c1e-651d-49bf-83d8-ab2caf6825f6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('84312c1e-651d-49bf-83d8-ab2caf6825f6', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, state_reformers).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Government officials and policymakers who advocate for and implement orthographic reforms, believing they will lead to higher literacy rates, more efficient administration, and stronger national cohesion. They benefit from the perceived success and legitimacy these reforms bring to the state.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_reformers, agenda_setter,
    institutional, generational, constrained, national).

% Individuals, often from rural or marginalized communities, who gain access to literacy and administrative services through the adoption of a simplified or standardized orthography. Their social mobility and participation in the modern state are enhanced.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    powerless, biographical, constrained, local).

% Traditional scholars, religious leaders, and bureaucrats whose social status, professional careers, and cultural authority are tied to mastery of the older, often more complex, script (e.g., Arabic script). The orthographic reform devalues their specialized knowledge and creates a barrier to their continued influence.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, biographical, identity_locked, national).

% Custodians of religious texts and traditions, often written in older scripts. They view orthographic reform as a rupture with sacred heritage and a threat to religious continuity, but their voices are often marginalized in the state-led reform process.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_religious_scholars, excluded,
    organized, generational, identity_locked, national).

% Academics and consultants who study language, literacy, and orthography. They provide technical advice on script reform, measure literacy rates, and analyze administrative efficiency, often influencing the arguments made by state reformers.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, linguistic_experts, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, state_reformers).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__instrumentalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To standardize written communication across a diverse population, enabling mass literacy campaigns and streamlining state administration and record-keeping.
% TRANSFER_FUNCTION: Transfers social and administrative power from an elite whose status is tied to mastery of complex, traditional scripts to a broader population capable of using a simplified, standardized orthography. It also transfers resources towards state-led education and away from traditional learning institutions.
% ABSENT_VOICES: Traditional religious scholars and cultural preservationists, who would argue for the intrinsic value of historical scripts and the continuity of tradition over purely pragmatic concerns. Their exclusion allows the instrumentalist narrative to dominate policy.
% DISAPPEARANCE_RATIONALE: If the instrumentalist drive for orthographic legitimacy vanished, state-led reforms would likely halt or reverse. This would lead to fragmented literacy, reduced administrative efficiency, and a potential resurgence of traditional scripts, fundamentally reorganizing the relationship between language, state, and society.
% FOUNDING_PROBLEM: Widespread illiteracy, administrative inefficiencies due to diverse and complex writing systems, and a perceived lack of national unity stemming from linguistic fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Educational statistics, economic productivity reports, and analyses from international development organizations consistently highlight the ongoing challenges of low literacy and administrative bottlenecks in many developing nations, corroborating the instrumentalist framing of the problem from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the genuine coordination benefits of mass literacy and efficient administration, but also the costs imposed on those whose existing linguistic capital is devalued. Suppression (0.55) is present as orthographic reforms often require active state enforcement, including educational mandates and legal changes, to overcome resistance from traditionalists. Theater ratio (0.15) is low because the reforms are genuinely aimed at practical outcomes, with little performative maintenance. Resistance (0.60) is significant, primarily from the Arabic-literate elite who face a loss of status and influence.
 *
 * PERSPECTIVAL GAP:
 *   State reformers and the newly literate population experience this as a beneficial 'rope' that facilitates progress and access. In contrast, the Arabic-literate elite experience it as a 'snare' or 'tangled_rope', as their skills are devalued and their identity is challenged by the imposed changes, with limited exit options. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State reformers and the newly literate population are beneficiaries, gaining efficiency and social mobility, respectively. The Arabic-literate elite are victims, bearing the costs of devalued skills and cultural disruption. The constraint subsidizes the state's goals and the new literates, while extracting from the traditional elite.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_motivation_ambiguity,
    'Is the orthographic reform genuinely driven by instrumentalist goals (literacy, efficiency), or are these a cover for deeper political or cultural reorientation (e.g., secularization, national identity construction)?',
    'Analysis of state archives, policy documents, and public discourse for explicit and implicit motivations, particularly examining the rhetoric used to justify reforms versus the actual outcomes and secondary effects.',
    'If primarily a cover, the constraint''s effective extractiveness and suppression would be higher, as the coordination story is less genuine, potentially reclassifying it closer to a ''snare'' for the affected elite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_motivation_ambiguity, conceptual, 'Distinguishing genuine instrumentalism from instrumentalist rhetoric masking other agendas.').

omega_variable(
    long_term_literacy_impact,
    'What is the actual long-term impact of the orthographic reform on overall literacy rates and administrative efficiency, accounting for initial disruption and potential resistance?',
    'Longitudinal studies comparing literacy rates, educational attainment, and administrative performance in reforming regions versus control regions, over several decades.',
    'If long-term benefits are marginal or negative, the instrumentalist justification weakens, increasing the perceived extraction from those who bore the costs of the transition, potentially shifting the classification towards ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_literacy_impact, empirical, 'Assessing the actual efficacy of orthographic reform in achieving its stated instrumental goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(orth_tr_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1935, 0.12).
narrative_ontology:measurement(orth_tr_t1942, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1942, 0.14).
narrative_ontology:measurement(orth_tr_t1949, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(orth_tr_t1956, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1956, 0.15).
narrative_ontology:measurement(orth_tr_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 1960, 0.15).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1928, 0.35).
narrative_ontology:measurement(orth_be_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1935, 0.4).
narrative_ontology:measurement(orth_be_t1942, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1942, 0.43).
narrative_ontology:measurement(orth_be_t1949, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1949, 0.44).
narrative_ontology:measurement(orth_be_t1956, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1956, 0.45).
narrative_ontology:measurement(orth_be_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 1960, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1928, 0.45).
narrative_ontology:measurement(orth_su_t1935, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1935, 0.5).
narrative_ontology:measurement(orth_su_t1942, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1942, 0.53).
narrative_ontology:measurement(orth_su_t1949, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1949, 0.54).
narrative_ontology:measurement(orth_su_t1956, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1956, 0.55).
narrative_ontology:measurement(orth_su_t1960, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 1960, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
