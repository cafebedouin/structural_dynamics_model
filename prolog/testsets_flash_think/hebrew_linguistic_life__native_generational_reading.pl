% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Hebrew Linguistic Life: Native Generational Reading
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint defines a language as 'alive' only when children acquire
 *   it as a mother tongue and use it for all daily, secular functions. This
 *   specific reading of linguistic life was central to the Hebrew revival
 *   movement, which aimed to transform Hebrew from a liturgical and scholarly
 *   language into a spoken vernacular. The constraint, when adopted and
 *   enforced, led to significant pressure on speakers of other Jewish
 *   languages (like Yiddish and Ladino) to abandon their mother tongues in
 *   favor of Hebrew, creating a victim set and high extraction.
 *
 * KEY AGENTS:
 *   - hebrew_revivalists: Primary agenda_setter (powerful/mobile) — drove the adoption and enforcement of this definition.
 *   - israeli_state: Primary beneficiary (institutional/arbitrage) — adopted and institutionalized the outcome of this definition.
 *   - yiddish_speakers: Primary payer (powerless/identity_locked) — bore the costs of linguistic shift and cultural loss.
 *   - ladino_speakers: Primary payer (powerless/identity_locked) — bore similar costs to Yiddish speakers.
 *   - linguistic_diversity_advocates: Excluded voice (moderate/constrained) — their concerns were marginalized.
 *   - sociolinguists: Analytical observer (analytical/analytical) — study the dynamics of language revival and shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.85).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d').
narrative_ontology:cs_kernel_codification('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', formalized).
narrative_ontology:cs_authority_grounding('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', practice).
narrative_ontology:cs_interpretation_layer_present('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d').
narrative_ontology:cs_reading_relation('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', foundational, native_generational_acquisition_is_sole_criterion).
narrative_ontology:cs_axiom_status(native_generational_acquisition_is_sole_criterion, holdable).
narrative_ontology:cs_axiom_grounding('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', native_generational_acquisition_is_sole_criterion, conventional).
narrative_ontology:cs_axiom('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', foundational, secular_mundane_use_is_essential_for_life).
narrative_ontology:cs_axiom_status(secular_mundane_use_is_essential_for_life, holdable).
narrative_ontology:cs_axiom_grounding('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', secular_mundane_use_is_essential_for_life, conventional).
narrative_ontology:cs_reference_frame('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', pre_revival_dormancy_narrative).
narrative_ontology:cs_drift_state('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', contemporary_multilingualism_advocacy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b70ccfde-1c40-4bf8-a8e5-b6e87d65a20d', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, israeli_state).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, linguistic_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted the definition of Hebrew linguistic life as requiring native, generational acquisition and mundane use. They organized schools, cultural institutions, and social pressure to achieve this vision, viewing it as essential for national identity and cultural renewal.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists, agenda_setter,
    powerful, generational, mobile, national).

% Adopted the revived Hebrew as its official language, benefiting from a unified national tongue and a powerful symbol of sovereignty. Its policies, though not always explicitly coercive, implicitly and explicitly favored Hebrew over other languages.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state, beneficiary,
    institutional, generational, arbitrage, national).

% Faced significant social, cultural, and institutional pressure to abandon Yiddish in favor of Hebrew. For many, this meant losing their mother tongue, cultural heritage, and a sense of belonging, as Yiddish was often stigmatized as a 'diaspora' language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, national).

% Similar to Yiddish speakers, Ladino speakers experienced pressure to shift to Hebrew, leading to the rapid decline of their language and the erosion of their distinct cultural identity within the new national framework.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, national).

% Argue for the intrinsic value of all languages and the importance of multilingualism. Their concerns about the suppression of other Jewish languages were largely marginalized or dismissed by the dominant narrative of Hebrew revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates, excluded,
    moderate, generational, constrained, global).

% Study the processes of language death, revival, and shift, analyzing the social, political, and ideological forces that shape linguistic communities. They often provide critical perspectives on the historical narrative of Hebrew's 'death' and 'revival'.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguists, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a common, living language for a nascent national identity, enabling full social, cultural, and political function for the Jewish people in their homeland.
% TRANSFER_FUNCTION: Transfers linguistic dominance, cultural capital, and national identity from other Jewish vernaculars (Yiddish, Ladino) to revived Hebrew, while transferring the burden of language shift and cultural loss to non-Hebrew speakers.
% ABSENT_VOICES: Speakers of other Jewish languages (Yiddish, Ladino) who were pressured to abandon their mother tongues, and advocates for linguistic diversity who were marginalized by the nationalist imperative. Their perspectives were actively suppressed or ignored in the pursuit of a monolingual national identity.
% DISAPPEARANCE_RATIONALE: If this definition of linguistic life vanished, the historical narrative of Hebrew's 'death' and 'revival' would be fundamentally re-evaluated. The continuous life of Hebrew through liturgical or scholarly use might be re-validated, altering the justification for past linguistic policies and potentially leading to a re-evaluation of the status of other Jewish languages.
% FOUNDING_PROBLEM: The perceived 'death' of Hebrew as a spoken language, leading to a lack of a unifying national language for the Jewish people and a perceived cultural stagnation in the diaspora.
% FOUNDING_PROBLEM_CORROBORATION: Hebrew revivalists and early Zionist leaders attested to the problem, framing Hebrew as 'dead' and in need of revival. Sociolinguists and historians outside the immediate benefiting parties corroborate the *perception* of the problem but contest the *necessity* of the specific solution or the 'dead' status of Hebrew prior to revival, pointing to its continuous liturgical and scholarly use.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the definition demands a complete linguistic shift, imposing significant costs on those whose languages do not fit the criteria. Suppression is very high, reflecting the active social, cultural, and institutional pressures exerted to achieve the desired linguistic state, often at the expense of other languages. Theater ratio is low because the goal was genuine, widespread daily use, not merely performative maintenance. Accessibility collapse is high as alternatives (other languages) were actively discouraged or suppressed. Resistance is moderate, reflecting the historical pushback from communities unwilling to abandon their mother tongues.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hebrew revivalists and the Israeli state, this constraint was a necessary and beneficial act of national self-determination and cultural renewal. From the perspective of Yiddish and Ladino speakers, it was a coercive force that led to the decline of their languages and cultures, imposing significant personal and communal costs. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and the Israeli state are beneficiaries, as they achieved their goal of a living, national Hebrew language. Yiddish and Ladino speakers are victims, as they bore the direct costs of linguistic shift and cultural erosion. Linguistic diversity advocates are excluded, as their perspective was not integrated into the dominant narrative or policy. Sociolinguists are observers, analyzing the process without direct participation in the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hebrew_death_status_ambiguity,
    'Was Hebrew truly ''dead'' between 70-1880 CE, or was it continuously ''alive'' in liturgical, scholarly, and limited inter-communal use?',
    'Re-evaluation of historical linguistic data and definitions of language vitality, considering continuous functional use beyond native generational acquisition.',
    'If Hebrew was continuously ''alive'' in other forms, the premise for ''revival'' (and thus the justification for suppressing other languages) weakens, reclassifying the constraint as more purely extractive (Snare) rather than a coordination for revival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hebrew_death_status_ambiguity, conceptual, 'Ambiguity regarding the historical status of Hebrew before the revival movement.').

omega_variable(
    linguistic_diversity_suppression_necessity,
    'Was the suppression of other Jewish languages (Yiddish, Ladino) a necessary cost for the successful revival of Hebrew as a national language, or an independent act of linguistic nationalism?',
    'Comparative studies of other language revival movements that achieved success without actively suppressing co-existing languages, or counterfactual historical analysis.',
    'If suppression was not necessary, the constraint''s extractiveness and suppression metrics are amplified, and its coordination function is diminished, pushing it closer to a Snare. If deemed necessary, the ''coordination'' aspect is strengthened, albeit with high costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_diversity_suppression_necessity, empirical, 'Whether the suppression of other languages was an unavoidable consequence or an independent policy choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(hebr_tr_t2020, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.55).
narrative_ontology:measurement(hebr_be_t1940, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1940, 0.7).
narrative_ontology:measurement(hebr_be_t1970, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(hebr_be_t2000, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(hebr_be_t2020, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.5).
narrative_ontology:measurement(hebr_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.65).
narrative_ontology:measurement(hebr_su_t1940, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1940, 0.8).
narrative_ontology:measurement(hebr_su_t1970, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1970, 0.83).
narrative_ontology:measurement(hebr_su_t2000, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(hebr_su_t2020, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel, defining linguistic vitality through native, generational acquisition and mundane use. It stands in contrast to liturgical and marketplace-pidgin readings, which offer alternative criteria for a language's 'life'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
