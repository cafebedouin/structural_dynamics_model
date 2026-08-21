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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   domain: Sociolinguistics/Religious Studies/Nationalism Studies
 *
 * SUMMARY:
 *   This constraint defines linguistic life exclusively through native,
 *   generational acquisition and use in all daily functions, including
 *   secular speech. It is a specific reading of the broader
 *   'hebrew_linguistic_life' kernel. This reading asserts that Hebrew was
 *   'dead' from 70 CE to 1880 CE, necessitating a revival, and implicitly
 *   positions other Jewish languages as 'not alive' in the desired sense. The
 *   high extractiveness and suppression reflect the intense social and
 *   political pressure applied during the Hebrew revival to abandon other
 *   Jewish languages in favor of modern Hebrew.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.8).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.75).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, snare).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "Sociolinguistics/Religious Studies/Nationalism Studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'f49978c4-9a09-4cf9-a728-dec9ef1e3e22').
narrative_ontology:cs_kernel_codification('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', implicit).
narrative_ontology:cs_authority_grounding('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', practice).
narrative_ontology:cs_reading_relation('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', foundational, native_acquisition_is_life).
narrative_ontology:cs_axiom_status(native_acquisition_is_life, holdable).
narrative_ontology:cs_axiom_grounding('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', native_acquisition_is_life, empirically_contingent).
narrative_ontology:cs_axiom('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', foundational, secular_mundane_use_is_life).
narrative_ontology:cs_axiom_status(secular_mundane_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', secular_mundane_use_is_life, empirically_contingent).
narrative_ontology:cs_reference_frame('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', native_generational_acquisition).
narrative_ontology:cs_drift_state('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', post_diaspora_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f49978c4-9a09-4cf9-a728-dec9ef1e3e22', '').
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

% Actively promoted the idea that Hebrew was 'dead' and needed to be revived as a spoken, native language for all daily functions, often at the expense of other Jewish languages. They set the ideological agenda for linguistic policy.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalists, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefited from the establishment of a unified national language, which this reading of linguistic life legitimized. It provided institutional support and educational policies to enforce Hebrew's dominance.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state, beneficiary,
    institutional, generational, arbitrage, national).

% Faced immense social, cultural, and political pressure to abandon Yiddish, their mother tongue, in favor of Hebrew. Their language was often deemed 'not alive' or a relic of the diaspora, leading to cultural loss and intergenerational linguistic breaks.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    powerless, biographical, identity_locked, local).

% Similar to Yiddish speakers, they experienced pressure to cease using Ladino, their traditional language, as it did not fit the 'native generational' criteria for linguistic life. This resulted in the decline of Ladino in many communities.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    powerless, biographical, identity_locked, local).

% Observe and analyze the impact of this exclusive definition of linguistic life on the broader landscape of Jewish languages and global linguistic diversity, often highlighting the coercive aspects of language revival movements.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates, observer,
    analytical, generational, analytical, global).

% Their understanding of Hebrew's continuous life through sacred texts and study was dismissed by the native generational reading. While they maintained their practices, their perspective was marginalized in the dominant discourse of revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, liturgical_scholars, excluded,
    powerful, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a clear, unambiguous criterion for what constitutes a 'living' language, enabling collective action towards its revitalization and use as a national language.
% TRANSFER_FUNCTION: Transfers linguistic legitimacy, cultural capital, and social status from other Jewish languages (Yiddish, Ladino) to modern Hebrew, while imposing social and cultural costs on speakers of those languages to abandon them.
% ABSENT_VOICES: Speakers of other Jewish languages (Yiddish, Ladino) whose linguistic vitality was denied by this definition; they were often marginalized or coerced into silence in the public discourse of the Hebrew revival, and their languages were actively suppressed in educational and public spheres.
% DISAPPEARANCE_RATIONALE: If this definition of linguistic life vanished, the historical narrative of Hebrew's 'death' and 'revival' would be fundamentally challenged. This would potentially re-legitimize other Jewish languages, alter the linguistic landscape of Israel and the Jewish diaspora, and require a re-evaluation of language policy and cultural identity.
% FOUNDING_PROBLEM: The perceived 'death' of Hebrew as a spoken language after 70 CE, leading to a desire for its revitalization as a national language for a nascent Jewish state.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., early Zionist ideologues, some linguists) cite historical linguistic analysis focusing on the absence of native acquisition and daily secular use. Critics (e.g., Yiddishists, some religious scholars, independent sociolinguists) offer counter-narratives of continuous linguistic life through liturgical, scholarly, or inter-communal use, highlighting the political nature of such definitions.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because this definition imposes a significant cost on speakers of other Jewish languages, demanding their abandonment for Hebrew to be considered 'alive.' Suppression is also high due to the active social, educational, and political campaigns to enforce Hebrew's dominance and marginalize other languages. The theater ratio is low because the definition is quite direct and functional; there's little performative maintenance of a 'dead' language under this strict criterion. Resistance is high from those who valued and continued to speak Yiddish and Ladino.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading view it as a necessary and natural criterion for a language's vitality, leading to a successful national revival. Victims, however, experience it as a coercive force that extracts their linguistic heritage and identity. The engine's classification as a Snare reflects this structural asymmetry, despite the claimed coordination function of linguistic unity.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revivalists and the Israeli state are beneficiaries, as this reading legitimizes their project of national linguistic unity. Speakers of Yiddish and Ladino are victims, as their languages are devalued and suppressed. Linguistic diversity advocates are observers, analyzing the broader impact. Liturgical scholars are excluded, as their definition of Hebrew's life is dismissed by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as the ''native_generational_reading'' of the ''hebrew_linguistic_life'' kernel?',
    'Analysis of primary source texts and historical linguistic debates to confirm the specific criteria for linguistic life articulated by proponents of this view.',
    'If misidentified, the entire kernel analysis for ''hebrew_linguistic_life'' would be compromised, leading to incorrect relationships between sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    historical_linguistic_status_ambiguity,
    'Was Hebrew truly ''dead'' from 70 CE to 1880 CE, or was it continuously ''alive'' in other forms (e.g., liturgical, scholarly, inter-communal) that this reading dismisses?',
    'Comprehensive historical sociolinguistic research examining all forms of Hebrew use during the dormancy period, not just native acquisition and secular speech.',
    'If Hebrew was continuously alive in other forms, the ''revival'' narrative loses its foundational premise, reclassifying the constraint''s justification from a necessary coordination to a purely extractive ideological project.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_linguistic_status_ambiguity, empirical, 'Challenges the premise of Hebrew''s ''death'' prior to revival.').

omega_variable(
    linguistic_diversity_cost_benefit,
    'Does the benefit of a unified national language (modern Hebrew) outweigh the cost of linguistic diversity (the decline and suppression of Yiddish, Ladino, and other Jewish languages)?',
    'A preference-based assessment by affected communities and a comprehensive cultural and economic analysis of the long-term impacts on all linguistic groups.',
    'If the costs are deemed to outweigh the benefits, the constraint''s extractive nature is further highlighted, potentially leading to policy recommendations for linguistic pluralism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(linguistic_diversity_cost_benefit, preference, 'Evaluates the trade-off between linguistic unity and diversity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent was the suppression of other Jewish languages structural (state policy, educational mandates) versus internalized (ideological pressure, social stigma)?',
    'Sociological studies examining individual language choices and community-level linguistic shifts in response to both formal policies and informal social pressures during the revival period.',
    'If suppression was largely internalized, the constraint''s effective suppression is higher than structural measures suggest, as the pressure persists even without overt enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for other Jewish languages.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(hebr_tr_t1895, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1895, 0.07).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.08).
narrative_ontology:measurement(hebr_tr_t1925, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1925, 0.09).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1940, 0.09).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.6).
narrative_ontology:measurement(hebr_be_t1895, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1895, 0.65).
narrative_ontology:measurement(hebr_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.7).
narrative_ontology:measurement(hebr_be_t1925, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1925, 0.75).
narrative_ontology:measurement(hebr_be_t1940, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1940, 0.78).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1950, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.6).
narrative_ontology:measurement(hebr_su_t1895, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1895, 0.65).
narrative_ontology:measurement(hebr_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement(hebr_su_t1925, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1925, 0.72).
narrative_ontology:measurement(hebr_su_t1940, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1940, 0.74).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1950, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_linguistic_life' kernel, each offering a distinct definition of what constitutes a 'living' language. This reading focuses on native, generational acquisition and daily secular use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
