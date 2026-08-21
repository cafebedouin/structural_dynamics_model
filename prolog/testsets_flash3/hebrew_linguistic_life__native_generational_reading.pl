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
 *   This constraint represents the 'native generational' reading of Hebrew's
 *   linguistic life, which posits that a language is truly alive only when
 *   acquired as a mother tongue and used for all daily, secular functions.
 *   This reading asserts that Hebrew was 'dead' for centuries (70-1880 CE)
 *   and required a deliberate, often coercive, revival. The high
 *   extractiveness and suppression reflect the active efforts to displace
 *   other Jewish vernaculars (Yiddish, Ladino, Judeo-Arabic) in favor of
 *   modern Hebrew, which was seen as essential for the Zionist nationalist
 *   project. The victim set includes speakers of these languages, who were
 *   pressured to abandon their mother tongues, and advocates for linguistic
 *   diversity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.85).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.9).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, snare).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Linguistic Life: Native Generational Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '69667e22-ed54-49e8-84f1-3406bda38891').
narrative_ontology:cs_kernel_codification('69667e22-ed54-49e8-84f1-3406bda38891', implicit).
narrative_ontology:cs_authority_grounding('69667e22-ed54-49e8-84f1-3406bda38891', extraction).
narrative_ontology:cs_interpretation_layer_present('69667e22-ed54-49e8-84f1-3406bda38891').
narrative_ontology:cs_reading_relation('69667e22-ed54-49e8-84f1-3406bda38891', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('69667e22-ed54-49e8-84f1-3406bda38891', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('69667e22-ed54-49e8-84f1-3406bda38891', foundational, language_is_living_only_if_native_tongue).
narrative_ontology:cs_axiom_status(language_is_living_only_if_native_tongue, holdable).
narrative_ontology:cs_axiom_grounding('69667e22-ed54-49e8-84f1-3406bda38891', language_is_living_only_if_native_tongue, conventional).
narrative_ontology:cs_axiom('69667e22-ed54-49e8-84f1-3406bda38891', foundational, secular_use_is_criterion_for_life).
narrative_ontology:cs_axiom_status(secular_use_is_criterion_for_life, holdable).
narrative_ontology:cs_axiom_grounding('69667e22-ed54-49e8-84f1-3406bda38891', secular_use_is_criterion_for_life, conventional).
narrative_ontology:cs_reference_frame('69667e22-ed54-49e8-84f1-3406bda38891', modern_national_language_paradigm).
narrative_ontology:cs_drift_state('69667e22-ed54-49e8-84f1-3406bda38891', contemporary_multicultural_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('69667e22-ed54-49e8-84f1-3406bda38891', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement_leaders).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_nationalist_project).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pioneered and enforced the vision of Hebrew as a living, spoken language for all daily functions, including secular speech. They actively promoted its use and discouraged other Jewish vernaculars, seeing them as obstacles to national rebirth. Their identity is fused with the success of the revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement_leaders, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from the establishment of Hebrew as the exclusive national language, which serves as a unifying force and a symbol of sovereignty. The linguistic revival is integral to its ideological foundation and political legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_nationalist_project, beneficiary,
    institutional, civilizational, arbitrage, national).

% Were pressured, often coercively, to abandon Yiddish in favor of Hebrew. Their cultural heritage and primary means of communication were devalued and suppressed, leading to significant linguistic and cultural loss. Exit meant abandoning their community or facing social ostracism.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    powerless, biographical, trapped, regional).

% Experienced similar pressures to abandon Ladino, their traditional Judeo-Spanish language, in favor of Hebrew. Their linguistic identity was marginalized, and the intergenerational transmission of Ladino was severely disrupted.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    powerless, biographical, trapped, regional).

% Migrated to Israel speaking various Judeo-Arabic dialects and faced strong institutional pressure to adopt Hebrew, leading to the rapid decline of their native languages and cultural alienation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jews, payer,
    powerless, biographical, trapped, regional).

% Argue for the value of multilingualism and the preservation of endangered Jewish languages. Their perspectives were largely ignored or actively suppressed during the Hebrew revival, as the dominant narrative prioritized linguistic unity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_diversity_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze the historical processes and social consequences of language revival, including the mechanisms of suppression and the impact on linguistic diversity. They provide an external, critical perspective on the constraint's operation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate a national identity and cultural cohesion around a single, revived language, providing a common medium for all social and political functions in a new state.
% TRANSFER_FUNCTION: Transfers linguistic dominance and cultural capital from diverse Jewish vernaculars (Yiddish, Ladino, Judeo-Arabic) to modern Hebrew, consolidating power and identity for the Zionist project.
% ABSENT_VOICES: Speakers of other Jewish languages, particularly Yiddish and Ladino, were actively marginalized and silenced. They would have argued for linguistic pluralism and the right to maintain their mother tongues, but their voices were suppressed in favor of the Hebrew-only agenda.
% DISAPPEARANCE_RATIONALE: If this constraint (the native generational reading of Hebrew's life) vanished, the historical narrative of Hebrew's 'death' and 'revival' would be fundamentally challenged. The legitimacy of the linguistic policies that suppressed other Jewish languages would be undermined, potentially leading to a re-evaluation of cultural heritage and national identity in Israel. The current linguistic landscape would be seen as a product of coercive choices, not natural evolution.
% FOUNDING_PROBLEM: The perceived lack of a common, modern, secular language for the Jewish people, seen as essential for national self-determination and the creation of a unified national culture in Palestine/Israel.
% FOUNDING_PROBLEM_CORROBORATION: The Hebrew revival movement leaders and the Zionist nationalist project attest that the problem of national unity and cultural cohesion around a common language remains live. Sociolinguists and linguistic diversity advocates, while acknowledging the historical context, corroborate the existence of the problem but contest the necessity and methods of its 'solution,' highlighting the costs to other languages.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.85) is high because the constraint demanded the abandonment of existing linguistic capital and cultural identity from large populations. Suppression (0.90) is very high, reflecting the institutional and social pressures, and sometimes outright coercion, used to enforce Hebrew-only policies in education, public life, and even private homes. Theater ratio is low (0.10) because the revival was a genuine, functional project, not merely performative; the goal was to create a living language, and the enforcement was direct and effective. Accessibility collapse (0.75) is high because alternatives (other Jewish languages) were actively devalued and their use discouraged, making it difficult for individuals to maintain them. Resistance (0.70) was substantial, particularly from Yiddish speakers, but was largely overcome by the institutional power of the revival movement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Hebrew revival movement leaders and the Zionist nationalist project, this constraint was a necessary 'rope' for national rebirth and cultural unity. From the perspective of Yiddish, Ladino, and Arabic-speaking Jews, it was a 'snare' that extracted their linguistic heritage and imposed a new identity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hebrew revival movement leaders and the Zionist nationalist project are clear beneficiaries and agenda-setters, as the constraint directly served their ideological and political goals. Speakers of Yiddish, Ladino, and Judeo-Arabic are the primary victims, bearing the costs of linguistic abandonment and cultural loss. Linguistic diversity advocates are excluded, as their arguments for pluralism were antithetical to the constraint's core premise. Sociolinguists act as analytical observers, documenting the process and its consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, while framed as a 'revival' (a form of scaffold for a new national language), operated with such high extractiveness and suppression against existing languages that it functions as a snare. The 'mandate' to create a unified national language was achieved, but the methods involved significant linguistic and cultural destruction, which the classification as a snare highlights. It prevents mislabeling a coercive process as mere coordination or temporary support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_reality,
    'Was Hebrew truly ''dead'' between 70 CE and 1880 CE, or was it continuously ''alive'' in liturgical and scholarly contexts, merely dormant in secular vernacular use?',
    'Historical linguistic analysis of textual corpora and community practices during the ''dormancy'' period, focusing on functional domains beyond liturgy.',
    'If found to be continuously ''alive'' in some functional sense, the ''revival'' narrative (and thus the justification for suppressing other languages) would be weakened, potentially reclassifying the constraint as more purely extractive. If ''dead'' is confirmed, the revival narrative holds more weight.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dormancy_period_reality, empirical, 'Ambiguity regarding the actual linguistic status of Hebrew prior to the revival movement.').

omega_variable(
    necessity_of_monolingualism,
    'Was the suppression of other Jewish languages (Yiddish, Ladino) a necessary condition for the successful revival of Hebrew as a national language, or could a multilingual national identity have been fostered?',
    'Comparative studies of other national language revivals or formations that successfully integrated multilingualism, or counterfactual historical analysis.',
    'If not necessary, the constraint''s high suppression and extractiveness would be seen as gratuitous, strengthening its classification as a snare. If deemed necessary for the specific historical context, it might shift towards a tangled_rope, acknowledging a coordination function with high, but contextually justified, costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_monolingualism, conceptual, 'Whether linguistic monolingualism was an unavoidable outcome of the Hebrew revival.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of other Jewish languages primarily structural (institutional policies, educational mandates) or internalized (social pressure, self-censorship by speakers)?',
    'Post-exit linguistic trajectory: if suppression of Yiddish/Ladino persists in diaspora communities after the direct institutional pressure is removed, it suggests a stronger internalized component. Analysis of personal testimonies and memoirs.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the targets carried the suppression with them. If primarily structural, removing the institutional barriers would have a more immediate and complete effect on linguistic choice.',
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
narrative_ontology:measurement(hebr_tr_t1890, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1890, 0.07).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.09).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(hebr_tr_t1930, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1940, 0.1).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(hebr_be_t1890, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(hebr_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.75).
narrative_ontology:measurement(hebr_be_t1920, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(hebr_be_t1930, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1930, 0.83).
narrative_ontology:measurement(hebr_be_t1940, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1940, 0.84).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1950, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.3).
narrative_ontology:measurement(hebr_su_t1890, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(hebr_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.8).
narrative_ontology:measurement(hebr_su_t1920, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(hebr_su_t1930, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1930, 0.88).
narrative_ontology:measurement(hebr_su_t1940, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1940, 0.89).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1950, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, israeli_national_identity_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
