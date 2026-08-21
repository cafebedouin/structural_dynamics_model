% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Pidgin
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes Hebrew's role as a contact language or 'pidgin'
 *   for interaction among diverse Jewish diaspora communities. It is neither
 *   purely liturgical nor a fully generative native language, but rather a
 *   functional bridge. This reading emphasizes its instrumental utility for
 *   inter-group communication and cultural continuity, often involving
 *   simplified grammar and vocabulary. It exists in tension with other
 *   readings that prioritize liturgical preservation or native generative
 *   use.
 *
 * KEY AGENTS:
 *   - diaspora_jews_seeking_intergroup_contact: Primary beneficiary (moderate/constrained) — gains communication
 *   - hebrew_language_educators_diaspora: Agenda setter (organized/constrained) — promotes and shapes its use
 *   - linguistic_purists_diaspora: Payer (moderate/constrained) — bears perceived degradation
 *   - native_hebrew_speakers_israel: Excluded (powerful/mobile) — dismisses pidgin as 'not real'
 *   - liturgical_hebrew_scholars: Excluded (organized/constrained) — dismisses pidgin as lacking rigor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.6).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.4).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Pidgin").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '89d47b40-2ae7-4167-87b8-1cf54e741ddf').
narrative_ontology:cs_kernel_codification('89d47b40-2ae7-4167-87b8-1cf54e741ddf', distributed).
narrative_ontology:cs_authority_grounding('89d47b40-2ae7-4167-87b8-1cf54e741ddf', practice).
narrative_ontology:cs_interpretation_layer_present('89d47b40-2ae7-4167-87b8-1cf54e741ddf').
narrative_ontology:cs_reading_relation('89d47b40-2ae7-4167-87b8-1cf54e741ddf', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('89d47b40-2ae7-4167-87b8-1cf54e741ddf', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_axiom('89d47b40-2ae7-4167-87b8-1cf54e741ddf', foundational, hebrew_as_functional_bridge).
narrative_ontology:cs_axiom_status(hebrew_as_functional_bridge, holdable).
narrative_ontology:cs_axiom_grounding('89d47b40-2ae7-4167-87b8-1cf54e741ddf', hebrew_as_functional_bridge, conventional).
narrative_ontology:cs_axiom('89d47b40-2ae7-4167-87b8-1cf54e741ddf', foundational, linguistic_evolution_is_natural).
narrative_ontology:cs_axiom_status(linguistic_evolution_is_natural, holdable).
narrative_ontology:cs_axiom_grounding('89d47b40-2ae7-4167-87b8-1cf54e741ddf', linguistic_evolution_is_natural, empirically_contingent).
narrative_ontology:cs_reference_frame('89d47b40-2ae7-4167-87b8-1cf54e741ddf', intergroup_communication_efficacy).
narrative_ontology:cs_drift_state('89d47b40-2ae7-4167-87b8-1cf54e741ddf', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('89d47b40-2ae7-4167-87b8-1cf54e741ddf', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jews_seeking_intergroup_contact).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, linguistic_purists_diaspora).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, those_unable_to_learn_pidgin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from a common linguistic bridge that facilitates communication and cultural exchange across diverse Jewish diaspora communities, without requiring full native fluency. Their exit options are constrained by the loss of specific cultural connection if they rely solely on other lingua francas.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jews_seeking_intergroup_contact, beneficiary,
    moderate, biographical, constrained, global).

% Educators and cultural institutions actively promote and teach this functional form of Hebrew, seeing it as vital for community cohesion and continuity. They shape its usage and acceptance, but are constrained by the need for practical utility and community buy-in.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_language_educators_diaspora, agenda_setter,
    organized, generational, constrained, global).

% These individuals bear the perceived cost of linguistic degradation, viewing the pidginized form as a dilution of 'authentic' Hebrew. They may resist its use or dismiss its legitimacy, but risk isolation from broader diaspora interaction if they do.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_purists_diaspora, payer,
    moderate, generational, constrained, global).

% Individuals who lack the resources or capacity to learn even a pidginized form of Hebrew are excluded from certain inter-diaspora interactions, bearing the cost of missed connections and cultural participation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, those_unable_to_learn_pidgin, payer,
    powerless, immediate, trapped, local).

% Primarily located in Israel, these speakers often view the pidginized diaspora Hebrew as 'not real' or inferior to their generative, native language. They are largely excluded from the pidgin's internal dynamics, though their linguistic authority implicitly influences its perception.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speakers_israel, excluded,
    powerful, biographical, mobile, national).

% Scholars focused on classical and liturgical Hebrew often dismiss the pidginized form as lacking the depth and grammatical rigor of the sacred language. They operate in a distinct linguistic domain and are largely outside the pidgin's functional sphere.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_hebrew_scholars, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, diaspora_jews_seeking_intergroup_contact).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables practical communication and cultural exchange among diverse Jewish diaspora communities that lack a common native language, fostering a sense of shared identity and facilitating inter-group contact.
% TRANSFER_FUNCTION: Transfers the burden of linguistic adaptation and simplification to participants, while facilitating the exchange of cultural, social, and informational capital across the diaspora.
% ABSENT_VOICES: Native Hebrew speakers from Israel and liturgical Hebrew scholars are largely absent from the conversation about the pidgin's legitimacy and form. They would likely object to its perceived linguistic impurity and argue for adherence to more 'authentic' forms.
% DISAPPEARANCE_RATIONALE: If Hebrew as a bridge pidgin vanished overnight, inter-diaspora communication would become significantly more fragmented and difficult, requiring reliance on less culturally specific lingua francas (e.g., English) or leading to increased isolation between communities. The unique cultural glue it provides would be lost.
% FOUNDING_PROBLEM: The need for a common, accessible linguistic bridge for Jewish communities globally, given the decline of traditional Jewish vernaculars (like Yiddish and Ladino) and the rise of diverse national languages, without requiring full native fluency in modern Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguists and cultural anthropologists observe the ongoing need for such a bridge for diaspora cohesion. Community leaders and educational organizations actively promote its use, indicating a persistent problem of inter-diaspora linguistic connection.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.6) due to the effort required to learn and adapt to this specific contact form, and the implicit cost of dismissal from those who prioritize other forms of Hebrew. Suppression (0.4) is present through social norms and the lack of readily available, equally culturally specific alternatives for inter-diaspora contact. While not formally enforced, social pressure to use this common language for certain interactions can be significant. Theater ratio is low (0.1) as its primary function is practical communication, with little performative maintenance. Accessibility collapse is moderate (0.5) because while other languages exist, they often lack the specific cultural resonance for these interactions. Resistance is low (0.3) but present, primarily from purists who view it as a degraded form.
 *
 * PERSPECTIVAL GAP:
 *   The 'diaspora_jews_seeking_intergroup_contact' seat experiences this as a beneficial coordination mechanism, enabling vital connections. In contrast, 'linguistic_purists_diaspora' perceive it as an extractive force that dilutes the language's authenticity. The 'excluded' seats (native speakers, liturgical scholars) largely dismiss its legitimacy, viewing it as outside the 'true' lineage of Hebrew.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'diaspora_jews_seeking_intergroup_contact' are beneficiaries, gaining direct utility from the communication bridge. 'Hebrew_language_educators_diaspora' act as agenda-setters, promoting and shaping its use. 'Linguistic_purists_diaspora' and 'those_unable_to_learn_pidgin' are payers, bearing the costs of perceived degradation or exclusion. The 'excluded' groups (native speakers, liturgical scholars) are not directly targeted but bear the cost of their preferred form being marginalized in this context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately classified as the ''bridge_pidginized'' reading of the ''hebrew_continuity'' kernel?',
    'Analysis of community linguistic practices and self-identification, comparing the functional use of Hebrew in diaspora contexts against the criteria for pidginization and bridge languages.',
    'If misclassified, the analysis of Hebrew''s role in Jewish continuity would be distorted, potentially conflating distinct linguistic phenomena and obscuring the unique challenges and benefits of this specific form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading of the Hebrew continuity kernel.').

omega_variable(
    definition_of_living_language,
    'What constitutes a ''living'' language in the context of Hebrew continuity, and how do different readings define this?',
    'Sociolinguistic research into language vitality, speaker communities, and functional domains, alongside philosophical inquiry into the nature of linguistic authenticity and evolution.',
    'If ''living'' is defined solely by native generative use, this reading would be dismissed as ''dead'' or ''artificial''. If defined by functional utility and community engagement, this reading gains legitimacy, shifting the perceived extractiveness and suppression from other readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_living_language, conceptual, 'Disagreement on what defines a ''living'' language.').

omega_variable(
    linguistic_authenticity_criteria,
    'What criteria are used to define linguistic authenticity for Hebrew, and how do they vary across different readings?',
    'Comparative analysis of linguistic norms, grammatical expectations, and historical precedents invoked by proponents of liturgical, native, and pidginized forms of Hebrew.',
    'If authenticity is tied strictly to classical grammar or native intuition, this reading''s perceived legitimacy and value would decrease, increasing its effective extraction from those who adopt it. If authenticity allows for functional adaptation, its perceived costs would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_authenticity_criteria, preference, 'Varying criteria for Hebrew''s linguistic authenticity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1950, hebrew_continuity__bridge_pidginized, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(hebr_tr_t1965, hebrew_continuity__bridge_pidginized, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_continuity__bridge_pidginized, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(hebr_tr_t1995, hebrew_continuity__bridge_pidginized, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_continuity__bridge_pidginized, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_continuity__bridge_pidginized, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1950, hebrew_continuity__bridge_pidginized, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(hebr_be_t1965, hebrew_continuity__bridge_pidginized, base_extractiveness, 1965, 0.53).
narrative_ontology:measurement(hebr_be_t1980, hebrew_continuity__bridge_pidginized, base_extractiveness, 1980, 0.56).
narrative_ontology:measurement(hebr_be_t1995, hebrew_continuity__bridge_pidginized, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(hebr_be_t2010, hebrew_continuity__bridge_pidginized, base_extractiveness, 2010, 0.59).
narrative_ontology:measurement(hebr_be_t2025, hebrew_continuity__bridge_pidginized, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1950, hebrew_continuity__bridge_pidginized, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(hebr_su_t1965, hebrew_continuity__bridge_pidginized, suppression_requirement, 1965, 0.33).
narrative_ontology:measurement(hebr_su_t1980, hebrew_continuity__bridge_pidginized, suppression_requirement, 1980, 0.36).
narrative_ontology:measurement(hebr_su_t1995, hebrew_continuity__bridge_pidginized, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(hebr_su_t2010, hebrew_continuity__bridge_pidginized, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(hebr_su_t2025, hebrew_continuity__bridge_pidginized, suppression_requirement, 2025, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hebrew_continuity' kernel, alongside 'liturgical_preservation' and 'native_generative'. Each reading represents a distinct structural claim about how Hebrew lives and functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
