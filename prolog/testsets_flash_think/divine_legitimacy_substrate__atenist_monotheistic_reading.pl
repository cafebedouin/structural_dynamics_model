% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Revelation of Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   This constraint represents the Atenist monotheistic reading of divine
 *   legitimacy during the reign of Pharaoh Akhenaten (c. 1353–1336 BCE). It
 *   asserts that divine authority flows exclusively through the pharaoh's
 *   revelation of Aten as the sole, universal deity, actively suppressing all
 *   other traditional gods and their cults. This reading led to a radical
 *   restructuring of Egyptian religion, politics, and economy, centralizing
 *   power in the pharaoh and dismantling the influence of the powerful Amun
 *   priesthood.
 *
 * KEY AGENTS:
 *   - Pharaoh Akhenaten: Primary agenda-setter and beneficiary (institutional/arbitrage) — consolidates power and wealth.
 *   - Atenist Priesthood: Secondary beneficiary (organized/constrained) — gains power through the new cult.
 *   - Amun Priesthood: Primary target/victim (institutional/trapped) — loses all power, wealth, and legitimacy.
 *   - Traditional Cults: Secondary target/victim (organized/trapped) — suppressed and dismantled.
 *   - Common Worshippers: Diffuse target/victim (powerless/identity_locked) — forced to abandon ancestral practices.
 *   - Folk Healers and Magicians: Excluded (moderate/constrained) — their practices are delegitimized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.85).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.92).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Revelation of Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'f9759f78-877a-42dd-becd-cd6cc9241bd2').
narrative_ontology:cs_kernel_codification('f9759f78-877a-42dd-becd-cd6cc9241bd2', formalized).
narrative_ontology:cs_authority_grounding('f9759f78-877a-42dd-becd-cd6cc9241bd2', lineage).
narrative_ontology:cs_interpretation_layer_present('f9759f78-877a-42dd-becd-cd6cc9241bd2').
narrative_ontology:cs_reading_relation('f9759f78-877a-42dd-becd-cd6cc9241bd2', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('f9759f78-877a-42dd-becd-cd6cc9241bd2', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('f9759f78-877a-42dd-becd-cd6cc9241bd2', foundational, aten_sole_creator_deity).
narrative_ontology:cs_axiom_status(aten_sole_creator_deity, holdable).
narrative_ontology:cs_axiom_grounding('f9759f78-877a-42dd-becd-cd6cc9241bd2', aten_sole_creator_deity, theological).
narrative_ontology:cs_axiom('f9759f78-877a-42dd-becd-cd6cc9241bd2', foundational, pharaoh_sole_interpreter_of_aten).
narrative_ontology:cs_axiom_status(pharaoh_sole_interpreter_of_aten, holdable).
narrative_ontology:cs_axiom_grounding('f9759f78-877a-42dd-becd-cd6cc9241bd2', pharaoh_sole_interpreter_of_aten, conventional).
narrative_ontology:cs_reference_frame('f9759f78-877a-42dd-becd-cd6cc9241bd2', pharaonic_monotheistic_revelation).
narrative_ontology:cs_drift_state('f9759f78-877a-42dd-becd-cd6cc9241bd2', akhenaten_reign_end, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('f9759f78-877a-42dd-becd-cd6cc9241bd2', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cults).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, common_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sole interpreter and conduit of Aten's will, consolidating all religious and political power. He initiates and enforces the monotheistic cult, dismantling rival power centers and seizing their wealth.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, civilizational, arbitrage, national).

% A newly established priestly class whose power and wealth derive entirely from their service to Aten and their proximity to the pharaoh. They administer the new cult and its rituals, replacing traditional religious institutions.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_priesthood, beneficiary,
    organized, generational, constrained, national).

% The formerly dominant religious institution, whose temples are closed, wealth confiscated, and cult suppressed. Their traditional authority and social standing are systematically dismantled, with no legitimate avenue for resistance.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, trapped, national).

% Local and regional cults dedicated to various deities, whose practices are outlawed and whose shrines are desecrated. Their followers are forced to abandon long-held traditions or practice them in secret.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_cults, payer,
    organized, biographical, trapped, local).

% The general populace, whose daily religious practices, festivals, and understanding of the cosmos are uprooted. They are compelled to worship Aten exclusively, often against deep-seated cultural and personal identities tied to traditional gods.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, common_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Practitioners of traditional folk religion and magic, whose activities are deemed illegitimate by the Atenist regime. While some practices might persist underground, their public role and legitimacy are severely curtailed.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_healers_and_magicians, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes religious authority and belief around a single, exclusive deity (Aten) and its sole interpreter (the pharaoh), aiming to unify the state under a singular divine patron and eliminate perceived fragmentation of divine power.
% TRANSFER_FUNCTION: Transfers immense religious authority, wealth (from confiscated temple estates), and social capital from the traditional Amun priesthood and other cults to Pharaoh Akhenaten and the newly established Atenist priesthood.
% ABSENT_VOICES: The dispossessed Amun priesthood, leaders of traditional cults, and common people whose ancestral religious practices are forbidden. They would object to the destruction of their religious heritage and the imposition of a foreign cult, but their voices are suppressed by state power.
% DISAPPEARANCE_RATIONALE: If the Atenist cult and its enforcement vanished overnight, the entire political and social order of the Amarna period would collapse. Traditional polytheism, particularly the cult of Amun, would rapidly reassert itself, and the Amun priesthood would regain its power, leading to a complete reorganization of religious and state structures.
% FOUNDING_PROBLEM: Pharaoh Akhenaten perceived the Amun priesthood as excessively powerful and corrupt, fragmenting divine authority and challenging royal prerogative. He sought to establish a more direct, centralized connection to the divine through a single, universal deity.
% FOUNDING_PROBLEM_CORROBORATION: Pharaoh Akhenaten's own decrees, hymns, and monumental inscriptions attest to this founding problem. Historians and archaeologists corroborate the *existence* of Akhenaten's stated rationale, but its *validity* as a genuine problem (versus a justification for a power grab) is a subject of ongoing debate among scholars, who provide external corroboration of the historical context.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) due to the systematic confiscation of wealth from traditional temples and the redirection of all religious offerings and state resources to the Aten cult. Suppression is extremely high (0.92) as the regime actively persecuted other gods, defaced their images, closed their temples, and enforced exclusive Aten worship, leaving no legitimate alternatives. Theater ratio is moderate (0.45): while Akhenaten's devotion to Aten was genuine, the elaborate rituals and monumental building projects served a performative function to legitimize the new, imposed order and mask the underlying coercion. Accessibility collapse is high (0.90) because the state actively eliminated all public alternatives to Aten worship. Resistance is high (0.78) due to the radical nature of the change and the deep cultural roots of traditional polytheism, leading to widespread, albeit often covert, opposition.
 *
 * PERSPECTIVAL GAP:
 *   From Akhenaten's perspective, this was a divine revelation and a necessary purification of Egyptian religion, a genuine 'rope' for spiritual unity. From the perspective of the Amun priesthood and common worshippers, it was a tyrannical imposition, a 'snare' that extracted their wealth, identity, and spiritual freedom through overwhelming force. The engine's classification as 'snare' reflects the structural reality of coercion and extraction, regardless of the pharaoh's claimed divine mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten and the Atenist priesthood are clear beneficiaries, gaining immense power and resources. The Amun priesthood, traditional cults, and common worshippers are direct victims, suffering loss of wealth, status, and freedom of worship. Common worshippers are 'identity_locked' as their entire worldview and daily life were intertwined with traditional deities, making psychological exit extremely difficult even if physical exit was impossible. The constraint subsidizes the new elite by extracting from the old order and the populace.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_power_grab,
    'Was Akhenaten''s Atenist revolution a genuine divine revelation and spiritual reform, or primarily a political maneuver to consolidate power and dismantle the Amun priesthood''s influence?',
    'Analysis of Akhenaten''s personal writings and decrees for consistency and theological depth versus the immediate political and economic gains for the crown. Corroboration from independent historical accounts (if available) or archaeological evidence of popular reception.',
    'If primarily a power grab, the ''snare'' classification is strongly reinforced, highlighting the extractive nature. If a genuine spiritual reform, it introduces a conceptual tension between the pharaoh''s intent and the constraint''s coercive operation, potentially shifting the ''claimed_type'' closer to a ''tangled_rope'' from the pharaoh''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_power_grab, conceptual, 'Ambiguity between spiritual reform and political consolidation.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent did the suppression of traditional worship lead to internalized belief in Aten among common worshippers, versus merely outward compliance enforced by structural coercion?',
    'Archaeological evidence from post-Amarna periods showing the speed and completeness of the return to traditional polytheism. Analysis of private religious artifacts and burial practices during Akhenaten''s reign for signs of continued traditional worship.',
    'If suppression was largely structural, the constraint''s effective suppression was high only while actively enforced. If internalized, the suppression had a deeper, more lasting impact on identity, making the constraint''s effective suppression higher even after the regime''s fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for religious belief.').

omega_variable(
    long_term_identity_impact,
    'What was the long-term impact of the Atenist experiment on Egyptian religious identity and the relationship between pharaoh and deity, even after the return to polytheism?',
    'Comparative analysis of religious texts, iconography, and royal ideology from pre-Amarna, Amarna, and post-Amarna periods for subtle shifts in theological concepts or the pharaoh''s divine role.',
    'If a lasting impact is found, the constraint''s influence extends beyond its active enforcement period, suggesting a more profound, albeit subtle, ''identity_coordination'' function that persisted as a latent force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_identity_impact, empirical, 'Lingering effects on religious identity post-Atenism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.45).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.8).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.83).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.85).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.86).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.89).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.92).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.93).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.92).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'divine_legitimacy_substrate' kernel. It actively forecloses the 'amun_polytheistic_reading' and 'folk_syncretistic_reading' by denying the existence and legitimacy of other gods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
