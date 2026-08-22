% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script as Preserver of Ottoman-Islamic Continuity
 *   domain: political/linguistic/commitment_system
 *
 * SUMMARY:
 *   The Arabic script functioned as the orthographic kernel of the Ottoman
 *   Empire, embedding Islamic textual tradition into administrative,
 *   educational, and cultural life. The continuity_reading holds that
 *   preserving Arabic script is necessary to maintain Ottoman cultural
 *   coherence and Islamic textual continuity. This reading instantiates a
 *   constraint that extracts high costs from the Ottoman literate class (who
 *   bear the cognitive and economic burden of a script ill-suited to Turkish
 *   phonology and modern technical vocabulary) and blocks the state
 *   modernization agenda (which requires Latin script for scientific
 *   integration). The beneficiaries are ulema institutions and Islamic
 *   scholarly networks that derive authority from script monopoly. The
 *   constraint requires active enforcement through educational curricula,
 *   official documentation standards, and fatwas against Latin script
 *   adoption.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.72).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.68).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script as Preserver of Ottoman-Islamic Continuity").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political/linguistic/commitment_system").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, 'ad36d5a7-52be-4665-af12-c39c0c183ce0').
narrative_ontology:cs_kernel_codification('ad36d5a7-52be-4665-af12-c39c0c183ce0', fixed_text).
narrative_ontology:cs_authority_grounding('ad36d5a7-52be-4665-af12-c39c0c183ce0', lineage).
narrative_ontology:cs_interpretation_layer_present('ad36d5a7-52be-4665-af12-c39c0c183ce0').
narrative_ontology:cs_reading_relation('ad36d5a7-52be-4665-af12-c39c0c183ce0', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad36d5a7-52be-4665-af12-c39c0c183ce0', orthographic_kernel__rupture_reading, forecloses).
narrative_ontology:cs_axiom('ad36d5a7-52be-4665-af12-c39c0c183ce0', foundational, arabic_script_is_sacral_conduit).
narrative_ontology:cs_axiom_status(arabic_script_is_sacral_conduit, holdable).
narrative_ontology:cs_axiom_grounding('ad36d5a7-52be-4665-af12-c39c0c183ce0', arabic_script_is_sacral_conduit, theological).
narrative_ontology:cs_axiom('ad36d5a7-52be-4665-af12-c39c0c183ce0', foundational, script_continuity_preserves_umma_coherence).
narrative_ontology:cs_axiom_status(script_continuity_preserves_umma_coherence, holdable).
narrative_ontology:cs_axiom_grounding('ad36d5a7-52be-4665-af12-c39c0c183ce0', script_continuity_preserves_umma_coherence, deontological).
narrative_ontology:cs_reference_frame('ad36d5a7-52be-4665-af12-c39c0c183ce0', classical_ottoman_islamic_order).
narrative_ontology:cs_drift_state('ad36d5a7-52be-4665-af12-c39c0c183ce0', post_tanzimat_modernization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ad36d5a7-52be-4665-af12-c39c0c183ce0', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ulema_institutions).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_class).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, islamic_scholarly_networks).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, state_modernization_agenda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_class_religious).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class_religious).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class_secular).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, islamic_textual_continuity).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, ottoman_cultural_coherence).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, arabic_script_sacrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control religious education, fatwa issuance, and textual interpretation across the Islamic world. Their authority derives from mastery of Arabic-script texts. The constraint subsidizes their institutional position by making Arabic script the sole gateway to Islamic knowledge. Exit would mean abandoning the epistemic foundation of their authority.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ulema_institutions, beneficiary,
    institutional, generational, identity_locked, global).

% Religious scholars and madrasa graduates whose professional identity is fused with Arabic script mastery. They benefit from the constraint's protection of their epistemic monopoly but pay in cognitive maintenance of a script system increasingly detached from spoken Turkish. Their exit is identity_locked: leaving the script means leaving their professional-religious self.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class_religious, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ottoman_literate_class_religious, payer).

% Administrators, journalists, merchants, and technicians who must use Arabic script for official and commercial life but find it ill-suited to Turkish phonology and modern vocabulary. They bear high cognitive and economic costs (printing, education, technical translation). Their exit is constrained: they can learn Latin script but at high professional cost and social stigma.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class_secular, payer,
    organized, biographical, constrained, national).

% The Ottoman state's reform apparatus (Tanzimat, Young Turk) requiring scientific, technical, and administrative integration with Europe. The constraint blocks Latin script adoption essential for printing technology, technical vocabulary, and educational standardization. The state is trapped: it cannot modernize without breaking the constraint, but breaking it triggers legitimacy crisis.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernization_agenda, payer,
    powerful, generational, trapped, national).

% Trans-regional networks of scholars, students, and texts spanning the Ottoman Empire, South Asia, and the Arab world. Arabic script is their coordination infrastructure. The constraint preserves their network's interoperability. Exit is identity_locked at civilizational scale: abandoning Arabic script would sever the textual continuity defining the network.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_scholarly_networks, beneficiary,
    institutional, civilizational, identity_locked, global).

% Early reform intellectuals (Namık Kemal, Şinasi) advocating Latin script for Turkish. They are excluded from the official orthographic framework; their publications face censorship and religious condemnation. Their exit is constrained: they can publish abroad or in minority scripts but cannot change the state system.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, young_ottoman_reformers, excluded,
    moderate, biographical, constrained, national).

% Comparative historical linguist / political scientist analyzing the script constraint from outside the commitment system. Sees the full structural picture: beneficiary/victim overlap, identity-lock mechanics, mandatrophy trajectory, and the three readings as distinct constraints from one kernel.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Islamic textual unity across a multi-ethnic, multi-lingual empire: a shared script enables fatwa circulation, legal precedent sharing, and scholarly communication from Istanbul to Cairo to Delhi without translation.
% TRANSFER_FUNCTION: Moves cognitive labor, educational resources, and economic opportunity from the Ottoman literate class (especially secular administrators and merchants) and the state modernization agenda to ulema institutions and Islamic scholarly networks, as the price of maintaining script monopoly.
% ABSENT_VOICES: The majority Turkish-speaking peasantry (illiterate in both scripts) would benefit from Latin script's phonological fit but had no voice. Women in Ottoman society, excluded from both madrasa and state education, bear the script's cognitive costs intergenerationally without representation. Minority communities (Armenians, Greeks, Jews) using Arabic script for Turkish (karamanlidika) have distinct interests not represented in the ulema/state binary.
% DISAPPEARANCE_RATIONALE: If Arabic script continuity vanished overnight (as it effectively did in 1928), the Ottoman/Turkish state would rapidly adopt Latin script, printing and technical vocabulary would modernize within a decade, ulema institutional authority would collapse, and the trans-regional Islamic scholarly network would fracture — the world rearranges completely.
% FOUNDING_PROBLEM: How to maintain Islamic legal, theological, and administrative coherence across an empire spanning three continents, dozens of languages, and diverse legal schools — without a central bureaucracy capable of enforcing uniformity.
% FOUNDING_PROBLEM_CORROBORATION: Ottoman state archives (Tanzimat edicts, Young Turk memoranda) document the founding problem's obsolescence by 1910: the empire had become majority Turkish-speaking, telegraph/print created new coordination channels, and European scientific integration required Latin script. The ulema's own fatwas defending Arabic script after 1910 shift from 'necessary for unity' to 'necessary for identity' — corroborating the founding problem's death. No source outside the beneficiary set attests the problem as live after 1915.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects the high cost imposed on literacy acquisition, technical vocabulary development, and printing technology adoption. The Ottoman literate class pays in time, cognitive load, and economic opportunity; the state modernization agenda pays in delayed scientific integration. Suppression (0.68) is substantial: the state enforces Arabic script through school curricula, official publications, and religious rulings. Theater ratio (0.25) is moderate — the coordination function (Islamic textual unity) is real but increasingly performative as the empire's administrative needs diverge from the script's capacity. Accessibility collapse (0.78) is high: alternatives (Latin script, Armenian script for Turkish) exist but collapse once the constraint's religious-cultural framing is internalized. Resistance (0.55) is significant from Young Ottoman and Young Turk reformers but fragmented by the constraint's identity-locking mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the ulema seat, the constraint is a Rope: it coordinates Islamic textual unity across the empire with minimal coercion (their d is low, χ is negative/subsidy). From the secular modernizer seat, it is a Snare: the coordination story is cover for extracting cognitive labor and blocking technological integration (their d is high, χ is amplified). The engine computes this divergence from the structural data; the claimed tangled_rope reflects the hybrid reality where both seats exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The ulema_institutions and islamic_scholarly_networks are structural beneficiaries (d ≈ 0.15): they control religious education, fatwa issuance, and textual authority — the constraint subsidizes their institutional position. The ottoman_literate_class is dually positioned: as religious scholars they benefit (d ≈ 0.2); as secular administrators/merchants they are targets (d ≈ 0.85). The state_modernization_agenda is a full target (d ≈ 0.95): the constraint blocks the reform path essential to its survival. Exit options differ: ulema are identity_locked (professional-religious identity fused with script), secular literate are constrained (can learn Latin script but at high professional cost), state actors are trapped (constraint is structural to the regime they must reform).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (Islamic textual unity across a multi-ethnic empire) was live in 1800 but dead by 1920 — the empire had become majority Turkish-speaking, and scientific modernity required Latin script. The arrangement persisted 8+ years after its founding problem died, maintained by ulema institutional inertia and identity fusion. This is classic mandatrophy: a coordination mechanism (Islamic textual unity) whose function atrophied but whose enforcement persisted through identity capture of the literate class.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine structural feature of the orthographic kernel, or a constructed reading that benefits identifiable agents?',
    'Comparative analysis of sibling readings (modernization_reading, rupture_reading) showing divergent beneficiary/victim structures and ε values from the same kernel.',
    'If constructed, the continuity_reading is a false summit masking extraction; if structural, it is a Mountain of cultural continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether continuity_reading is a natural-law claim or a constructed constraint with beneficiaries').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state enforcement of Arabic script) or internalized (cultural/religious identity fusion making Latin script unthinkable)?',
    'Post-1928 suppression trajectory: if suppression persists in diaspora communities without state enforcement, reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests; the constraint carries its own enforcement mechanism through identity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in script continuity').

omega_variable(
    beneficiary_victim_overlap,
    'The Ottoman literate class appears as both beneficiary and victim — do they benefit from cultural continuity or bear the cost of blocked modernization?',
    'Disaggregate by sub-group: religious scholars (ulema) vs. secular administrators vs. merchants; trace divergent exit options and power positions.',
    'If the same group is both beneficiary and victim, the constraint is a snare for that group; if sub-groups diverge, it is a genuine tangled rope with internal fracture lines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_victim_overlap, conceptual, 'Whether the Ottoman literate class is a unified beneficiary/victim or fractured into distinct structural positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 1800, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1800, orthographic_kernel__continuity_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1850, orthographic_kernel__continuity_reading, theater_ratio, 1850, 0.12).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1900, orthographic_kernel__continuity_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1910, orthographic_kernel__continuity_reading, theater_ratio, 1910, 0.22).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1920, orthographic_kernel__continuity_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_tr_t1928, orthographic_kernel__continuity_reading, theater_ratio, 1928, 0.25).

% Extraction over time
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1800, orthographic_kernel__continuity_reading, base_extractiveness, 1800, 0.35).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1850, orthographic_kernel__continuity_reading, base_extractiveness, 1850, 0.45).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1900, orthographic_kernel__continuity_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1910, orthographic_kernel__continuity_reading, base_extractiveness, 1910, 0.62).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1920, orthographic_kernel__continuity_reading, base_extractiveness, 1920, 0.72).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_be_t1928, orthographic_kernel__continuity_reading, base_extractiveness, 1928, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1800, orthographic_kernel__continuity_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1850, orthographic_kernel__continuity_reading, suppression_requirement, 1850, 0.5).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1900, orthographic_kernel__continuity_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1910, orthographic_kernel__continuity_reading, suppression_requirement, 1910, 0.65).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1920, orthographic_kernel__continuity_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(orthographic_kernel__continuity_reading_su_t1928, orthographic_kernel__continuity_reading, suppression_requirement, 1928, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, tanzimat_education_reforms).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, young_turk_nationalism).

% DUAL FORMULATION NOTE:
% The orthographic_kernel decomposes into three constraint stories (continuity_reading, modernization_reading, rupture_reading) linked by network.affects_constraints. continuity_reading has high ε for Ottoman literate class (victim) and blocks state modernization; modernization_reading has lower ε, coordinates technological integration; rupture_reading has highest suppression (deliberate cultural severance). They are not the same constraint viewed differently — ε values differ by >0.3, beneficiary/victim structures invert, and failure modes are distinct. The confusion was in the label 'Arabic script' — the framework disambiguates into three structurally precise claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, institutional, 0.15).
constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, organized, 0.5).
constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, powerful, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
