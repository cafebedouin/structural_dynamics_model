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
 *   constraint_id: orthographic_kernel__rupture_reading
 *   human_readable: Turkish Script Reform as Cultural Rupture
 *   domain: political_linguistics/state_formation/cultural_policy
 *
 * SUMMARY:
 *   This constraint describes the Turkish script reform (1928) as a
 *   deliberate cultural rupture, a state-mandated act designed to sever ties
 *   with the Ottoman/Islamic past and forge a new, secular national identity.
 *   From this 'rupture_reading' perspective, the reform was a foundational
 *   act of cultural engineering, intentionally extracting from the old to
 *   build the new. The high extractiveness reflects the profound cultural
 *   loss experienced by the pre-reform literate population, while high
 *   suppression indicates the coercive power of the state in enforcing this
 *   radical change.
 *
 * KEY AGENTS:
 *   - post_reform_state_apparatus: Primary agenda_setter and beneficiary (institutional/arbitrage)
 *   - new_national_identity_proponents: Primary beneficiary (powerful/mobile)
 *   - ottoman_literate_population: Primary target/victim (powerless/trapped)
 *   - islamic_scholars: Target/victim (organized/identity_locked)
 *   - cultural_conservatives: Target/victim (moderate/constrained)
 *   - traditional_cultural_institutions: Excluded voice (organized/trapped)
 *   - international_observers: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__rupture_reading, 0.9).
domain_priors:suppression_score(orthographic_kernel__rupture_reading, 0.95).
domain_priors:theater_ratio(orthographic_kernel__rupture_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(orthographic_kernel__rupture_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__rupture_reading, snare).
narrative_ontology:human_readable(orthographic_kernel__rupture_reading, "Turkish Script Reform as Cultural Rupture").
narrative_ontology:topic_domain(orthographic_kernel__rupture_reading, "political_linguistics/state_formation/cultural_policy").

domain_priors:requires_active_enforcement(orthographic_kernel__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__rupture_reading, '56570c90-ee4f-4444-bf3c-ca549ab0fb3f').
narrative_ontology:cs_kernel_codification('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', formalized).
narrative_ontology:cs_authority_grounding('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', lineage).
narrative_ontology:cs_interpretation_layer_present('56570c90-ee4f-4444-bf3c-ca549ab0fb3f').
narrative_ontology:cs_reading_relation('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', orthographic_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_axiom('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', foundational, ottoman_past_as_burden).
narrative_ontology:cs_axiom_status(ottoman_past_as_burden, holdable).
narrative_ontology:cs_axiom_grounding('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', ottoman_past_as_burden, conventional).
narrative_ontology:cs_axiom('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', foundational, national_identity_requires_rupture).
narrative_ontology:cs_axiom_status(national_identity_requires_rupture, holdable).
narrative_ontology:cs_axiom_grounding('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', national_identity_requires_rupture, conventional).
narrative_ontology:cs_reference_frame('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', secular_turkish_republic).
narrative_ontology:cs_drift_state('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('56570c90-ee4f-4444-bf3c-ca549ab0fb3f', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(orthographic_kernel__rupture_reading, new_national_identity_proponents).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, ottoman_literate_population).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, islamic_scholars).
narrative_ontology:constraint_victim(orthographic_kernel__rupture_reading, cultural_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforced the script change through legislative decree and state institutions, aiming to create a new, secular national identity by severing linguistic and cultural ties to the Ottoman past. Benefits from the consolidation of state power and the redefinition of national heritage.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, post_reform_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Actively championed and benefited from the creation of a distinct, modern Turkish identity, free from perceived Ottoman/Islamic baggage. Their vision of nationhood was realized through this radical cultural engineering.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, new_national_identity_proponents, beneficiary,
    powerful, generational, mobile, national).

% Lost their functional literacy overnight, severing their direct connection to historical texts, literature, and cultural heritage written in the Arabic script. This imposed a profound personal and cultural cost.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, ottoman_literate_population, payer,
    powerless, biographical, trapped, national).

% Their religious texts, scholarly tradition, and authority were primarily rooted in Arabic script. The reform made their work inaccessible to the new generation, undermining their social role and the transmission of their knowledge.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, islamic_scholars, payer,
    organized, biographical, identity_locked, national).

% Opposed the script change as a fundamental attack on tradition, religious values, and the continuity of Turkish culture. Despite their resistance, they lacked the power to prevent the state-mandated reform.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, cultural_conservatives, payer,
    moderate, biographical, constrained, national).

% Institutions responsible for preserving and transmitting Ottoman/Islamic culture (e.g., madrasas, Sufi lodges, traditional publishing houses) were systematically undermined, marginalized, or dismantled by the reform, effectively removing their voice and function from the national discourse.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, traditional_cultural_institutions, excluded,
    organized, generational, trapped, national).

% Analyzed the Turkish script reform as a radical and unprecedented act of state-building and cultural engineering, noting its profound and often traumatic implications for national identity and historical continuity.
narrative_ontology:constraint_stakeholder(orthographic_kernel__rupture_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinated the creation of a new, distinct national identity by severing linguistic ties to the Ottoman past, enabling a unified, secular Turkish cultural sphere aligned with the new republic's ideology.
% TRANSFER_FUNCTION: Transferred cultural capital, historical legitimacy, and textual authority from the Ottoman/Islamic tradition to the new secular Turkish state and its emerging national identity, effectively re-founding the nation's cultural basis.
% ABSENT_VOICES: The vast majority of the pre-reform literate population, whose literacy was rendered obsolete overnight, were effectively silenced. Religious authorities and traditional educators, whose institutions were dismantled, also had their voices excluded from the national conversation.
% DISAPPEARANCE_RATIONALE: If the script reform had not occurred, or if its effects vanished, the cultural and political landscape of modern Turkey would be fundamentally different, retaining stronger ties to its Ottoman and Islamic heritage. This would impact national identity, education, foreign policy, and the very self-conception of the Turkish state, leading to a profound reorganization of society.
% FOUNDING_PROBLEM: The perceived need to decisively break from a decaying Ottoman past and a religiously-inflected identity to forge a modern, secular, and distinct Turkish nation-state, aligning it with Western modernity.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the reform, state historians, and some secular intellectuals corroborate this, arguing that the need for a distinct, modern national identity free from the 'burden' of the past remains relevant. Critics (e.g., cultural conservatives, some historians) dispute the necessity and the severity of the rupture, arguing for a more continuous evolution, but the state's narrative persists.
narrative_ontology:disappearance_verdict(orthographic_kernel__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__rupture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__rupture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(orthographic_kernel__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__rupture_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very high (0.9) because the reform effectively rendered an entire generation illiterate in their native script, severing their direct access to centuries of written culture and religious texts. Suppression is extremely high (0.95) due to the state's swift and comprehensive enforcement, with no legal alternatives for education or publication in the old script. The theater ratio is low (0.1) because the reform was a genuinely functional and effective tool for its stated purpose of cultural rupture, not a performative facade. Accessibility collapse is high (0.88) as the old script quickly became inaccessible to new generations. Resistance was significant (0.7) but ultimately overwhelmed by state power. The measurements show initial high extractiveness and suppression, with a slight normalization over time as the new script became entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'post_reform_state_apparatus' and 'new_national_identity_proponents', the script reform was a necessary and successful act of nation-building, a 'rope' or 'scaffold' for a modern Turkey. However, from the 'ottoman_literate_population' and 'islamic_scholars' seats, the same constraint operated as a 'snare', imposing immense cultural and personal costs through coercive extraction. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'post_reform_state_apparatus' and 'new_national_identity_proponents' are clear beneficiaries (low d) as they achieved their goal of cultural rupture and national redefinition. The 'ottoman_literate_population', 'islamic_scholars', and 'cultural_conservatives' are direct targets (high d) as they bore the immediate and profound costs of lost literacy, undermined authority, and cultural discontinuity. 'Traditional_cultural_institutions' are excluded, their very existence challenged by the reform's aims.
 *
 * MANDATROPHY ANALYSIS:
 *   From this 'rupture_reading' perspective, the constraint's mandate to create a new national identity by severing the past is considered 'live'. The classification as a 'snare' prevents mislabeling this as mere 'modernization' or 'coordination', instead highlighting the coercive, extractive nature of the cultural rupture and the identifiable victims it created. The persistence of the new national identity, achieved through this rupture, is seen as the ongoing function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    purpose_of_script_reform,
    'Was the primary purpose of the script reform practical modernization, cultural preservation, or deliberate cultural rupture?',
    'Analysis of primary historical documents (legislative debates, state propaganda, educational curricula) and contemporary accounts from diverse social strata, focusing on stated and implicit goals.',
    'If primarily practical modernization, extractiveness would be lower and the classification might shift towards ''tangled_rope'' or ''rope''. If primarily cultural preservation (as in the ''continuity_reading''), the victim set would be different, and the ''rupture_reading'' would be foreclosed. This omega clarifies the core conceptual framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purpose_of_script_reform, conceptual, 'Ambiguity in the primary intent behind the script reform.').

omega_variable(
    long_term_cultural_cost_vs_benefit,
    'Does the long-term benefit of a distinct national identity outweigh the immediate and generational cultural costs imposed by the rupture?',
    'Longitudinal sociological and cultural studies, including intergenerational surveys of cultural identity, historical literacy, and national cohesion, compared with counterfactual analyses of alternative reform paths.',
    'If the costs are deemed to perpetually outweigh the benefits, the ''snare'' classification is reinforced. If a strong, widely accepted net benefit emerges, the classification might soften towards a ''tangled_rope'' or even ''scaffold'' (if the transition is seen as complete and beneficial).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_cultural_cost_vs_benefit, preference, 'Normative evaluation of the net impact of cultural rupture over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__rupture_reading, 1928, 1978).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__rupture_reading, theater_ratio, 1928, 0.1).
narrative_ontology:measurement(orth_tr_t1938, orthographic_kernel__rupture_reading, theater_ratio, 1938, 0.09).
narrative_ontology:measurement(orth_tr_t1948, orthographic_kernel__rupture_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(orth_tr_t1958, orthographic_kernel__rupture_reading, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(orth_tr_t1968, orthographic_kernel__rupture_reading, theater_ratio, 1968, 0.09).
narrative_ontology:measurement(orth_tr_t1978, orthographic_kernel__rupture_reading, theater_ratio, 1978, 0.1).

% Extraction over time
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__rupture_reading, base_extractiveness, 1928, 0.85).
narrative_ontology:measurement(orth_be_t1938, orthographic_kernel__rupture_reading, base_extractiveness, 1938, 0.88).
narrative_ontology:measurement(orth_be_t1948, orthographic_kernel__rupture_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement(orth_be_t1958, orthographic_kernel__rupture_reading, base_extractiveness, 1958, 0.9).
narrative_ontology:measurement(orth_be_t1968, orthographic_kernel__rupture_reading, base_extractiveness, 1968, 0.89).
narrative_ontology:measurement(orth_be_t1978, orthographic_kernel__rupture_reading, base_extractiveness, 1978, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__rupture_reading, suppression_requirement, 1928, 0.95).
narrative_ontology:measurement(orth_su_t1938, orthographic_kernel__rupture_reading, suppression_requirement, 1938, 0.92).
narrative_ontology:measurement(orth_su_t1948, orthographic_kernel__rupture_reading, suppression_requirement, 1948, 0.88).
narrative_ontology:measurement(orth_su_t1958, orthographic_kernel__rupture_reading, suppression_requirement, 1958, 0.85).
narrative_ontology:measurement(orth_su_t1968, orthographic_kernel__rupture_reading, suppression_requirement, 1968, 0.83).
narrative_ontology:measurement(orth_su_t1978, orthographic_kernel__rupture_reading, suppression_requirement, 1978, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__rupture_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'rupture_reading' of the 'orthographic_kernel'. It is part of a constraint family that includes 'orthographic_kernel__continuity_reading' and 'orthographic_kernel__modernization_reading', each representing a distinct interpretation of the Turkish script reform with different structural properties and consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
