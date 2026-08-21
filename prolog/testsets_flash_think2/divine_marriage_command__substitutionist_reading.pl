% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading)
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'substitutionist_reading' of the divine
 *   marriage command, where a religious institution, facing severe external
 *   pressure from secular authorities to abandon polygamy, issues a
 *   'Manifesto' declaring monogamy as a new divine revelation that supersedes
 *   prior commands. This reading asserts that monogamy is now doctrinally
 *   required, and any continued practice of polygamy constitutes apostasy.
 *   The constraint functions to ensure the institution's survival and legal
 *   standing, but at the cost of profound extraction from adherents whose
 *   prior practices and beliefs are now deemed illegitimate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.85).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.9).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'f707c9b0-be71-4ae9-a27d-f1e326bbd89d').
narrative_ontology:cs_kernel_codification('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', fixed_text).
narrative_ontology:cs_authority_grounding('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', lineage).
narrative_ontology:cs_interpretation_layer_present('f707c9b0-be71-4ae9-a27d-f1e326bbd89d').
narrative_ontology:cs_reading_relation('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_axiom('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', foundational, monogamy_is_divine_command).
narrative_ontology:cs_axiom_status(monogamy_is_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', monogamy_is_divine_command, theological).
narrative_ontology:cs_axiom('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', foundational, new_revelation_supersedes_prior).
narrative_ontology:cs_axiom_status(new_revelation_supersedes_prior, holdable).
narrative_ontology:cs_axiom_grounding('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', new_revelation_supersedes_prior, theological).
narrative_ontology:cs_reference_frame('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', monogamous_revelation_framework).
narrative_ontology:cs_drift_state('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', post_manifesto_consolidation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f707c9b0-be71-4ae9-a27d-f1e326bbd89d', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, religious_hierarchy).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, conforming_members).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, community_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamous_adherents).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, framing it as a new divine revelation. They benefit from institutional survival, legal compliance with secular authorities, and a unified, less controversial public image. They actively enforce the new monogamous doctrine through excommunication and social pressure.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, religious_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Members who accept and conform to the new monogamous doctrine. They benefit from continued membership in the community, social acceptance, and avoidance of conflict with secular law. Their exit options are constrained by deep social and identity ties to the religious community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, conforming_members, beneficiary,
    moderate, biographical, constrained, global).

% Individuals and families who previously practiced polygamy and view it as a sacred command. They bear the direct cost of the new doctrine, facing excommunication, social ostracization, and the dissolution of their family structures. Their identity is deeply tied to the prior practice, making exit unthinkable.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamous_adherents, payer,
    powerless, generational, identity_locked, local).

% Organized groups within the community who resist the new doctrine, viewing it as a capitulation to secular pressure rather than genuine revelation. They face institutional sanctions, including excommunication of their leaders and members, but maintain a degree of internal cohesion and alternative social structures.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, fundamentalist_factions, payer,
    organized, generational, constrained, regional).

% Government and legal bodies that historically exerted pressure on the religious community to abandon polygamy. They now observe the community's compliance with secular law, and their pressure was a key driver for the Manifesto's issuance.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% The broader body of members who, while not directly polygamous, benefit from the community's improved public image and reduced conflict with external society. They experience social cohesion and reduced stigma, but are constrained by the new doctrinal requirements.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify the religious community under a single, legally compliant marriage doctrine, ensuring institutional survival, social acceptance within secular society, and reducing internal and external conflict.
% TRANSFER_FUNCTION: Transfers theological legitimacy from prior polygamous practice to a new monogamous command; transfers social and legal risk from the religious institution to individual adherents who must conform or face excommunication; transfers institutional autonomy from internal theological interpretation to external legal compliance.
% ABSENT_VOICES: Those who were excommunicated or chose to leave the community rather than abandon polygamy; their narratives of persecution and doctrinal betrayal are excluded from the official institutional history.
% DISAPPEARANCE_RATIONALE: If the divine marriage command (substitutionist reading) vanished overnight, the religious community's relationship with secular law would revert to conflict, its internal social structure would fragment, and its theological self-understanding would be profoundly destabilized. The entire institutional and social order built upon this command would collapse.
% FOUNDING_PROBLEM: The religious community faced existential threats from secular legal systems prohibiting polygamy, leading to persecution, confiscation of property, imprisonment of leaders, and the potential dissolution of the institution.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal documents from secular authorities, and independent sociological studies from outside the benefiting religious hierarchy corroborate the severe existential threat faced by the community due to polygamy. While the immediate legal threat is mitigated, the underlying tension between religious practice and secular law remains a live concern for institutional legitimacy.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because the constraint demands a fundamental and often painful change in deeply held religious practice and family structure, enforced by the highest religious authority. Suppression is also very high (0.90) due to the threat of excommunication, social ostracization, and the framing of non-compliance as apostasy. The theater ratio is moderate (0.45) as the 'revelation' aspect serves a performative function to legitimize a pragmatic response to political coercion, masking the underlying power dynamics. Accessibility collapse is high (0.90) because the new doctrine effectively eliminates the legitimacy of prior practices within the community. Resistance is significant (0.70) from those who adhere to the prior command, but ultimately suppressed by institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the religious hierarchy's perspective, the Manifesto is a necessary and divinely guided revelation for the salvation and continuity of the institution. From the perspective of polygamous adherents and fundamentalist factions, it is a coerced abandonment of sacred principles, a betrayal of prior divine commands, and a profound act of extraction disguised as revelation. The engine's classification will highlight this divergence between the claimed 'rope' (divine guidance for unity) and the computed 'tangled_rope' (coordination for institutional survival, extraction from dissenters).
 *
 * DIRECTIONALITY LOGIC:
 *   The religious hierarchy is the primary beneficiary (agenda_setter), gaining institutional survival, legal legitimacy, and a unified public image. Conforming members and the broader community also benefit from social cohesion and reduced external conflict. Polygamous adherents and fundamentalist factions are the primary targets (payers), bearing the costs of doctrinal shift, excommunication, and the dismantling of their family structures. Secular authorities act as observers, having exerted the initial pressure that led to the constraint's formation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential threat from secular authorities) is still live, as the institution's survival and legal standing remain contingent on its adherence to monogamy. However, the 'divine revelation' framing of the solution carries a significant theatrical component, as it masks the political necessity. The constraint prevents mandatrophy by continuously adapting its theological justification to maintain institutional legitimacy in the face of external pressures, even as the means of adaptation are highly extractive for some members.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_coercion_authenticity,
    'Is the Manifesto a genuine divine revelation, or primarily a pragmatic response to overwhelming secular coercion, framed as revelation for internal legitimacy?',
    'Analysis of internal theological discourse and external political pressures leading up to and following the Manifesto''s issuance, including private communications of leaders and the timing of legal actions by secular authorities.',
    'If primarily coerced, the ''divine command'' aspect of the constraint''s legitimacy is significantly weakened, increasing its effective extractiveness and theater ratio, pushing it further towards a Snare. If genuinely believed as revelation, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_coercion_authenticity, conceptual, 'Ambiguity of the Manifesto''s origin: divine will vs. political necessity.').

omega_variable(
    internalized_belief_vs_outward_conformity,
    'To what extent did polygamous adherents genuinely internalize the new monogamous doctrine, versus merely outwardly conforming to avoid excommunication and social penalties?',
    'Longitudinal sociological studies of former polygamous families, ethnographic research on private practices, and analysis of dissident narratives over generations.',
    'If conformity was largely outward, the suppression metric''s effective impact is higher, as the internal cost of non-belief persists despite outward compliance. This would amplify the effective extraction for identity_locked individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_belief_vs_outward_conformity, empirical, 'The degree of genuine belief vs. forced compliance with the new doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 1890, 1940).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1890, divine_marriage_command__substitutionist_reading, theater_ratio, 1890, 0.3).
narrative_ontology:measurement(divi_tr_t1900, divine_marriage_command__substitutionist_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(divi_tr_t1910, divine_marriage_command__substitutionist_reading, theater_ratio, 1910, 0.4).
narrative_ontology:measurement(divi_tr_t1920, divine_marriage_command__substitutionist_reading, theater_ratio, 1920, 0.43).
narrative_ontology:measurement(divi_tr_t1930, divine_marriage_command__substitutionist_reading, theater_ratio, 1930, 0.44).
narrative_ontology:measurement(divi_tr_t1940, divine_marriage_command__substitutionist_reading, theater_ratio, 1940, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t1890, divine_marriage_command__substitutionist_reading, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(divi_be_t1900, divine_marriage_command__substitutionist_reading, base_extractiveness, 1900, 0.75).
narrative_ontology:measurement(divi_be_t1910, divine_marriage_command__substitutionist_reading, base_extractiveness, 1910, 0.8).
narrative_ontology:measurement(divi_be_t1920, divine_marriage_command__substitutionist_reading, base_extractiveness, 1920, 0.83).
narrative_ontology:measurement(divi_be_t1930, divine_marriage_command__substitutionist_reading, base_extractiveness, 1930, 0.84).
narrative_ontology:measurement(divi_be_t1940, divine_marriage_command__substitutionist_reading, base_extractiveness, 1940, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1890, divine_marriage_command__substitutionist_reading, suppression_requirement, 1890, 0.75).
narrative_ontology:measurement(divi_su_t1900, divine_marriage_command__substitutionist_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(divi_su_t1910, divine_marriage_command__substitutionist_reading, suppression_requirement, 1910, 0.85).
narrative_ontology:measurement(divi_su_t1920, divine_marriage_command__substitutionist_reading, suppression_requirement, 1920, 0.88).
narrative_ontology:measurement(divi_su_t1930, divine_marriage_command__substitutionist_reading, suppression_requirement, 1930, 0.89).
narrative_ontology:measurement(divi_su_t1940, divine_marriage_command__substitutionist_reading, suppression_requirement, 1940, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, each representing a distinct structural interpretation of the Manifesto's impact on marriage doctrine within the religious community.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
