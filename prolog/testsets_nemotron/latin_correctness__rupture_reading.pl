% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purity Standard (Rupture Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   The rupture reading of Latin correctness treats classical Latin as a
 *   fixed, reconstructible standard (Ciceronian/Golden Age) and delegitimizes
 *   all post-classical usage as corruption. This constraint operated from the
 *   14th through 17th centuries as the editorial and pedagogical norm of the
 *   humanist movement and its institutional successors. It coordinates a
 *   pan-European textual standard (genuine coordination function) while
 *   extracting epistemic authority and professional legitimacy from medieval
 *   Latin users who cannot meet the reconstructed norm (asymmetric
 *   extraction). The constraint is actively enforced through university
 *   statutes, printing privileges, and ecclesiastical censorship.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.78).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.72).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purity Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '7763c2f7-6e09-4173-b193-47b3a24d292b').
narrative_ontology:cs_kernel_codification('7763c2f7-6e09-4173-b193-47b3a24d292b', fixed_text).
narrative_ontology:cs_authority_grounding('7763c2f7-6e09-4173-b193-47b3a24d292b', lineage).
narrative_ontology:cs_interpretation_layer_present('7763c2f7-6e09-4173-b193-47b3a24d292b').
narrative_ontology:cs_reading_relation('7763c2f7-6e09-4173-b193-47b3a24d292b', latin_correctness__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('7763c2f7-6e09-4173-b193-47b3a24d292b', latin_correctness__hybrid_reading, influences).
narrative_ontology:cs_axiom('7763c2f7-6e09-4173-b193-47b3a24d292b', foundational, ciceronian_usage_as_exclusive_norm).
narrative_ontology:cs_axiom_status(ciceronian_usage_as_exclusive_norm, holdable).
narrative_ontology:cs_axiom_grounding('7763c2f7-6e09-4173-b193-47b3a24d292b', ciceronian_usage_as_exclusive_norm, deontological).
narrative_ontology:cs_axiom('7763c2f7-6e09-4173-b193-47b3a24d292b', foundational, post_classical_latin_as_degeneration).
narrative_ontology:cs_axiom_status(post_classical_latin_as_degeneration, holdable).
narrative_ontology:cs_axiom_grounding('7763c2f7-6e09-4173-b193-47b3a24d292b', post_classical_latin_as_degeneration, empirically_contingent).
narrative_ontology:cs_reference_frame('7763c2f7-6e09-4173-b193-47b3a24d292b', ciceronian_canonical_standard).
narrative_ontology:cs_drift_state('7763c2f7-6e09-4173-b193-47b3a24d292b', post_tridentine_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7763c2f7-6e09-4173-b193-47b3a24d292b', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classicist_establishment).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_educational_reformers).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, renaissance_print_culture).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_technical_practitioners).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, ecclesiastical_administrators).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, non_elite_latin_users).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, ciceronian_supremacy_doctrine).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, textual_reconstruction_as_recovery).
narrative_ontology:constraint_vindicates(latin_correctness__rupture_reading, linguistic_purity_as_moral_virtue).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls university curricula, editorial standards, and patronage networks. Defines correctness by reference to a reconstructed Ciceronian norm. Extracts prestige, career advancement, and institutional authority from gatekeeping Latinity. Can pivot to vernacular scholarship if needed (arbitrage-grade exit).
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, classicist_establishment, agenda_setter,
    institutional, generational, arbitrage, continental).

% Gain professional identity and pedagogical coherence by aligning with the classical standard. Their textbooks, grammars, and teaching positions depend on the purity norm. Mobile exit: can shift to other humanist projects or vernacular education if the constraint weakens.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, humanist_educational_reformers, beneficiary,
    organized, biographical, mobile, continental).

% Printers and publishers profit from standardized classical editions that displace medieval manuscripts. The constraint creates a stable, authoritative product line. Institutional power through control of textual transmission. Arbitrage exit: can pivot to vernacular printing if Latin market collapses.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, renaissance_print_culture, beneficiary,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__rupture_reading, renaissance_print_culture, agenda_setter).

% Their linguistic practice is retroactively delegitimized as 'corruption.' Must either adopt humanist norms (costly retraining, loss of inherited technical vocabulary) or be marginalized. Constrained exit: embedded in ecclesiastical and academic institutions that cannot easily abandon Latin.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, medieval_scholars, payer,
    moderate, biographical, constrained, continental).

% Legal notaries, medical practitioners, merchants, and artisans who use Latin for technical documentation. The purity standard makes their working Latin 'incorrect' by definition, yet they lack the leisure for humanist reconstruction. Trapped: their professional legitimacy requires Latin, but the constraint defines their Latin as illegitimate.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, vernacular_technical_practitioners, payer,
    powerless, immediate, trapped, regional).

% Church bureaucracy runs on medieval Latin formulae. The classical standard threatens the validity of centuries of legal, liturgical, and administrative documents. Constrained exit: canonical Latin is structurally required by their institutional role; switching to vernacular or classical Latin entails massive transition costs and doctrinal risk.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, ecclesiastical_administrators, payer,
    organized, generational, constrained, global).

% Clergy, minor officials, students who inherit medieval Latin as a living skill. The purity standard renders their competence 'corrupt' without providing accessible classical training. Trapped: no exit from the linguistic demands of their station, no resources for reconstruction.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, non_elite_latin_users, payer,
    powerless, immediate, trapped, local).

% Modern linguist/historian analyzing the constraint from outside. Sees the classical standard as a constructed ideological project, not a natural linguistic fact. No stake in the contest; exit is analytical by definition.
narrative_ontology:constraint_stakeholder(latin_correctness__rupture_reading, philological_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative textual standard for pan-European learned communication, enabling textual criticism, editorial practice, and a shared canonical corpus across fragmented political units.
% TRANSFER_FUNCTION: Moves epistemic authority, patronage, and professional legitimacy from medieval Latin users (scholars, administrators, technical practitioners) to classicist editors, humanist educators, and print entrepreneurs — by declaring the inherited language 'corrupt' and the reconstructed classical form the only legitimate Latin.
% ABSENT_VOICES: Vernacular-language intellectuals who argued for mother-tongue scholarship (e.g., Dante, later Galileo) — excluded from the Latin debate by the very terms of the constraint. Also the vast majority of medieval manuscript producers whose work is erased by the 'corruption' label.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished, medieval Latin would be re-legitimized as a continuous tradition. Ecclesiastical and technical domains would retain their working language without stigma. The humanist editorial industry would lose its central justification. The pan-European Latin republic of letters would reorganize around a pluricentric standard rather than a single reconstructed norm.
% FOUNDING_PROBLEM: Humanist scholars encountered a textual landscape where 'Latin' meant a millennium of divergent manuscript traditions. They needed a single authoritative standard to ground textual criticism, pedagogy, and a revived classical culture — and chose to reconstruct Cicero rather than codify living usage.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (need for a stable editorial standard) is corroborated by Erasmus's own prefaces and the pre-humanist chaos of manuscript transmission attested by Lorenzo Valla. However, the status as 'dead' is corroborated by the fact that by 1600 the classical standard had been achieved and the constraint persisted as exclusion rather than construction — attested by the Jesuit Ratio Studiorum's enforcement of Ciceronianism long after textual stability was secured.
narrative_ontology:disappearance_verdict(latin_correctness__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__rupture_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__rupture_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) reflects the constraint's function as a filter that excludes the vast majority of actual Latin users from legitimacy. Suppression (0.72) is high because medieval forms are actively suppressed in print, pedagogy, and official discourse — not merely disfavored. Theater ratio (0.41) rises over time as the coordination function (textual stability) is achieved early but enforcement continues to defend the purity boundary. Accessibility collapse (0.68) is substantial: once the Ciceronian norm is internalized, medieval Latin becomes cognitively inaccessible as 'Latin.' Resistance (0.58) is moderate: medievalists, Jesuits (early), and vernacular advocates push back, but the constraint's institutional embedding is deep.
 *
 * PERSPECTIVAL GAP:
 *   From the classicist seat, the constraint is a rope: it solves the genuine coordination problem of textual instability. From the medieval scholar seat, it is a snare: their inherited competence is declared corrupt by a standard they had no hand in creating. From the vernacular practitioner seat, it is a pure extraction mechanism with no coordination benefit — they never needed Ciceronian Latin for notarial formulae. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Classicist establishment and print culture are structural beneficiaries (d ~ 0.1-0.2): they collect rents from gatekeeping and standardized editions. Humanist reformers are beneficiaries with mobile exit (d ~ 0.25). Medieval scholars, ecclesiastical administrators are payers with constrained exit (d ~ 0.7-0.8): they bear the costs of delegitimization but cannot easily leave Latin-dependent roles. Vernacular technical practitioners and non-elite users are trapped payers (d ~ 0.9+): the constraint defines their necessary working language as illegitimate with no accessible alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (textual instability) was solved by ~1600. The constraint persists as mandatrophy: the editorial standard achieved, but the purity boundary becomes the extraction mechanism. The Jesuit educational system, having adopted the standard, becomes its enforcer — not because textual chaos threatens, but because the purity norm now structures professional advancement and institutional identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_standard,
    'Is the Ciceronian standard a recovery of a natural linguistic optimum, or a constructed ideological project masquerading as recovery?',
    'Comparative philology: if Cicero''s usage was itself one variant among many in antiquity, and the ''standard'' reflects 15th-century selection criteria, the natural-law claim collapses.',
    'If constructed, the constraint is a tangled_rope or snare with identifiable beneficiaries. If natural, it approaches a mountain (linguistic law) — but the beneficiary declarations would then trigger false_summit_mountain detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_standard, conceptual, 'Whether the classical purity norm is discovered or invented.').

omega_variable(
    coordination_necessity_of_purity,
    'Was the exclusion of medieval forms structurally necessary for the coordination function (pan-European textual stability), or was a pluricentric standard viable?',
    'Counterfactual history: examine whether the hybrid reading''s domain separation (classical for rhetoric, medieval for technical) could have stabilized without the purity enforcement.',
    'If pluricentric standard was viable, the purity enforcement is pure extraction riding on a weaker coordination claim. If not, some exclusion is coordination cost — but the specific Ciceronian form remains a choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_of_purity, conceptual, 'Whether the constraint''s extraction is structurally coupled to its coordination function.').

omega_variable(
    committer_frame_ambiguity,
    'Does the rupture reading''s delegitimization of medieval practice reflect a genuine epistemic break, or a rhetorical strategy to secure patronage for humanist scholars?',
    'Corpus analysis of humanist prefaces and dedicatory letters: track the co-occurrence of purity rhetoric with patronage requests.',
    'If rhetorical strategy, the constraint''s extractiveness is intentional design. If genuine epistemic break, extraction is a byproduct of sincere reconstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, empirical, 'Intentionality of the rupture reading''s extractive structure.').

omega_variable(
    kernel_reading_relation_continuity,
    'What is the structural relationship between the rupture reading and the continuity reading of the latin_correctness kernel?',
    'Formal analysis of whether a single intellectual framework can simultaneously hold ''medieval Latin is corruption'' and ''medieval Latin is legitimate continuation'' without contradiction.',
    'If forecloses: the readings cannot coexist in one framework — the kernel is genuinely contested at the logical level. If coexists_with: the contest is sociological (different factions), not logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_continuity, conceptual, 'Structural relation from rupture_reading to continuity_reading sibling.').

omega_variable(
    kernel_reading_relation_hybrid,
    'What is the structural relationship between the rupture reading and the hybrid reading of the latin_correctness kernel?',
    'Historical analysis of whether hybrid-position holders (e.g., early Jesuits, Melanchthon) were pressured toward rupture orthodoxy or maintained stable intermediate positions.',
    'If influences: rupture reading creates downstream pressure on hybrid positions (legitimacy erosion). If coexists_with: stable pluralism across domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_hybrid, empirical, 'Structural relation from rupture_reading to hybrid_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 1300, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcrr_tr_t1300, latin_correctness__rupture_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(lcrr_tr_t1400, latin_correctness__rupture_reading, theater_ratio, 1400, 0.18).
narrative_ontology:measurement(lcrr_tr_t1500, latin_correctness__rupture_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(lcrr_tr_t1550, latin_correctness__rupture_reading, theater_ratio, 1550, 0.32).
narrative_ontology:measurement(lcrr_tr_t1600, latin_correctness__rupture_reading, theater_ratio, 1600, 0.37).
narrative_ontology:measurement(lcrr_tr_t1650, latin_correctness__rupture_reading, theater_ratio, 1650, 0.4).
narrative_ontology:measurement(lcrr_tr_t1700, latin_correctness__rupture_reading, theater_ratio, 1700, 0.41).

% Extraction over time
narrative_ontology:measurement(lcrr_be_t1300, latin_correctness__rupture_reading, base_extractiveness, 1300, 0.35).
narrative_ontology:measurement(lcrr_be_t1400, latin_correctness__rupture_reading, base_extractiveness, 1400, 0.45).
narrative_ontology:measurement(lcrr_be_t1500, latin_correctness__rupture_reading, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(lcrr_be_t1550, latin_correctness__rupture_reading, base_extractiveness, 1550, 0.67).
narrative_ontology:measurement(lcrr_be_t1600, latin_correctness__rupture_reading, base_extractiveness, 1600, 0.73).
narrative_ontology:measurement(lcrr_be_t1650, latin_correctness__rupture_reading, base_extractiveness, 1650, 0.76).
narrative_ontology:measurement(lcrr_be_t1700, latin_correctness__rupture_reading, base_extractiveness, 1700, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(lcrr_su_t1300, latin_correctness__rupture_reading, suppression_requirement, 1300, 0.25).
narrative_ontology:measurement(lcrr_su_t1400, latin_correctness__rupture_reading, suppression_requirement, 1400, 0.35).
narrative_ontology:measurement(lcrr_su_t1500, latin_correctness__rupture_reading, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement(lcrr_su_t1550, latin_correctness__rupture_reading, suppression_requirement, 1550, 0.58).
narrative_ontology:measurement(lcrr_su_t1600, latin_correctness__rupture_reading, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(lcrr_su_t1650, latin_correctness__rupture_reading, suppression_requirement, 1650, 0.7).
narrative_ontology:measurement(lcrr_su_t1700, latin_correctness__rupture_reading, suppression_requirement, 1700, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.03).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, vernacular_rise_constraint).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, jesuit_educational_standard_constraint).

% DUAL FORMULATION NOTE:
% Part of the latin_correctness constraint family. This rupture_reading extracts from medieval users by fixing a reconstructed Ciceronian norm. The continuity_reading treats the same linguistic continuum as legitimate (low extraction, mountain-like). The hybrid_reading domain-separates (moderate extraction, tangled_rope). All three share the referent (Latin correctness) but instantiate different constraints with different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__rupture_reading, organized, 0.75).
constraint_indexing:directionality_override(latin_correctness__rupture_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
