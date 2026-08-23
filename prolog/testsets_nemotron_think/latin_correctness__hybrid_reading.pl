% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__hybrid_reading, []).

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
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Classical Norms for Literary Domains; Medieval Forms Legitimate for Technical Domains
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of Latin correctness — dominant from the Carolingian
 *   reform through the early modern period — bifurcates legitimacy: classical
 *   norms (Cicero, Caesar, Virgil) govern literary and rhetorical domains
 *   (poetry, history, oratory, epistolography), while medieval Latin forms
 *   remain legitimate for technical and practical domains (medicine, law,
 *   administration, natural philosophy). This constraint structures the
 *   entire Latinate intellectual world for nine centuries. It presents as
 *   coordination (a shared literary standard enabling transregional
 *   communication) but operates as extraction: technical writers must perform
 *   classical competence in paratexts to gain legitimacy, while their working
 *   texts legitimately use medieval vocabulary. The constraint is actively
 *   enforced through humanist curricula, publication gatekeeping, and
 *   patronage networks. Its claimed type is tangled_rope — genuine
 *   coordination function plus asymmetric extraction requiring active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.42).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.55).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Classical Norms for Literary Domains; Medieval Forms Legitimate for Technical Domains").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b').
narrative_ontology:cs_kernel_codification('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', formalized).
narrative_ontology:cs_authority_grounding('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', lineage).
narrative_ontology:cs_interpretation_layer_present('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b').
narrative_ontology:cs_reading_relation('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', latin_correctness__rupture_reading, forecloses).
narrative_ontology:cs_axiom('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', foundational, bifurcated_legitimacy_by_domain).
narrative_ontology:cs_axiom_status(bifurcated_legitimacy_by_domain, holdable).
narrative_ontology:cs_axiom_grounding('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', bifurcated_legitimacy_by_domain, conventional).
narrative_ontology:cs_axiom('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', secondary, technical_latin_autonomy).
narrative_ontology:cs_axiom_status(technical_latin_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', technical_latin_autonomy, empirically_contingent).
narrative_ontology:cs_reference_frame('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', carolingian_bifurcated_standard).
narrative_ontology:cs_drift_state('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', early_modern_humanist_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2378d31a-a5ce-4ce1-b5d7-2f30dfbc9d1b', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, literary_humanists).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classical_scholars).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medical_practitioners).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, legal_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, medical_practitioners).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, medieval_latin_practitioners).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_differentiated_linguistic_legitimacy).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, classical_corpus_as_literary_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce classical Latin norms for poetry, oratory, history, and epistolography. Control access to patronage, publication, and academic positions through mastery of Cicero and Virgil. Their authority rests on claiming direct lineage to antiquity.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, literary_humanists, agenda_setter,
    powerful, biographical, constrained, regional).

% Produce editions, commentaries, and grammatical treatises that codify the classical standard. Benefit professionally from the constraint's demand for philological expertise. Can move between courts and universities where the standard prevails.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_scholars, beneficiary,
    organized, generational, mobile, regional).

% Physicians, lawyers, and administrators who write in Latin for practical purposes. Pressured to adopt classical forms in prefaces and dedications to gain literary legitimacy, while their working texts use medieval vocabulary and syntax. Mastering classical norms costs years of study with no practical payoff in their domain.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_writers, payer,
    moderate, biographical, constrained, regional).

% Write case histories, pharmacopeias, and anatomical treatises. Medieval medical Latin has precise technical vocabulary lacking in classical sources. Forced to choose: use effective medieval terminology (legitimate in practice, stigmatized in prefaces) or invent classical neologisms (legitimate in form, obscure in practice).
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medical_practitioners, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, medical_practitioners, beneficiary).

% Produce charters, statutes, and court records. Medieval legal Latin has evolved precise procedural terminology. Humanist pressure to classicize creates ambiguity in binding documents. Resist through institutional inertia but face legitimacy costs when documents are judged 'barbarous' by literary standards.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, legal_administrators, payer,
    organized, generational, constrained, regional).

% Scholars, clerics, and technicians who work primarily in technical domains. The hybrid reading explicitly legitimizes their Latin as appropriate to its purpose. They escape the classical standard's demands without losing all scholarly standing.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_latin_practitioners, beneficiary,
    organized, generational, mobile, regional).

% Adherents of the rupture reading who insist ALL Latin must conform to reconstructed classical norms. View the hybrid reading as compromise and corruption. Excluded from institutional power because their standard is unusable for technical communication, but they set the aspirational ceiling that humanists enforce.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classical_purists, excluded,
    moderate, biographical, trapped, regional).

% Scholars who view medieval Latin as organic evolution, not corruption. They see the hybrid reading as artificial bifurcation. Their position is marginalized by both humanist and purist camps but persists in monastic and some university contexts.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, continuity_adherents, observer,
    organized, generational, mobile, regional).

% Contemporary historians of Latin who analyze the constraint from outside. They document the bifurcation's historical operation but have no stake in its enforcement. Their analytical frame reveals the status hierarchy the constraint creates.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, modern_philologists, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, literary_humanists).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared literary standard (classical Latin) enabling transregional scholarly communication and textual stability across Western Europe, while allowing practical domains (medicine, law, administration) to use living medieval Latin with domain-appropriate vocabulary and syntax.
% TRANSFER_FUNCTION: Moves legitimacy and scholarly status from technical writers (who must master dead classical forms for literary recognition in prefaces, dedications, and theoretical sections) to literary humanists and classical scholars (who gatekeep the classical standard through education, publication, and patronage). Technical writers pay in study-time and expressive constraint; humanists collect in authority and professional monopoly.
% ABSENT_VOICES: Vernacular writers excluded from Latin discourse entirely — the constraint only operates within Latinity. Women scholars largely excluded from humanist circles and university chairs; their Latin (when they wrote) was judged by the same bifurcated standard but without access to the classical education that made compliance possible. Non-European Latin users (e.g., in missionary contexts) subject to the standard without representation in its formation.
% DISAPPEARANCE_RATIONALE: If the bifurcated standard vanished overnight, two outcomes would compete: (1) full classicization — humanists extend classical norms to all domains, making technical communication harder but unifying the standard; (2) full vernacularization — technical domains abandon Latin for vernaculars, ending Latin's role as practical lingua franca. The literary domain would either become a pure classical preserve or dissolve into vernacular literature. The historical outcome was (2) for technical domains, (1) for literary — the constraint's disappearance is the Renaissance-to-Early-Modern transition.
% FOUNDING_PROBLEM: After Carolingian fragmentation, Latin had diverged regionally. Scholars needed a stable transregional literary standard for poetry, history, and philosophy — the classical corpus provided this. Simultaneously, practical administration, medicine, and law needed a usable living Latin with precise technical vocabulary that the classical corpus lacked. The hybrid reading solved both: freeze the literary standard at Cicero/Virgil, let technical Latin evolve.
% FOUNDING_PROBLEM_CORROBORATION: Carolingian reform documents (Alcuin's letters, Council of Tours 813) explicitly impose classical norms for literary texts while technical manuscripts continue medieval usage. Petrarch's letters attest the humanist revival's conscious classicizing program. Modern scholarship (Waquet 'Latin or the Empire of a Sign', Jensen 'Reading and Writing in the Renaissance') corroborates the domain bifurcation as deliberate policy, not organic drift. No beneficiary-group source disputes the historical facts; they dispute the evaluation.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).
:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: technical writers pay real costs (years of classical study, expressive constraint in prefaces) but the constraint also provides genuine coordination (stable literary standard). Suppression (0.55) is moderate: enforcement operates through institutional gatekeeping (university chairs, printing privileges, patronage) not violence. Theater ratio (0.38) reflects that classical performance is partially real (humanists genuinely master the corpus) but increasingly performative as spoken Latin diverges. Accessibility collapse (0.52) is partial: medieval forms remain fully accessible and legitimate in technical domains; only literary domain collapses alternatives. Resistance (0.48) is significant: technical writers resist classicization of working texts; legal and medical corpora maintain medieval forms throughout the period. The measurement series shows extraction and theater rising with printing press standardization (1450+), suppression peaking at high humanism (1550), then stabilizing as vernaculars displace technical Latin.
 *
 * PERSPECTIVAL GAP:
 *   From the literary_humanist seat: the constraint IS the coordination function — a hard-won standard that makes European scholarship possible. From the technical_writer seat: the same structure is a status tax — they must perform a dead dialect to be taken seriously, while their actual work uses a living technical language. From the medieval_latin_practitioner seat: the constraint is a protective carve-out — their domain is explicitly legitimized. From the classical_purist seat: the constraint is a betrayal — it concedes ground to corruption. The engine computes these divergences from power/exit/role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Literary_humanists and classical_scholars are beneficiaries (d near 0.0) — they collect status, patronage, and professional monopoly from the standard they enforce. Technical_writers, medical_practitioners, and legal_administrators are payers (d near 0.8-0.9) — they bear study costs and expressive constraints, with constrained exit (vernacular not yet viable for transregional technical communication). Medieval_latin_practitioners are beneficiaries in their domain (d ~0.2) but excluded from literary prestige. Classical_purists are excluded (d irrelevant — they're outside the constraint's operation). Continuity_adherents and modern_philologists are observers (d=0.5 analytical). The hybrid reading's bifurcation creates this split: same person can be payer in literary preface and beneficiary in medical treatise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transregional literary standard + usable technical Latin) is dead: Latin ceased to be a living scholarly lingua franca. The constraint persists as academic convention (classical Latin still taught as 'correct' Latin; medieval Latin as separate subfield). This is mandatrophy — the arrangement outlives its function. The hybrid reading prevents mislabeling: it is not pure coordination (extraction is real) nor pure extraction (coordination function was real and historically necessary). The tangled_rope classification captures the historical layering: coordination function atrophied, extraction layer remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the hybrid reading a structurally distinct constraint, or a pragmatic compromise position that collapses under pressure into either continuity or rupture?',
    'Test whether the domain bifurcation holds as a stable institutional equilibrium: do technical writers consistently produce medieval-form working texts AND classical-form prefaces across the period, or does pressure push them toward one pole? Corpus analysis of preface vs. body language in medical/legal texts 1300-1700.',
    'If the bifurcation is unstable, the hybrid reading reduces to a transitional state between continuity and rupture, not a distinct constraint type. This would change the claimed_type from tangled_rope to scaffold (transitional) and alter the omega variables for sibling relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the hybrid reading''s domain bifurcation is a stable structural position or an unstable compromise.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional gatekeeping: university curricula, printing privileges, patronage) or internalized (technical writers'' self-doubt about their Latin''s legitimacy)?',
    'Compare suppression in domains with strong institutional enforcement (university medicine) vs. weak enforcement (private legal practice). If suppression persists in low-enforcement domains, internalized component is significant. Analyze private correspondence of technical writers for ''linguistic anxiety'' markers.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint operates through the target''s own cognition. This would increase effective extraction for payer seats and strengthen the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Latin correctness constraint.').

omega_variable(
    extraction_measurement_status_hierarchy,
    'How to quantify the status/legitimacy extraction when the transfer is not monetary but symbolic (scholarly reputation, patronage access, publication acceptance)?',
    'Build proxy metrics: (1) career outcome differential between technical writers who classicize prefaces vs. those who don''t; (2) citation/edition frequency of classical-preface vs. medieval-preface works; (3) patronage records showing classical competence as criterion. Requires prosopographic databases not yet fully compiled.',
    'If extraction is quantifiably high, tangled_rope classification strengthens. If extraction is negligible (classical preface is cheap signaling), the constraint may be closer to rope. The current 0.42 is a qualitative judgment awaiting quantitative calibration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_status_hierarchy, empirical, 'Quantifying symbolic extraction in the Latin correctness constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 800, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(latin_correctness_hybrid_tr_t800, latin_correctness__hybrid_reading, theater_ratio, 800, 0.15).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1000, latin_correctness__hybrid_reading, theater_ratio, 1000, 0.18).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1150, latin_correctness__hybrid_reading, theater_ratio, 1150, 0.22).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1300, latin_correctness__hybrid_reading, theater_ratio, 1300, 0.28).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1450, latin_correctness__hybrid_reading, theater_ratio, 1450, 0.33).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1550, latin_correctness__hybrid_reading, theater_ratio, 1550, 0.36).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1650, latin_correctness__hybrid_reading, theater_ratio, 1650, 0.38).
narrative_ontology:measurement(latin_correctness_hybrid_tr_t1700, latin_correctness__hybrid_reading, theater_ratio, 1700, 0.38).

% Extraction over time
narrative_ontology:measurement(latin_correctness_hybrid_be_t800, latin_correctness__hybrid_reading, base_extractiveness, 800, 0.25).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1000, latin_correctness__hybrid_reading, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1150, latin_correctness__hybrid_reading, base_extractiveness, 1150, 0.32).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1300, latin_correctness__hybrid_reading, base_extractiveness, 1300, 0.35).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1450, latin_correctness__hybrid_reading, base_extractiveness, 1450, 0.4).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1550, latin_correctness__hybrid_reading, base_extractiveness, 1550, 0.42).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1650, latin_correctness__hybrid_reading, base_extractiveness, 1650, 0.41).
narrative_ontology:measurement(latin_correctness_hybrid_be_t1700, latin_correctness__hybrid_reading, base_extractiveness, 1700, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(latin_correctness_hybrid_su_t800, latin_correctness__hybrid_reading, suppression_requirement, 800, 0.35).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1000, latin_correctness__hybrid_reading, suppression_requirement, 1000, 0.38).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1150, latin_correctness__hybrid_reading, suppression_requirement, 1150, 0.42).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1300, latin_correctness__hybrid_reading, suppression_requirement, 1300, 0.48).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1450, latin_correctness__hybrid_reading, suppression_requirement, 1450, 0.52).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1550, latin_correctness__hybrid_reading, suppression_requirement, 1550, 0.55).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1650, latin_correctness__hybrid_reading, suppression_requirement, 1650, 0.54).
narrative_ontology:measurement(latin_correctness_hybrid_su_t1700, latin_correctness__hybrid_reading, suppression_requirement, 1700, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.08).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three readings with distinct ε values: continuity_reading (low ε, mountain-like coordination), hybrid_reading (moderate ε, tangled_rope coordination+extraction), rupture_reading (high ε, snare-like extraction). The hybrid reading influences both siblings: it coexists_with continuity_reading (different scholarly communities hold each), but forecloses rupture_reading in any single framework (cannot simultaneously hold 'medieval forms legitimate in technical domains' and 'all medieval forms are corruption'). The hybrid reading's axioms (bifurcated_legitimacy_by_domain, technical_latin_autonomy) are unique to this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__hybrid_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
