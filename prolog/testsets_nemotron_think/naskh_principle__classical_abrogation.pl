% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh: Chronological Abrogation of Quranic Verses
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The classical naskh (abrogation) principle holds that later-revealed
 *   Quranic verses abrogate earlier verses on the same legal or theological
 *   topic, establishing a chronological hierarchy of legal force. Developed
 *   in the 2nd/8th–4th/10th centuries as a systematic solution to apparent
 *   Quranic contradictions, it became the dominant hermeneutic in Sunni usul
 *   al-fiqh (legal theory). The constraint coordinates legal certainty by
 *   fixing which rulings are operative, but extracts interpretive
 *   flexibility: verses deemed 'mansukh' (abrogated) lose legal force while
 *   retaining spiritual value, and the authority to determine chronology and
 *   abrogation scope concentrates in classical scholarly institutions. Modern
 *   reformist readings (contextual harmonization, progressive restriction)
 *   contest both the chronology and the abrogation logic, arguing all verses
 *   remain contextually valid or represent progressive restriction rather
 *   than invalidation. The constraint is claimed as tangled_rope: genuine
 *   coordination (contradiction resolution) coexists with asymmetric
 *   extraction (classical authority structures benefit, interpretive
 *   alternatives are suppressed).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.58).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh: Chronological Abrogation of Quranic Verses").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, 'cc904607-fc72-4ced-af91-b4e987122493').
narrative_ontology:cs_kernel_codification('cc904607-fc72-4ced-af91-b4e987122493', formalized).
narrative_ontology:cs_authority_grounding('cc904607-fc72-4ced-af91-b4e987122493', lineage).
narrative_ontology:cs_interpretation_layer_present('cc904607-fc72-4ced-af91-b4e987122493').
narrative_ontology:cs_reading_relation('cc904607-fc72-4ced-af91-b4e987122493', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_reading_relation('cc904607-fc72-4ced-af91-b4e987122493', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('cc904607-fc72-4ced-af91-b4e987122493', foundational, chronological_revelation_determines_legal_hierarchy).
narrative_ontology:cs_axiom_status(chronological_revelation_determines_legal_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('cc904607-fc72-4ced-af91-b4e987122493', chronological_revelation_determines_legal_hierarchy, conventional).
narrative_ontology:cs_axiom('cc904607-fc72-4ced-af91-b4e987122493', secondary, mansukh_verses_retain_spiritual_not_legal_force).
narrative_ontology:cs_axiom_status(mansukh_verses_retain_spiritual_not_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('cc904607-fc72-4ced-af91-b4e987122493', mansukh_verses_retain_spiritual_not_legal_force, conventional).
narrative_ontology:cs_reference_frame('cc904607-fc72-4ced-af91-b4e987122493', classical_usul_settlement).
narrative_ontology:cs_drift_state('cc904607-fc72-4ced-af91-b4e987122493', contemporary_reformist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cc904607-fc72-4ced-af91-b4e987122493', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_fuqaha).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, traditional_madhhabs).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, lay_muslims_seeking_certainty).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, modern_reformist_scholars).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, interpretive_flexibility).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_advocates).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, gradual_divine_legislation).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, scholarly_consensus_authority).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, legal_certainty_over_textual_plurality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classical jurists (2nd–10th c. CE) who developed naskh doctrine as a systematic hermeneutic. They determined chronology (nasikh/mansukh), defined abrogation scope, and established the methodological authority of usul al-fiqh. Their rulings became the foundation of madhhab law. They benefit from the constraint by controlling the interpretive gateway to legal authority; exit is arbitrage-grade (they built the system).
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_fuqaha, agenda_setter,
    institutional, generational, arbitrage, global).

% The four Sunni schools of law (Hanafi, Maliki, Shafi'i, Hanbali) and their institutional successors. Their legal corpora are built on classical naskh determinations. They benefit from the constraint's stability — their authority derives from transmitting the 'settled' hierarchy. Exit is identity-locked: abandoning naskh undermines the school's methodological coherence and historical legitimacy.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, traditional_madhhabs, beneficiary,
    institutional, generational, identity_locked, global).

% Ordinary Muslims who rely on clear, authoritative rulings for worship, family law, and commercial transactions. The constraint delivers coordination: a single operative ruling per topic instead of apparent contradictions. They also pay through rigidity — rulings abrogated in context (e.g., gradual alcohol prohibition) become permanently fixed in restrictive form. Exit is constrained: alternative interpretations exist but lack institutional authentication.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, lay_muslims_seeking_certainty, beneficiary,
    moderate, biographical, constrained, global).

% Contemporary scholars (e.g., Fazlur Rahman, Abdullahi An-Na'im, Amina Wadud, Khaled Abou El Fadl) who argue for contextual harmonization or progressive restriction. They bear the constraint's extraction: their interpretations are excluded from official fatwa bodies, state-appointed ulama councils, and mainstream madrasa curricula. Career advancement requires tacit adherence to classical naskh. Exit is constrained: they can publish academically but not authoritatively within the tradition.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, modern_reformist_scholars, payer,
    moderate, biographical, constrained, global).

% The abstract capacity for the Quranic text to sustain multiple valid readings across contexts. The classical naskh doctrine structurally collapses this plurality into a single chronological hierarchy. Once a verse is declared mansukh, its legal voice is silenced permanently. This is not an agent but a structural victim — the constraint's extraction is the foreclosure of the text's polyvalence.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, interpretive_flexibility, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(naskh_principle__classical_abrogation, interpretive_flexibility).

% Scholars and believers troubled by the theological implication that God 'changes His mind' or that earlier revelation was 'mistaken.' The classical doctrine mitigates this via 'divine wisdom in gradual legislation,' but the tension remains. They pay through cognitive dissonance and the need for secondary theological harmonization. Exit is constrained: rejecting naskh requires adopting a minority hermeneutic with limited institutional support.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_coherence_advocates, payer,
    moderate, generational, constrained, global).

% Academic scholars (e.g., Nöldeke, Welch, Neuwirth, Sinai) who analyze naskh historically: tracing its development from a limited exegetical tool to a comprehensive legal theory, comparing with biblical abrogation doctrines, and assessing chronological reliability. They neither collect nor pay — they map the constraint's genealogy and structural dynamics.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, western_quranic_studies, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves apparent contradictions in Quranic legal rulings by establishing a chronological hierarchy: the later-revealed verse governs, the earlier is superseded. This enables a single, coherent body of positive law (sharia) from a revelation spanning 23 years with changing circumstances.
% TRANSFER_FUNCTION: Moves interpretive authority and legal determinacy from the text's plural meanings to the chronological-abrogation hierarchy controlled by classical scholarly institutions. The cost is the permanent silencing of 'mansukh' verses' legal voice; the benefit is a fixed, authoritative ruling per topic.
% ABSENT_VOICES: Pre-classical companions and successors who treated naskh as rare and specific (not a general principle); early Kufan and Basran jurists who favored contextual specification over chronological supersession; contemporary Muslim communities in non-state contexts who practice informal harmonization without institutional authentication; women's collectives whose reformist tafsir is excluded from official fatwa production.
% DISAPPEARANCE_RATIONALE: If classical naskh vanished overnight, the madhhab legal corpora would lose their primary contradiction-resolution mechanism. Fatwa councils would face immediate pressure to re-adjudicate hundreds of rulings based on 'mansukh' verses (e.g., alcohol penalty, inheritance shares, warfare rules). Reformist hermeneutics would gain institutional legitimacy. The global Islamic legal field would reorganize around contextual or progressive readings.
% FOUNDING_PROBLEM: Early Muslim community faced apparent Quranic contradictions: verses permitting alcohol (2:219, 4:43) vs. prohibiting it (5:90); verses permitting defensive war only (22:39) vs. commanding offensive war (9:5); verses on inheritance with different shares. The community needed a coherent positive law for the rapidly expanding caliphate. Classical naskh provided a systematic method: determine revelation order, later verse governs.
% FOUNDING_PROBLEM_CORROBORATION: Classical usul texts (al-Shafi'i's Risala, al-Ghazali's Mustasfa) attest the problem was live and naskh solved it. Modern historians (Hallaq, Weiss, Melchert) corroborate from outside the beneficiary set: naskh doctrine expanded alongside state-building needs, not merely textual contradictions. Reformist scholars attest the founding problem (legal coherence) remains live but the classical solution has become the problem — it forecloses context-sensitive readings the founding problem never required.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the constraint's transfer of interpretive authority from the text's plural meanings to a chronologically determined hierarchy controlled by classical institutions. Suppression (0.58) is moderate: alternative readings exist but face institutional marginalization (fatwa councils, madrasa curricula, state-appointed ulama). Theater ratio (0.35) captures performative adherence to classical methodology even where scholars privately favor contextual readings. Accessibility collapse (0.72) is high: once the chronological-abrogation framework is accepted, the legal force of 'mansukh' verses collapses nearly completely. Resistance (0.48) is significant and growing: modern reformist movements, feminist tafsir, and historical-critical Qur'anic studies explicitly contest the framework. Measurements show rising extractiveness and suppression over ~200 years (formative to late classical to modern), with theater increasing as the coordination function atrophies relative to the authority-maintenance function.
 *
 * PERSPECTIVAL GAP:
 *   From the classical fuqaha seat, the constraint is genuine coordination: it resolves genuine contradictions in the revelation and provides the legal certainty necessary for a functioning sharia. From the modern reformist seat, the same structure operates as enforced extraction: the chronology is often uncertain, the abrogation logic privileges classical authority over textual evidence, and the 'coordination' serves to entrench patriarchal/commercial/penal rulings that reformists seek to re-read. The engine computes this divergence from the structural data — the claimed type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical fuqaha and traditional madhhabs are structural beneficiaries (agenda_setter/beneficiary): they control the chronology determinations, define abrogation scope, and derive institutional authority from being the gatekeepers of the 'settled' hierarchy. Lay Muslims seeking certainty are beneficiaries (coordination function delivers clear rulings) but also payers (rigidity in changing contexts). Modern reformist scholars are payers/victims: their interpretive projects are structurally excluded from authoritative fatwa production. Theological coherence advocates (those troubled by apparent divine self-contradiction) are victims: the framework resolves legal contradiction but creates theological tension. Non-Muslim academics are observers. Directionality derives from who controls the nasikh/mansukh determinations and who bears the cost of foreclosed interpretive paths.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving Quranic contradictions for legal coherence) remains live, but the classical solution has accumulated extraction: the chronology determinations often reflect madhhab interests rather than historical certainty, and the abrogation count expanded from ~5 verses (early) to ~100+ (late classical), suggesting mandate creep. The constraint persists not because the founding problem is solved, but because the authority structure built on it extracts benefit from its own maintenance. This is mandatrophy: the mandate (resolve contradictions) has atrophied into a self-justifying authority mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the classical abrogation reading a distinct constraint from the contextual harmonization and progressive restriction readings of the same naskh kernel?',
    'Structural comparison of beneficiary/victim sets, coordination/extraction profiles, and drift trajectories across the three readings. If ε values and stakeholder situations differ materially, they are distinct constraints per ε-invariance.',
    'Confirms this JSON models one reading only; sibling readings require separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel decomposition: this constraint is the classical_abrogation reading of kernel naskh_principle.').

omega_variable(
    abrogation_scope_ambiguity,
    'Does the classical naskh doctrine apply to all legal/theological topics uniformly, or only to specific domains (ritual vs. commercial vs. penal)?',
    'Survey classical usul al-fiqh texts for domain-restricted naskh claims versus universalist claims; compare with historical fatwa practice.',
    'If domain-restricted, the constraint''s extraction and suppression are lower than a universalist reading implies; coordination function is more targeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_scope_ambiguity, empirical, 'Whether the abrogation constraint''s scope is universal or domain-limited in classical doctrine.').

omega_variable(
    chronology_epistemic_access,
    'How reliably can chronological revelation order (nasikh/mansukh) be established for disputed verse pairs?',
    'Compare classical asbab al-nuzul literature, hadith chronologies, and modern historical-critical Qur''anic studies on contested pairs (e.g., 2:256 vs. 9:5, 4:15 vs. 24:2).',
    'If chronology is irreducibly uncertain for key pairs, the constraint''s coordination function degrades (genuine uncertainty about which verse governs) and extraction increases (authority fills the gap).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chronology_epistemic_access, empirical, 'Epistemic reliability of the chronological premise that drives the abrogation hierarchy.').

omega_variable(
    suppression_mechanism_scholarly_exclusion,
    'Is the marginalization of non-abrogation readings structural (institutional gatekeeping in madrasas, fatwa councils) or internalized (reformists self-censor due to heresy accusations)?',
    'Track career trajectories of scholars proposing harmonization/progressive readings: institutional sanctions vs. voluntary withdrawal; survey anonymous reformist scholars on perceived constraints.',
    'If internalized dominates, suppression persists beyond institutional reform; effective suppression exceeds structural measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_scholarly_exclusion, empirical, 'Structural vs. internalized suppression of alternative naskh readings in contemporary Islamic discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_classical_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(naskh_classical_tr_t50, naskh_principle__classical_abrogation, theater_ratio, 50, 0.2).
narrative_ontology:measurement(naskh_classical_tr_t100, naskh_principle__classical_abrogation, theater_ratio, 100, 0.25).
narrative_ontology:measurement(naskh_classical_tr_t150, naskh_principle__classical_abrogation, theater_ratio, 150, 0.3).
narrative_ontology:measurement(naskh_classical_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(naskh_classical_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(naskh_classical_be_t50, naskh_principle__classical_abrogation, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(naskh_classical_be_t100, naskh_principle__classical_abrogation, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(naskh_classical_be_t150, naskh_principle__classical_abrogation, base_extractiveness, 150, 0.62).
narrative_ontology:measurement(naskh_classical_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(naskh_classical_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(naskh_classical_su_t50, naskh_principle__classical_abrogation, suppression_requirement, 50, 0.35).
narrative_ontology:measurement(naskh_classical_su_t100, naskh_principle__classical_abrogation, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(naskh_classical_su_t150, naskh_principle__classical_abrogation, suppression_requirement, 150, 0.52).
narrative_ontology:measurement(naskh_classical_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).

% DUAL FORMULATION NOTE:
% Constraint family: naskh_principle kernel decomposed into three readings. Classical_abrogation (this story) claims chronological supersession with ε=0.68 (tangled_rope). Contextual_harmonization claims no abrogation, contextual specification only — lower ε, rope-like. Progressive_restriction claims divine pedagogy of restriction — intermediate ε, scaffold-like. The three differ in beneficiary/victim structure, coordination/extraction balance, and drift trajectory. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__classical_abrogation, institutional, 0.15).
constraint_indexing:directionality_override(naskh_principle__classical_abrogation, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
