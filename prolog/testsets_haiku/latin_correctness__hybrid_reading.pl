% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: latin_correctness__hybrid_reading
 *   human_readable: Bifurcated Latin Legitimacy (Hybrid Classical/Medieval Reading)
 *   domain: intellectual/linguistic
 *
 * SUMMARY:
 *   The bifurcated Latin legitimacy reading asserts that classical norms
 *   apply authoritatively to literary and rhetorical domains, while medieval
 *   forms remain legitimate (if subordinate) for technical and practical
 *   communication. This reading emerges from the 9th-11th centuries as a
 *   compromise between two pressures: the desire to maintain classical
 *   standards as a sign of intellectual authority and Christendom's unity,
 *   and the impossibility of requiring all Latin users to master Ciceronian
 *   style when technical communication demands functional medieval usage. The
 *   hybrid reading creates a status hierarchy (classical > medieval) while
 *   avoiding the rupture reading's demand for universal classical
 *   reconstruction or the continuity reading's acceptance of unguided
 *   medieval evolution. This constraint instantiates moderate extractiveness
 *   because the prestige transfer is real (technical writers must justify
 *   their forms), but the accommodation is also genuine (medieval forms are
 *   legitimized for their domains). The bifurcation itself is the extraction
 *   mechanism: it preserves classical authority at the cost of relegating
 *   technical domains to permanent lower status, and it requires technical
 *   writers and medieval specialists to constantly justify their language
 *   against an impossible external standard.
 *
 * KEY AGENTS:
 *   - Literary rhetoricians: Control prestige certification; benefit from bifurcation because it reserves the highest authority for their domain.
 *   - Technical writers: Bear prestige cost; must compose functionally but justify their medieval forms constantly.
 *   - Medieval specialists: Bear prestige cost; their corpus is legitimized as 'medieval' but thereby marked as secondary.
 *   - Classicizing pedagogues: Set and enforce the standards; maintain the status hierarchy through education.
 *   - Church authorities: Enforce bifurcation strategically—use classical forms for prestige texts, medieval forms for administration.
 *   - Textual authorities: Observe and transmit both forms; witness the gap between actual medieval practice and the prestige claims about it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.58).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.61).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Bifurcated Latin Legitimacy (Hybrid Classical/Medieval Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "intellectual/linguistic").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, '34172a2f-4ab8-47af-9244-68ce73fd33d4').
narrative_ontology:cs_kernel_codification('34172a2f-4ab8-47af-9244-68ce73fd33d4', fixed_text).
narrative_ontology:cs_authority_grounding('34172a2f-4ab8-47af-9244-68ce73fd33d4', lineage).
narrative_ontology:cs_interpretation_layer_present('34172a2f-4ab8-47af-9244-68ce73fd33d4').
narrative_ontology:cs_reading_relation('34172a2f-4ab8-47af-9244-68ce73fd33d4', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('34172a2f-4ab8-47af-9244-68ce73fd33d4', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('34172a2f-4ab8-47af-9244-68ce73fd33d4', foundational, classical_forms_authoritative_prestige_domains).
narrative_ontology:cs_axiom_status(classical_forms_authoritative_prestige_domains, holdable).
narrative_ontology:cs_axiom_grounding('34172a2f-4ab8-47af-9244-68ce73fd33d4', classical_forms_authoritative_prestige_domains, conventional).
narrative_ontology:cs_axiom('34172a2f-4ab8-47af-9244-68ce73fd33d4', foundational, medieval_forms_functionally_legitimate_technical_domains).
narrative_ontology:cs_axiom_status(medieval_forms_functionally_legitimate_technical_domains, holdable).
narrative_ontology:cs_axiom_grounding('34172a2f-4ab8-47af-9244-68ce73fd33d4', medieval_forms_functionally_legitimate_technical_domains, empirically_contingent).
narrative_ontology:cs_reference_frame('34172a2f-4ab8-47af-9244-68ce73fd33d4', bifurcated_latin_legitimacy).
narrative_ontology:cs_drift_state('34172a2f-4ab8-47af-9244-68ce73fd33d4', contemporary_philology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('34172a2f-4ab8-47af-9244-68ce73fd33d4', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, literary_rhetoricians).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classicizing_pedagogues).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, medieval_specialists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, church_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate within the prestige domain of rhetorical and literary composition where classical norms are canonical. Their authority rests on mastery of Ciceronian style and classical grammar. They set the standards for what counts as 'correct' Latin in prestigious contexts—ecclesiastical documents, diplomatic correspondence, humanist treatises. Benefit from bifurcation because it reserves the highest prestige tier for their domain while leaving technical domains to lesser practitioners.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, literary_rhetoricians, beneficiary,
    institutional, generational, arbitrage, continental).

% Compose in Latin for practical domains: medical treatises, mathematical texts, engineering manuals, legal charters, monastic computus. Must navigate constant pressure to approximate classical forms even when medieval usages are more functional and semantically precise for their purposes. Face institutional pressure and reputation damage if their work violates classical norms in literary circles, yet cannot abandon medieval forms without sacrificing clarity and functional communication within their own specialist communities.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_writers, payer,
    moderate, biographical, constrained, continental).

% Study and edit medieval texts using medieval Latin forms as their primary objects. Caught between scholarly honesty (medieval forms are what the texts actually contain and employ functionally) and institutional pressure from classicizing authorities who treat medieval usage as degradation. Require legitimacy for their corpus, but legitimacy is structurally withheld by the bifurcated reading—they are acknowledged to exist in a separate domain with implicit lower prestige.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, medieval_specialists, payer,
    moderate, biographical, constrained, continental).

% Teach classical Latin in cathedral schools, monasteries, and courtly circles. Set examination standards and credential writers. Enforce the bifurcation by training students that classical forms are the ideal and medieval forms are acceptable only in technical contexts—thus maintaining the status hierarchy. Control the prestige certification system.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classicizing_pedagogues, agenda_setter,
    institutional, generational, arbitrage, continental).

% Use both classical and medieval Latin strategically. Diplomatic and doctrinal texts employ classical norms for authority and prestige; practical texts and internal administration use medieval forms functionally. Benefit from bifurcation because it legitimizes both without forcing a single standard. Can demand classical form when prestige matters and accept medieval form when efficiency matters, without contradiction.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, church_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, church_authorities, beneficiary).

% Preserve, edit, and transmit texts across both domains. Witness the actual linguistic practices of technical and medieval writers and measure them against the declared norms. See the constraint most clearly from the outside: know what medieval writers actually wrote and what they were constrained to claim about it.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, textual_authorities, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, literary_rhetoricians).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared evaluative framework for Latin composition across diverse domains by anchoring prestige to classical forms while preserving functional legitimacy for medieval usage in technical contexts. Solves the problem of how to maintain unified Latin pedagogy across expanding domains (rhetoric, theology, medicine, law, computation) when the actual linguistic practices diverge.
% TRANSFER_FUNCTION: Transfers prestige and credentialing authority toward the literary/rhetorical domain and those who control classical standards. Transfers legitimacy-claim burden onto technical writers and medieval specialists, who must justify their forms as necessary deviations rather than simply using them as standard practice. Moves social standing from those who work in technical domains toward those who master classical style.
% ABSENT_VOICES: Practical medieval writers themselves are largely absent from the literati discussions that enforce the bifurcation—their defenses appear in technical treatises and margins of manuscripts, not in grammatical handbooks or pedagogical texts where legitimacy is adjudicated. Guild-trained technicians and scribes whose linguistic competence is deep within their domains but classically unpolished are structurally excluded from credentialing conversations.
% DISAPPEARANCE_RATIONALE: If the bifurcated reading vanished and a single standard (either continuity or rupture) replaced it, technical writers would either be freed to use medieval forms without prestige penalty (continuity) or forced to approximate classical forms everywhere with severe communication costs (rupture). The constraint's disappearance would redistribute prestige and reshape pedagogy. The coexistence of two legitimacy frameworks would collapse.
% FOUNDING_PROBLEM: Early medieval Latin diverged significantly from classical forms as the language was used for practical purposes and absorbed barbarian linguistic patterns. By the 9th-10th centuries, two problems emerged: (1) How to maintain classical Latin as a unifying intellectual standard across an expanding Christendom when living practice had drifted? (2) How to preserve functional communication in technical domains without requiring scribes to master classical rhetoric? The bifurcated reading solved both by declaring classical norms authoritative for prestige domains and medieval forms legitimate for technical necessity.
% FOUNDING_PROBLEM_CORROBORATION: Classicizing authorities attest the founding problem is live: Latin must remain grounded in classical forms to preserve unity and intellectual authority. Medieval specialists and technical writers attest the founding problem is partly dead: medieval forms are functionally evolved, not corrupt deviations, and the bifurcation creates arbitrary prestige hierarchies. Manuscript evidence from textual authorities shows medieval writers used their forms functionally and competently, not as degradation. Legislative and monastic records show both forms in simultaneous use according to genre, not according to chronological replacement.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) rather than high because the bifurcation is partly functional—technical domains genuinely do use medieval forms more efficiently. However, it is substantially extractive because the prestige system itself is the constraint: technical writers cannot simply use their forms without constant external invalidation. Suppression is moderate-high (0.61) because enforcing the bifurcation requires active pedagogical effort—students must be taught that classical forms are superior, that medieval forms are acceptable only in subordinate domains, that deviations from classical norms require justification. Theater ratio is high-moderate (0.48) because much of the enforcement activity is performed through teaching, credentialing, and prestige attribution rather than through direct coercion—the prestige system is theatrical in the sense that it performs a hierarchy that might collapse if questioned directly. Accessibility collapse is substantial (0.67) because the classical norms are anchored in ancient texts that are genuinely authoritative, and medieval forms are genuinely present in medieval texts; the alternatives are not invented, so collapse is not total, but the status hierarchy makes it costly to choose medieval forms even when functionally superior. Resistance is moderate (0.52) because medieval specialists and technical writers do resist—they publish their work, defend medieval forms as fit for purpose, preserve medieval texts as canonical within their domains. This resistance prevents the constraint from achieving complete suppression, but it operates at the cost of accepting lower institutional prestige.
 *
 * PERSPECTIVAL GAP:
 *   The bifurcated reading creates dramatic seat divergence. From the agenda-setter seats (classicizing pedagogues, church authorities), the arrangement is functional coordination—it preserves classical authority while accommodating practical necessity. From the payer seats (technical writers, medieval specialists), it is extraction disguised as accommodation—they are permitted to use medieval forms only by accepting permanent subordination. The literary beneficiaries experience the constraint as natural (of course classical norms are superior; they are grounded in the greatest authors). The technical payers experience it as forced extraction (our forms work better for our purposes, but we are penalized for using them). The engine derives directionality from these structural differences: beneficiaries show low d (they benefit from the prestige hierarchy), technical payers show high d (they bear its cost), medieval specialists show high d (their work is legitimized but marked as secondary). This divergence is the analytic point of constraint-story methodology: the same reading produces different effective extraction depending on which seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (literary rhetoricians, classicizing pedagogues, partly church authorities) have high arbitrage options—they can move between domains and preserve their prestige in all of them because their domain is the prestige domain. This drives their d toward the beneficiary end (~0.15-0.25). Victims (technical writers, medieval specialists) have constrained exit—leaving their technical domains means abandoning their expertise and functional value. They must compose in Latin for their purposes, they cannot unlearn medieval forms or invent new technical vocabularies, and they cannot move to classical composition without sacrificing the semantic precision their domains require. This drives their d toward the target end (~0.75-0.85). The asymmetry is structural: the constraint requires active institutional enforcement (teaching, credentialing, prestige attribution) to maintain the hierarchy, so requires_active_enforcement is true. Beneficiaries collect from the system (prestige, authority, credentialing power) and payers bear its costs (constant invalidation of their forms, lower institutional standing, pressure to conform to standards that do not serve their purposes).
 *
 * MANDATROPHY ANALYSIS:
 *   The bifurcated reading avoids falsely claiming coordination where extraction occurs (rupture reading risk) and avoids falsely claiming medieval forms are a free evolution without prestige consequences (continuity reading risk). It names the real hybrid structure: coordination happens in the accommodation of technical domains to medieval forms, but extraction happens in the prestige system that subordinates those domains. The classification as tangled_rope is justified because both coordination (domain-specific legitimacy) and extraction (prestige hierarchy) are present and require active enforcement. The founding problem was real (how to preserve classical standards while accommodating medieval divergence), and the solution partially solved it (technical domains can use medieval forms). However, the cost of the solution is the prestige hierarchy itself, which transforms accommodation into extraction. A mandatrophy diagnosis would claim the founding problem (preserve unified classical standards across diverse domains) has been supplanted by a new problem (maintain prestige hierarchy in a bifurcated legitimacy system), but because the old problem is still invoked as justification for the constraint, the reading shows mandatrophy_resolved as false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prestige_hierarchy_internalization,
    'Is the prestige hierarchy between classical and medieval forms internalized by medieval specialists and technical writers as a genuine status difference (they believe classical forms are superior), or is it externally imposed and resisted?',
    'Textual evidence from technical and medieval authors: do they defend their forms as equal to classical, or do they apologize for deviations? Do they adopt classical forms when not required by their domain, or do they maintain medieval forms consistently? Post-constraint analysis: do technical writers abandon medieval forms when the prestige pressure is removed?',
    'If internalized, the constraint operates with lower suppression requirement (the payers have accepted the hierarchy). If externally imposed, suppression requirement is higher and the constraint edges toward snare classification. Internalization also affects theater ratio: internalized hierarchy requires less performative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prestige_hierarchy_internalization, empirical, 'Whether prestige hierarchy is self-accepted or externally enforced.').

omega_variable(
    functional_equivalence_of_medieval_forms,
    'Are medieval forms genuinely more functionally suitable for technical domains, or is the hybrid reading''s accommodation a rhetorical justification for a prestige system that would work equally well if classical forms were required everywhere?',
    'Comparative analysis of classical vs. medieval usage in technical texts: measure semantic precision, compression ratio, and functional fit. Compare readability and comprehension metrics for technical content composed in classical vs. medieval forms. Historical natural experiment: where classical forms were enforced for technical writing, did it produce inferior results?',
    'If medieval forms are genuinely functionally superior, the coordination function is real and the constraint is a true tangled rope. If medieval forms are merely more convenient and equally functional classical forms could be used, the accommodation is rhetorical and the constraint edges toward pure snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_equivalence_of_medieval_forms, empirical, 'Whether medieval forms provide genuine functional advantage in technical domains.').

omega_variable(
    kernel_reading_alternative_framing,
    'What would the constraint look like under the continuity_reading (medieval Latin is legitimate organic evolution) vs. the rupture_reading (classical Latin is fixed, medieval usage is corruption)?',
    'Generate constraint stories for the sibling readings and compare ε values, beneficiary/victim structures, and enforcement requirements. The three readings together constitute a kernel family: their divergence measures which reading is most structurally accurate.',
    'This omega documents the irreducible ambiguity about which reading is correct. The hybrid reading''s moderate extractiveness (0.58) reflects compromise; the rupture reading would show higher extractiveness (pressure to reconstruct everywhere), and the continuity reading would show lower extractiveness (medieval forms are unmarked). The constraint''s classification depends partly on which reading is true.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framing, conceptual, 'Which sibling reading is structurally accurate; the kernel contest itself.').

omega_variable(
    medieval_resistance_to_classicism,
    'Are medieval specialists and technical writers actively resisting the prestige hierarchy, or have they accepted it as natural?',
    'Textual evidence from defenses of medieval Latin in scholarly and technical literature. Records of challenges to the bifurcation in councils or pedagogical debates. Analysis of whether medieval-focused scholars claim equal legitimacy for their forms or frame medieval Latin as acceptable-but-subordinate.',
    'Resistance is measured as 0.52 (moderate). If resistance is actually higher (scholars actively claim medieval forms as equally valid, not merely acceptable in their domains), the constraint edges toward snare. If resistance is lower (medieval specialists internalize the subordination), the constraint becomes more stable but remains a tangled rope due to active enforcement of the hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_resistance_to_classicism, empirical, 'Degree of active scholarly resistance to the prestige hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(lati_tr_t0, observed).
narrative_ontology:measurement(lati_tr_t5, latin_correctness__hybrid_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(lati_tr_t5, observed).
narrative_ontology:measurement(lati_tr_t10, latin_correctness__hybrid_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(lati_tr_t10, observed).
narrative_ontology:measurement(lati_tr_t15, latin_correctness__hybrid_reading, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(lati_tr_t15, observed).
narrative_ontology:measurement(lati_tr_t20, latin_correctness__hybrid_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(lati_tr_t20, observed).
narrative_ontology:measurement(lati_tr_t25, latin_correctness__hybrid_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(lati_tr_t25, observed).
narrative_ontology:measurement(lati_tr_t30, latin_correctness__hybrid_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(lati_tr_t30, observed).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__hybrid_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(lati_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(lati_be_t0, observed).
narrative_ontology:measurement(lati_be_t5, latin_correctness__hybrid_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(lati_be_t5, observed).
narrative_ontology:measurement(lati_be_t10, latin_correctness__hybrid_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(lati_be_t10, observed).
narrative_ontology:measurement(lati_be_t15, latin_correctness__hybrid_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(lati_be_t15, observed).
narrative_ontology:measurement(lati_be_t20, latin_correctness__hybrid_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(lati_be_t20, observed).
narrative_ontology:measurement(lati_be_t25, latin_correctness__hybrid_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(lati_be_t25, observed).
narrative_ontology:measurement(lati_be_t30, latin_correctness__hybrid_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(lati_be_t30, observed).
narrative_ontology:measurement(lati_be_t40, latin_correctness__hybrid_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(lati_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(lati_su_t0, observed).
narrative_ontology:measurement(lati_su_t5, latin_correctness__hybrid_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(lati_su_t5, observed).
narrative_ontology:measurement(lati_su_t10, latin_correctness__hybrid_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(lati_su_t10, observed).
narrative_ontology:measurement(lati_su_t15, latin_correctness__hybrid_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(lati_su_t15, observed).
narrative_ontology:measurement(lati_su_t20, latin_correctness__hybrid_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement_basis(lati_su_t20, observed).
narrative_ontology:measurement(lati_su_t25, latin_correctness__hybrid_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement_basis(lati_su_t25, observed).
narrative_ontology:measurement(lati_su_t30, latin_correctness__hybrid_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(lati_su_t30, observed).
narrative_ontology:measurement(lati_su_t40, latin_correctness__hybrid_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(lati_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the latin_correctness kernel. The three readings (continuity, rupture, hybrid) share an authority kernel (what counts as correct Latin?) but produce structurally different constraints because they derive different ε-values from the same underlying dispute. The hybrid reading creates a bifurcated legitimacy system where classical forms are authoritative for prestige domains and medieval forms are legitimate for technical domains. This reading coexists with continuity (which claims medieval evolution is unmarked and natural) and forecloses the purest rupture claim (that medieval usage is simply corruption), but it does influence both siblings by establishing a framework where domain-dependent legitimacy is recognized. All three readings belong to the same kernel family and must be evaluated together to understand the constraint's true structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__hybrid_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
