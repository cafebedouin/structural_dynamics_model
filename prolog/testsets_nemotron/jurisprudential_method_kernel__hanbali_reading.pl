% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method — Literalist Reading
 *   domain: legal/philosophical/religious
 *
 * SUMMARY:
 *   The Hanbali reading of the jurisprudential method kernel asserts that law
 *   derives exclusively from the literal text of the Qur'an and Hadith and
 *   the opinions of the Companions. Analogical reasoning (qiyas) and juristic
 *   preference (istihsan) are classified as bid'ah (blameworthy innovation)
 *   that corrupts the divine kernel. Only unanimous consensus (ijma') of the
 *   salaf is valid; majority or scholarly consensus is insufficient. This
 *   reading instantiate a constraint that closes the interpretive space: it
 *   coordinates Hanbali identity and legal determinacy by suppressing
 *   methodological pluralism. The claimed type is tangled_rope — the
 *   constraint performs a genuine coordination function (providing a closed,
 *   determinate method) but does so through asymmetric extraction
 *   (rationalist jurists and customary practitioners bear the cost of
 *   closure). Active enforcement is required: the constraint persists only
 *   through institutional boundary-maintenance (curricula, appointments,
 *   fatwa authority, takfir threats against innovators).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.82).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.88).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method — Literalist Reading").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "legal/philosophical/religious").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b').
narrative_ontology:cs_kernel_codification('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', fixed_text).
narrative_ontology:cs_authority_grounding('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', lineage).
narrative_ontology:cs_interpretation_layer_present('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b').
narrative_ontology:cs_reading_relation('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', foundational, literal_text_and_companion_opinions_only).
narrative_ontology:cs_axiom_status(literal_text_and_companion_opinions_only, holdable).
narrative_ontology:cs_axiom_grounding('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', literal_text_and_companion_opinions_only, deontological).
narrative_ontology:cs_axiom('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', foundational, qiyas_and_istihsan_are_bidah).
narrative_ontology:cs_axiom_status(qiyas_and_istihsan_are_bidah, holdable).
narrative_ontology:cs_axiom_grounding('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', qiyas_and_istihsan_are_bidah, deontological).
narrative_ontology:cs_axiom('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', foundational, unanimous_consensus_only_valid_ijma).
narrative_ontology:cs_axiom_status(unanimous_consensus_only_valid_ijma, holdable).
narrative_ontology:cs_axiom_grounding('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', unanimous_consensus_only_valid_ijma, deontological).
narrative_ontology:cs_reference_frame('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', salafi_textual_closure).
narrative_ontology:cs_drift_state('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', classical_madhhab_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f8f667e-08ba-4a96-b9a8-d58d72fd5e0b', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_legal_institutions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, non_hanbali_schools).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, literalist_hermeneutics).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, rejection_of_qiyas).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, rejection_of_istihsan).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, unanimous_consensus_only).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose professional identity, institutional positions, and interpretive authority are constituted by adherence to literal text and Companion opinions. Their careers, discipleship networks, and epistemic legitimacy depend on the kernel remaining closed to analogical expansion. Exit would mean abandoning the methodological commitment that defines their school and their standing within it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Madrasas, courts, and fatwa bodies (historically and in modern revivalist contexts) that administer the Hanbali method as binding law. They set the agenda for what counts as valid legal reasoning, enforce methodological boundaries through appointments and curriculum, and extract institutional coherence from the constraint's closure. Their exit is constrained by the institutional sunk cost of centuries of textualist architecture.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_legal_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% Jurists who employ qiyas (analogical reasoning), istihsan (juristic preference), maslaha (public interest), or other expansive tools. They bear the cost of exclusion: their opinions are labeled bid'ah, their students are marginalized, and their methodological contributions are treated as corruption rather than development. Exit means either conforming to literalism or operating outside the Hanbali framework entirely — losing access to its institutional channels and interpretive lineage.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).

% Communities whose local customs ('urf, 'adah) solve coordination problems but lack textual warrant. The literalist constraint renders their practices legally invisible or explicitly invalid unless they find a Companion opinion to anchor them. They cannot exit the constraint because it is enforced by the very courts and scholars they depend on for legal recognition; their practices persist socially but carry no legal weight.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents, payer,
    powerless, generational, trapped, regional).

% Hanafi, Maliki, Shafi'i, and other schools whose methodological premises (qiyas, istihsan, 'amal ahl al-Madina, hierarchical usul) are structurally foreclosed by the Hanbali reading. They are not participants in the Hanbali constraint but are shaped by its boundary: the Hanbali claim to methodological purity positions them as innovators. Their exit is mobile — they maintain their own frameworks — but the Hanbali constraint's dominance in certain regions and eras forces engagement on its terms.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, non_hanbali_schools, excluded,
    organized, civilizational, mobile, global).

% Contemporary scholars who engage the Hanbali method critically — some from within the tradition (e.g., Salafi reformers negotiating literalism vs. maslaha), some from outside (academic Islamic legal studies). They analyze the constraint's operation, document its exclusionary effects, and occasionally contest its epistemic closure, but they do not set its agenda nor bear its primary costs.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, modern_reformist_scholars, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a closed, determinate legal method that resolves disputes by reference to a fixed textual kernel (Qur'an, Hadith, Companion opinions) without requiring juristic discretion. Coordinates belief and practice across the Hanbali community by eliminating interpretive pluralism within the school.
% TRANSFER_FUNCTION: Moves interpretive authority and legal determinacy from rationalist jurists (who would extend the law by analogy and preference) to textualist scholars and institutions (who control the canonical text and its literal application). The cost is the suppression of customary practice and methodological innovation; the gain is a unified, anti-innovation legal identity.
% ABSENT_VOICES: Customary practice adherents (often rural, tribal, or pre-modern urban communities) whose legal world was mediated through local custom rather than textual scholarship. They were never in the room when the Hanbali method was codified; their exclusion is structural — the constraint defines legal validity in terms that make their practices invisible. Also absent: early dissenting voices within the Hanbali trajectory (e.g., Ibn 'Aqil's limited opening to maslaha) who were marginalized by the school's later self-definition.
% DISAPPEARANCE_RATIONALE: If the literalist constraint vanished overnight, the Hanbali school would lose its defining methodological boundary. Rationalist tools (qiyas, istihsan, maslaha) would flood the interpretive space; customary practices would regain legal recognition; the school's institutional coherence would dissolve into methodological pluralism. The 'Hanbali' label would persist but its content would rearrange fundamentally — as seen in modern Salafi debates where literalism is contested from within.
% FOUNDING_PROBLEM: The early Islamic legal landscape featured rampant methodological disagreement: multiple companion opinions, conflicting analogies, regional customs claiming prophetic authority, and no agreed hierarchy of sources. The Hanbali reading was built to solve the problem of epistemic anarchy by anchoring law exclusively in the literal text and the unanimous consensus of the salaf, treating any expansion as bid'ah.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (epistemic anarchy) is attested by early usul al-fiqh literature across schools (e.g., al-Shafi'i's Risala, which itself responds to the same chaos). However, whether the Hanbali solution (total closure) was necessary or proportionate is contested: Hanafis and Malikis attest that their open methods produced stable law without total closure; modern historians of Islamic law (e.g., Hallaq, Melchert) corroborate that the anarchy narrative is partly a retrospective construction by the Hanbali school to justify its distinctiveness.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.82) because the constraint's operation transfers interpretive authority from a broad class of rationalist jurists and customary practitioners to a narrow class of textualist scholars and institutions, and the transfer is enforced rather than voluntary. Suppression is very high (0.88) because the constraint's persistence depends on actively excluding alternative methodologies — labeling them bid'ah, marginalizing their practitioners, and in historical contexts enforcing conformity through institutional and occasionally coercive means. Theater ratio is low (0.15) because the coordination function (determinate legal method) is real and not merely performative; the performance serves the function rather than substituting for it. Accessibility collapse is high (0.78) because once the literalist premise is accepted, alternatives (qiyas, istihsan, custom) appear not just wrong but epistemically illegitimate — the constraint redefines what counts as legal reasoning. Resistance is substantial (0.65) because rationalist jurists, other schools, and customary communities have persistently contested the closure, both from within (Ibn 'Aqil, Ibn Taymiyya's limited openings) and from without (other schools' methodologies, modern reform movements).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (textualist scholars) experiences the constraint as a mountain — a natural, divinely ordained method that simply is the truth. The payer seats (rationalist jurists, customary practitioners) experience it as a snare — an enforced closure that extracts their interpretive labor and customary wisdom. The agenda-setter seat (Hanbali institutions) experiences it as a rope — a coordination mechanism they maintain because it solves the problem of epistemic anarchy for their community. The engine computes these divergent seat types from the structural data; the authored claim (tangled_rope) reflects the structural reality that the constraint coordinates for some while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and Hanbali institutions are structural beneficiaries (d near 0.0): they collect interpretive authority, institutional control, and epistemic legitimacy from the constraint's closure. Their exit is identity_locked (scholars) or constrained (institutions) — the constraint constitutes their professional and institutional identity. Rationalist jurists are payers (d near 0.8): they bear the cost of exclusion, their methodological tools are delegitimized, and their exit is constrained (conform or leave the framework). Customary practice adherents are payers with trapped exit (d near 1.0): they cannot exit the constraint's jurisdiction because it controls the legal system they depend on, and their practices are structurally invisible to it. Non-Hanbali schools are excluded (not coordinated, not directly extracted from) but shaped by the boundary — their mobile exit reflects their independent institutional frameworks. Modern reformist scholars are analytical observers (d = 0.5): they engage critically but do not bear the constraint's primary costs or collect its primary gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic anarchy in early Islamic law) was real and the literalist closure was a proportionate response in the 9th-10th century context. However, the constraint persists long after the anarchy problem was solved by other means (Shafi'i's hierarchical usul, the consolidation of madhhab boundaries, the institutionalization of taqlid). The mandate has atrophied: the coordination function (determinate method) is now served by the very pluralism the constraint suppresses. The constraint persists through identity_locked beneficiaries (scholars whose identity fuses with literalism) and institutional inertia (madrasas, courts, fatwa bodies). This is not a piton — the theater ratio is low and the coordination function is still real for the Hanbali community — but it is a tangled_rope where the extraction has grown disproportionate to the coordination need. The mandatrophy is unresolved: the constraint's beneficiaries treat the founding problem as live (contested status), while external observers see it as dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literalism_vs_constructed_boundary,
    'Is the Hanbali literalist boundary a genuine discovery of the kernel''s natural limits, or a constructed boundary that benefits textualist scholars and institutions?',
    'Historical analysis of early Hanbali formation: did the school''s closure emerge from textual evidence alone, or from political/institutional competition with rival schools (especially the rationalist Baghdad school and the Shafi''i standardization)? Comparative analysis with the Zahiri school (which took literalism further) and the early Hanafis (who used the same texts but different methods).',
    'If constructed, the constraint is a false summit mountain (presented as natural law, actually benefiting identifiable agents) — the FSM signature would trigger reclassification. If genuine discovery, the high extractiveness reflects the kernel''s intrinsic structure, not scholarly rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalism_vs_constructed_boundary, conceptual, 'Whether the literalist closure is intrinsic to the kernel or constructed by the reading''s beneficiaries').

omega_variable(
    unanimous_consensus_empirical_status,
    'Does ''unanimous consensus of the salaf'' exist as an empirical reality for any substantive legal question, or is it a theoretical limit that functions as a closure device?',
    'Survey of classical ijma'' claims in Hanbali literature: identify cases where unanimous consensus is cited and verify whether the unanimity is historical or stipulative. Check whether the consensus invoked is actually consensus of the salaf (Companions and Successors) or consensus of later Hanbali scholars projected backward.',
    'If unanimous consensus is empirically empty for contested questions, the constraint''s coordination function is illusory — it coordinates around a null referent, making the extraction pure. If genuine cases exist, the coordination function has empirical grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unanimous_consensus_empirical_status, empirical, 'Whether the unanimous-consensus condition is empirically satisfiable or a theoretical closure device').

omega_variable(
    bidah_label_as_extraction_tool,
    'Does the bid''ah label function primarily as a theological category or as an extraction tool that delegates the cost of methodological closure to its targets?',
    'Genealogical analysis of bid''ah usage in Hanbali polemics: trace how the label shifts from theological (worship innovations) to methodological (qiyas, istihsan, maslaha). Compare with Shafi''i and Maliki usage where bid''ah is narrower. Test whether the expansion of bid''ah correlates with institutional consolidation of Hanbali authority.',
    'If primarily an extraction tool, the suppression metric understates the constraint''s coercive structure — the label itself is the enforcement mechanism. If primarily theological, the suppression is a side effect of genuine doctrinal commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidah_label_as_extraction_tool, conceptual, 'Whether the bid''ah designation is a sincere doctrinal boundary or a strategic exclusion mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_reading_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hanbali_reading_tr_t200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 200, 0.08).
narrative_ontology:measurement(hanbali_reading_tr_t400, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement(hanbali_reading_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement(hanbali_reading_tr_t800, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 800, 0.14).
narrative_ontology:measurement(hanbali_reading_tr_t1000, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(hanbali_reading_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.15).

% Extraction over time
narrative_ontology:measurement(hanbali_reading_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hanbali_reading_be_t200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(hanbali_reading_be_t400, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 400, 0.67).
narrative_ontology:measurement(hanbali_reading_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.74).
narrative_ontology:measurement(hanbali_reading_be_t800, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 800, 0.79).
narrative_ontology:measurement(hanbali_reading_be_t1000, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1000, 0.81).
narrative_ontology:measurement(hanbali_reading_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_reading_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hanbali_reading_su_t200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(hanbali_reading_su_t400, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 400, 0.78).
narrative_ontology:measurement(hanbali_reading_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.83).
narrative_ontology:measurement(hanbali_reading_su_t800, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 800, 0.86).
narrative_ontology:measurement(hanbali_reading_su_t1000, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1000, 0.87).
narrative_ontology:measurement(hanbali_reading_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four readings with distinct ε values and beneficiary/victim structures. The Hanbali reading has the highest ε (0.82) because it rejects all expansive tools. The Hanafi reading has lower ε (est. 0.35) because qiyas and istihsan distribute interpretive authority more broadly. The Maliki reading's ε (est. 0.45) reflects the Medinan 'amal as a semi-closed but community-embedded source. The Shafi'i reading's ε (est. 0.55) reflects the hierarchical closure at the hadith-transmission tier. All four are linked because they share the same kernel (Qur'an/Hadith as divine law) but instantiate different constraints with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, organized, 0.1).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, institutional, 0.05).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, moderate, 0.75).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanbali_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
