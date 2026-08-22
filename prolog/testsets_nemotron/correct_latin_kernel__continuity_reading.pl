% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__continuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__continuity_reading
 *   human_readable: Medieval Latin as Natural Evolution of Classical Latin
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint story instantiates the continuity_reading of the
 *   correct_latin_kernel: the position that Medieval Latin is Classical Latin
 *   after natural linguistic evolution, and that humanist 'reconstruction'
 *   was prescriptive purism imposing an idealized Classical standard on a
 *   living, lawfully evolved system. The reading treats Medieval innovations
 *   (syntactic, lexical, morphological) as legitimate developments internal
 *   to Latin's trajectory, not as corruptions requiring textual recovery. The
 *   kernel contest centers on whether the Classical/Medieval boundary is
 *   continuous (this reading), a rupture requiring symbolic reoccupation
 *   (discontinuity_reading), or a layered continuity with recovery only in
 *   syntax/lexicon (hybrid_reading).
 *
 * KEY AGENTS:
 *   - medieval_scribes: Primary beneficiaries (moderate/identity_locked) — their Latin is the living standard; continuity reading validates their practice
 *   - scholastic_authors: Primary beneficiaries (powerful/identity_locked) — their philosophical/theological Latin is continuous with Classical authority
 *   - vernacular_latin_users: Beneficiaries (organized/constrained) — clergy, administrators, students using evolved Latin daily
 *   - humanist_reformers: Payers/excluded in this reading (powerful/mobile) — framed as external imposers of an artificial standard
 *   - philologists_19th_20th_century: Observers (analytical/analytical) — later scholars adjudicating the kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__continuity_reading, 0.12).
domain_priors:suppression_score(correct_latin_kernel__continuity_reading, 0.18).
domain_priors:theater_ratio(correct_latin_kernel__continuity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(correct_latin_kernel__continuity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__continuity_reading, rope).
narrative_ontology:human_readable(correct_latin_kernel__continuity_reading, "Medieval Latin as Natural Evolution of Classical Latin").
narrative_ontology:topic_domain(correct_latin_kernel__continuity_reading, "historical_linguistics/philology/intellectual_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__continuity_reading, 'd462b8a0-1097-4f13-ba10-c8e8f4abadc0').
narrative_ontology:cs_kernel_codification('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', implicit).
narrative_ontology:cs_authority_grounding('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', practice).
narrative_ontology:cs_interpretation_layer_present('d462b8a0-1097-4f13-ba10-c8e8f4abadc0').
narrative_ontology:cs_reading_relation('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', correct_latin_kernel__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', foundational, language_change_is_lawful_and_continuous).
narrative_ontology:cs_axiom_status(language_change_is_lawful_and_continuous, holdable).
narrative_ontology:cs_axiom_grounding('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', language_change_is_lawful_and_continuous, empirically_contingent).
narrative_ontology:cs_axiom('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', foundational, usage_authority_resides_in_living_community).
narrative_ontology:cs_axiom_status(usage_authority_resides_in_living_community, holdable).
narrative_ontology:cs_axiom_grounding('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', usage_authority_resides_in_living_community, deontological).
narrative_ontology:cs_reference_frame('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', living_latin_community_practice).
narrative_ontology:cs_drift_state('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', humanist_reform_ascendancy, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d462b8a0-1097-4f13-ba10-c8e8f4abadc0', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, medieval_scribes).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, scholastic_authors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__continuity_reading, vernacular_latin_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(correct_latin_kernel__continuity_reading, humanist_reformers).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, language_change_is_lawful).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, usage_authority_resides_in_community).
narrative_ontology:constraint_vindicates(correct_latin_kernel__continuity_reading, prescriptive_purism_is_external).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Copy and transmit texts in the Latin they inherited and use daily. Their practice IS the living standard. The continuity reading validates their work as correct Latin without requiring them to conform to a fossilized Classical norm. Exit would mean abandoning their professional identity and the textual tradition they maintain — identity_locked because their craft and self-concept are constituted through this Latin.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, medieval_scribes, beneficiary,
    moderate, biographical, identity_locked, continental).

% Write philosophy, theology, and science in the evolved Latin of the universities. They set the de facto standard for learned Latin. The continuity reading treats their syntax and vocabulary as legitimate developments, not corruptions. Their intellectual authority is fused with this Latin — identity_locked because their conceptual vocabulary and professional legitimacy depend on the continuity claim.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, scholastic_authors, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, scholastic_authors, agenda_setter).

% Clergy, administrators, lawyers, physicians, and students who use Latin daily for liturgy, law, medicine, and education. They benefit from a standard that matches their actual usage rather than an idealized Classical norm. Exit is constrained — they need Latin for their roles, but could shift to vernaculars (which many eventually did).
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, vernacular_latin_users, beneficiary,
    organized, biographical, constrained, continental).

% Scholars (Erasmus, Valla, Bude, etc.) who argued that Medieval Latin had corrupted the Classical language and advocated reform based on Classical texts. In this reading, they are payers: they invest effort in producing a 'purified' Latin that the continuity reading treats as unnecessary prescriptivism. They are also excluded from the Medieval university's de facto standard. Their exit is mobile — they created new academies, printed editions, and educational programs outside the university system.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, humanist_reformers, payer,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__continuity_reading, humanist_reformers, excluded).

% Later linguists and philologists (Meyer-Lübke, Waquet, Herman, Adams) who analyze the kernel contest from outside. They do not collect or pay within the Medieval system. Their analytical seat sees the full structure: continuity as a genuine coordination mechanism for Medieval users, discontinuity as a humanist construction, hybrid as a nuanced middle ground.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__continuity_reading, philologists_19th_20th_century, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(correct_latin_kernel__continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a living, lawfully evolved standard for learned Latin that matches the actual usage of the community (scribes, scholars, clergy, administrators) without requiring conformity to an external, fossilized Classical norm. Solves the problem of how Latin remains a functional learned language across centuries of change.
% TRANSFER_FUNCTION: Moves legitimacy and authority from an external Classical ideal (which no living community speaks) to the actual evolving usage of the Medieval Latin community. No material transfer; the transfer is epistemic and normative — who counts as 'speaking correct Latin.'
% ABSENT_VOICES: Humanist reformers and their allies (printers, patrons, some princes) who argued for Classical purification are the absent voices in the Medieval period — they were present but structurally excluded from the university's de facto standard. Later, the continuity reading itself became the absent voice when humanist Latin dominated education and printing (16th century onward).
% DISAPPEARANCE_RATIONALE: If the continuity claim vanished overnight (e.g., if humanist purism had been universally imposed in 1100), Medieval scribes and scholars would have been forced to conform to an artificial Classical standard they did not speak natively, disrupting textual transmission, education, and administration. The living Latin tradition would have been replaced by a learned reconstruction — the world of Medieval Latin practice rearranges.
% FOUNDING_PROBLEM: How to maintain Latin as a living learned language across centuries of natural change without fossilizing it into a dead standard or letting it fragment into mutually unintelligible varieties.
% FOUNDING_PROBLEM_CORROBORATION: The continuity reading's beneficiaries (Medieval scribes, scholastic authors) attest the problem was live and solved by natural evolution. 19th-20th century philologists outside the beneficiary set (Meyer-Lübke, Waquet, Herman) corroborate that Medieval Latin was a functioning learned language with lawful internal development — the problem was real and the continuity solution was genuine. Humanist reformers and their intellectual heirs contest this, attesting the problem was NOT solved (Latin had corrupted) and their reform was the solution.
narrative_ontology:disappearance_verdict(correct_latin_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(correct_latin_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__continuity_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__continuity_reading_tests).
:- end_tests(correct_latin_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Low extractiveness (0.12): the continuity claim describes a linguistic process; it does not extract resources. The coordination function is real — it legitimizes the Latin actually in use across Medieval Europe as 'correct' without requiring conformity to an external fossilized norm. Suppression is low (0.18) but nonzero: the reading itself was later suppressed by humanist dominance in education and printing, not by its own operation. Theater ratio (0.22) reflects some performative invocation of 'natural evolution' to resist humanist reform pressure in the 14th-16th centuries. Accessibility collapse (0.78) is high because once the continuity frame is accepted, alternatives (discontinuity, hybrid) appear as misreadings of the same evidence. Resistance (0.35) is moderate: humanist reformers actively contested this reading from the 14th century onward.
 *
 * PERSPECTIVAL GAP:
 *   The continuity_reading and discontinuity_reading compute different per-seat types from the same historical facts. For medieval_scribes, continuity is a rope (their practice is the standard); for humanist_reformers, the same continuity claim is a snare (it legitimizes what they see as corruption). The engine computes this divergence from power/exit/beneficiary declarations — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Medieval scribes and scholastic authors are structural beneficiaries: the continuity reading validates their Latin as correct without requiring conformity to an external Classical norm — they are coordinated, not extracted from. Vernacular Latin users (clergy, administrators) benefit similarly. Humanist reformers are payers in this reading: they invest in producing a 'purified' Latin that the continuity reading treats as unnecessary and externally imposed. Their exit is mobile (they could and did create new institutions), but within the Medieval university system they were constrained. No victims are declared because this reading does not posit asymmetric extraction — the coordination function is genuine and benefits the community of use.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'how to maintain Latin as a living learned language without fossilizing it' — is contested (some say the problem persisted; others say humanist reform solved it by creating a stable Classical standard). The continuity reading declares the problem dead (the living language continued until displaced by vernaculars), but this is not self-attested — 19th-century philologists (e.g., Wilhelm Meyer-Lübke, Francoise Waquet) corroborate that Medieval Latin was a functioning learned language, not a corrupted one. The arrangement did not persist as a zombie; it was displaced by historical forces (print, vernacular rise, humanist education), not by internal extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint one reading of the contested kernel correct_latin_kernel, and if so, which reading does it instantiate?',
    'The committer frame is declared by construction: this JSON is the continuity_reading of correct_latin_kernel. Sibling readings (discontinuity_reading, hybrid_reading) instantiate separate constraints with distinct ε and beneficiary/victim structures. The disagreement is located in whether Medieval Latin constitutes legitimate continuity or rupture requiring textual recovery.',
    'If the kernel frame is rejected, this constraint would need to be re-authored as a flat story without cs_structure reading_relations/axioms. The structural delta (Medieval innovations as legitimate vs. rupture) is the dividing line.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Commitment that this constraint is a kernel reading, not a flat story').

omega_variable(
    naturalness_of_evolution_vs_constructed_standard,
    'Does the ''natural evolution'' framing describe a genuine linguistic process, or does it naturalize a socially constructed standard that benefited clerical and scholarly elites?',
    'Corpus linguistics comparing actual Medieval usage patterns against the classical norm; sociolinguistic analysis of who enforced which norms and when. If the ''natural'' trajectory was selectively curated by institutional power, the constraint carries extraction masked as description.',
    'If the continuity narrative was institutionally curated to legitimize clerical Latin against vernacular competition, the rope classification shifts toward tangled_rope — coordination function genuine but extraction present via gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_evolution_vs_constructed_standard, empirical, 'Whether the continuity claim is descriptive fact or constructed legitimacy').

omega_variable(
    humanist_reform_as_purism_vs_recovery,
    'Are humanist Latin reforms correctly characterized as ''prescriptive purism'' (external imposition) rather than ''textual recovery'' (internal correction)?',
    'Comparative analysis of humanist treatises (e.g., Erasmus, Valla) against Medieval practice: did they appeal to attested Classical usage or to an idealized norm? Philological reception history of their reforms.',
    'If humanists recovered genuine Classical forms lost in Medieval transmission, the discontinuity_reading gains empirical ground; if they imposed an idealized standard, the continuity_reading''s characterization of them as purists holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanist_reform_as_purism_vs_recovery, conceptual, 'Whether humanist reform was recovery or imposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__continuity_reading, 500, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__continuity_reading, theater_ratio, 500, 0.05).
narrative_ontology:measurement(corr_tr_t800, correct_latin_kernel__continuity_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement(corr_tr_t1100, correct_latin_kernel__continuity_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(corr_tr_t1350, correct_latin_kernel__continuity_reading, theater_ratio, 1350, 0.22).
narrative_ontology:measurement(corr_tr_t1500, correct_latin_kernel__continuity_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement(corr_tr_t1650, correct_latin_kernel__continuity_reading, theater_ratio, 1650, 0.22).

% Extraction over time
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__continuity_reading, base_extractiveness, 500, 0.08).
narrative_ontology:measurement(corr_be_t800, correct_latin_kernel__continuity_reading, base_extractiveness, 800, 0.1).
narrative_ontology:measurement(corr_be_t1100, correct_latin_kernel__continuity_reading, base_extractiveness, 1100, 0.11).
narrative_ontology:measurement(corr_be_t1350, correct_latin_kernel__continuity_reading, base_extractiveness, 1350, 0.12).
narrative_ontology:measurement(corr_be_t1500, correct_latin_kernel__continuity_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(corr_be_t1650, correct_latin_kernel__continuity_reading, base_extractiveness, 1650, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t500, correct_latin_kernel__continuity_reading, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(corr_su_t800, correct_latin_kernel__continuity_reading, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(corr_su_t1100, correct_latin_kernel__continuity_reading, suppression_requirement, 1100, 0.15).
narrative_ontology:measurement(corr_su_t1350, correct_latin_kernel__continuity_reading, suppression_requirement, 1350, 0.18).
narrative_ontology:measurement(corr_su_t1500, correct_latin_kernel__continuity_reading, suppression_requirement, 1500, 0.18).
narrative_ontology:measurement(corr_su_t1650, correct_latin_kernel__continuity_reading, suppression_requirement, 1650, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__continuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__discontinuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__continuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint family (correct_latin_kernel) decomposes the single colloquial label 'the Latin correctness question' into three structurally distinct readings with different ε values and beneficiary structures. The continuity_reading has low ε (0.12) because it describes a coordination function (legitimizing living usage). The discontinuity_reading likely has higher ε because it legitimizes an external standard imposed on a living community. The hybrid_reading sits between. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__continuity_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
