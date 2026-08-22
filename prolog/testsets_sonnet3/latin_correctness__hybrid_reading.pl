% ============================================================================
% CONSTRAINT STORY: latin_correctness__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Domain-Bifurcated Latin Correctness Standard (Hybrid Reading)
 *   domain: historical_linguistics/intellectual_history/philology
 *
 * SUMMARY:
 *   This story instantiates the hybrid reading of the latin_correctness
 *   kernel: the position, associated with Renaissance humanist pedagogy, that
 *   classical (chiefly Ciceronian) norms should govern literary and
 *   rhetorical Latin while medieval forms remain legitimate in their own
 *   technical and practical domains (law, medicine, notarial practice,
 *   natural philosophy). Unlike the continuity reading (which denies any
 *   correctness discontinuity at all) or the rupture reading (which treats
 *   medieval usage as corruption everywhere), the hybrid reading concedes
 *   legitimacy to medieval Latin but confines that legitimacy to a
 *   lower-prestige domain. This concession is what makes the reading
 *   structurally distinct: it is neither pure toleration nor pure rejection,
 *   but a bifurcated legitimacy grant that produces its own status hierarchy.
 *   The delta from the sibling readings is exactly this: moderate (not low,
 *   not high) extractiveness, and a partial victim set — technical writers
 *   are not accused of corruption (as under rupture) but are also not granted
 *   equal prestige (as under continuity); they are formally legitimate and
 *   informally subordinate.
 *
 * KEY AGENTS:
 *   - classicizing_humanist_scholars: agenda-setting beneficiary who defines and enforces the bifurcated standard
 *   - literary_academies: institutional beneficiary collecting prestige from the literary tier
 *   - technical_treatise_writers: primary payer, formally legitimated but structurally subordinated
 *   - vernacular_adjacent_notaries: powerless payer bearing the sharpest edge of the status gradient
 *   - students_seeking_advancement: excluded voice preferring a unified standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__hybrid_reading, 0.47).
domain_priors:suppression_score(latin_correctness__hybrid_reading, 0.42).
domain_priors:theater_ratio(latin_correctness__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(latin_correctness__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__hybrid_reading, "Domain-Bifurcated Latin Correctness Standard (Hybrid Reading)").
narrative_ontology:topic_domain(latin_correctness__hybrid_reading, "historical_linguistics/intellectual_history/philology").

domain_priors:requires_active_enforcement(latin_correctness__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__hybrid_reading, 'd7581b1b-2e54-42b3-821e-0caa2b139982').
narrative_ontology:cs_kernel_codification('d7581b1b-2e54-42b3-821e-0caa2b139982', distributed).
narrative_ontology:cs_authority_grounding('d7581b1b-2e54-42b3-821e-0caa2b139982', practice).
narrative_ontology:cs_interpretation_layer_present('d7581b1b-2e54-42b3-821e-0caa2b139982').
narrative_ontology:cs_reading_relation('d7581b1b-2e54-42b3-821e-0caa2b139982', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7581b1b-2e54-42b3-821e-0caa2b139982', latin_correctness__rupture_reading, influences).
narrative_ontology:cs_axiom('d7581b1b-2e54-42b3-821e-0caa2b139982', foundational, domain_appropriate_register_legitimacy).
narrative_ontology:cs_axiom_status(domain_appropriate_register_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d7581b1b-2e54-42b3-821e-0caa2b139982', domain_appropriate_register_legitimacy, conventional).
narrative_ontology:cs_axiom('d7581b1b-2e54-42b3-821e-0caa2b139982', secondary, classical_standard_confined_to_literary_rhetorical_sphere).
narrative_ontology:cs_axiom_status(classical_standard_confined_to_literary_rhetorical_sphere, holdable).
narrative_ontology:cs_axiom_grounding('d7581b1b-2e54-42b3-821e-0caa2b139982', classical_standard_confined_to_literary_rhetorical_sphere, conventional).
narrative_ontology:cs_reference_frame('d7581b1b-2e54-42b3-821e-0caa2b139982', humanist_bifurcated_pedagogy).
narrative_ontology:cs_drift_state('d7581b1b-2e54-42b3-821e-0caa2b139982', post_printing_press_standardization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7581b1b-2e54-42b3-821e-0caa2b139982', '').
narrative_ontology:cs_kernel_id(latin_correctness__hybrid_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, classicizing_humanist_scholars).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, literary_academies).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, rhetoric_instructors).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, technical_treatise_writers).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, vernacular_adjacent_notaries).
narrative_ontology:constraint_victim(latin_correctness__hybrid_reading, provincial_latin_teachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(latin_correctness__hybrid_reading, provincial_latin_teachers).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, domain_appropriate_register_theory).
narrative_ontology:constraint_vindicates(latin_correctness__hybrid_reading, ciceronian_stylistic_primacy_in_letters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and police the standard that literary and rhetorical Latin must track classical (chiefly Ciceronian) usage, while conceding that technical and practical writing may retain medieval forms. They author grammars, edit florilegia, and adjudicate disputes over what counts as barbarism in a poem versus acceptable shorthand in a legal deed. Their own prestige and patronage depend on being the recognized arbiters of the literary tier.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, classicizing_humanist_scholars, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, classicizing_humanist_scholars, beneficiary).

% Function as the institutional home of the classicizing standard, awarding membership, commissions, and reputational capital to those who write acceptable literary Latin. They benefit from the bifurcation because it lets them claim a rarefied domain of excellence while not having to certify or police the vast bulk of technical writing.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, literary_academies, beneficiary,
    organized, generational, mobile, continental).

% Teach classical composition to students aiming at literary, ecclesiastical, or diplomatic careers. The hybrid standard gives their curriculum a defensible monopoly over the prestige track, since technical Latin is explicitly carved out as a lesser, non-competing domain.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, rhetoric_instructors, beneficiary,
    moderate, biographical, constrained, regional).

% Write medical, legal, agricultural, and craft treatises in the medieval Latin idiom their fields have always used. The hybrid reading formally legitimizes their register but simultaneously fixes them at the bottom of a status hierarchy: their Latin is 'permitted' rather than 'correct,' and any of their number who seeks broader recognition is measured against a classical bar their training never targeted and their genre does not require.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, technical_treatise_writers, payer,
    moderate, biographical, constrained, regional).

% Draft contracts, wills, and administrative records in a working Latin heavily inflected by local vernacular. They are told their usage is legitimate for its purpose but are excluded from any avenue of advancement that requires literary Latin, and are occasionally mocked by classicizing critics despite the hybrid standard's formal tolerance of their register.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, vernacular_adjacent_notaries, payer,
    powerless, biographical, trapped, local).

% Teach Latin in provincial schools where classical texts are scarce and expensive. They benefit from the hybrid standard's tolerance for medieval forms in practical instruction, but are penalized in professional evaluation whenever their students are compared against classicizing benchmarks set by metropolitan academies for the literary tier they are nominally exempt from.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, provincial_latin_teachers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(latin_correctness__hybrid_reading, provincial_latin_teachers, beneficiary).

% The classical corpus itself, invoked as the fixed reference point for the literary tier of the hybrid standard, without agency in how it is deployed.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, ancient_roman_authors, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(latin_correctness__hybrid_reading, ancient_roman_authors).

% Aspiring clerics, notaries, and civil servants from technical backgrounds who would prefer a single unified standard so that competence in one register transfers to prestige in the other. Their preference for register-neutral evaluation is not represented in the hybrid framework, which structurally requires two tiers to exist.
narrative_ontology:constraint_stakeholder(latin_correctness__hybrid_reading, students_seeking_advancement, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(latin_correctness__hybrid_reading, literary_academies).
narrative_ontology:fixing_cost_class(latin_correctness__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a workable division of linguistic labor: literary and rhetorical production is coordinated around a stable classical reference so that eloquence can be judged by a shared standard, while technical and practical writing is coordinated around functional intelligibility so that medicine, law, and administration are not disrupted by demands for stylistic purity they do not need.
% TRANSFER_FUNCTION: Moves prestige, patronage, and access to advancement channels toward those who can perform classical literary Latin, and away from those whose Latin is technically competent but medieval in form — even though the hybrid standard formally declares the latter legitimate within its own domain.
% ABSENT_VOICES: Students and mid-career technical writers who would prefer a single continuous standard of Latin competence (closer to the continuity reading) are not consulted in the bifurcation; their objection — that the two-tier system manufactures a status gradient neither classical nor medieval usage on its own would produce — has no institutional seat.
% DISAPPEARANCE_RATIONALE: If the hybrid standard vanished, either a single continuity standard or a single rupture standard would have to take its place; literary academies would lose their exclusive claim to a rarefied register, technical writers would lose the (partial) legitimacy shield the hybrid reading extends to their idiom, and the current status hierarchy between literary and technical Latin would need to be renegotiated or dissolved.
% FOUNDING_PROBLEM: Humanist scholars needed a way to elevate literary and diplomatic Latin to classical standards for prestige and international communication, without simultaneously declaring the entire medieval administrative, legal, and scientific apparatus (written in medieval Latin) illegitimate or unusable.
% FOUNDING_PROBLEM_CORROBORATION: Humanist scholars and literary academies attest the bifurcation solves a real coordination problem (register-appropriate standards). Independent testimony from historians of medieval science and law, and from provincial teachers outside the humanist patronage network, attests that the founding problem has been substantially resolved wherever it mattered practically, and that the surviving two-tier hierarchy now functions mainly to preserve literary prestige rather than to solve any live coordination need in technical domains.
narrative_ontology:disappearance_verdict(latin_correctness__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(latin_correctness__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(latin_correctness__hybrid_reading, 0.47, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored moderate (0.47 at interval end) because the hybrid reading's extraction is real but bounded: it does not deny technical writers legitimacy outright (which would push extraction toward the rupture reading's territory), it merely subordinates their register's prestige. Suppression is correspondingly moderate (0.42): there is real enforcement of the tier boundary (classicizing critics do police crossover attempts, and advancement gatekeepers do measure technical writers against literary benchmarks), but no wholesale suppression of medieval Latin's use within its own domain — that is precisely what the reading concedes. Theater ratio is moderate-rising (0.20 to 0.38) as the literary tier's classicizing performance intensifies over the two centuries of the interval, while its practical coordination function (a real division of linguistic labor) remains largely intact underneath.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (classicizing humanist scholars) experiences the hybrid standard as principled domain-appropriate coordination — a sensible division that respects both classical eloquence and practical necessity. The payer seats (technical treatise writers, notaries, provincial teachers) experience the same structure as a status hierarchy dressed in tolerant language: their register is 'legitimate' in name but subordinate in every channel that confers advancement. This divergence is the seat-computed signal the engine should register; the story does not resolve it by picking a side.
 *
 * DIRECTIONALITY LOGIC:
 *   Classicizing scholars and literary academies sit near the beneficiary end: they set the terms of the bifurcation and collect the prestige and patronage that flow from being recognized arbiters of the literary tier. Technical treatise writers and notaries sit toward the target end: they bear the classification's downstream effect (subordination) despite the reading's surface concession of legitimacy to their register. Provincial teachers occupy a mixed position — beneficiaries of the tolerance clause for their local teaching, but payers whenever their output is compared against metropolitan literary benchmarks; this justifies their dual role assignment rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading prevents one mislabeling (treating all medieval Latin as pure corruption, per the rupture reading) while enabling another (treating the resulting status hierarchy as costless tolerance rather than as extraction). The founding problem — coordinating register expectations across domains with genuinely different functional needs — is largely resolved in practice; what persists past that resolution is the prestige gradient itself, which now serves the literary tier's status interests more than it serves any live coordination need. This is the R5 mismatch the six_questions block surfaces: founding_problem_status is contested, and disappearance_verdict is world_rearranges, flagging exactly the kind of partial-obsolescence structure the framework is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_reading_stability_of_domain_boundary,
    'Is the literary/technical domain boundary that the hybrid reading relies on a stable, principled distinction, or does it shift opportunistically to preserve the literary tier''s prestige whenever technical writing threatens to encroach on prestige domains (e.g., vernacular philosophy edging toward the ''literary'')?',
    'Track historical cases where technical genres (medical treatises, legal commentary) achieved literary prestige recognition and examine whether the domain boundary was redrawn to exclude them retroactively, or whether the boundary held independent of prestige pressure.',
    'If the boundary shifts to protect prestige, the hybrid reading''s coordination claim is substantially cover for extraction, pushing the effective classification toward snare; if the boundary holds independently, the tangled_rope classification (genuine coordination plus asymmetric extraction) is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_stability_of_domain_boundary, empirical, 'Whether the domain boundary is principled or prestige-opportunistic.').

omega_variable(
    kernel_committer_structure,
    'The latin_correctness kernel admits (at least) three readings — continuity, hybrid, and rupture — each of which structurally differs in where legitimacy is granted and to whom. Which reading a given historical actor or institution held determined who counted as a victim of the correctness standard at all.',
    'This is not resolvable by further data about Latin usage; it is a question of which normative framework a given community of practice adopted, and different communities (humanist academies vs. notarial guilds vs. monastic scriptoria) demonstrably adopted different readings simultaneously.',
    'Under the continuity reading, this story''s entire victim set (technical writers subordinated by a status hierarchy) does not exist as an extraction structure at all, since there is no domain-based legitimacy split to subordinate anyone. Under the rupture reading, the victim set would be universal (all medieval usage) rather than partial (only the prestige-comparison effect on technical writers). The hybrid reading''s moderate extractiveness and partial victim set are contingent on this reading being the one in force for a given institutional context; a sibling story with continuity_reading or rupture_reading would author a different epsilon for what is nominally ''the same'' historical dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'The kernel''s reading-plurality is the structural source of this constraint''s distinctive moderate-extraction, partial-victim profile; siblings sharing the kernel author different epsilon values by design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__hybrid_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t0, latin_correctness__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lati_tr_t40, latin_correctness__hybrid_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(lati_tr_t80, latin_correctness__hybrid_reading, theater_ratio, 80, 0.31).
narrative_ontology:measurement(lati_tr_t120, latin_correctness__hybrid_reading, theater_ratio, 120, 0.34).
narrative_ontology:measurement(lati_tr_t160, latin_correctness__hybrid_reading, theater_ratio, 160, 0.36).
narrative_ontology:measurement(lati_tr_t200, latin_correctness__hybrid_reading, theater_ratio, 200, 0.38).

% Extraction over time
narrative_ontology:measurement(lati_be_t0, latin_correctness__hybrid_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lati_be_t40, latin_correctness__hybrid_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(lati_be_t80, latin_correctness__hybrid_reading, base_extractiveness, 80, 0.43).
narrative_ontology:measurement(lati_be_t120, latin_correctness__hybrid_reading, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(lati_be_t160, latin_correctness__hybrid_reading, base_extractiveness, 160, 0.46).
narrative_ontology:measurement(lati_be_t200, latin_correctness__hybrid_reading, base_extractiveness, 200, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t0, latin_correctness__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(lati_su_t40, latin_correctness__hybrid_reading, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(lati_su_t80, latin_correctness__hybrid_reading, suppression_requirement, 80, 0.38).
narrative_ontology:measurement(lati_su_t120, latin_correctness__hybrid_reading, suppression_requirement, 120, 0.4).
narrative_ontology:measurement(lati_su_t160, latin_correctness__hybrid_reading, suppression_requirement, 160, 0.41).
narrative_ontology:measurement(lati_su_t200, latin_correctness__hybrid_reading, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(latin_correctness__hybrid_reading, 0.1).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__hybrid_reading, latin_correctness__rupture_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the latin_correctness kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. continuity_reading treats medieval Latin as organically legitimate everywhere (low extraction, no domain split, no victim set). rupture_reading treats classical Latin as the sole fixed standard and medieval usage as corruption everywhere (high extraction, universal victim set among medieval-usage writers). hybrid_reading (this story) occupies the structural middle: it grants medieval Latin legitimacy but confines that legitimacy to technical/practical domains, producing a status hierarchy and a partial victim set distinct from either sibling's profile. The three stories are linked via affects_constraints because a shift in which reading dominates institutional practice (e.g., humanist academies losing patronage power) structurally redistributes legitimacy and extraction across all three readings' domains simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
