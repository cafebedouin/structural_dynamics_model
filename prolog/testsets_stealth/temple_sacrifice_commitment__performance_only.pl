% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment — Performance-Only Reading
 *   domain: religious law/commitment system theory
 *
 * SUMMARY:
 *   This story authors ONE reading of the temple_sacrifice_commitment kernel:
 *   the performance-only position that the sacrificial commandments require
 *   material instantiation and that study, however exhaustive, archives a
 *   defunct practice rather than occupying the commitment. The standing
 *   arrangement under contest — the referent of epsilon — is the present
 *   regime in which the commitment sits unperformed while an extensive study
 *   apparatus maintains its procedural knowledge. Assessed by this reading's
 *   own lights, that arrangement extracts little: the obligation is dormant,
 *   participation is voluntary, and no material goods move. The constraint's
 *   work is coordinative — distributing preservation of executable ritual
 *   knowledge across generations of learners so the commitment stays
 *   instantiable. Family decomposition per the epsilon-invariance principle:
 *   the colloquial label 'the sacrifice commitment' covers four structurally
 *   distinct claims, and this file's epsilon (0.18, dormant husk) differs
 *   sharply from the study_as_exercise file (study fully occupies the
 *   commitment — near-zero outstanding obligation), the
 *   symbolic_transformation file (material requirement retired — the
 *   transformed arrangement extracts nothing), and the hybrid_preparatory
 *   file (study holds the commitment in suspension — intermediate). Each
 *   sibling is a separate constraint story linked through
 *   network.affects_constraints; the contest between readings is not
 *   described inside this constraint.
 *
 * KEY AGENTS:
 *   - restoration_anticipating_communities: primary beneficiary (organized/identity_locked) — sustains the commitment's future-facing validity and funds the preservation apparatus
 *   - priestly_lineages: secondary beneficiary (moderate/identity_locked) — holds hereditary role definition priced against present-day restrictions
 *   - talmudic_academies: beneficiary and preservation operator (organized/constrained) — receives the study flow and converts it into institutional continuity
 *   - halakhic_authorities: agenda_setter (institutional/identity_locked) — adjudicates the boundary between preparation and presumption
 *   - animal_welfare_advocates: excluded (organized/mobile) — would contest any material restoration; no current standing
 *   - reform_judaism_movements: excluded (organized/mobile) — formally exited the arrangement; objection on record
 *   - religious_studies_scholars: analytical observer (moderate/analytical) — sees the full structure from outside its authority relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.18).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.08).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment — Performance-Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious law/commitment system theory").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, restoration_anticipating_communities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, priestly_lineages).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, talmudic_academies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities in Israel and the diaspora whose liturgy, calendar, and education assume the temple service will resume. They fund and perform the study of sacrificial law as preparation, treat the long-unanswered obligation as a mark of fidelity rather than a burden, and could not abandon the anticipation without revising their core self-understanding.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, restoration_anticipating_communities, beneficiary,
    organized, generational, identity_locked, global).

% Families descended from the temple priesthood who carry present-day marks of the role — distinct priestly blessing, marriage restrictions, precaution around cemetery entry — in exchange for an office that currently has no workplace. The preserved sacrificial law keeps their future duties concretely defined; intermarriage and assimilation steadily erode the lineage records the role depends on.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, priestly_lineages, beneficiary,
    moderate, generational, identity_locked, global).

% Yeshivot and kollelim whose advanced curricula include the talmudic orders covering sacrifices. They employ teachers, enroll students, and publish the commentaries through which the procedural detail survives; the material is a substantial share of their identity and operating budget, and moving it out would mean reorganizing the program of study itself.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, talmudic_academies, beneficiary,
    organized, generational, constrained, global).

% Decisors, rabbinic courts, and chief rabbinate offices that rule on how sacrificial law may be taught and on restoration-adjacent projects — breeding a flawless red heifer, fabricating vessels, ascending the temple mount. They hold that the commandments await material performance and patrol the line between preparation and presumptuous action; their adjudicative standing grows out of the same continuity they guard.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Organizations and publics that campaign against industrial and ritual slaughter. They have no seat in the halakhic process; if a concrete restoration project advanced to procuring and slaughtering animals at scale, they would become its most visible opponents, bringing legal, diplomatic, and media leverage the tradition currently never confronts.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, animal_welfare_advocates, excluded,
    organized, biographical, mobile, global).

% Denominations whose nineteenth-century platforms formally removed sacrifice from the liturgy and from messianic expectation, substituting universalist formulations. They have already left the arrangement; their objection is on record, and their institutions demonstrate that departure is survivable.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, reform_judaism_movements, excluded,
    organized, generational, mobile, continental).

% Academic historians and theorists of religion who study how communities maintain unperformable commandments across exile. They take no side, produce the analyses the tradition itself sometimes cites in self-defense, and see the whole structure from outside its authority relations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, religious_studies_scholars, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, talmudic_academies).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complete executable knowledge of the sacrificial rite — procedures, measurements, eligibility lineages, vessel and altar specifications — across an indefinite period in which performance is impossible, distributing the preservation load as a study obligation across the learning community so the commitment remains instantiable if conditions change.
% TRANSFER_FUNCTION: Moves study time, curricular capacity, and communal attention from living learners into maintenance of the dormant ritual repertoire; confers role-definition and prospective office on priestly lineages; moves no material goods — no animals, funds, or site access change hands while the commitment is dormant.
% ABSENT_VOICES: Animal-welfare advocates, and the animals who would bear any restored rite's material cost, stand wholly outside the halakhic conversation; non-Orthodox movements that formally retired the commitment are not seated; within traditional circles, voices favoring formal retirement or full symbolic conversion lack adjudicative standing.
% DISAPPEARANCE_RATIONALE: If the performance-only requirement and its preservation apparatus vanished overnight — if the community agreed study fully occupies the commitment, or the commitment was formally retired — yeshiva curricula would shed the sacrificial orders, priestly lineages would lose their prospective office and with it part of the point of their present-day restrictions, restorationist projects (red-heifer breeding, vessel reconstruction) would lose warrant, and messianic liturgy would reframe. The rearrangement concentrates inside the tradition; the outside economy and state barely register it.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), a covenant whose central commandments specified material rites at a fixed site faced the problem of remaining a live commitment across an indefinite performance-impossible exile: how to keep the obligation binding, the knowledge intact, and resumption possible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: academic historians of the Second Temple's destruction and of rabbinic adaptation document the post-70 practice crisis as a historical event, and the physical record — the destroyed site itself, attested by independent archaeology and by the non-Jewish custodial authorities controlling the mount — establishes that the performance site is absent and the practice interrupted. No beneficiary attestation is relied on for the founding facts.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18): the commitment is dormant, no rite is performed, no animal or fund moves, and the study load falls on volunteers who count the study itself as devotion. Suppression (0.08) is a raw structural property — unscaled by power or scope — reflecting only mild social pressure toward continued study; nothing bars exit, and the reform movements demonstrate exit is survivable. Theater ratio (0.35) is moderate: the preservation function is real (executable detail is genuinely maintained), but a growing share of activity — annual avodah recitals, model-sanctuary construction, vessel fabrication — rehearses performance rather than preserves executability, and this reading itself classifies study short of performance as archival. Accessibility collapse (0.5): within the reading's frame, accepting the material requirement collapses substitutes entirely, since nothing else discharges the commitment; yet the cross-reading alternative space remains wide and live. Resistance (0.45): movement-level formal rejection, latent welfare opposition, and intra-traditional transformationist currents. Claimed type rope: the arrangement solves a genuine collective-action problem — carrying complex ritual technology across a civilizational discontinuity — with minimal coercive overhead and net-benefiting participants. The measurement series share one grid (points 0-30, indexing the modern restorationist era in decade units); suppression_requirement is deliberately not serialized because the enforcement picture is static — the scalar above carries it.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. To the restoration-anticipating communities the arrangement is faithful waiting — something they would rebuild identically. To priestly lineages it is a deferred-office bargain: present restrictions priced against a future workplace. To the halakhic authorities it is guardianship — their adjudicative standing exists because the commitment persists unperformed. To the excluded and exited seats — welfare advocates, retired movements — the same structure reads as an obsolete imposition kept alive by inertia and identity. The engine derives these divergences from the power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (restoration communities, priestly lineages, academies) derive low directionality — the arrangement subsidizes their identity, role-definition, and institutional continuity. No victims are declared because none currently bear material costs; consequently no seat sits near the full-target end, and effective extraction stays near the low base epsilon. The excluded seats are presently unaffected (near-symmetric d): their stakes activate only if restoration proceeds. One override: halakhic_authorities appear on the stakeholder surface only as agenda_setter, with no beneficiary/victim declaration for the derivation chain to read, so the canonical fallback would park them mid-scale; structurally their authority is sustained by the commitment's persistence, placing them near the beneficiary end (institutional, d=0.2).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a performance site absent — is still live, so mandatrophy is not resolved and the mismatch consumer should find no zombie flag (contested status x world_rearranges reflects inter-reading dispute, not a dead mandate). The classification guards against two symmetrical errors. Reading the dormant apparatus as a piton — pure theater around an atrophied function — mistakes preservation for performance: by this reading's lights the preservation function is live and load-bearing, which is why theater_ratio stays moderate rather than high. Reading it as a snare — coordinated cover for extraction — fails for want of a victim: nothing is taken, and the arrangement's costs are borne voluntarily by its net beneficiaries. The genuine hazard is temporal: if restoration ever proceeded without the ethical evolution the omega variables track, victims would materialize and the same structure would recompute toward tangled_rope or snare. The low epsilon is a property of dormancy, not of the commitment's content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (performance_only) of the temple_sacrifice_commitment kernel; what would the sibling readings — study_as_exercise, hybrid_preparatory, symbolic_transformation — change structurally if adopted?',
    'Comparative analysis across the four reading-files in the family; the disagreement is located at whether intellectual engagement can discharge a materially specified commandment.',
    'Under study_as_exercise the study apparatus fully occupies the commitment (outstanding obligation near zero, no dormant husk); under symbolic_transformation the material requirement is retired and the future victim surface closes permanently; under hybrid_preparatory study acquires maintainer status over the suspended commitment. Each adoption recomputes this story''s classification from different structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one-of-four readings of a contested kernel; sibling adoptions change the beneficiary/victim surface and epsilon.').

omega_variable(
    restoration_activation_extraction,
    'Does the dormant commitment carry latent extraction that activates if material restoration is attempted without ethical evolution?',
    'Monitoring of concrete restoration projects — animal procurement, altar and vessel construction, site-access changes — and of whether any proposal embeds welfare safeguards, consent mechanisms, or phased implementation.',
    'Activation would introduce victims (slaughtered animals, dissenters subject to coercion, displaced custodians) and push the computed classification toward tangled_rope or snare; continued dormancy keeps extraction near the low base value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_activation_extraction, empirical, 'Latent victim set: the arrangement''s low extraction is conditional on non-performance.').

omega_variable(
    preservation_vs_theater,
    'Is ongoing study genuinely functional preservation of performative capacity, or has it drifted into identity rehearsal with negligible restorative yield?',
    'Test whether studied detail tracks actionable specificity — measurements, procedures, and eligibility rules recoverable and executable by a trained cohort — versus homiletic reuse detached from executability.',
    'High theater would push the arrangement toward piton dynamics within this reading''s own frame (maintenance of an atrophied function); low theater confirms the rope reading and the rising theater series should flatten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_vs_theater, empirical, 'Whether the preservation function is live or the apparatus is drifting into performance of memory.').

omega_variable(
    dormancy_vs_partial_lapse,
    'Is the commitment binding-but-dormant in full, or have categories of it lapsed such that only a subset awaits restoration?',
    'Intra-halakhic analysis of the medieval disputes over which offerings resume — e.g., whether sin-offerings of idolatry return, and the scope of the future service in the major legal codifications.',
    'Partial lapse shrinks the future victim surface and the preservation burden proportionally; full binding sustains the current structure and the full latent-exposure estimate in the activation omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dormancy_vs_partial_lapse, conceptual, 'Scope ambiguity inside the reading: which commandments actually await material instantiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.22).
narrative_ontology:measurement(temp_tr_t6, temple_sacrifice_commitment__performance_only, theater_ratio, 6, 0.25).
narrative_ontology:measurement(temp_tr_t12, temple_sacrifice_commitment__performance_only, theater_ratio, 12, 0.27).
narrative_ontology:measurement(temp_tr_t18, temple_sacrifice_commitment__performance_only, theater_ratio, 18, 0.29).
narrative_ontology:measurement(temp_tr_t24, temple_sacrifice_commitment__performance_only, theater_ratio, 24, 0.32).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__performance_only, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.11).
narrative_ontology:measurement(temp_be_t6, temple_sacrifice_commitment__performance_only, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(temp_be_t12, temple_sacrifice_commitment__performance_only, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(temp_be_t18, temple_sacrifice_commitment__performance_only, base_extractiveness, 18, 0.16).
narrative_ontology:measurement(temp_be_t24, temple_sacrifice_commitment__performance_only, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__performance_only, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the temple sacrifice commitment' decomposes into four epsilon-invariant constraint stories sharing one kernel: performance_only (this file), study_as_exercise, hybrid_preparatory, and symbolic_transformation. Measuring the standing arrangement through different observables — does study discharge the command? was a transformation authorized? — yields different epsilon values, which under DP-001 signals distinct constraints rather than one observable-dependent constraint. This file authors the material-text baseline reading; its edges run to each sibling, and the upstream/downstream ordering follows empirical confidence: the fixed-text material requirement is cited by and exerts pressure on the other readings without resolving them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__performance_only, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
