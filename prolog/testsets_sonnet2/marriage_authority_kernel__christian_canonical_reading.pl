% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This story is one reading of the marriage_authority_kernel: the Indian
 *   personal-law system's kernel commitment that marriage/family law derives
 *   its legitimacy from a religiously-grounded source rather than a unified
 *   secular code. Under the christian_canonical_reading, the Indian Christian
 *   Marriage Act 1872 codifies canonical categories of valid marriage and
 *   matrimonial offense drawn from Christian ecclesiastical tradition,
 *   administered in parallel by civil courts and denominational church
 *   tribunals. This reading is structurally distinct from the sibling
 *   readings that assign the same kernel-function to Hindu codified law,
 *   Shariat, Parsi custom, or the secular civil code — each reading has its
 *   own beneficiary/victim structure, its own ε, and its own classification.
 *   This story does not describe or average across those readings; it authors
 *   only the Christian canonical instantiation.
 *
 * KEY AGENTS:
 *   - church_ecclesiastical_tribunals: Primary agenda-setter (institutional/arbitrage) — administers canonical marriage validity and annulment categories
 *   - clergy_officiants: Beneficiary (organized/constrained) — licensed solemnization authority under the Act
 *   - christian_wives_seeking_divorce: Primary target (powerless/trapped) — bore the pre-2001 fault-based divorce burden most acutely
 *   - christian_spouses_in_failed_marriages: Secondary target (moderate/constrained) — general population bearing restrictive dissolution costs
 *   - interfaith_christian_couples: Target (powerless/constrained) — jurisdictional ambiguity costs
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicated the equality challenge that forced 2001 reform
 *   - womens_rights_advocates: Excluded voice (organized/constrained) — external pressure for reform, not seated in ecclesiastical administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.52).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Reading of Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b').
narrative_ontology:cs_kernel_codification('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', formalized).
narrative_ontology:cs_authority_grounding('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', lineage).
narrative_ontology:cs_interpretation_layer_present('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b').
narrative_ontology:cs_reading_relation('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', foundational, matrimonial_fault_as_sole_dissolution_ground).
narrative_ontology:cs_axiom_status(matrimonial_fault_as_sole_dissolution_ground, overridden).
narrative_ontology:cs_axiom_grounding('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', matrimonial_fault_as_sole_dissolution_ground, conventional).
narrative_ontology:cs_axiom('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', foundational, ecclesiastical_tribunal_annulment_primacy).
narrative_ontology:cs_axiom_status(ecclesiastical_tribunal_annulment_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', ecclesiastical_tribunal_annulment_primacy, conventional).
narrative_ontology:cs_axiom('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', secondary, sacramental_indissolubility_of_marriage).
narrative_ontology:cs_axiom_status(sacramental_indissolubility_of_marriage, holdable).
narrative_ontology:cs_axiom_grounding('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', sacramental_indissolubility_of_marriage, theological).
narrative_ontology:cs_reference_frame('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', canonical_matrimonial_offense_doctrine).
narrative_ontology:cs_drift_state('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', post_2001_amendment_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('5b13ab7e-0ce4-4e4f-95f3-b5bced8a859b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_ecclesiastical_tribunals).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, clergy_officiants).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, denominational_institutions).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_spouses_in_failed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate annulment and, historically, divorce petitions for Christians under canonical categories (adultery as near-sole ground until 2001 reform), certifying marriages performed by ordained clergy and interpreting canonical impediments. Continues to administer marriage registration and annulment proceedings that run parallel to or feed into civil court process, and its doctrinal categories still shape what counts as a valid marital claim before civil judges.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_ecclesiastical_tribunals, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Solemnize marriages under the Act's licensing scheme, deriving institutional standing and fee income from being the recognized officiant class for Christian marriages. Their authority to certify unions depends on the Act's continued deference to denominational procedure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, clergy_officiants, beneficiary,
    organized, generational, constrained, national).

% Dioceses and denominational bodies retain jurisdiction over what counts as a canonically valid marriage and annulment, preserving institutional relevance and moral authority over the community's most intimate legal transactions even where civil courts hold final say over property and custody.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, denominational_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, denominational_institutions, agenda_setter).

% Until the 2001 amendment, could only obtain civil divorce on grounds effectively modeled on adultery-plus-cruelty (a stricter bar than grounds available to Hindu or secular-code wives), and even post-reform continue to navigate a fault-based framework and a parallel ecclesiastical annulment track that shapes social legitimacy of the civil outcome. Exit from a failed marriage costs more in time, stigma, and proof burden than under sibling readings of the same kernel.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% Bear the cost of a restrictive, fault-based dissolution regime requiring proof of matrimonial offense rather than mutual consent or breakdown, prolonging litigation and requiring reconciliation attempts mandated by both canon-influenced procedure and the Act's own cooling-off provisions.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_spouses_in_failed_marriages, payer,
    moderate, biographical, constrained, national).

% Couples where one party is Christian and one is not face jurisdictional ambiguity: whether the marriage falls under this Act, the Special Marriage Act, or another personal law depends on contested characterization, and denominational tribunals may refuse to recognize unions that do not conform to canonical marriage categories, pushing them toward costlier secular-code alternatives.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples, payer,
    powerless, biographical, constrained, national).

% Adjudicate constitutional challenges to the Act's fault-based divorce grounds (as in the 1990s litigation preceding the 2001 amendment) under Article 14/15 equality claims, weighing personal-law autonomy against constitutional equal-protection guarantees, and can compel legislative amendment.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Have long argued the canonical-derived grounds discriminate against Christian women relative to women under other personal-law regimes, but sit outside the ecclesiastical bodies that administer the kernel and must litigate or lobby externally to force reform, as happened in 2001.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, womens_rights_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, denominational_institutions).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, denomination-recognized procedure for solemnizing, registering, and dissolving Christian marriages, so that community, church, and state each recognize the same union as valid without requiring couples to construct ad hoc private arrangements.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority over marital validity and dissolution from the individual couple to church tribunals and civil courts applying canonically-derived categories; moves litigation cost and delay disproportionately onto the spouse (historically and disproportionately the wife) seeking exit from a failed marriage.
% ABSENT_VOICES: Christian women's advocacy groups and interfaith couples are not seated within the ecclesiastical bodies that historically set annulment and divorce standards; their objections reached the kernel only through external constitutional litigation and legislative lobbying, culminating in the 2001 amendment loosening divorce grounds.
% DISAPPEARANCE_RATIONALE: If this reading's authority collapsed, Christian marriages would default either to the secular civil code (Special Marriage Act) or to a legislative vacuum requiring new codification; denominational tribunals would lose their gatekeeping role over annulment, clergy would lose exclusive solemnization standing, and dissolution would likely shift toward consent-based grounds already available to Hindu and secular-code couples.
% FOUNDING_PROBLEM: Colonial administrators needed a uniform statute to govern marriage among India's diverse Christian denominations (Catholic, Anglican, and various Protestant communions), and codified existing Christian canonical categories of valid marriage and matrimonial offense into a single 1872 Act to reduce jurisdictional confusion between denominational practice and colonial courts.
% FOUNDING_PROBLEM_CORROBORATION: Denominational church bodies attest the founding problem remains live — canonical categories still define what a valid Christian marriage is, in their account. Constitutional courts and independent legal scholars (cited in the litigation leading to the 2001 amendment) attest the founding problem of denominational uniformity was solved decades ago and that the surviving restrictive divorce provisions functioned as inherited doctrinal residue rather than a live coordination need, which is why Parliament ultimately amended the fault-based grounds.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate, not extreme) because the coordination function — providing a single recognized marriage/dissolution procedure for a religious minority community — is genuine, but the historically much stricter divorce grounds relative to sibling personal-law regimes constituted a real asymmetric burden, concentrated on women, that required litigation to unwind. Suppression is authored higher (0.58) and shows a step-down at 2001 reflecting the amendment that widened divorce grounds to parity with the Hindu Marriage Act — a genuine loosening of active legal suppression, not merely rhetorical change. Theater ratio rises slowly over the interval: as the underlying fault-based restriction loosened, some of the ecclesiastical tribunal's residual annulment apparatus increasingly performs continuity with tradition rather than resolving disputes civil courts don't already resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Church tribunals and denominational institutions sit at the beneficiary end: they administer and are legitimated by the kernel's canonical framing, and their institutional standing depends on continued deference to their categories. Christian wives seeking divorce sit at the target end with the least mobility (trapped): pre-2001 they had structurally worse dissolution options than similarly situated women under sibling readings of the same kernel, and even post-reform carry residual procedural and social costs. Interfaith couples are targets via jurisdictional ambiguity rather than direct doctrinal harm. Constitutional courts are analytical (institutional/analytical exit) — they adjudicate but do not participate in the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — jurisdictional uniformity for a fragmented colonial-era Christian population — was substantially solved by the mid-20th century, yet the fault-based divorce structure persisted for over a century past its coordination necessity, extracting disproportionate cost from women seeking exit until external constitutional pressure forced the 2001 amendment. This is the mandatrophy signature: the coordination function (uniform recognition) remained genuinely useful, but the specific restrictive mechanism (fault-based-only grounds) outlived its necessity and was sustained by institutional inertia and ecclesiastical resistance to loosening after the coordination need it originally served (denominational consensus-building) had been achieved through consolidation, not through the restrictiveness itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_pressure,
    'Why does this constraint instantiate the christian_canonical_reading of the marriage_authority_kernel rather than the secular_civil_reading, given that Christians in India can already opt into the Special Marriage Act?',
    'Track actual opt-in rates: what fraction of Christian couples choose the Special Marriage Act versus the Indian Christian Marriage Act, and whether social/family pressure to use the denominational Act constitutes de facto suppression of the secular alternative.',
    'If opt-in to the secular reading is heavily suppressed by social and familial pressure despite formal legal availability, this reading''s suppression score understates its true coercive force; if opt-in is genuinely free and common, this reading''s extraction is better characterized as a minority preference that coexists benignly with the secular alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, empirical, 'Whether formal legal pluralism (co-existing secular option) masks informal suppression of exit from the canonical reading.').

omega_variable(
    post_2001_residual_extraction,
    'After the 2001 amendment brought Christian divorce grounds closer to parity with the Hindu Marriage Act, does meaningful asymmetric extraction persist, or has the tangled_rope structure resolved into something closer to a rope?',
    'Comparative empirical study of post-2001 divorce case timelines, cost, and outcomes for Christian petitioners versus Hindu and secular-code petitioners under materially similar fact patterns.',
    'If post-2001 outcomes are now statistically indistinguishable from sibling readings, the current-day classification should trend toward rope; persistent gaps would confirm continued tangled_rope status independent of the formal statutory reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_2001_residual_extraction, empirical, 'Whether the 2001 reform substantively closed the extraction gap or left a residual asymmetry.').

omega_variable(
    ecclesiastical_versus_civil_authority_framing,
    'Should this reading''s kernel be framed as the church tribunal''s canonical authority, or as the civil courts'' statutory incorporation of that authority via the 1872 Act — two coherent framings that could yield different cs_pattern classifications?',
    'Examine whether contested cases are ultimately resolved by ecclesiastical tribunal ruling or by civil court judgment applying the Act''s codified categories; the framing that determines actual case outcomes is the operative authority.',
    'If civil courts are the true final arbiters and church tribunals are advisory/social rather than legally binding, authority_grounding would better read as extraction-via-statute rather than lineage-via-church; this story authors the lineage framing because denominational annulment retains real social and some jurisdictional force, but the alternative framing would shift the cs_structure classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecclesiastical_versus_civil_authority_framing, conceptual, 'Alternative framing of where final interpretive authority actually sits — church tribunal versus civil court applying codified canon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1872, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(marr_tr_t1985, observed).
narrative_ontology:measurement(marr_tr_t2001, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement_basis(marr_tr_t2001, observed).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement_basis(marr_tr_t2012, observed).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(marr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.62).
narrative_ontology:measurement_basis(marr_be_t1872, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement_basis(marr_be_t1985, observed).
narrative_ontology:measurement(marr_be_t2001, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement_basis(marr_be_t2001, observed).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2012, 0.48).
narrative_ontology:measurement_basis(marr_be_t2012, observed).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(marr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.7).
narrative_ontology:measurement_basis(marr_su_t1872, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement_basis(marr_su_t1985, observed).
narrative_ontology:measurement(marr_su_t2001, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement_basis(marr_su_t2001, observed).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2012, 0.56).
narrative_ontology:measurement_basis(marr_su_t2012, observed).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(marr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of marriage_authority_kernel, each a separate constraint file with its own ε, beneficiary/victim structure, and classification (per the ε-invariance decomposition principle). The christian_canonical_reading is linked to all four siblings because each reading structurally influences the others' relative attractiveness: couples who find this reading's fault-based restrictions burdensome can opt into the secular_civil_reading (the Special Marriage Act), creating competitive pressure between readings that the hindu_codified_reading (post-1976 mutual-consent divorce) does not face to the same degree. The 2001 amendment to this reading was itself partly motivated by comparison to the more permissive grounds already available under the hindu_codified_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
