% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: IP as First Holding: Author Entry into Legitimate Claimant Set (1710)
 *   domain: legal/philosophical/historical
 *
 * SUMMARY:
 *   The Statute of Anne 1710 is read by the first-holding perspective as a
 *   structural shift in the legitimate claimant set for printed works: the
 *   author-as-rights-holder entered the occupied set, displacing the
 *   Stationers' Company monopoly. Where previously a corporate guild held and
 *   enforced a private ordering of print control, the statute assigned
 *   statutory title to individual creators, establishing a new enforcement
 *   beneficiary. This constraint story treats the post-1710 author-rights
 *   regime as the standing arrangement under contest: it coordinates literary
 *   markets by assigning clear title, but also extracts from the prior
 *   monopoly holder and suppresses unlicensed reproduction. The first-holding
 *   reading emphasizes membership change over conceptual coherence, treating
 *   1710 as a re-occupation of the property-like space rather than the birth
 *   of a new legal category.
 *
 * KEY AGENTS:
 *   - statutory_authors: Primary beneficiary (moderate/constrained) â entered the legitimate claimant set in 1710
 *   - stationers_company: Primary payer (organized/constrained) â lost monopoly and enforcement beneficiary status
 *   - parliament_crown: Agenda setter (institutional/analytical) â enacted the claimant reassignment
 *   - licensed_booksellers: Secondary beneficiary (moderate/constrained) â operated under new contractual licensing regime
 *   - unlicensed_printers: Secondary payer (powerless/trapped) â suppressed by the new enforcement machinery
 *   - legal_historians: Analytical observer â examine the structural shift and its long doctrinal shadow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "IP as First Holding: Author Entry into Legitimate Claimant Set (1710)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal/philosophical/historical").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, '58572512-6fa8-4f2d-8612-191bc39e9d55').
narrative_ontology:cs_kernel_codification('58572512-6fa8-4f2d-8612-191bc39e9d55', fixed_text).
narrative_ontology:cs_authority_grounding('58572512-6fa8-4f2d-8612-191bc39e9d55', lineage).
narrative_ontology:cs_interpretation_layer_present('58572512-6fa8-4f2d-8612-191bc39e9d55').
narrative_ontology:cs_reading_relation('58572512-6fa8-4f2d-8612-191bc39e9d55', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('58572512-6fa8-4f2d-8612-191bc39e9d55', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('58572512-6fa8-4f2d-8612-191bc39e9d55', foundational, authorial_occupancy_priority).
narrative_ontology:cs_axiom_status(authorial_occupancy_priority, holdable).
narrative_ontology:cs_axiom_grounding('58572512-6fa8-4f2d-8612-191bc39e9d55', authorial_occupancy_priority, deontological).
narrative_ontology:cs_axiom('58572512-6fa8-4f2d-8612-191bc39e9d55', secondary, statutory_claimant_reassignment).
narrative_ontology:cs_axiom_status(statutory_claimant_reassignment, holdable).
narrative_ontology:cs_axiom_grounding('58572512-6fa8-4f2d-8612-191bc39e9d55', statutory_claimant_reassignment, conventional).
narrative_ontology:cs_reference_frame('58572512-6fa8-4f2d-8612-191bc39e9d55', author_centered_property_regime).
narrative_ontology:cs_drift_state('58572512-6fa8-4f2d-8612-191bc39e9d55', contemporary_ip_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('58572512-6fa8-4f2d-8612-191bc39e9d55', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, licensed_booksellers).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_company).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, unlicensed_printers).
narrative_ontology:constraint_vindicates(ip_category_emergence__first_holding_reading, authorial_property_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individual writers and composers who, under the Statute of Anne 1710, became recognized as the originary holders of exclusive rights in their works, entitled to statutory protection and legal recourse against unauthorized printing, though in practice they often assigned those rights to publishers.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% The chartered company of booksellers and printers whose prior monopoly over English print culture was statutorily dissolved in 1710; previously the sole enforcement beneficiary, they became one competitor among many seeking licenses from authors, bearing the cost of lost exclusive control.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_company, payer,
    organized, generational, constrained, national).

% The legislative authority that enacted the Statute of Anne, reassigning the right to control printed works from a corporate guild to individual authors, and establishing the legal machinery of copyright enforcement that displaced guild self-regulation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament_crown, agenda_setter,
    institutional, civilizational, analytical, national).

% Booksellers operating under the new statutory framework who obtained reproduction rights through contract with authors rather than corporate charter, benefiting from a legally codified market in literary property while accepting new compliance and transactional costs.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, licensed_booksellers, beneficiary,
    moderate, biographical, constrained, national).

% Printers and small presses operating outside the statutory framework who faced legal suppression under the new author-centered enforcement regime, lacking the prior tacit toleration afforded by the Stationers' informal monopoly and unable to absorb fines or litigation.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, unlicensed_printers, payer,
    powerless, immediate, trapped, local).

% Analysts examining the 1710 shift as a structural reordering of the legitimate claimant set, debating whether the change marks a genuine normative innovation in property assignment or a redistribution of extraction between guild and author.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves contested ownership of printed works by assigning clear statutory title to authors, enabling a market for literary property without relying on a single corporate guild's private ordering or leaving rights undefined.
% TRANSFER_FUNCTION: Moves the legal power to control reproduction and distribution from the Stationers' Company monopoly to individual authors, who may then transfer or license those rights to competing publishers under a state-backed framework.
% ABSENT_VOICES: Common readers, unlettered producers of folk culture, and continental authors whose works circulated in England without statutory protection; they would challenge the narrowness of the author category, the territorial limits of the regime, and the silence on non-print media.
% DISAPPEARANCE_RATIONALE: If the author-as-rights-holder framework had vanished after 1710, the Stationers' monopoly would likely have reasserted itself or a different property regime emerged; the entire structure of Anglo-American copyright, licensing markets, and author-publisher relations would reorganize around a different claimant set.
% FOUNDING_PROBLEM: The Stationers' Company monopoly had politically and economically broken down, creating uncertainty about who could legitimately print what, and the prior system offered no statutory recognition of authors' interests against publishers or rival printers.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and competing non-Stationer pamphleteers from the 1690sâ1710s attest that the Stationers' monopoly had become politically untenable; modern legal historians outside the publishing industry corroborate that the statutory shift responded to genuine pressure, though author-advocacy groups and publishing heirs also self-assert the claim.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the regime creates statutory monopolies with rent-like features, though it is not pure extraction because it solves a genuine title-assignment problem. Suppression (0.62) reflects the active legal machinery required to move enforcement from guild self-regulation to state-backed author rights. Theater ratio (0.32) captures the growing performative dimension of authorial rhetoric as the regime matures, especially as corporate holders adopt author-centered justifications while practice drifts. Accessibility collapse (0.60) measures how alternatives to the author-rights framework (commons-based production, guild continuance) collapsed once statutory copyright was established. Resistance (0.72) registers the Stationers' opposition and subsequent struggles over the limits of authorial control.
 *
 * PERSPECTIVAL GAP:
 *   The statutory author seat experiences the constraint as legitimate entry into a previously closed claimant set â a coordinative gain. The Stationers' Company seat experiences the same statute as expropriation of a generations-old enforcement privilege. The licensed bookseller seat experiences mixed effects: new contractual opportunities but also new compliance burdens. These divergences are structurally determined by the directionality derivation: beneficiaries (authors, licensed booksellers) sit at low d, while payers (Stationers, unlicensed printers) sit at high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as statutory_authors and licensed_booksellers: they receive legal title and market position from the constraint. Victims are declared as stationers_company and unlicensed_printers: they bear the costs of displaced monopoly and legal suppression. Parliament sits as agenda_setter with analytical exit, outside the benefit/cost flow. The engine will derive low d for authors and booksellers, high d for Stationers and unlicensed printers, producing the seat divergence that makes this a tangled_rope rather than a uniform rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy check, one might misread the Statute of Anne as a scaffold (transitional support for authors) or a pure rope (neutral coordination). The founding problem â breakdown of Stationers' monopoly control â is dead: the monopoly is not returning, and the author-rights regime has persisted for three centuries. The mismatch between founding_problem_status: dead and disappearance_verdict: world_rearranges flags this as a persisted arrangement whose coordination function has atrophied into institutional inertia, though not yet pure piton because active enforcement still serves real (if evolved) market functions. The classification as tangled_rope captures that both coordination and extraction remain structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    first_holding_vs_thinkability,
    'Does the 1710 shift represent a genuine conceptual innovation in legal coherence (thinkability) or merely a redistribution of existing property-like claims to a new occupant (first holding)?',
    'Historical analysis of pre-1710 Stationers'' records versus statutory drafting history; if pre-1710 claims were conceptually incoherent without the statute, thinkability is supported; if they were simply misassigned, first-holding is supported.',
    'Resolution determines whether the kernel describes a category emergence or a claimant reassignment, altering whether the constraint is read as ontological innovation or political redistribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(first_holding_vs_thinkability, conceptual, 'Whether 1710 marks conceptual coherence or claimant reassignment').

omega_variable(
    authorial_rights_naturalness,
    'Is the author-as-rights-holder a natural moral fact that the statute recognized, or a conventional legal fiction constructed for policy ends?',
    'Examination of natural-law jurisprudential sources in 1710 parliamentary debates versus instrumentalist policy justifications; subsequent doctrinal history showing oscillation between natural-rights and positivist framings.',
    'If purely conventional, the constraint''s persistence depends on continued enforcement and institutional maintenance rather than recognition of a pre-legal fact, raising its effective extraction and lowering its coordination purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_rights_naturalness, conceptual, 'Natural law versus conventional basis of authorial rights').

omega_variable(
    enforcement_beneficiary_shift,
    'Did the shift from Stationers to authors as enforcement beneficiaries represent a genuine expansion of protected parties, or did publishers capture author rights through contractual assignment?',
    'Quantitative analysis of 18th-century publication contracts and litigation records: who initiated suits, who collected damages, and how quickly authors assigned rights to publishers.',
    'If publishers immediately captured author rights, the first-holding reading''s coordination story is partly cover for a continued publisher-extractive regime, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_beneficiary_shift, empirical, 'Whether author rights were captured by publishers contractually').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_first_hold_tr_t0, ip_category_emergence__first_holding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ip_first_hold_tr_t50, ip_category_emergence__first_holding_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement(ip_first_hold_tr_t100, ip_category_emergence__first_holding_reading, theater_ratio, 100, 0.2).
narrative_ontology:measurement(ip_first_hold_tr_t150, ip_category_emergence__first_holding_reading, theater_ratio, 150, 0.24).
narrative_ontology:measurement(ip_first_hold_tr_t200, ip_category_emergence__first_holding_reading, theater_ratio, 200, 0.27).
narrative_ontology:measurement(ip_first_hold_tr_t250, ip_category_emergence__first_holding_reading, theater_ratio, 250, 0.3).
narrative_ontology:measurement(ip_first_hold_tr_t300, ip_category_emergence__first_holding_reading, theater_ratio, 300, 0.32).

% Extraction over time
narrative_ontology:measurement(ip_first_hold_be_t0, ip_category_emergence__first_holding_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ip_first_hold_be_t50, ip_category_emergence__first_holding_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(ip_first_hold_be_t100, ip_category_emergence__first_holding_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement(ip_first_hold_be_t150, ip_category_emergence__first_holding_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(ip_first_hold_be_t200, ip_category_emergence__first_holding_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(ip_first_hold_be_t250, ip_category_emergence__first_holding_reading, base_extractiveness, 250, 0.6).
narrative_ontology:measurement(ip_first_hold_be_t300, ip_category_emergence__first_holding_reading, base_extractiveness, 300, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(ip_category_emergence__first_holding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into at least three structurally distinct readings: first_holding_reading (membership shift in claimant set), thinkability_reading (conceptual coherence of ownable expression), and synchronic_diachronic_seam (temporal framing artifact). Each reading carries a different epsilon and a different structural relationship to the 1710 Statute of Anne. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
