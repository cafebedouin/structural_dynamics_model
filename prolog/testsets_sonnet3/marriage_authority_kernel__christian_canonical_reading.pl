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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage Authority (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This story authors one reading of the marriage_authority_kernel: the
 *   claim that marriage/family law authority for Indian Christians derives
 *   from Christian canonical law as codified in the Indian Christian Marriage
 *   Act 1872 and the Indian Divorce Act. Extraction has declined modestly
 *   over 150 years as legislative amendment (notably 2001) and
 *   constitutional-equality litigation eroded the harshest fault-based
 *   asymmetries, but the core structure — fault-based dissolution, church
 *   tribunal primacy over annulment, ecclesiastical doctrinal control —
 *   persists and continues to disadvantage wives seeking exit relative to
 *   husbands and relative to parties under sibling readings of the kernel
 *   (Hindu, Muslim, Parsi, secular). This is NOT a claim about marriage law
 *   in general; it is a claim about THIS specific reading's authority
 *   structure, ε, and beneficiary/victim set, distinct from and linked to the
 *   four sibling readings via network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - church_hierarchy: institutional agenda-setter, doctrinal authority behind the codified statute
 *   - clergy_administering_marriage_rites: organized beneficiary, gatekeeping function
 *   - male_spouses_in_intact_marriages: moderate-power beneficiary of asymmetric exit difficulty
 *   - christian_wives_seeking_divorce: powerless, trapped payer bearing the fault-based burden
 *   - christian_women_in_abusive_marriages: powerless, trapped payer at the sharpest edge
 *   - civil_courts: institutional dual-role seat, both enforcer and partial corrective
 *   - womens_rights_advocates: excluded organized voice pressing for reform from outside
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
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Reading of Marriage Authority (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'bc36bd33-b4ad-474f-a368-910d58ae9c10').
narrative_ontology:cs_kernel_codification('bc36bd33-b4ad-474f-a368-910d58ae9c10', fixed_text).
narrative_ontology:cs_authority_grounding('bc36bd33-b4ad-474f-a368-910d58ae9c10', lineage).
narrative_ontology:cs_interpretation_layer_present('bc36bd33-b4ad-474f-a368-910d58ae9c10').
narrative_ontology:cs_reading_relation('bc36bd33-b4ad-474f-a368-910d58ae9c10', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc36bd33-b4ad-474f-a368-910d58ae9c10', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc36bd33-b4ad-474f-a368-910d58ae9c10', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc36bd33-b4ad-474f-a368-910d58ae9c10', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('bc36bd33-b4ad-474f-a368-910d58ae9c10', foundational, marriage_as_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('bc36bd33-b4ad-474f-a368-910d58ae9c10', marriage_as_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('bc36bd33-b4ad-474f-a368-910d58ae9c10', secondary, fault_based_dissolution_required).
narrative_ontology:cs_axiom_status(fault_based_dissolution_required, holdable).
narrative_ontology:cs_axiom_grounding('bc36bd33-b4ad-474f-a368-910d58ae9c10', fault_based_dissolution_required, conventional).
narrative_ontology:cs_reference_frame('bc36bd33-b4ad-474f-a368-910d58ae9c10', colonial_ecclesiastical_codification).
narrative_ontology:cs_drift_state('bc36bd33-b4ad-474f-a368-910d58ae9c10', post_2001_amendment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bc36bd33-b4ad-474f-a368-910d58ae9c10', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, male_spouses_in_intact_marriages).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, clergy_administering_marriage_rites).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_in_abusive_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sanctity_of_marriage_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_jurisdiction_over_matrimony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ecclesiastical framework that the 1872 Act codifies into civil law, sets doctrine on annulment grounds and sacramental validity, and lobbies against liberalizing amendments. Retains authority whether or not congregants remain observant, because the Act channels state enforcement power behind its categories.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Solemnizes marriages, certifies validity, and sits on or advises diocesan tribunals adjudicating annulment petitions. Draws institutional standing and fees from being the necessary gatekeeper; exit would mean abandoning a role the Act makes legally load-bearing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, clergy_administering_marriage_rites, beneficiary,
    organized, biographical, constrained, regional).

% Benefit from a legal regime where dissolving a marriage requires proving fault against the other party, which historically has been easier for husbands to invoke (desertion, adultery standards asymmetrically applied) and harder for wives to use against husbands given evidentiary and social barriers. Their position inside a stable marriage is legally reinforced by the difficulty of the other side exiting.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, male_spouses_in_intact_marriages, beneficiary,
    moderate, biographical, mobile, national).

% Must prove statutory fault grounds (until amendment eased some grounds, cruelty/adultery standards were historically harsher for wives than husbands) to a civil court applying canon-derived categories, or petition a church tribunal for annulment on narrow doctrinal grounds. Facing an abusive or dead marriage, their legal exit is slower, costlier, and more evidentiary than under other personal-law or secular regimes, and social stigma inside the community compounds the legal barrier.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_wives_seeking_divorce, payer,
    powerless, biographical, trapped, national).

% Bear the sharpest cost of fault-based divorce: proving cruelty in court is retraumatizing and slow, and the community's investment in marital sanctity discourages both filing and being believed. For many, the practical exit is prolonged separation without legal dissolution, remaining formally married with none of the marriage's protections.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_in_abusive_marriages, payer,
    powerless, immediate, trapped, local).

% Must navigate whether the Christian canonical framework or a different personal-law/secular framework governs their union, often facing conflicting jurisdictional claims and additional documentation burdens. The kernel's reading assumes both parties fit the Christian communal category, and mismatch generates friction the other readings do not impose in the same way.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, interfaith_christian_couples, payer,
    moderate, biographical, constrained, national).

% Adjudicate divorce and annulment petitions under the Act's fault-based grounds and the Indian Divorce Act's successive amendments, occasionally importing constitutional equality reasoning to soften the harshest gender asymmetries. Sit both inside the enforcement machinery and as a partial check on it, which is why the seat carries dual roles.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, civil_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, civil_courts, agenda_setter).

% Have long argued the fault-based structure and church tribunal primacy disadvantage women disproportionately, and have pushed for amendment (achieved partially in 2001) and for treating Christian women's divorce access as a constitutional equality matter. Largely outside the church's own deliberative bodies that set doctrine.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, womens_rights_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, communally legible framework for solemnizing marriage, establishing legitimacy of children, and adjudicating its dissolution for India's Christian population, sparing individual couples from constructing ad hoc private arrangements and giving the state a codified category to administer.
% TRANSFER_FUNCTION: Moves practical control over marital dissolution from the spouse seeking to exit toward the spouse resisting exit and toward the ecclesiastical/civil apparatus that adjudicates fault — historically concentrated disadvantage on wives seeking divorce, and standing/authority toward church hierarchy and clergy.
% ABSENT_VOICES: Christian women's rights advocates and survivors of marital abuse were not meaningfully present when the 1872 framework and its successors were drafted; their objections entered only through decades of litigation and the 2001 amendment, and church doctrinal bodies remain largely closed to lay input on annulment standards.
% DISAPPEARANCE_RATIONALE: If this reading's authority vanished, Christian couples would default to the secular civil code (Special Marriage Act) or a different personal-law framework; church tribunals would lose civil-legal force for annulment; fault-based divorce barriers would fall away and dissolution would likely move toward the more permissive standards available elsewhere in the kernel contest — a substantial rearrangement for currently trapped parties.
% FOUNDING_PROBLEM: Colonial administrators and the Christian community needed a single codified law to govern marriage, legitimacy, and divorce for British India's Christian subjects, replacing fragmented ecclesiastical practice with a uniform statute enforceable in civil courts.
% FOUNDING_PROBLEM_CORROBORATION: Church hierarchy and many clergy attest the framework still serves a live function protecting marital sanctity and doctrinal coherence. Independent legal scholars, the Law Commission of India (whose reports preceded the 2001 amendment), and women's rights organizations attest that the fault-based structure has substantially outlived any coordination need and now functions chiefly to preserve ecclesiastical jurisdiction and gendered leverage within marriage — corroboration from outside the beneficiary set exists and is substantial.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.52 at present) reflects a structure with a genuine coordination function (uniform marriage/legitimacy/dissolution rules for the Christian community) alongside asymmetric extraction (fault-based divorce burdens fall disproportionately on wives, and church tribunals retain gatekeeping power over annulment that generates no reciprocal benefit for petitioners). Suppression (0.58) captures continuing legal and social barriers to exit, moderated over time by amendment. Theater ratio (0.28) is moderate-low: much of the apparatus performs a real adjudicative function, though an increasing share is defensive doctrinal maintenance as the founding problem (uniform colonial-era codification need) recedes.
 *
 * PERSPECTIVAL GAP:
 *   From the church hierarchy's seat, this reading is a rope: a coordination mechanism providing legal certainty and doctrinal integrity for a religious community's central life-cycle event. From the seat of a wife seeking divorce under fault-based grounds, the same structure operates as enforced extraction — she bears evidentiary and temporal costs a husband historically did not face symmetrically, and the tribunal that could grant relief is staffed by the institution invested in marital permanence. The engine computes these as different per-seat classifications from the same structural data; the tangled_rope claim asserts both the coordination function and the extraction are real, not that one cancels the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchy and clergy sit near the full-beneficiary end: they administer the framework, collect institutional standing, and face no structural cost from its persistence. Male spouses in intact marriages benefit indirectly from the asymmetric difficulty of exit. Christian wives seeking divorce and women in abusive marriages sit near the full-target end: trapped exit options, the fault-based burden falls on them, and the church tribunal's discretion applies directly to their petitions. Interfaith couples experience moderate extraction through jurisdictional friction rather than direct doctrinal targeting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing British India's Christian population with a uniform, administrable marriage/divorce statute where none existed — is largely solved; India now has multiple functioning personal-law and secular alternatives. What persists is the doctrinal claim that ecclesiastical categories should still control divorce and annulment standards for Christians specifically, which increasingly serves institutional and gendered interests rather than solving an unsolved coordination problem. This is a founding_problem_status of 'contested' rather than flatly 'dead' because church hierarchy still asserts the doctrinal function is live; corroboration from the Law Commission and women's rights litigation history supports the 'dead as coordination problem, live as institutional interest' reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_authority_vs_state_codification,
    'Is the Christian canonical reading''s authority grounded in the church''s independent doctrinal jurisdiction, or entirely in the state''s decision to codify and enforce that doctrine through the 1872/1869 Acts — such that the ''canonical'' framing is itself doing legitimating work the state''s coercive apparatus actually performs?',
    'Compare enforcement outcomes where church tribunal rulings on annulment diverge from civil court rulings on the same facts; if civil courts consistently defer to church tribunal findings even absent independent civil-law grounds, the canonical framing is doing real legitimating work beyond state codification.',
    'If authority is substantially state-manufactured rather than genuinely ecclesiastical, the ''lineage'' grounding is partly cover for what is structurally an extraction-grounded arrangement, which would push classification toward snare at the payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_authority_vs_state_codification, conceptual, 'Whether canonical authority is independent doctrine or state-manufactured legitimation.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the christian_canonical_reading and the secular_civil_reading disagree — is it about WHO has jurisdiction over Christian marriages (a jurisdictional dispute) or about WHAT substantive divorce standard should apply (a substantive dispute), given that a Christian couple can already opt into the Special Marriage Act?',
    'Track cases where Christian couples affirmatively choose the secular civil reading over the canonical one; if uptake is substantial and unobstructed, the disagreement is more substantive-preference than jurisdictional-monopoly, weakening the suppression case for this reading.',
    'If exit to the secular reading is genuinely open and commonly exercised, effective suppression for this reading is lower than authored (0.58) and directionality should shift toward mobile for payer seats who simply have not opted out.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, empirical, 'Whether the christian/secular kernel disagreement is jurisdictional monopoly or substantive coexistence with real exit.').

omega_variable(
    gender_equity_trajectory_ceiling,
    'Does the 2001 amendment and subsequent litigation represent a trajectory toward full gender parity in fault standards, or a ceiling beyond which church doctrinal resistance to further liberalization (e.g., on annulment grounds, remarriage recognition) will hold?',
    'Track legislative and church-tribunal responses to pending reform proposals over the next decade; a further amendment or continued doctrinal resistance would resolve the trajectory question.',
    'A hard ceiling would support treating the current moderate-equity state as a stable tangled_rope equilibrium; continued liberalization would support treating current extraction as transitional (scaffold-adjacent) rather than settled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equity_trajectory_ceiling, empirical, 'Whether gender-equity improvement will continue or has reached a doctrinal ceiling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(marr_tr_t60, observed).
narrative_ontology:measurement(marr_tr_t90, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 90, 0.23).
narrative_ontology:measurement_basis(marr_tr_t90, observed).
narrative_ontology:measurement(marr_tr_t120, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 120, 0.26).
narrative_ontology:measurement_basis(marr_tr_t120, observed).
narrative_ontology:measurement(marr_tr_t150, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 150, 0.28).
narrative_ontology:measurement_basis(marr_tr_t150, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(marr_be_t60, observed).
narrative_ontology:measurement(marr_be_t90, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 90, 0.6).
narrative_ontology:measurement_basis(marr_be_t90, observed).
narrative_ontology:measurement(marr_be_t120, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 120, 0.55).
narrative_ontology:measurement_basis(marr_be_t120, observed).
narrative_ontology:measurement(marr_be_t150, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 150, 0.52).
narrative_ontology:measurement_basis(marr_be_t150, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(marr_su_t60, observed).
narrative_ontology:measurement(marr_su_t90, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 90, 0.65).
narrative_ontology:measurement_basis(marr_su_t90, observed).
narrative_ontology:measurement(marr_su_t120, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement_basis(marr_su_t120, observed).
narrative_ontology:measurement(marr_su_t150, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 150, 0.58).
narrative_ontology:measurement_basis(marr_su_t150, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposed from the natural-language concept 'marriage/family law authority in India,' per the ε-invariance principle: each religious/civil personal-law framework generates a structurally distinct constraint with its own ε, beneficiary/victim set, and divorce/annulment regime, rather than one constraint evaluated under different observables. The christian_canonical_reading shares the kernel_id marriage_authority_kernel with hindu_codified_reading, muslim_shariat_reading, parsi_communal_reading, and secular_civil_reading; all five are linked bidirectionally in the family and the committer structure (which reading, what siblings would change, where disagreement is located) is routed to omega variables rather than folded into this story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
