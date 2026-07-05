% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story isolates the institutional MECHANISM by which marriage
 *   authority converges toward a constitutional floor without formal
 *   legislative codification: case-by-case Supreme Court review. It is
 *   deliberately narrower than a normative reading of who SHOULD hold
 *   marriage authority (that contest belongs to the sibling readings —
 *   communal_autonomy, secularist, gender_rights, federalist_millet). This
 *   story's claim is structural: the judiciary has become the de facto site
 *   of harmonization, accreting a constitutional floor precedent by
 *   precedent, and this mechanism itself has beneficiaries (the judiciary's
 *   institutional authority, litigants able to access it) and payers
 *   (community authorities losing predictable autonomy, ordinary litigants
 *   bearing legal uncertainty, and the legislature whose codification role
 *   atrophies through disuse). It is authored as scaffold because the
 *   mechanism's own justification is transitional — a stopgap for legislative
 *   incapacity — though whether that transitional character will ever resolve
 *   into either full codification or permanent judicial supremacy is exactly
 *   the mandatrophy question below.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.42).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.38).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'd3af983f-693d-4904-8c5d-dee74c87b03a').
narrative_ontology:cs_kernel_codification('d3af983f-693d-4904-8c5d-dee74c87b03a', distributed).
narrative_ontology:cs_authority_grounding('d3af983f-693d-4904-8c5d-dee74c87b03a', practice).
narrative_ontology:cs_interpretation_layer_present('d3af983f-693d-4904-8c5d-dee74c87b03a').
narrative_ontology:cs_reading_relation('d3af983f-693d-4904-8c5d-dee74c87b03a', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('d3af983f-693d-4904-8c5d-dee74c87b03a', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('d3af983f-693d-4904-8c5d-dee74c87b03a', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3af983f-693d-4904-8c5d-dee74c87b03a', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('d3af983f-693d-4904-8c5d-dee74c87b03a', foundational, constitutional_floor_binds_without_legislative_enactment).
narrative_ontology:cs_axiom_status(constitutional_floor_binds_without_legislative_enactment, holdable).
narrative_ontology:cs_axiom_grounding('d3af983f-693d-4904-8c5d-dee74c87b03a', constitutional_floor_binds_without_legislative_enactment, conventional).
narrative_ontology:cs_axiom('d3af983f-693d-4904-8c5d-dee74c87b03a', secondary, case_by_case_adjudication_is_legitimate_substitute_for_codification).
narrative_ontology:cs_axiom_status(case_by_case_adjudication_is_legitimate_substitute_for_codification, holdable).
narrative_ontology:cs_axiom_grounding('d3af983f-693d-4904-8c5d-dee74c87b03a', case_by_case_adjudication_is_legitimate_substitute_for_codification, instrumental).
narrative_ontology:cs_reference_frame('d3af983f-693d-4904-8c5d-dee74c87b03a', post_independence_deference_to_personal_law_autonomy).
narrative_ontology:cs_drift_state('d3af983f-693d-4904-8c5d-dee74c87b03a', contemporary_constitutional_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3af983f-693d-4904-8c5d-dee74c87b03a', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, public_interest_litigants).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, reform_minded_litigants_within_personal_law_systems).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, personal_law_boards_and_community_authorities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, litigants_facing_unpredictable_case_by_case_outcomes).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, legislature_whose_codification_role_is_displaced).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, reform_minded_litigants_within_personal_law_systems).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, constitutional_supremacy_over_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority__judicial_harmonization_reading, basic_structure_doctrine_extends_to_family_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears individual petitions challenging specific personal law provisions (triple talaq, polygamy, maintenance, succession) and decides each on constitutional equality/dignity grounds. Accumulates a body of precedent that functions as a de facto floor across all personal law systems, without ever needing legislative sanction. Gains institutional authority, docket relevance, and doctrinal legacy from being the venue where the harmonization actually happens; bears none of the compliance cost.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, constitutional_judiciary, beneficiary).

% NGOs and advocacy coalitions bring test cases strategically, using the judiciary as a legislature-substitute because the actual legislature has never assembled a majority willing to pass a Uniform Civil Code. They get durable, constitutionally entrenched wins one case at a time, which is slower but more resilient to electoral reversal than statute.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, public_interest_litigants, beneficiary,
    organized, biographical, mobile, national).

% Individuals (frequently women) seeking relief from specific personal law provisions get access to constitutional remedy they would never get from their community's internal forum or from a legislature reluctant to touch the issue. But relief depends on whether and when a suitable test case reaches the top court, and outcomes are provision-by-provision rather than systemic — someone whose grievance doesn't map onto an existing precedent gets nothing.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, reform_minded_litigants_within_personal_law_systems, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, reform_minded_litigants_within_personal_law_systems, payer).

% Administer marriage, divorce, and inheritance according to their own codes and see their authority eroded provision by provision through litigation they cannot predict or negotiate with in the way they could negotiate with a legislature. Each adverse ruling removes autonomy without any comprehensive settlement — there is no floor they can reach and then rely on; the next case can move the floor again.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, personal_law_boards_and_community_authorities, payer,
    organized, civilizational, constrained, national).

% Ordinary parties to marriage or divorce disputes who cannot afford to litigate to the constitutional court and must live under whichever version of personal law prevails in their community until (if ever) a case reaches the top and changes it. They bear the cost of legal uncertainty — not knowing whether their marriage's governing rules will be the same in five years — without the resources to be the vehicle of that change themselves.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, litigants_facing_unpredictable_case_by_case_outcomes, payer,
    powerless, biographical, trapped, local).

% The elected body with formal authority to pass a Uniform Civil Code has repeatedly declined to exercise it, for coalition-management reasons. Judicial harmonization fills the vacuum it leaves, which relieves it of political cost but also strips it of the comprehensive, prospectively-legitimated settlement that only legislation could provide — the harmonization that happens is judicial, not democratic, and the legislature has no formal mechanism to correct or ratify it wholesale.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislature_whose_codification_role_is_displaced, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, legislature_whose_codification_role_is_displaced, excluded).

% Document the pattern of court-led convergence as an alternative to legislative uniformity, comparing it to similar dynamics in other pluralist jurisdictions. Have no stake in outcomes but supply the vocabulary ('constitutional floor,' 'judicial UCC') that other seats use to describe what is happening.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, comparative_family_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for establishing minimum constitutional guarantees (equality, dignity, non-discrimination) across religiously and communally distinct personal law systems in a polity where a single comprehensive legislative code is politically unreachable — coordination happens incrementally, case by case, rather than through one negotiated settlement.
% TRANSFER_FUNCTION: Moves interpretive and normative authority over marriage and family law away from personal law boards and the legislature and toward the constitutional judiciary; moves specific substantive protections (e.g., against arbitrary divorce, discriminatory maintenance) from community authorities to individual litigants who successfully bring test cases, while litigants without access to that forum receive no corresponding transfer.
% ABSENT_VOICES: Ordinary community members who are neither strategic litigants nor board authorities have no seat in the process — they experience the outcomes of test cases as external shocks to the law governing their marriages, but the litigation process itself is inaccessible to them (cost, information, lack of standing in a case not their own). Legislators who might prefer a negotiated, prospective settlement are also structurally absent from the actual harmonization event, since it occurs in a courtroom rather than a chamber.
% DISAPPEARANCE_RATIONALE: If judicial review of personal law stopped tomorrow, the personal law boards would regain de facto insulation from constitutional challenge, reform litigants would lose their only viable venue, and pressure would shift back toward the legislature — which has shown no independent will to act, meaning the practical effect would likely be a freeze of existing personal law provisions rather than their replacement by anything else.
% FOUNDING_PROBLEM: A constitution promising equality and non-discrimination coexists with multiple personal law systems containing provisions that facially conflict with those guarantees, and the legislature empowered to reconcile them lacks the political majority or will to pass a comprehensive Uniform Civil Code.
% FOUNDING_PROBLEM_CORROBORATION: Legislators across multiple electoral cycles have publicly acknowledged the absence of political consensus for a UCC, corroborating that the underlying legislative deadlock persists; comparative law scholars external to any litigating party document the same deadlock pattern in other pluralist constitutional democracies, supporting that the founding problem (legislative incapacity) is real rather than a pretext invented by the judiciary or by litigants.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).
:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.22 to 0.42) because the mechanism increasingly substitutes for a formal costed lawmaking process without producing the comprehensive settlement legislation would provide — accumulating precedent looks like coordination but leaves community authorities and legislatures unable to negotiate a stable, complete resolution, so the transaction cost of legal uncertainty compounds. Suppression is moderate: no one is stopped from litigating, but community authorities cannot appeal to any forum besides accepting adverse precedent, and ordinary litigants without resources cannot access the venue where change actually happens — that is a form of structural suppression even absent overt coercion. Theater is low-moderate and rising slowly, reflecting the judiciary's tendency to frame incremental rulings as modest and case-specific even as their cumulative effect is systemic reconstruction of personal law, which is a genuine but growing performative gap between stated scope and actual effect.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the structural beneficiary: it collects institutional authority and doctrinal legacy from occupying the harmonization role, at zero compliance cost to itself. Public interest litigants and reform-minded individual litigants also benefit, though the latter's benefit is conditional on access. Personal law boards, the legislature, and unresourced litigants are payers: boards lose predictable autonomy, the legislature loses (or cedes) its codification function, and ordinary litigants bear the uncertainty cost of law that can change unpredictably beneath them. This differs from the gender_rights_reading (which would treat outcomes as vindicating equality claims) in that this story is agnostic about whether the substantive outcomes are just — it measures only the mechanism's extraction and suppression profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legislative incapacity to pass a comprehensive code — remains live (per corroboration from legislators themselves and external comparative scholars), which argues against calling this pure mandatrophy: the mechanism still answers a real, unresolved coordination gap. But the scaffold has no legislated sunset; it persists indefinitely by default rather than by design, and its beneficiary (the judiciary) has no structural incentive to hasten a legislative resolution that would end its own harmonizing role. This is the productive tension the scaffold classification is meant to hold open: real transitional function, no actual transition mechanism, and a beneficiary positioned to prefer permanence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_vs_normative_reading_boundary,
    'Is judicial harmonization a neutral institutional mechanism that could in principle serve any of the four normative readings'' goals, or does the mechanism itself structurally favor one reading (gender_rights_reading, since equality claims are the most litigable vehicle) over the others?',
    'Track the doctrinal content of the accumulated precedent set: if it disproportionately expands individual equality claims relative to, say, consociational protections for communal autonomy, the mechanism is not neutral among readings despite being framed as a pure institutional pathway.',
    'If the mechanism structurally favors gender_rights outcomes, this story''s claim to being merely a convergence pathway (rather than a disguised instantiation of the gender_rights_reading) weakens, and the two constraints may need tighter network linkage or reconsideration of independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_vs_normative_reading_boundary, conceptual, 'Whether the harmonization mechanism is normatively neutral among sibling readings or covertly favors one.').

omega_variable(
    judiciary_as_beneficiary_or_reluctant_default,
    'Does the judiciary actively seek the harmonizing role (institutional benefit, doctrinal legacy) or is it a reluctant default forced to decide cases it would prefer the legislature to resolve?',
    'Examine judicial opinions and extrajudicial statements (speeches, dissents urging legislative action) for explicit reluctance versus doctrinal ambition; a consistent pattern of judges calling for legislative codification while nonetheless deciding ever-more-systemic cases would support ''reluctant default.''',
    'If reluctant, the judiciary''s classification as structural beneficiary weakens and the extraction reading shifts toward ''unavoidable byproduct of legislative default'' rather than ''judiciary capturing authority'' — this would lower the appropriate extractiveness attribution to the judiciary seat specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_as_beneficiary_or_reluctant_default, empirical, 'Whether judicial accumulation of harmonizing authority is sought or merely absorbed by default.').

omega_variable(
    sunset_pathway_plausibility,
    'Is there any plausible political pathway by which this scaffold resolves into either comprehensive legislation or an explicit, ratified judicial supremacy doctrine, or is indefinite case-by-case drift the actual steady state?',
    'Longitudinal tracking of legislative attempts at codification over multiple electoral cycles; absence of any serious attempt over an extended multi-decade period would support ''indefinite drift is the steady state'' and would push the classification toward piton rather than scaffold.',
    'If no sunset pathway is plausible, the scaffold classification is itself a false transitional framing and this constraint should be re-evaluated as piton (mostly performative transitional framing over what is actually a permanent, inertial arrangement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_pathway_plausibility, empirical, 'Whether the scaffold has any realistic path to sunset or is permanently transitional in name only.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(marr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(marr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(marr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(marr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(marr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(marr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.36).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This story is the mechanism-level member of the marriage_authority kernel family: it describes HOW convergence actually happens (judicial case-by-case review) rather than asserting WHO SHOULD hold authority. It is linked to all four normative sibling readings because the mechanism it describes is the shared institutional pathway through which each normative claim gets tested, advanced, or blocked in practice — a ruling favoring gender_rights_reading's equality claims is delivered through this exact mechanism, as is any ruling that preserves communal_autonomy_reading's deference to community tradition. Each sibling should link back to this constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
