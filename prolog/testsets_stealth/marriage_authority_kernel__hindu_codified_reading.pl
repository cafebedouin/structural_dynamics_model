% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Codified Hindu Personal Law Regime (Hindu Marriage Act 1955, Civil-Court Reading)
 *   domain: legal/religious-governance/comparative-constitutional
 *
 * SUMMARY:
 *   This story instantiates one reading of the marriage_authority_kernel: the
 *   claim that family-law authority for the Hindu community legitimately
 *   flows from state-enacted codification — the Hindu Marriage Act 1955 and
 *   its companion statutes (Succession 1956, Minority and Guardianship 1956,
 *   Adoptions and Maintenance 1956) — as interpreted by civil courts rather
 *   than religious forums. The codification was the Nehruvian reform
 *   settlement: it unified a fragmented customary landscape, introduced
 *   divorce and monogamy, and transferred adjudication from pandits and caste
 *   panchayats to the judiciary, while preserving pockets of custom through
 *   saving clauses and leaving restitution of conjugal rights and a weakly
 *   executed maintenance regime as its patriarchal residue. The expected
 *   structural delta against sibling readings — uniform rules within the
 *   community, state adjudication, gender equity better than the Shariat
 *   reading and worse than the secular civil reading — is exactly the hybrid
 *   signature this story authors: genuine coordination delivered through the
 *   same structure that carries asymmetric costs. The claim and the metrics
 *   are independent authored facts: claimed_type records the structure I
 *   judge true; the metrics record the operation I judge descriptively
 *   accurate; where the engine's per-seat computation diverges from the
 *   claim, that divergence is data, not error.
 *
 * KEY AGENTS:
 *   - - indian_legislature: Agenda-setter (institutional/arbitrage) — enacted and amends the statute; the only seat that can rewrite the arrangement
 *   - - civil_judiciary: Primary beneficiary and co-administrator (institutional/arbitrage) — absorbed the adjudicatory and interpretive authority; every petition runs through its dockets
 *   - - hindu_women_statutory_rights_holders: Net beneficiary seat (moderate/constrained) — holds divorce, maintenance, and since 2005 equal coparcenary rights that uncodified custom denied
 *   - - hindu_women_subject_to_restitution_orders: Primary target seat (moderate/trapped) — bears Section 9 coercion and maintenance-execution failure
 *   - - hindu_men_litigants: Dual-positioned litigant seat (moderate/constrained) — pays maintenance and monogamy obligations, wields Section 9 leverage
 *   - - customary_law_communities: Overridden party (organized/trapped) — lost regional and matrilineal custom to uniform codification without a consent seat
 *   - - traditional_dharmashastric_authorities: Dispossessed interpreter (moderate/identity_locked) — lost legal interpretive authority to civil courts
 *   - - interfaith_couples: Excluded party (moderate/trapped) — outside the Act's scope entirely; routed into the Special Marriage Act notice regime
 *   - - feminist_legal_scholars: Analytical observer (moderate/analytical) — documents the gap between the reform narrative and courtroom outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.52).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.5).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Codified Hindu Personal Law Regime (Hindu Marriage Act 1955, Civil-Court Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "legal/religious-governance/comparative-constitutional").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'fdcd56d6-afa3-4a63-bbb8-cb9934650569').
narrative_ontology:cs_kernel_codification('fdcd56d6-afa3-4a63-bbb8-cb9934650569', formalized).
narrative_ontology:cs_authority_grounding('fdcd56d6-afa3-4a63-bbb8-cb9934650569', expertise).
narrative_ontology:cs_interpretation_layer_present('fdcd56d6-afa3-4a63-bbb8-cb9934650569').
narrative_ontology:cs_reading_relation('fdcd56d6-afa3-4a63-bbb8-cb9934650569', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdcd56d6-afa3-4a63-bbb8-cb9934650569', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdcd56d6-afa3-4a63-bbb8-cb9934650569', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdcd56d6-afa3-4a63-bbb8-cb9934650569', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('fdcd56d6-afa3-4a63-bbb8-cb9934650569', foundational, state_codification_of_religious_family_law_is_legitimate).
narrative_ontology:cs_axiom_status(state_codification_of_religious_family_law_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('fdcd56d6-afa3-4a63-bbb8-cb9934650569', state_codification_of_religious_family_law_is_legitimate, conventional).
narrative_ontology:cs_axiom('fdcd56d6-afa3-4a63-bbb8-cb9934650569', secondary, legislative_amendment_is_the_reform_mechanism).
narrative_ontology:cs_axiom_status(legislative_amendment_is_the_reform_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('fdcd56d6-afa3-4a63-bbb8-cb9934650569', legislative_amendment_is_the_reform_mechanism, instrumental).
narrative_ontology:cs_reference_frame('fdcd56d6-afa3-4a63-bbb8-cb9934650569', codified_statutory_personal_law).
narrative_ontology:cs_drift_state('fdcd56d6-afa3-4a63-bbb8-cb9934650569', contemporary_ucc_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fdcd56d6-afa3-4a63-bbb8-cb9934650569', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, indian_legislature).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_rights_holders).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_subject_to_restitution_orders).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, customary_law_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, traditional_dharmashastric_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_men_litigants).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_rights_holders).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_men_litigants).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, legislative_supremacy_over_personal_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Hindu Marriage Act and companion statutes in 1955-1956 and holds power to amend or replace them by simple parliamentary majority. Gains a uniform, governable family-law domain for the largest religious community; manages the political cost of touching communal law by amending rarely and selectively, as with the 2005 succession amendment.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, indian_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets and applies the Act through the family courts, High Courts, and Supreme Court; every divorce, maintenance, custody, and restitution petition runs through its dockets. Absorbed the interpretive role that dharmashastric authorities previously held and shapes the law's meaning through precedent without needing new legislation. Its constitutional role forecloses declining the jurisdiction.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_judiciary, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, civil_judiciary, agenda_setter).

% Hold divorce, monogamy protection, maintenance, and — since the 2005 amendment — equal coparcenary inheritance rights that uncodified custom largely denied. Exercising these rights requires litigation through congested courts, and the same statute that confers the rights also carries provisions (restitution of conjugal rights, weak execution of maintenance orders) that operate against them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_rights_holders, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_women_statutory_rights_holders, payer).

% Face court orders under Section 9 to return to the matrimonial home; refusal can cost them maintenance claims and be pleaded as desertion in later divorce proceedings. Their practical alternatives — separation without decree, or a divorce they may be unable to prove grounds for — are slower and costlier than complying with the order.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_subject_to_restitution_orders, payer,
    moderate, biographical, trapped, national).

% Are bound to monogamy and liable to maintenance and alimony under the Act; they also invoke Section 9 restitution orders and benefit from interpretive readings that have favored breadwinner claims. Their obligations are enforceable against them; their leverage inside marriage disputes varies with income and family standing.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_men_litigants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_men_litigants, beneficiary).

% Were governed before 1955 by regionally and sect-wise varying custom, including matrilineal property systems in parts of Kerala and the northeast. The uniform code overrode much of this, preserving pockets through saving clauses. These communities had no consent-based seat in the codification and cannot opt back into their overridden customs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, customary_law_communities, payer,
    organized, generational, trapped, regional).

% Pandits, shastris, and dharmashastric scholars whose interpretive authority over marriage, divorce, and succession was displaced by statute and civil adjudication. They retain ceremonial roles but no legal decision power; their standing is constituted by the very tradition the statute codified around them, so exiting means ceasing to be what they are.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, traditional_dharmashastric_authorities, excluded,
    moderate, generational, identity_locked, national).

% A Hindu marrying a non-Hindu cannot marry under this Act at all; the couple is pushed to the Special Marriage Act, whose 30-day public notice regime exposes them to family and community opposition, sometimes violence. The communal boundary that defines this reading's scope is drawn without them in the conversation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, excluded,
    moderate, biographical, trapped, national).

% Litigate and write against Section 9 and for maintenance-execution reform, and document the gap between the statute's reform narrative and courtroom outcomes. They hold no decision power over the statute; their influence runs through constitutional litigation, Law Commission consultations, and public argument.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, feminist_legal_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, civil_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces fragmented, regionally variable Hindu customary law with a single statutory framework for marriage, divorce, maintenance, and succession adjudicated by civil courts — giving the community uniform rules, enforceable marital exit, and recorded property transmission.
% TRANSFER_FUNCTION: Moves adjudicatory authority over family life from customary and dharmashastric forums to civil courts; moves statutory maintenance payments from husbands to wives; moves ceremonial conformity and registration compliance from families to the state; and, relative to uncodified custom, moves secure divorce and inheritance rights to women.
% ABSENT_VOICES: Matrilineal and customary communities whose practices were overridden were not parties to the codification and object from outside the statute's drafting history; dharmashastric authorities lost interpretive standing without a seat in the civil framework; interfaith couples are outside the Act's scope entirely and have no voice in how the communal boundary that excludes them is drawn.
% DISAPPEARANCE_RATIONALE: Millions of registered marriages, divorces, maintenance orders, and successions currently governed by the Act would lose their governing framework overnight; courts would fall back on fragmented custom or force couples into the secular code; the state's family-law adjudication and the rights structure women hold under the statute would require immediate reconstruction.
% FOUNDING_PROBLEM: Post-independence India inherited fragmented Hindu customary law that varied by region and sect, denied women divorce and secure inheritance, and was administered through unpredictable custom; the new state needed uniform, enforceable family law as both a nation-building and a social-reform project.
% FOUNDING_PROBLEM_CORROBORATION: Law Commission of India consultation papers and reports, Supreme Court and High Court dicta (including the Sareetha-Saroj Rani line on restitution of conjugal rights), and academic legal history attest both halves: fragmentation and denial of women's exit were real and were substantially addressed, while Section 9 and maintenance-execution gaps keep part of the founding problem live. These sources sit outside the statute's beneficiary set (the state and judiciary), so the status is corroborated rather than self-asserted.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 because the standing arrangement delivers real, enforceable gains (divorce, monogamy protection, maintenance claims, and since the 2005 amendment equal coparcenary inheritance) while carrying durable asymmetric costs: Section 9 restitution decrees that order women back into marriages, maintenance awards that stall in execution for years, evidentiary burdens on cruelty and desertion, and the legislative override of matrilineal and regional customs without those communities' consent. Suppression is authored at 0.50: the regime is coercively administered (decrees, contempt exposure, bigamy prosecution) but alternatives partially survive — the Special Marriage Act opt-out, protected customs, and the frame's own amendment mechanism. Theater is 0.25: adjudication is real work, but a growing share of the regime's public life is the performance of the 'reformed Hindu law' narrative while execution-stage failure persists. Accessibility_collapse at 0.45 reflects alternatives that persist but carry social price; resistance at 0.55 reflects sustained feminist litigation against Section 9, Law Commission attention, uniform-civil-code pressure from one flank, and conservative resistance from the other. The measurement series run on one shared grid (t = years since 1955); the 2005 dip in extractiveness is the coparcenary amendment — a real equity gain delivered through the frame's own mechanism — after which enforcement-gap accumulation resumes. The series are not cyclical; the single dip is a discrete legislative event, not an oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently because the same statute is a different constraint at each seat. From the legislature and judiciary the arrangement is a reform achievement and a coordination framework they administer; from the restitution-targeted seat it is a coercive order backed by maintenance forfeiture and desertion findings; from customary communities it is dispossession of inherited law; from dharmashastric authorities it is the loss of interpretive standing; from interfaith couples it is a boundary that excludes them and routes them into the Special Marriage Act's notice regime. The engine derives these per-seat types from the power, exit, and beneficiary/victim data authored here; the divergence between the agenda-setter's view and the trapped payers' view is the perspectival gap the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: the civil judiciary (collects adjudicatory jurisdiction and the interpretive role dharmashastric authorities lost) and the legislature (collects a governable family-law domain) sit at the beneficiary end with arbitrage-grade exit; hindu_women_statutory_rights_holders hold enforceable rights custom denied and sit low-to-mid. Victim declarations: women facing Section 9 orders (trapped — refusal risks maintenance forfeiture and desertion findings), customary law communities (trapped — overridden custom is not recoverable), and dharmashastric authorities (identity_locked — their standing is constituted by the displaced tradition) sit at the target end. Hindu men sit near symmetric: they pay maintenance and monogamy obligations while wielding Section 9 leverage and benefiting from interpretive bias. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is the engine-scaled quantity — amplified for trapped target-side seats, damped toward subsidy for the judiciary and rights-holding seats, and modestly scope-amplified at national scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented custom denying women divorce and secure inheritance, adjudicated unpredictably — was largely solved by codification, and the frame's own amendment mechanism renewed the mandate in 2005 (equal coparcenary). This is not an inertial remnant: adjudication is real, theater is low, and the legislature retains both the power and the political exposure to change it. Nor is it pure extraction: classifying it as such would erase the rights transfer that actually occurred. The tangled_rope claim keeps both halves visible and prevents the two mislabelings — coordination-as-innocence (which would bury Section 9 and the maintenance-execution failure) and extraction-as-totality (which would erase the divorce and inheritance gains women hold relative to custom). Residual atrophy risk sits in specific provisions, not the frame: Section 9 is a vestige whose original marital-repair function has decayed into litigation leverage, tracked by the restitution_constitutional_status omega; the maintenance_enforcement_gap omega tracks whether a second provision has gone nominal in execution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_omega,
    'This constraint is the hindu_codified_reading of marriage_authority_kernel, one of five live readings. What structurally changes under the sibling readings (muslim_shariat_reading, christian_canonical_reading, parsi_communal_reading, secular_civil_reading), and where exactly is the disagreement located?',
    'Compare the sibling stories'' beneficiary/victim sets, epsilon values, and exit structures. The disagreement is located in the authority-source premise: whose act grounds family-law authority (divine command, community custom, state statute, or individual civil right).',
    'Under the secular_civil_reading the communal boundary and its excluded populations vanish and the victim set re-forms around notice-regime exposure; under muslim_shariat_reading adjudication moves from civil courts to community boards and qazis, changing who is trapped. This file''s classification holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_omega, conceptual, 'Committer structure: one reading of a five-reading kernel; siblings are separate constraints with separate epsilon values.').

omega_variable(
    restitution_constitutional_status,
    'Is Section 9 restitution of conjugal rights compatible with post-Puttaswamy dignity and privacy jurisprudence, or does the Sareetha line (which struck it down in 1983 before Saroj Rani reversed) prevail on a larger bench?',
    'A constitutional challenge reaching a Supreme Court bench larger than the two-judge Saroj Rani court, decided under the post-2017 privacy doctrine.',
    'Striking Section 9 removes the regime''s most coercive provision against women, lowering effective extraction on the trapped seat and moving the reading toward its coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_constitutional_status, empirical, 'Constitutional survival of the regime''s most coercive provision.').

omega_variable(
    maintenance_enforcement_gap,
    'What share of maintenance awarded under the Act is actually realized, and does execution delay and failure track the claimant''s gender and resources?',
    'Execution-stage docket data across family courts: time from award to receipt, attachment rates, arrears accumulation.',
    'A large realized gap means the statutory maintenance right is substantially nominal, raising the effective burden on women and tilting the per-seat computation toward the target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_enforcement_gap, empirical, 'Whether the maintenance right is real in execution or nominal on paper.').

omega_variable(
    ucc_supersession_trajectory,
    'Will a national uniform civil code under Article 44 supersede this reading''s standing arrangement, and does the Uttarakhand 2024 state code generalize?',
    'Central legislative action; watch whether the Uttarakhand model is replicated or remains a state outlier.',
    'National supersession would retire this arrangement as a standing constraint and convert its classification into historical record; continued fragmentation preserves the five-reading kernel structure indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_supersession_trajectory, empirical, 'Whether the kernel''s readings are headed for consolidation under the secular reading.').

omega_variable(
    customary_survival_extent,
    'How much pre-codification custom survives under the Act''s saving clauses and succession-act exceptions, and does the surviving custom protect or harm the women inside those communities?',
    'Empirical legal mapping of pleaded and judicially recognized protected customs in family-court records.',
    'Wide survival lowers accessibility_collapse (alternatives persist) and refines the victim set: customary communities may be partially shielded or doubly burdened depending on which customs survive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_survival_extent, empirical, 'Extent and valence of surviving customary law inside the codified frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'marriage/family law authority in India' covers five structurally distinct claims — one per community reading plus the secular civil code — each with its own epsilon, beneficiary/victim structure, and stakeholder surface. This file instantiates the hindu_codified_reading only; its epsilon (0.52) reflects the codified-Hindu arrangement's hybrid operation, which differs from the shariat reading's (board/qazi adjudication, unilateral-divorce victim set) and from the secular reading's (no communal boundary, but notice-regime exposure and its own costs). The hindu codified reading is upstream of the secular reading in the reform sequence: the 1955-56 codification demonstrated that parliamentary reform of personal law is feasible, and uniform-civil-code advocacy cites it as the template the secular reading would complete. Sibling files link back via their own network blocks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
