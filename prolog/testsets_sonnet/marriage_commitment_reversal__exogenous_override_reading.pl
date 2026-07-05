% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally Coerced Practice Reversal (Doctrine Unrevised)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the 1890
 *   Manifesto kernel: the reversal of public plural marriage practice by the
 *   LDS Church is here read as a coerced institutional capitulation to
 *   escalating federal legislation (culminating in the Edmunds-Tucker Act's
 *   disincorporation and asset seizure), not as an internally generated
 *   doctrinal revision. Under this reading, Section 132 of the Doctrine and
 *   Covenants — the scriptural basis for plural marriage as an eternal
 *   principle — was never withdrawn or renounced; only the public practice
 *   was suspended, administratively, under duress. The federal government
 *   functions as the extracting party, taking institutional sovereignty and
 *   practice-continuity from the church and from practicing households, in
 *   exchange for the church's continued corporate existence and Utah's
 *   eventual statehood. This is a distinct constraint from its sibling
 *   readings: the endogenous-reinterpretation reading treats the same 1890
 *   announcement as a genuine revelatory event (a different ε — negligible
 *   external extraction, high internal doctrinal legitimacy); the
 *   practice-doctrine-gap reading treats the persistence of Section 132
 *   alongside suspended practice as the central structural fact rather than
 *   adjudicating causation. Each is a separate constraint with its own ε and
 *   its own stakeholder set, linked here only through the shared kernel_id.
 *
 * KEY AGENTS:
 *   - federal_territorial_government: primary beneficiary and coercive agenda-setter (institutional/arbitrage) — extracts institutional sovereignty via legislative threat
 *   - lds_institutional_sovereignty: primary institutional victim (organized/trapped) — absorbs loss of temporal power and public practice to preserve corporate survival
 *   - plural_marriage_practicing_households: primary personal-level victim (powerless/identity_locked) — bears direct legal and relational risk from suspended-but-not-doctrinally-revoked practice
 *   - church_hierarchy_post_1890: administers the coerced transition and captures the institutional-survival benefit while preserving Section 132 in canon
 *   - historians_and_legal_scholars: analytical observer corroborating the coercion timeline against the church's own revelatory framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "1890 Manifesto as Federally Coerced Practice Reversal (Doctrine Unrevised)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '93c593f7-84b9-468e-bc23-d7f8b3c5d051').
narrative_ontology:cs_kernel_codification('93c593f7-84b9-468e-bc23-d7f8b3c5d051', fixed_text).
narrative_ontology:cs_authority_grounding('93c593f7-84b9-468e-bc23-d7f8b3c5d051', extraction).
narrative_ontology:cs_interpretation_layer_present('93c593f7-84b9-468e-bc23-d7f8b3c5d051').
narrative_ontology:cs_reading_relation('93c593f7-84b9-468e-bc23-d7f8b3c5d051', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('93c593f7-84b9-468e-bc23-d7f8b3c5d051', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('93c593f7-84b9-468e-bc23-d7f8b3c5d051', foundational, practice_reversal_caused_by_external_coercion_not_revelation).
narrative_ontology:cs_axiom_status(practice_reversal_caused_by_external_coercion_not_revelation, holdable).
narrative_ontology:cs_axiom_grounding('93c593f7-84b9-468e-bc23-d7f8b3c5d051', practice_reversal_caused_by_external_coercion_not_revelation, empirically_contingent).
narrative_ontology:cs_axiom('93c593f7-84b9-468e-bc23-d7f8b3c5d051', foundational, section_132_remains_operative_unrevoked_scripture).
narrative_ontology:cs_axiom_status(section_132_remains_operative_unrevoked_scripture, holdable).
narrative_ontology:cs_axiom_grounding('93c593f7-84b9-468e-bc23-d7f8b3c5d051', section_132_remains_operative_unrevoked_scripture, conventional).
narrative_ontology:cs_reference_frame('93c593f7-84b9-468e-bc23-d7f8b3c5d051', section_132_eternal_principle_unrevoked).
narrative_ontology:cs_drift_state('93c593f7-84b9-468e-bc23-d7f8b3c5d051', post_edmunds_tucker_1890, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('93c593f7-84b9-468e-bc23-d7f8b3c5d051', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalition).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practicing_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, church_hierarchy_post_1890).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_over_territorial_religious_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Passes and escalates anti-polygamy legislation (Edmunds Act 1882, Edmunds-Tucker Act 1887) disincorporating the church, seizing its property, disenfranchising practitioners, and threatening territorial statehood indefinitely absent compliance. Holds the coercive lever the whole reversal turns on; does not need to persuade the church of anything doctrinally, only to make continuation materially impossible.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government, agenda_setter,
    institutional, generational, arbitrage, national).

% National reformers, rival Utah political factions, and Protestant moral-reform networks who campaigned for federal intervention. They collect the political and moral victory of practice suspension without bearing any of the institutional cost; their exit was never at stake since they were never inside the constraint.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalition, beneficiary,
    organized, biographical, mobile, national).

% The church as an institution loses corporate charter, has assets escheated, faces the literal unseating of Utah's path to statehood. Issues the 1890 Manifesto announcing suspension of new plural marriages in response, while its president frames the document publicly as counsel rather than revelation revoking Section 132 itself. The institution absorbs the loss of temporal power to preserve its continued legal existence; there is no route to keep both plural marriage practice and institutional survival.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty, payer,
    organized, generational, trapped, national).

% Families already living in plural marriages bear the direct personal cost: legal jeopardy, forced separation or concealment, loss of standing, and in many cases continued underground practice at severe personal risk after 1890, since the Manifesto addresses new marriages, not existing ones or the underlying doctrine. Their identity as covenant participants in a preserved principle makes exit from the marriage itself unthinkable even as public practice is suppressed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practicing_households, payer,
    powerless, biographical, identity_locked, regional).

% The presiding quorum administers the transition, controls how the Manifesto is characterized to members and to Congress, and secures statehood and corporate survival as a direct institutional benefit of compliance — while declining to formally rescind Section 132 as scripture, preserving the doctrinal principle in the canon even as the practice is administratively suspended.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, church_hierarchy_post_1890, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, church_hierarchy_post_1890, beneficiary).

% Members and later splinter groups who read the doctrine-practice gap as proof the 1890 reversal was political capitulation rather than genuine revelation, and continue or later revive the practice on the premise that Section 132 was never doctrinally withdrawn. Their objection — that the church substituted institutional survival for revealed principle — is structurally excluded from the official post-Manifesto narrative.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, dissenting_practitioners_and_fundamentalist_successors, excluded,
    powerless, civilizational, trapped, regional).

% Examine the documentary record (Woodruff's diary, the Edmunds-Tucker Act, congressional testimony, the absence of any canonical revocation of Section 132) to assess whether the reversal was internally or externally driven. Their analysis is the primary corroborating source used to evaluate the church's own contemporaneous framing.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, historians_and_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuinely coordinative from this reading's vantage: the arrangement that resulted (public suspension of new plural marriages) solved a problem imposed from outside — continued federal seizure and disincorporation — not a problem internal to the religious community's own coordination needs.
% TRANSFER_FUNCTION: Moves institutional autonomy and practice-continuity from the LDS Church and plural-marriage households to the federal government and the national political coalition that campaigned against the practice; the church's temporal assets, corporate standing, and territorial political future are the transferred stakes.
% ABSENT_VOICES: Existing plural households whose marriages predate the Manifesto and who are not addressed by its text; fundamentalist successors who read the gap between Section 132's continued canonical status and the suspended practice as evidence of unresolved doctrinal contradiction. Neither voice appears in the official 1890 narrative, which frames the Manifesto as sufficient closure.
% DISAPPEARANCE_RATIONALE: If the federal coercive apparatus (Edmunds-Tucker disincorporation, property seizure, statehood conditioning) had not existed, there is no structural reason internal to LDS governance for the practice to have been publicly suspended in 1890 on this reading's premises — the institution would have continued administering plural marriage as a live doctrinal practice, and Utah statehood negotiations would have proceeded on a wholly different, likely much longer, timeline.
% FOUNDING_PROBLEM: The federal government sought to eliminate polygamy as a condition of Utah's admission to statehood and as an assertion of federal supremacy over territorial religious practice; the church sought to preserve its corporate existence and temporal assets against confiscation.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the church's own institutional voice (examining the Edmunds-Tucker Act's escheatment provisions, contemporaneous congressional debate, and the absence of any subsequent canonical revocation of Section 132) corroborate that the timing and language of the Manifesto track the coercive legislative timeline rather than an independently dated revelatory event; the church's own framing, by contrast, emphasizes Woodruff's private account of revelation and is the account the benefiting institution itself supplies.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks at 0.78 in 1890 — the year of the Manifesto itself — reflecting the moment federal pressure most directly converted into institutional capitulation; it recedes afterward (0.5 by 1904) as the coercive apparatus relaxes once statehood is secured (1896) and enforcement need declines. Suppression tracks the same arc but peaks slightly earlier and higher (0.85 at 1887-1890), reflecting the active legal machinery (disincorporation, disenfranchisement, property seizure) required to force compliance — suppression is the raw coercive force, not scaled by the eventual outcome. Theater ratio rises steadily and does not fall with the other metrics (0.55 by 1904): this captures the growing gap between the church's public performance of doctrinal closure (the Manifesto framed as sufficient, later reinforced by the 1904 Second Manifesto) and the substantive fact, under this reading, that Section 132 remains canonical and uncontradicted scripture — the performance of resolution outlives the coercive pressure that produced it.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat this looks like successful, closed coordination (a national moral and political problem solved). From the institutional church's seat, and especially from the individual practicing household's seat, the same events register as coerced extraction under continued unresolved doctrinal commitment — the engine's per-seat computation should diverge sharply between the agenda_setter/beneficiary seats and the payer seats precisely because the underlying structural relationship (who set the terms, who bore the cost, whose doctrine was actually revised) differs by seat, not merely by interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal territorial government sits at the far beneficiary end: it sets the terms, faces no material cost, and achieves its stated policy goal (elimination of the practice as a condition of statehood) without needing to engage the doctrinal substance at all. LDS institutional sovereignty and plural-marriage households are both victims but at different power levels and with different exit geometries: the institution is organized and trapped by its corporate stakes (dissolution vs. compliance), while individual practicing households are powerless and identity-locked — their marriages are pre-existing covenant relationships that cannot simply be exited even when public practice must be suspended. Church hierarchy post-1890 occupies a genuinely dual position: it is simultaneously the payer's agent (administering the coerced loss) and a beneficiary in its own right (securing institutional survival and eventual statehood benefits) — this is exactly the kind of dual-positioned seat the schema's secondary_role field exists for.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine-practice gap is the mechanism that prevents this from being cleanly resolved as either full capitulation or full continuity: because Section 132 was never formally revoked, the founding problem (federal elimination of polygamy) was only ever partially and provisionally solved from the church's institutional perspective, while being treated by the federal government and national coalition as fully and permanently solved. The founding_problem_status is authored as contested precisely because these two readings of resolution never converged — which is what licenses the practice_doctrine_gap sibling reading as a separate, non-redundant constraint rather than mere restatement of this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_causal_priority,
    'Was the 1890 Manifesto causally produced by federal legislative coercion, by an internally authentic revelatory experience, or by some inseparable combination — and can historical evidence adjudicate between these readings at all?',
    'Close documentary analysis of Wilford Woodruff''s diary entries, the timing correlation between Edmunds-Tucker enforcement escalation and the Manifesto''s drafting, and comparison with the church''s own subsequent canonization of the Manifesto (Official Declaration 1) as scripture in 1908 — which itself represents a retrospective doctrinal move that complicates a clean coercion/revelation binary.',
    'If coercion is established as sufficient and necessary cause, this reading''s classification as substantially extractive tangled_rope is strongly supported. If genuine revelation is established as the operative cause regardless of coercive context, the sibling endogenous_reinterpretation_reading better captures the constraint, and this story''s extraction attribution would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causal_priority, conceptual, 'Whether the reversal''s cause is exogenous coercion or endogenous revelation, and whether historical method can settle it.').

omega_variable(
    section_132_non_revocation_significance,
    'Does the continued canonical status of Section 132 (never formally rescinded, still printed in the Doctrine and Covenants) constitute meaningful evidence that the doctrine was preserved as principle while only practice was suspended, or is its retention better explained by institutional reluctance to disturb scriptural continuity regardless of the reversal''s cause?',
    'Examination of internal church deliberations (where available) around 1890-1904 regarding whether to amend or annotate Section 132, and comparison with how the church treats other later-superseded revelations in its canon.',
    'If retention reflects deliberate principle-preservation, it strongly supports this reading''s victim framing (LDS institutional sovereignty forced to publicly abandon a still-affirmed principle). If retention reflects mere scriptural inertia unconnected to any live doctrinal commitment, the doctrine-practice gap is less load-bearing than this reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_132_non_revocation_significance, empirical, 'Whether Section 132''s non-revocation reflects deliberate doctrine-preservation or institutional inertia.').

omega_variable(
    victim_beneficiary_asymmetry_at_household_level,
    'Individual plural-marriage households bore severe direct costs (legal jeopardy, family separation) that were categorically different in kind and degree from the institutional church''s costs (asset loss, political leverage). Does treating both under a single ''payer'' framing obscure a further asymmetry that should be modeled as a separate constraint?',
    'Comparative case analysis of household-level outcomes (prosecutions under the Edmunds Act, documented family separations) versus institutional-level outcomes (property restoration timeline, statehood grant terms) to assess whether the two victim classes experienced sufficiently different extraction profiles to warrant decomposition per the ε-invariance principle.',
    'If household-level extraction is substantially higher and persists longer than institutional-level extraction, a separate constraint story for household-level costs may be warranted rather than folding both into one victim set here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_beneficiary_asymmetry_at_household_level, empirical, 'Whether institutional and individual-household victimhood should be decomposed into separate constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.2).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.3).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.42).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1896, 0.5).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1904, 0.55).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.25).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.45).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.7).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.78).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1896, 0.62).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1904, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.3).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.55).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.82).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.85).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1896, 0.6).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1904, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_reversal kernel, decomposed per the ε-invariance principle because the natural-language label ('the 1890 Manifesto,' 'the reversal of plural marriage') conflates structurally distinct causal and doctrinal claims. exogenous_override_reading (this story) asserts federal coercion as the operative cause and treats Section 132's non-revocation as evidence of incomplete doctrinal capitulation — high extractiveness, tangled_rope. endogenous_reinterpretation_reading asserts genuine internal revelation as the operative cause — low external extraction, closer to a rope or mountain-adjacent legitimate doctrinal development. practice_doctrine_gap brackets the causal question entirely and treats the persisting gap between canonical Section 132 and suspended practice as the primary structural object of study, independent of which causal story is correct. All three share the same historical events but assign different ε values because they answer different structural questions about those events.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
