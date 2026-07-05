% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 as a Bounded Remedy for Documented 1215 Royal Abuses (Originalist Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the originalist_limitation_reading of the
 *   magna_carta_clause_39 kernel: Clause 39 as a narrowly bounded remedy for
 *   specific, documented abuses committed by King John's administration
 *   against the baronial class in the years preceding 1215 — disseisin
 *   without judgment, arbitrary imprisonment, extralegal fines and reliefs.
 *   Under this reading the clause's victim set (the crown administration
 *   whose discretion was curtailed) and beneficiary set (barons and the free
 *   tenants within their contemplated class) are both fixed to the 1215
 *   negotiating context. The clause does not, on this reading, establish a
 *   general theory of due process or preserve a static feudal hierarchy —
 *   those are the liberal_due_process_reading and feudal_prerogative_reading,
 *   generated as separate sibling constraints with their own ε values, victim
 *   sets, and classifications. This story's ε is stable at approximately
 *   0.34-0.5 across the interval precisely because it is scoped to the
 *   documented 1215 grievances and does not track later doctrinal extension.
 *
 * KEY AGENTS:
 *   - baronial_signatories_1215: primary beneficiary and agenda-setter, negotiated and enforces the bounded limitation
 *   - king_john_administration: primary target, bears the curtailment of previously unchecked prerogative
 *   - royal_exchequer_officials: secondary payer, operational discretion narrowed
 *   - unfree_peasantry_excluded_from_charter: excluded voice, structurally identical grievance but outside 1215 scope
 *   - legal_historians: analytical observer, reconstructs original drafting intent from documentary record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.34).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.28).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 as a Bounded Remedy for Documented 1215 Royal Abuses (Originalist Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '7a10c687-6a2c-4298-be3a-2c141fc3b194').
narrative_ontology:cs_kernel_codification('7a10c687-6a2c-4298-be3a-2c141fc3b194', fixed_text).
narrative_ontology:cs_authority_grounding('7a10c687-6a2c-4298-be3a-2c141fc3b194', lineage).
narrative_ontology:cs_interpretation_layer_present('7a10c687-6a2c-4298-be3a-2c141fc3b194').
narrative_ontology:cs_reading_relation('7a10c687-6a2c-4298-be3a-2c141fc3b194', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a10c687-6a2c-4298-be3a-2c141fc3b194', magna_carta_clause_39__liberal_due_process_reading, influences).
narrative_ontology:cs_axiom('7a10c687-6a2c-4298-be3a-2c141fc3b194', foundational, clause_scope_fixed_by_1215_negotiating_record).
narrative_ontology:cs_axiom_status(clause_scope_fixed_by_1215_negotiating_record, holdable).
narrative_ontology:cs_axiom_grounding('7a10c687-6a2c-4298-be3a-2c141fc3b194', clause_scope_fixed_by_1215_negotiating_record, empirically_contingent).
narrative_ontology:cs_axiom('7a10c687-6a2c-4298-be3a-2c141fc3b194', secondary, founding_grievance_resolution_narrows_legitimate_application).
narrative_ontology:cs_axiom_status(founding_grievance_resolution_narrows_legitimate_application, holdable).
narrative_ontology:cs_axiom_grounding('7a10c687-6a2c-4298-be3a-2c141fc3b194', founding_grievance_resolution_narrows_legitimate_application, conventional).
narrative_ontology:cs_reference_frame('7a10c687-6a2c-4298-be3a-2c141fc3b194', runnymede_1215_negotiated_settlement).
narrative_ontology:cs_drift_state('7a10c687-6a2c-4298-be3a-2c141fc3b194', post_1225_reissue_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('7a10c687-6a2c-4298-be3a-2c141fc3b194', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, landholding_freemen_within_baronial_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_administration).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, royal_exchequer_officials).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, unfree_peasantry_excluded_from_charter).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, sovereign_power_is_bounded_by_prior_agreement).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, documented_grievance_grounds_legitimate_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the charter at Runnymede under threat of civil war against King John, having suffered specific documented abuses: disseisin without judgment, arbitrary imprisonment, extralegal exaction of fines and reliefs. Clause 39 is drafted to bind the crown against exactly these enumerated practices as they were exercised against the baronial class. They administer the charter's enforcement through the security council of twenty-five barons and benefit directly from the restored procedural predictability.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, baronial_signatories_1215, beneficiary).

% Forced under military and political pressure to accept a specific, enumerated limitation on the previously unchecked royal prerogative to seize, imprison, or dispossess without judgment. The crown's exit options are foreclosed by the armed coalition arrayed against it; it bears the cost of the newly bounded prerogative but only with respect to the free tenants covered by the charter's actual 1215 terms, not a general renunciation of sovereign power.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_administration, payer,
    institutional, immediate, trapped, national).

% Administered the extralegal fines, disseisins, and arbitrary judgments that provoked baronial revolt. Under the originalist reading, Clause 39 directly names and curtails their prior operating procedure; their discretion is narrowed specifically with respect to the documented practices the barons complained of, not reformed as a general theory of due process.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, royal_exchequer_officials, payer,
    moderate, immediate, constrained, national).

% Free tenants below the great barons but within the charter's contemplated class gain the same procedural protection against seizure or imprisonment without lawful judgment of peers or the law of the land, as understood in the context of 1215 feudal tenure. Their gain is real but strictly bounded to the categories of abuse the charter names.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, landholding_freemen_within_baronial_class, beneficiary,
    moderate, biographical, constrained, national).

% Villeins and unfree tenants fall outside the charter's contemplated class of 'free man' (liber homo) entirely under the 1215 terms. They would have grievances against arbitrary lordly and royal power identical in kind to the barons', but the originalist reading holds they were never intended beneficiaries and remain wholly unprotected by Clause 39 as drafted.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, unfree_peasantry_excluded_from_charter, excluded,
    powerless, generational, trapped, local).

% Study the charter's drafting context, the specific 1215 grievances enumerated in the Articles of the Barons, and the negotiating record to establish what Clause 39 was actually understood to prohibit at the time of sealing, as distinct from later doctrinal accretions.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an acute 1215 crisis of trust between King John and his tenants-in-chief by converting a set of specific, named royal abuses (disseisin, imprisonment, exaction without judgment) into an enforceable bilateral limitation, averting immediate civil war.
% TRANSFER_FUNCTION: Moves discretionary power to seize, imprison, and fine free tenants from the crown's exchequer apparatus back to a judgment-of-peers process controlled by the baronial class itself, restoring procedural predictability the barons had lost under John's fiscal exactions.
% ABSENT_VOICES: Unfree peasants, women, and townsfolk outside the free-tenant class had grievances against arbitrary power structurally identical to the barons' own complaints but were never party to the Runnymede negotiation and are not contemplated by the clause's 1215 language.
% DISAPPEARANCE_RATIONALE: Under the originalist reading, if Clause 39 vanished, the specific 1215 political settlement between John and the baronial coalition would unwind, but because the reading holds the clause's scope to be narrowly historical, its disappearance would not by itself dissolve any later constitutional structure built on subsequent reinterpretation — whether the world rearranges depends on which downstream doctrinal accretions (outside this reading's own scope) are counted.
% FOUNDING_PROBLEM: King John's administration was seizing lands, imprisoning tenants, and levying fines and reliefs without the judgment of the accused's peers or recourse to established feudal custom, provoking a coalition of barons to rebel and force negotiated limits at Runnymede in June 1215.
% FOUNDING_PROBLEM_CORROBORATION: Charter historians working from the Articles of the Barons and the Pipe Rolls documenting John's exactions (independent of any baronial descendant or crown-affiliated source) attest that the specific abuses named in Clause 39 — disseisin without judgment, arbitrary imprisonment, extralegal fines — ceased to be live administrative practice within a few generations of the charter's confirmation; the 13th-century grievance the clause targets no longer exists as a live royal practice.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.34, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.5) at the charter's sealing, reflecting the genuine, if narrow, transfer of discretionary power away from the crown, and declines over the decade as the specific 1215 crisis resolves through the 1216 and 1217 reissues and the abuses named cease to be live administrative practice — this is a story of a bounded, resolving constraint, not an escalating one. Theater ratio rises modestly (0.2 to 0.42) as the clause's practical bite against the original grievances fades while its ceremonial invocation in subsequent reissues (1216, 1217, 1225) persists, foreshadowing (but not yet completing) the drift toward later reinterpretation that the sibling readings will pick up. Suppression is moderate (0.28) — enforcement runs through the twenty-five-baron security council rather than a standing coercive apparatus, appropriate to a narrowly scoped bilateral settlement rather than a universal legal guarantee.
 *
 * DIRECTIONALITY LOGIC:
 *   Baronial signatories sit near the beneficiary end: they set the charter's terms, administer its enforcement council, and their exit was already exercised via armed rebellion rather than through the clause itself. The crown administration sits near the target end: trapped by the military coalition, its prerogative curtailed with respect to the specific named abuses. Exchequer officials are secondary payers with constrained exit — their operational practices are directly named and narrowed. Unfree peasants are excluded rather than positioned on the beneficiary-target axis at all under this reading; they are outside the charter's contemplated class of liber homo, so the derivation correctly assigns them no directionality with respect to this clause's transfer, only to the world it never touches.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is 'dead' — the specific 1215 abuses the clause targets (disseisin without judgment, arbitrary exaction) ceased as live royal administrative practice within a generation, corroborated by independent Pipe Roll analysis rather than baronial or crown self-report. Under a naive reading this would suggest the constraint should have dissolved; the originalist reading's answer is that it correctly SHOULD have narrowed in scope once its founding grievance resolved, and the theater_ratio's rise across the measured interval is exactly the signature of a constraint whose practical function faded while its ceremonial reissue persisted. This reading does not claim the clause remains vital for a live universal purpose — that claim belongs to the liberal_due_process_reading sibling, generated separately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_scope_vs_later_doctrine,
    'Is the clause''s meaning fixed by what the 1215 negotiators actually intended and understood, or does its text license the much broader due-process reading that later constitutional tradition attached to it?',
    'Comparative analysis of the Articles of the Barons, the 1215 charter text, and the 1216/1217/1225 reissues against subsequent 17th-century (Coke) and modern constitutional invocations, tracking where the plain 1215 language stops constraining interpretation.',
    'If the originalist scope is correct, later due-process readings are anachronistic extensions riding on the clause''s prestige rather than its actual content — reclassifying this constraint''s descendants as scaffolds or tangled ropes built atop a narrower kernel. If the broader reading is correct, this story understates the clause''s true beneficiary set and the originalist reading itself becomes the narrower, contested sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_scope_vs_later_doctrine, conceptual, 'Whether Clause 39''s legitimate scope is bounded by 1215 intent or licenses later universal extension.').

omega_variable(
    free_man_class_boundary,
    'Was the 1215 category of liber homo understood by the negotiating parties to exclude unfree peasants categorically, or was the boundary more porous in practice than the formal legal category suggests?',
    'Manorial court records and tenure surveys from the 1215-1225 period examining whether disputes involving villeins ever invoked Clause 39 protections in practice, despite the formal exclusion.',
    'If the boundary was porous in practice, the victim/excluded-voice structure of this reading understates actual protection extended; if rigid, the originalist reading''s narrow victim set is confirmed and the exclusion of unfree peasantry is structurally accurate rather than merely nominal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_man_class_boundary, empirical, 'Whether the free/unfree tenure boundary was as rigid in practice as in formal 1215 legal categorization.').

omega_variable(
    reading_framing_underdetermination,
    'Is the originalist_limitation_reading itself a neutral historical reconstruction, or does bounding the clause to 1215 grievances already presuppose a jurisprudential commitment (textualism/originalism) that the 13th-century parties themselves would not have recognized as a distinct interpretive method?',
    'Examine whether medieval legal reasoning distinguished ''original intent'' from ''ongoing customary application'' as separate interpretive modes, or whether this distinction is itself a modern jurisprudential import being read back onto the 1215 settlement.',
    'If the originalist/living-document distinction is anachronistic, all three kernel readings (including this one) partly misdescribe how the 1215 parties themselves would have understood the clause''s temporal scope, and the kernel''s framing itself may need a fourth, meta-level reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the originalist framing presupposes a modern interpretive category anachronistic to the 1215 negotiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1217, 0.28).
narrative_ontology:measurement_basis(magn_tr_t1217, observed).
narrative_ontology:measurement(magn_tr_t1219, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1219, 0.33).
narrative_ontology:measurement_basis(magn_tr_t1219, observed).
narrative_ontology:measurement(magn_tr_t1221, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1221, 0.37).
narrative_ontology:measurement_basis(magn_tr_t1221, observed).
narrative_ontology:measurement(magn_tr_t1223, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1223, 0.4).
narrative_ontology:measurement_basis(magn_tr_t1223, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.42).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.5).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1217, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1217, 0.44).
narrative_ontology:measurement_basis(magn_be_t1217, observed).
narrative_ontology:measurement(magn_be_t1219, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1219, 0.4).
narrative_ontology:measurement_basis(magn_be_t1219, observed).
narrative_ontology:measurement(magn_be_t1221, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1221, 0.37).
narrative_ontology:measurement_basis(magn_be_t1221, observed).
narrative_ontology:measurement(magn_be_t1223, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1223, 0.35).
narrative_ontology:measurement_basis(magn_be_t1223, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.34).
narrative_ontology:measurement_basis(magn_be_t1225, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_clause_39__originalist_limitation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_clause_39 kernel, decomposed per the ε-invariance principle because 'what Clause 39 does' resolves to structurally distinct claims with different ε values and different victim/beneficiary sets depending on interpretive scope. The originalist_limitation_reading has the narrowest scope and lowest theater ratio at founding (0.2) since it tracks a genuinely resolving 1215 grievance; the liberal_due_process_reading (higher ε, broader victim set encompassing modern state actors) and feudal_prerogative_reading (different beneficiary structure, preserving hierarchy rather than limiting abuse) are generated as separate files and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
