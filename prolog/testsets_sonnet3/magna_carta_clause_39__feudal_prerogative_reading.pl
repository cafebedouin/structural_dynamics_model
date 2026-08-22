% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal-prerogative reading of Magna Carta
 *   Clause 39: the clause is read as a narrow, class-bound procedural
 *   concession wrung from the crown by its most powerful tenants,
 *   guaranteeing peer judgment and lawful process to the baronial free-man
 *   class while leaving the crown's discretionary authority over villeins,
 *   serfs, and non-elite free subjects fully intact. Under this reading, the
 *   clause is a settlement within feudal hierarchy, not a rupture of it — it
 *   stabilizes crown-baron relations by giving the barons an enforceable
 *   procedural claim, and its persistence over the thirteenth century
 *   depended on baronial political and military leverage, not on any
 *   universalist principle later readings would attribute to it. This is a
 *   sibling of, not identical to, the liberal_due_process_reading (which
 *   reads 'liber homo' as reaching toward universal individual right) and the
 *   originalist_limitation_reading (which reads the clause as bounded
 *   strictly to 1215-specific royal abuses without committing to either the
 *   narrow-class or universalist trajectory). The ε authored here (0.28)
 *   reflects low extraction FROM the traditional authority structure the
 *   reading is about — the crown concedes real but bounded ground to the
 *   barons — and is not comparable to the other readings' ε values, which
 *   describe different constraints with different victim sets.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.55).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '48804600-ce51-4d62-98e9-1b6bb6340883').
narrative_ontology:cs_kernel_codification('48804600-ce51-4d62-98e9-1b6bb6340883', fixed_text).
narrative_ontology:cs_authority_grounding('48804600-ce51-4d62-98e9-1b6bb6340883', lineage).
narrative_ontology:cs_interpretation_layer_present('48804600-ce51-4d62-98e9-1b6bb6340883').
narrative_ontology:cs_reading_relation('48804600-ce51-4d62-98e9-1b6bb6340883', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('48804600-ce51-4d62-98e9-1b6bb6340883', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('48804600-ce51-4d62-98e9-1b6bb6340883', foundational, protection_scoped_to_tenurial_class).
narrative_ontology:cs_axiom_status(protection_scoped_to_tenurial_class, holdable).
narrative_ontology:cs_axiom_grounding('48804600-ce51-4d62-98e9-1b6bb6340883', protection_scoped_to_tenurial_class, conventional).
narrative_ontology:cs_axiom('48804600-ce51-4d62-98e9-1b6bb6340883', secondary, hierarchy_preservation_is_legitimate_settlement_function).
narrative_ontology:cs_axiom_status(hierarchy_preservation_is_legitimate_settlement_function, holdable).
narrative_ontology:cs_axiom_grounding('48804600-ce51-4d62-98e9-1b6bb6340883', hierarchy_preservation_is_legitimate_settlement_function, conventional).
narrative_ontology:cs_reference_frame('48804600-ce51-4d62-98e9-1b6bb6340883', baronial_tenurial_privilege_settlement).
narrative_ontology:cs_drift_state('48804600-ce51-4d62-98e9-1b6bb6340883', post_reissue_common_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48804600-ce51-4d62-98e9-1b6bb6340883', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, landed_baronial_class).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_administrative_apparatus).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, urban_commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extracted the clause from King John at Runnymede to guarantee that judgment against a free man of baronial rank would proceed by lawful judgment of peers or the law of the land, not by royal fiat. They are simultaneously the drafting party and the primary class the guarantee protects; enforcement depends on their continued military and political leverage over the crown, not on any universal principle.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, landed_baronial_class, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, landed_baronial_class, agenda_setter).

% Concedes the clause under coercion (rebellion, threat of civil war) but retains the hierarchy the clause operates within: the king still judges, taxes, and disposes of unfree tenants and non-baronial subjects without this procedural check. The concession stabilizes the crown's rule over a narrower, more securely bound class of vassals.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_administrative_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, crown_administrative_apparatus, beneficiary).

% Villeins and serfs bound to baronial or crown land are structurally outside 'free man' (liber homo) as read in this feudal-prerogative sense; the clause changes nothing about their liability to summary seizure, disseisin, or punishment at their lord's discretion. They bear the ordinary costs of the hierarchy the clause leaves intact.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasantry, payer,
    powerless, biographical, trapped, local).

% Townsmen and free commoners without baronial standing gain no enforceable claim under this reading's restricted class of protected persons; their procedural protection, where it exists at all, runs through separate borough charters and custom, not through Clause 39's peer-judgment guarantee.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, urban_commoners, payer,
    powerless, biographical, constrained, local).

% Administer the procedural machinery the clause requires for the baronial class specifically, while continuing ordinary summary process against non-elite subjects. Their discretion is narrowed only where a peer of the realm is involved.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_justices_and_sheriffs, agenda_setter,
    institutional, biographical, constrained, national).

% Examine charter rolls, baronial correspondence, and the 1215 political context to establish whether 'liber homo' and 'judicium parium suorum' were understood at the time as class-restricted terms of art within feudal tenure law, distinct from later universalist readings retrofitted onto the text.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, landed_baronial_class).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a settlement between the crown and its most powerful tenants-in-chief: it solves the immediate 1215 problem of arbitrary royal seizure of baronial land and person by requiring judgment of peers, restoring predictable rules to the crown-baron relationship and averting civil war.
% TRANSFER_FUNCTION: Moves a specific procedural guarantee (peer judgment, lawful process) from the crown's unilateral discretion to the baronial class, while leaving crown discretion over unfree tenants, villeins, and non-baronial free subjects untouched. No corresponding guarantee flows downward to the rest of the population.
% ABSENT_VOICES: Villeins, serfs, and urban commoners had no seat at Runnymede and no representative among the twenty-five sureties; their interests do not appear in the text and this reading holds that omission as evidence the clause was never meant to reach them, not as an oversight to be filled by later interpretation.
% DISAPPEARANCE_RATIONALE: If this narrow procedural guarantee vanished, the baronial class would lose its principal charter-based check on arbitrary royal seizure and would likely revert to reliance on private military leverage and ad hoc rebellion to constrain the crown — the 1215-1225 settlement structure depends on the clause holding as a class-specific bargain.
% FOUNDING_PROBLEM: King John's arbitrary disseisin, imprisonment, and extralegal punishment of tenants-in-chief without trial had destabilized crown-baron relations to the point of armed rebellion; the barons needed an enforceable procedural check specific to their class to prevent recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the baronial tradition (drawing on charter-roll scholarship and comparative feudal law) attest that the specific 1215 crisis of unchecked royal disseisin against tenants-in-chief was resolved by the reissues of the Charter and subsequent common-law development; the clause's original narrow function no longer answers a live problem for that class, though its later universalist afterlife (a different reading) persists independently.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28) because, under this reading, the clause redistributes very little: it is a coordination device between crown and barons that leaves the broader extractive structure of feudal tenure (villeinage, seigneurial justice, urban subordination) completely undisturbed. Suppression is moderate-high (0.55, declining slowly across the interval) because maintaining the class boundary — excluding villeins and commoners from the peer-judgment guarantee — required active legal and social enforcement (manorial courts, status distinctions in pleading) that gradually loosened as common-law process broadened informally, though this reading holds that loosening as drift away from, not fulfillment of, the clause's original design. Theater ratio rises from 0.10 to 0.40 across the interval because later reissues and commentaries increasingly invoked the clause in more expansive rhetorical terms even as its class-restricted operative content stayed largely fixed — an early Goodhart-style drift where the label outran the function, which this reading treats as evidence for identifying two distinct historical constraints (the narrow original and the invoked universal), not as this constraint's own extraction rising.
 *
 * PERSPECTIVAL GAP:
 *   From the baronial seat, Clause 39 is pure coordination — a hard-won constraint on royal caprice that operates exactly as intended. From the seat of an unfree tenant or urban commoner, the same clause, and the same crown-baron settlement it stabilizes, changes nothing about their own vulnerability to arbitrary lordly or royal power; the engine's per-seat computation should register this as the divergence between a beneficiary-class rope-like experience and an excluded class's structural non-participation, without treating the excluded class as this reading's 'victim' in a strong extraction sense.
 *
 * DIRECTIONALITY LOGIC:
 *   The baronial class and the crown apparatus sit near the beneficiary end: barons gain an enforceable procedural claim, and the crown gains a stabilized, narrower, more legible relationship with its most dangerous tenants. Unfree peasantry and urban commoners sit near the target end not because the clause directly extracts from them, but because they are structurally excluded from a protection the political settlement could in principle have extended to them — the coordination cost of excluding them from consideration is a background feature of the era rather than an active new extraction, hence the low-but-nonzero ε rather than a high one.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists retrofitting Clause 39 into a universal-rights mandate it never functionally carried in its own century; treating the 1215 clause as already accomplishing what the later liberal_due_process_reading claims would mislabel thirteenth-century class-bound coordination as completed emancipation, obscuring the centuries of separate struggle by which non-baronial subjects actually acquired comparable procedural protections.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_class_scope_ambiguity,
    'Did the drafters and immediate contemporary readers of ''liber homo'' (free man) in 1215 understand it as reaching only tenants-in-chief and the baronial class, or did it already contemplate a broader category of free (non-villein) persons including some urban freemen?',
    'Comparative analysis of charter rolls, contemporary legal treatises (e.g., Bracton), and borough charter language from the immediate post-1215 decades to establish the operative scope of ''free man'' in practice, as distinct from later retrospective gloss.',
    'If contemporary practice shows the clause was invoked on behalf of non-baronial freemen even in the thirteenth century, this reading''s restricted victim set is too narrow and the originalist_limitation_reading becomes the more defensible sibling; if practice confirms strict baronial restriction, this reading''s structural claims are corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_class_scope_ambiguity, empirical, 'Whether ''liber homo'' was understood in practice as baronial-only or as a broader free-commoner category.').

omega_variable(
    feudal_vs_constructed_hierarchy_ambiguity,
    'Is the hierarchy this reading holds as ''preserved'' (crown over unfree tenants, barons over their own vassals) a natural feature of thirteenth-century social organization, or a constructed arrangement actively maintained by baronial and crown interest, such that the clause''s low measured extraction is itself an artifact of accepting that hierarchy''s legitimacy?',
    'Comparative feudal historiography examining alternative tenurial arrangements available in the period (manumission rates, urban charter expansion, continental variation) to assess whether the hierarchy was as fixed as this reading treats it.',
    'If the hierarchy was more contingent and actively defended than this reading assumes, the low ε authored here understates the clause''s role in legitimating an extractive social order, and the beneficiary/victim boundary should be redrawn wider.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feudal_vs_constructed_hierarchy_ambiguity, conceptual, 'Whether the preserved feudal hierarchy is natural-to-the-era or an actively maintained construction this reading under-weights.').

omega_variable(
    cs_framing_charter_vs_settlement,
    'Should the kernel be framed as the fixed Clause 39 text (fixed_text framing) or as the broader 1215 political settlement of which the clause is one term (a distributed, multi-instrument framing including the wider Charter, the Articles of the Barons, and the enforcement council of twenty-five)?',
    'Compare classification outcomes under a text-only framing versus a settlement-level framing; the settlement framing would pull in the enforcement council''s coercive apparatus as part of the kernel''s authority-grounding, potentially shifting authority_grounding from lineage toward extraction.',
    'Under the text-only framing (adopted here), authority grounds in lineage from the reissued Charter tradition; under a settlement framing, the clause would read as one term within a coercively extracted baronial charter, strengthening a snare-adjacent reading of the settlement as a whole rather than tangled_rope for this single clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_charter_vs_settlement, conceptual, 'Alternative framing of the kernel as isolated clause-text versus full 1215 political settlement, and its effect on authority_grounding classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1230, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1230, 0.18).
narrative_ontology:measurement(magn_tr_t1245, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1245, 0.25).
narrative_ontology:measurement(magn_tr_t1260, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1260, 0.3).
narrative_ontology:measurement(magn_tr_t1280, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1280, 0.36).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.22).
narrative_ontology:measurement(magn_be_t1230, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1230, 0.24).
narrative_ontology:measurement(magn_be_t1245, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1245, 0.25).
narrative_ontology:measurement(magn_be_t1260, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1260, 0.26).
narrative_ontology:measurement(magn_be_t1280, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1280, 0.27).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.62).
narrative_ontology:measurement(magn_su_t1230, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1230, 0.6).
narrative_ontology:measurement(magn_su_t1245, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1245, 0.58).
narrative_ontology:measurement(magn_su_t1260, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1260, 0.57).
narrative_ontology:measurement(magn_su_t1280, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1280, 0.56).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'Magna Carta Clause 39' per the ε-invariance principle. feudal_prerogative_reading (this file) authors low extraction against a preserved hierarchy with a class-restricted victim set; liberal_due_process_reading authors the same text as establishing universal individual right, with a correspondingly different beneficiary/victim structure; originalist_limitation_reading brackets the question by binding the clause strictly to documented 1215 abuses. All three share the fixed clause text as their kernel but diverge in authority_grounding interpretation, victim scope, and consequently ε. This reading forecloses the liberal reading's core premise (universal reach) within a single interpretive framework, while coexisting with the originalist reading, which is agnostic on the class-scope question this reading affirmatively answers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
