% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the feudal-prerogative reading of the Magna Carta
 *   Clause 39 kernel: the clause as a narrow, class-bounded procedural
 *   settlement between King John and his rebellious tenants-in-chief,
 *   extracted at Runnymede in 1215 to end a specific cycle of arbitrary royal
 *   seizure of baronial persons and lands. On this reading, the clause is a
 *   Tangled Rope: it genuinely coordinates crown-baronial relations by fixing
 *   a peer-judgment floor that ends open warfare, but that coordination is
 *   purchased by leaving villeins, women outside the baronial class, and
 *   landless freemen entirely outside its protection — the same structure
 *   that pacifies the elite leaves the excluded majority's exposure to
 *   arbitrary power untouched or, arguably, more entrenched by comparison.
 *   This is a sibling of two other constraints, liberal_due_process_reading
 *   and originalist_limitation_reading, each instantiating a structurally
 *   distinct ε and victim set from the same 1215 text; per the ε-invariance
 *   principle they are not measured here.
 *
 * KEY AGENTS:
 *   - landed_barons: Primary beneficiary and co-agenda-setter (powerful/arbitrage) — extracted the clause and administers its peer-judgment mechanism
 *   - the_crown: Beneficiary-and-payer (institutional/constrained) — conceded the limitation to end rebellion while preserving the broader feudal order
 *   - unfree_villeins: Excluded majority (powerless/trapped) — entirely outside the clause's contemplated class of protected persons
 *   - constitutional_historians: Analytical observer — traces the clause's narrow original scope against later expansive readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.42).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Clause 39 as Baronial Procedural Privilege Within Feudal Hierarchy").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '6b919b42-d8a1-490a-8bd8-6b6158cbd776').
narrative_ontology:cs_kernel_codification('6b919b42-d8a1-490a-8bd8-6b6158cbd776', fixed_text).
narrative_ontology:cs_authority_grounding('6b919b42-d8a1-490a-8bd8-6b6158cbd776', lineage).
narrative_ontology:cs_interpretation_layer_present('6b919b42-d8a1-490a-8bd8-6b6158cbd776').
narrative_ontology:cs_reading_relation('6b919b42-d8a1-490a-8bd8-6b6158cbd776', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b919b42-d8a1-490a-8bd8-6b6158cbd776', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('6b919b42-d8a1-490a-8bd8-6b6158cbd776', foundational, peer_judgment_is_class_bound_privilege).
narrative_ontology:cs_axiom_status(peer_judgment_is_class_bound_privilege, holdable).
narrative_ontology:cs_axiom_grounding('6b919b42-d8a1-490a-8bd8-6b6158cbd776', peer_judgment_is_class_bound_privilege, conventional).
narrative_ontology:cs_axiom('6b919b42-d8a1-490a-8bd8-6b6158cbd776', foundational, hierarchical_order_is_legitimate_background_condition).
narrative_ontology:cs_axiom_status(hierarchical_order_is_legitimate_background_condition, overridden).
narrative_ontology:cs_axiom_grounding('6b919b42-d8a1-490a-8bd8-6b6158cbd776', hierarchical_order_is_legitimate_background_condition, conventional).
narrative_ontology:cs_reference_frame('6b919b42-d8a1-490a-8bd8-6b6158cbd776', feudal_tenurial_hierarchy).
narrative_ontology:cs_drift_state('6b919b42-d8a1-490a-8bd8-6b6158cbd776', post_common_law_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6b919b42-d8a1-490a-8bd8-6b6158cbd776', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, landed_barons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_in_chief).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, unfree_villeins).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, women_outside_baronial_class).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, non_landed_freemen).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, the_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extracted the clause at Runnymede as a guarantee that they, as the king's direct tenants and legal peers, could not be disseised, imprisoned, or outlawed except by lawful judgment of their equals or the law of the land. They administer this protection through their own class's participation in judgment and enforcement, and their exit from royal overreach is real: they hold armed retinues and can rebel, as they demonstrated in 1215 itself.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, landed_barons, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, landed_barons, agenda_setter).

% Concedes a specific, bounded limitation on arbitrary seizure of baronial persons and property in exchange for ending the immediate baronial revolt and preserving the broader structure of feudal obligation, taxation, and military service. The clause costs the crown some unilateral power over its greatest vassals but purchases continued legitimacy and loyalty from the class the realm depends on to function.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, the_crown, beneficiary,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, the_crown, payer).

% Hold land directly of the crown and fall within the protected class of 'free men' the clause contemplates by the standards of 1215. They benefit from the procedural guarantee without needing to fight for it directly, riding on the barons' leverage, though their standing is lesser and their protection more dependent on baronial goodwill in practice.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_tenants_in_chief, beneficiary,
    powerful, generational, constrained, national).

% Constitute the overwhelming majority of the population and are bound to the land and to their lords under villeinage. The clause's language of judgment by peers and the law of the land does not contemplate them as legal persons capable of holding such rights against their own lords; their subjection to arbitrary seizure and punishment by manorial authority continues entirely undisturbed by the clause.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_villeins, excluded,
    powerless, generational, trapped, local).

% Are almost entirely absent from the class of persons the clause protects in its original operative sense; property and legal standing run through male heads of household, and a woman's protection from seizure or judgment depends on her relationship to a protected man, not on any right of her own under the clause.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, women_outside_baronial_class, excluded,
    powerless, biographical, trapped, local).

% Are nominally free but hold no land of the crown or of a great lord in a way that would trigger baronial-style protection; in practice they have no peers positioned to sit in judgment on their behalf and no armed leverage to compel the crown or local lords to honor any implied protection.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, non_landed_freemen, excluded,
    powerless, biographical, constrained, local).

% Study the 1215 text, the baronial demands that produced it, and the near-immediate annulment and reissue history to assess what the clause actually secured for whom, as against later readings projected onto it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, landed_barons).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the crown and its most powerful tenants-in-chief by fixing a procedural floor — judgment by peers or the law of the land — below which the crown may not act against baronial persons or property, ending a specific cycle of arbitrary baronial disseisin and imprisonment that had provoked armed revolt.
% TRANSFER_FUNCTION: Moves a measure of unilateral coercive discretion from the crown to a joint crown-baronial process, but only within the baronial and free-tenant class; it moves nothing at all for villeins, women outside that class, or landless freemen, whose exposure to arbitrary lordly and royal power is untouched.
% ABSENT_VOICES: Villeins, who form the majority of the population bound to manorial lords, have no voice in the Runnymede negotiation and no class of peers positioned to judge on their behalf; women outside the baronial class and non-landed freemen are similarly absent from the negotiating table and from the clause's contemplated protections.
% DISAPPEARANCE_RATIONALE: If the clause vanished, the crown would regain unrestricted discretion to disseise, imprison, or outlaw barons and free tenants-in-chief without recourse to peer judgment, collapsing the specific truce that ended the 1215 baronial war; the arrangement of the highest tier of feudal society would rearrange sharply, even though the lives of villeins and other excluded groups would be entirely unaffected either way.
% FOUNDING_PROBLEM: King John had been seizing baronial lands, imprisoning barons, and disseising them without trial or judgment, provoking a coalition of the greatest barons into armed rebellion; the clause was extracted as a specific truce term to end that rebellion by binding the crown's future conduct toward that class.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the baronial tradition (e.g. scholarship tracing the clause's near-immediate practical irrelevance after 1215, its annulment by papal bull within weeks, and its narrow thirteenth-century judicial application) attest that the specific crisis of arbitrary royal disseisin of great barons was resolved or transformed within a generation through evolving common-law process; the clause's continued symbolic invocation persists independent of that original problem, largely sustained by later constitutional narrative rather than by the barons themselves, who no longer exist as a distinct political class.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored low (0.28) because, on this reading, the clause extracts almost nothing from the class it actually governs — barons and free tenants-in-chief are net beneficiaries of a genuine peer-judgment coordination mechanism, and the crown's concession is a real transfer of discretion rather than rent extraction. Suppression is moderate (0.42, declining slightly over the interval) because enforcing the peer-judgment floor against a recalcitrant crown required real baronial coercive capacity (armed retinues, periodic renewed confrontation), which softened somewhat as the practice normalized into common-law process. Theater ratio rises modestly (0.15 to 0.30) reflecting the clause's gradual drift from live baronial leverage toward increasingly symbolic invocation as the specific 1215 crisis receded. Accessibility collapse is fairly high (0.62) because, for the protected baronial class, the alternative of unrestrained royal prerogative genuinely closed off once the peer-judgment norm took hold; resistance is moderate (0.35), reflecting periodic royal attempts to erode the norm met with baronial pushback through the thirteenth century.
 *
 * DIRECTIONALITY LOGIC:
 *   Barons and free tenants-in-chief sit near the beneficiary end: they authored the constraint, administer its judgment mechanism through their own class, and hold real exit via armed and political leverage. The crown sits ambiguously as both a constrained payer (loses unilateral discretion) and a beneficiary (buys legitimacy and ends costly rebellion) — hence its dual role. Villeins, women outside the baronial class, and non-landed freemen are excluded rather than victimized through the clause's operation in the classic extraction sense — the clause does not seize from them, it simply never contemplates them, which under this narrow reading keeps their directionality near-irrelevant to this specific constraint's χ, though their structural subordination continues by other means entirely outside Clause 39's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary royal disseisin and imprisonment of great barons without peer judgment — is genuinely dead as a live crisis by the modern era; no baronial class analogous to 1215 persists, and common-law due process has long superseded the specific peer-judgment mechanism. Reading the clause narrowly, as this story does, prevents mislabeling its continued invocation as unresolved coordination need; it is preserved today largely as constitutional-genealogical symbol rather than as active machinery, which the founding_problem_status of 'dead' combined with a disappearance_verdict of 'world_rearranges' (for the historical baronial class specifically, not the modern world) flags as a genealogy worth scrutinizing rather than accepting at face value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    class_boundary_of_liber_homo,
    'Did the phrase ''liber homo'' (free man) in the 1215 text contemplate a narrow class of tenants-in-chief and free tenants only, or a broader and expanding class of free persons that the feudal-prerogative reading understates?',
    'Close philological and legal-historical analysis of contemporary charters, court rolls, and the demographic composition of thirteenth-century free tenure, cross-checked against how the clause was actually invoked in litigation in the decades following 1215.',
    'If ''liber homo'' is shown to have covered a substantially wider population than the great baronial class, the feudal-prerogative reading''s restricted victim set is too narrow and this reading understates the clause''s original extraction/coordination boundary — pushing the story toward the originalist_limitation_reading''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(class_boundary_of_liber_homo, empirical, 'Whether the protected class was narrower or broader than the baronial elite this reading assumes.').

omega_variable(
    natural_vs_constructed_hierarchy_frame,
    'Is the feudal hierarchy this reading treats as the background ''established order'' itself a natural social fact of 1215, or a constructed arrangement that Clause 39 helped entrench and legitimize on behalf of identifiable beneficiaries?',
    'Comparative analysis against contemporaneous societies with different tenurial structures, and examination of whether baronial and crown interests actively shaped the clause''s drafting to preserve their mutual position against the excluded majority.',
    'If the hierarchy is constructed and actively defended rather than a natural background condition, this reading''s implicit treatment of the feudal order as a stable frame (rather than itself an extractive structure) understates the clause''s role in entrenching baronial and crown privilege against villeins and others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_hierarchy_frame, conceptual, 'Whether the feudal hierarchical backdrop this reading takes as given is itself a constructed, defended arrangement.').

omega_variable(
    committer_reading_selection,
    'Given that Clause 39''s text is genuinely compatible with the feudal-prerogative, liberal due-process, and originalist-limitation readings, what determines which reading a given legal or political tradition adopts, and is that selection itself extractive?',
    'Trace which institutional actors invoke which reading in which contexts (e.g. modern civil-liberties litigation citing the liberal reading vs. legal historians favoring the originalist reading vs. traditionalist political theory favoring the feudal reading) and whether the selection correlates with the invoking party''s interest.',
    'If reading-selection tracks invoker interest rather than textual or historical fidelity, the kernel itself functions as a flexible legitimating resource rather than a fixed constraint, which would recharacterize all three sibling readings as instruments in an ongoing legitimation contest rather than as competing historical truths.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_selection, conceptual, 'Whether the choice among the three sibling readings of Clause 39 is itself driven by present-day extractive interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1225, 0.2).
narrative_ontology:measurement(magn_tr_t1240, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1240, 0.24).
narrative_ontology:measurement(magn_tr_t1260, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1260, 0.27).
narrative_ontology:measurement(magn_tr_t1280, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1280, 0.29).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.3).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.22).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1225, 0.24).
narrative_ontology:measurement(magn_be_t1240, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1240, 0.26).
narrative_ontology:measurement(magn_be_t1260, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1260, 0.27).
narrative_ontology:measurement(magn_be_t1280, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1280, 0.28).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.5).
narrative_ontology:measurement(magn_su_t1225, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1225, 0.46).
narrative_ontology:measurement(magn_su_t1240, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1240, 0.44).
narrative_ontology:measurement(magn_su_t1260, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1260, 0.43).
narrative_ontology:measurement(magn_su_t1280, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1280, 0.42).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the magna_carta_clause_39 kernel, decomposed per the ε-invariance principle: feudal_prerogative_reading (this story, low ε, class-bound victim set, Tangled Rope), liberal_due_process_reading (higher claimed universality, contested extraction against the modern state), and originalist_limitation_reading (narrowest scope, tied strictly to 1215 documented abuses). Each carries its own ε, stakeholders, and classification; they are linked here rather than merged because measuring the same text through different interpretive commitments yields structurally distinct constraints, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
