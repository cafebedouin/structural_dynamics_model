% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 (Originalist Limitation Reading): 1215 Specific Abuses Interpretation
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta Clause 39 stands as one of the most contested passages in
 *   English constitutional history. In the originalist limitation reading,
 *   the clause represents a specific response to documented abuses committed
 *   by King John between 1199-1215: arbitrary fines, wardship extraction,
 *   forest encroachments, and denial of trial by peers to baronial
 *   defendants. The originalist reading treats the constraint as bounded by
 *   these enumerated grievances and the coalition that negotiated them. The
 *   clause guarantees that 'no free man shall be taken or imprisoned...
 *   except by the lawful judgment of his peers or by the law of the land' —
 *   but under the originalist reading, this protection applies narrowly to
 *   the documented abuses of 1215, not expansively to all arbitrary exercise
 *   of royal power. This reading creates a tangled coordination-extraction
 *   hybrid: it coordinates between the baronial coalition's security needs
 *   and the crown's legitimacy interest in having limits codified, but it
 *   simultaneously extracts from non-baronial subjects (commoners, Jews,
 *   merchants outside the coalition) and from later generations who wish to
 *   expand the due process principle beyond its 1215 boundaries. The
 *   constraint's theater ratio (0.58) reflects the interpretive work required
 *   to maintain the 1215 boundary against centuries of pressure to expand it
 *   — the text is treated as having a specific, limited meaning, but that
 *   meaning requires constant performative reassertion against alternative
 *   readings that would make it more broadly applicable.
 *
 * KEY AGENTS:
 *   - Baronial Coalition (1215): Primary beneficiary (organized/constrained) — achieves mutual protection against King John's documented abuses; creates enforcement through collective baronial action
 *   - Non-Baronial Subjects and Future Generations: Primary victims (powerless/trapped) — clause 39's protection excludes them; originalist reading forecloses expansion to broader due process claims
 *   - Crown/Monarchy (Post-1215): Secondary actor (powerful/constrained) — benefits from legitimacy of having power limited, but constrained by textual fixity and originalist boundary
 *   - Liberal Expansion Advocates: Competing readership (analytical/mobile) — view clause 39 as foundational to broader due process rights; originalist reading blocks their interpretive path
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as coordinating 1215 grievances while extracting from all excluded parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.35).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 (Originalist Limitation Reading): 1215 Specific Abuses Interpretation").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'eda6c1b7-7935-471a-a942-54acb6ea9635').
narrative_ontology:cs_kernel_codification('eda6c1b7-7935-471a-a942-54acb6ea9635', fixed_text).
narrative_ontology:cs_authority_grounding('eda6c1b7-7935-471a-a942-54acb6ea9635', lineage).
narrative_ontology:cs_interpretation_layer_present('eda6c1b7-7935-471a-a942-54acb6ea9635').
narrative_ontology:cs_reading_relation('eda6c1b7-7935-471a-a942-54acb6ea9635', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('eda6c1b7-7935-471a-a942-54acb6ea9635', magna_carta_clause_39__feudal_prerogative_reading, forecloses).
narrative_ontology:cs_axiom('eda6c1b7-7935-471a-a942-54acb6ea9635', foundational, textual_fidelity_to_1215_grievances).
narrative_ontology:cs_axiom_status(textual_fidelity_to_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('eda6c1b7-7935-471a-a942-54acb6ea9635', textual_fidelity_to_1215_grievances, empirically_contingent).
narrative_ontology:cs_axiom('eda6c1b7-7935-471a-a942-54acb6ea9635', foundational, coalition_boundary_closure).
narrative_ontology:cs_axiom_status(coalition_boundary_closure, holdable).
narrative_ontology:cs_axiom_grounding('eda6c1b7-7935-471a-a942-54acb6ea9635', coalition_boundary_closure, conventional).
narrative_ontology:cs_reference_frame('eda6c1b7-7935-471a-a942-54acb6ea9635', charter_as_specific_baronial_remedy).
narrative_ontology:cs_drift_state('eda6c1b7-7935-471a-a942-54acb6ea9635', contemporary_constitutional_theory, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('eda6c1b7-7935-471a-a942-54acb6ea9635', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_coalition).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, crown_legitimacy).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, future_rights_claimants).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, broader_due_process_conception).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-BARONIAL SUBJECTS (SNARE) — Clause 39's protection applies only to the named baronial coalition of 1215. Commoners, Jews, merchants outside baronial networks, and all subsequent generations born after the charter's sealing have no claim under this constraint's scope. Trapped by the textual boundary — cannot exit the limitation by appealing to broader due process because the originalist reading forbids such appeal. Pure extraction: the constraint protects specific named agents while excluding others.
constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BARONIAL COALITION (ROPE) — The named parties to the 1215 negotiation experience Clause 39 as genuine coordination: it establishes known grievances, specifies remedies (trial by peers, due process for named abuses), and creates mutual enforcement against King John's documented violations. The coordination benefit is real — it prevents recurrence of the specific abuses (forest encroachments, arbitrary fines, wardship extraction). Low extractiveness because the protection aligns with the coalition's interests and the enforcement mechanism (baronial collective action) is symmetrical.
constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MONARCHY POST-1215 (TANGLED ROPE) — Successive monarchs inherit both the coordination benefit (legitimate authority grounded in constraining their own power) and the extraction problem (the text's fixity prevents adaptation to changed circumstances; the boundary of 1215 grievances no longer aligns with actual governance needs). Constrained by the written text but also benefiting from its legitimacy certification. Moderate extraction because the monarchy can invoke reinterpretation but the originalist reading forecloses this — the text becomes a constraint the monarchy cannot flexibly apply.
constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, Clause 39 coordinates between the baronial grievance frame (real abuses needing remedy) and the legitimacy frame (the crown needs legal constraint). But it simultaneously extracts from future claimants outside the 1215 coalition and from interpretive traditions that would expand due process protections. The originalist reading's strength (textual fidelity to named abuses) is also its weakness (exclusion of all others). Moderate extractiveness reflecting genuine coordination value with embedded asymmetric scope limitation.
constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_clause_39__originalist_limitation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The originalist reading produces lower extractiveness than the liberal expansion reading would (where the same text becomes protection for all subjects). This is because the originalist interpretation narrows the scope of protection to documented 1215 abuses and named baronial parties. The extraction consists of excluding all other subjects from appeal to clause 39's protections. However, extractiveness is not maximal (would be snare-level ≥0.46) because the coordination function is genuine: the clause does provide mutual protection against the documented abuses and legitimacy constraint on monarchy. Suppression (0.35): Moderate. The textual boundary is enforced through judicial interpretation and precedent — not through physical coercion but through the interpretive authority structure that can declare non-matching claims outside the clause's scope. Alternative readings are suppressed through citation hierarchy and precedent authority, not eliminated. Theater ratio (0.58): Moderate-high. The originalist reading requires continuous performative work to maintain the 1215 boundary against pressure to expand. Each court decision reaffirming the boundary, each scholarly article defending textual fidelity, each rejection of claims seeking broader due process — all are performative acts sustaining the interpretation. The theater has increased over time (from 0.42 to 0.68 across 400 years) as the historical distance from 1215 grows and the interpretive tradition becomes more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap. The baronial coalition (rope perspective) experiences clause 39 as genuine, symmetric coordination — it protects them against King John's abuses while binding them to mutual enforcement. Non-baronial subjects trapped by the originalist boundary (snare perspective) experience pure extraction — they cannot appeal to clause 39 because they were not party to 1215 and are not covered by its documented-abuse limitation. The crown (tangled rope perspective) experiences both benefit (legitimacy) and constraint (textual fixity). The analytical observer (tangled rope perspective) recognizes both functions but sees the structural asymmetry: the protection is real for the named parties and documented abuses, but the exclusion is absolute for everyone else. This perspectival gap is not resolvable by choosing the 'correct' interpretation — it is structural to the originalist reading's core logic. The reading's strength (fidelity to 1215 context and documented abuses) is simultaneously its weakness (exclusion of all others).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's structural position relative to clause 39. The baronial coalition members are beneficiaries with constrained exit — they negotiated the charter and cannot easily abandon its protections. They experience low-to-moderate extraction (d ≈ 0.25-0.35). Non-baronial subjects are victims with trapped exit — they cannot access clause 39's protections and have no alternative legal recourse for arbitrary power outside the documented 1215 abuses. They experience high extraction (d ≈ 0.85-0.90). The crown experiences mixed directionality: as a beneficiary of legitimacy (low d) but also as constrained by textual limits (higher d for enforcement burden). The originalist reading's directionality derives not from power imbalance but from textual scope: the text's boundary determines who counts as protected, and that boundary was set in 1215 to address specific grievances. The analytical observer recognizes this asymmetry and derives moderate extraction (d ≈ 0.55-0.60) reflecting both the genuine coordination and the systematic exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved here through explicit acknowledgment of the constraint's scope boundary. The originalist reading avoids the mandatrophy trap by being clear about what it coordinates (documented 1215 abuses to the named baronial coalition) and what it excludes (all other subjects and all abuses not enumerated in 1215). The constraint is tangled rope because it genuinely coordinates the baronial interest in mutual protection and the crown's interest in legitimacy through legal constraint, AND it simultaneously extracts from non-baronial subjects by excluding them from the protection. This is not confusion about whether it is coordination or extraction — it is both, with clear asymmetry about who benefits and who bears cost. The alternative would be the liberal reading (which claims clause 39 is coordination for all subjects) or the feudal reading (which claims it is mere temporary extraction). The originalist reading's strength is that it makes the asymmetry explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_boundary_ambiguity,
    'Does ''every free man'' in Clause 39 refer to every free man living in 1215, or to the category of free men in perpetuity?',
    'Textual analysis of parallel provisions in Magna Carta; examination of how ''free man'' was used in royal writs of the period; comparison with how later monarchs applied the clause to newly enfranchised persons',
    'If temporal boundary: originalist reading is correct and future non-baronial free men fall outside protection (snare from their perspective). If categorical boundary: the clause applies to all free men regardless of era, forcing a liberal expansion reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_boundary_ambiguity, empirical, 'Whether ''every free man'' is temporally or categorically bounded').

omega_variable(
    documented_abuse_sufficiency,
    'What counts as a ''documented'' abuse in King John''s reign — only those explicitly grieved in 1215, or those tacitly understood as part of the feudal prerogative context?',
    'Historical comparison: King John''s pipe rolls and forest records vs charter clauses; identification of which abuses were explicitly named vs implied by context; analysis of chronicler accounts vs charter text',
    'If only explicit abuses: clause 39 covers only the narrow set named (widows'' wardships, forest encroachments, arbitrary fines — approximately 3-4 abuse types). If contextual abuses included: scope expands significantly to cover any prerogative excess implied in the feudal relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_abuse_sufficiency, empirical, 'What counts as a documented 1215 abuse').

omega_variable(
    feudal_prerogative_reading_coexistence,
    'Can the originalist limitation reading coexist with the feudal prerogative reading within a single legitimate interpretive framework, or does the presence of clause 39 itself foreclose the feudal reading''s core premise (that royal prerogative is unlimited)?',
    'Jurisprudential analysis: does the originalist reading acknowledge feudal prerogative as a valid interpretive alternative, or does it treat prerogative as superseded by the charter? Examination of whether a judge could apply both readings to different cases.',
    'If coexistence: both readings remain live (different judges/courts can adopt different approaches). If foreclosure: the originalist reading''s existence logically eliminates the pure-prerogative reading as defensible within English common law tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_prerogative_reading_coexistence, conceptual, 'Whether originalist reading forecloses feudal prerogative reading').

omega_variable(
    liberal_expansion_inversion,
    'Is the liberal due process reading historically derived FROM the originalist limitation reading (the originalist text about 1215 abuses gradually expands to principles), or does the liberal reading represent a competing reading that was always latent in the text?',
    'Historical tracking: examine Coke''s Institutes, the 1628 Petition of Right, and American constitutional commentary to identify whether liberal due process doctrine treats clause 39 as its foundation or as a competing text',
    'If derived: the liberal reading is downstream of this constraint (influences relation). If competing: the two readings coexist as alternative framings that never reconcile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_expansion_inversion, empirical, 'Whether liberal reading derives from or competes with originalist reading').

omega_variable(
    monarchy_flexibility_cost,
    'What is the actual cost to monarchy of adhering strictly to the 1215 abuse boundary rather than allowing interpretive expansion to cover new forms of arbitrary power?',
    'Comparative analysis: monarchies that adopted strict originalist readings vs those that allowed liberal expansion; measurement of governance dysfunction or legitimacy erosion under each approach',
    'If flexibility cost is high: monarchy has incentive to overturn or reinterpret constraint (snare dynamics). If cost is low: originalist reading is stable coordination (rope dynamics).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(monarchy_flexibility_cost, empirical, 'Actual cost to monarchy of originalist limitation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_orig_theater_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mc39_orig_theater_t200, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 200, 0.58).
narrative_ontology:measurement(mc39_orig_theater_t400, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 400, 0.68).

% Extraction over time
narrative_ontology:measurement(mc39_orig_extract_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(mc39_orig_extract_t200, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 200, 0.24).
narrative_ontology:measurement(mc39_orig_extract_t400, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 400, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 instantiates three distinct constraints corresponding to three competing readings of the same kernel text. The originalist limitation reading (this constraint) treats clause 39 as bounded by documented 1215 abuses (ε=0.28, tangled_rope); the liberal due process reading treats it as foundational to broad due process protection (higher ε, closer to rope); the feudal prerogative reading treats it as temporary constraint leaving underlying prerogative intact (different ε profile, snare-to-rope spectrum). These are not the same constraint viewed from different angles — they represent genuinely different structural claims about what the clause does. Each reading instantiates its own constraint story. The network links them because they address the same kernel and compete for interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, powerless, 0.88).
constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
