% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Combatant Status Definition — State-Centric Reading
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story instantiates the state-centric reading of the
 *   combatant status kernel: combatant status and POW protections under
 *   Geneva Convention III Article 4 require formal membership in state armed
 *   forces meeting four criteria (command structure, fixed distinctive sign,
 *   open carrying of arms, compliance with laws of war). Non-state actors —
 *   insurgents, national liberation movements, organized armed groups — are
 *   categorically excluded from POW status and may be prosecuted under
 *   domestic criminal law for mere participation in hostilities. The reading
 *   is the operative framework for most states not party to Additional
 *   Protocol I, and for all states regarding non-international armed
 *   conflicts. The high extractiveness (0.82) reflects the transfer of legal
 *   immunity from non-state fighters to state authorities; the high
 *   suppression (0.91) reflects the active prosecutorial machinery that
 *   maintains the exclusion. Theater is low (0.12) — the Article 4 criteria
 *   are applied straightforwardly, not performatively.
 *
 * KEY AGENTS:
 *   - state_militaries: Primary beneficiary (institutional/arbitrage) — receive full Geneva protections, immunity from prosecution for lawful acts of war
 *   - state_prosecutorial_authorities: Primary beneficiary (institutional/arbitrage) — retain jurisdiction to prosecute non-state fighters under domestic law
 *   - non_state_fighters: Primary victim (powerless/trapped) — denied POW immunity, subject to criminal prosecution for participation in hostilities
 *   - detained_insurgents: Primary victim (powerless/trapped) — denied GC III protections, held without combatant privilege
 *   - national_liberation_combatants: Primary victim (organized/trapped) — fighting colonial/occupation regimes but excluded from combatant status under this reading
 *   - international_courts: Observer (institutional/analytical) — adjudicate status disputes but lack enforcement power over state practice
 *   - icrc: Observer (institutional/analytical) — monitors compliance, advocates for functional protections
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.82).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.91).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, snare).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Combatant Status Definition — State-Centric Reading").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '98b15002-50fe-491e-9b2b-4fe866e0be4b').
narrative_ontology:cs_kernel_codification('98b15002-50fe-491e-9b2b-4fe866e0be4b', formalized).
narrative_ontology:cs_authority_grounding('98b15002-50fe-491e-9b2b-4fe866e0be4b', lineage).
narrative_ontology:cs_interpretation_layer_present('98b15002-50fe-491e-9b2b-4fe866e0be4b').
narrative_ontology:cs_reading_relation('98b15002-50fe-491e-9b2b-4fe866e0be4b', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('98b15002-50fe-491e-9b2b-4fe866e0be4b', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('98b15002-50fe-491e-9b2b-4fe866e0be4b', foundational, combatant_status_requires_state_membership).
narrative_ontology:cs_axiom_status(combatant_status_requires_state_membership, holdable).
narrative_ontology:cs_axiom_grounding('98b15002-50fe-491e-9b2b-4fe866e0be4b', combatant_status_requires_state_membership, conventional).
narrative_ontology:cs_axiom('98b15002-50fe-491e-9b2b-4fe866e0be4b', foundational, article_4_criteria_are_exhaustive).
narrative_ontology:cs_axiom_status(article_4_criteria_are_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('98b15002-50fe-491e-9b2b-4fe866e0be4b', article_4_criteria_are_exhaustive, conventional).
narrative_ontology:cs_reference_frame('98b15002-50fe-491e-9b2b-4fe866e0be4b', id_1949_geneva_convention_iii_article_4).
narrative_ontology:cs_drift_state('98b15002-50fe-491e-9b2b-4fe866e0be4b', post_ap_i_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('98b15002-50fe-491e-9b2b-4fe866e0be4b', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_prosecutorial_authorities).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, detained_insurgents).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, national_liberation_combatants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive full GC III POW protections: immunity from prosecution for lawful acts of war, humane detention standards, repatriation rights. Their status is automatic by virtue of state membership and Article 4 compliance. They face no risk of criminal prosecution for participation in hostilities. Exit from the constraint is irrelevant — they are its primary beneficiaries and authors.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, global).

% Administer and enforce the combatant status definition through domestic criminal law. Retain jurisdiction to prosecute non-state fighters for mere participation in hostilities (treason, terrorism, sedition statutes). They set the prosecutorial agenda, control the courts, and determine whether captured fighters face criminal trial. The constraint's persistence depends on their active enforcement.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_prosecutorial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Fighters in non-state armed groups (insurgents, rebels, militia) who participate in hostilities but lack state military status. Denied POW immunity under this reading — subject to prosecution under domestic criminal law for acts that would be lawful if committed by state soldiers. No exit from the classification: their identity as 'fighter' is constituted by the conflict itself, and they cannot renounce it to avoid prosecution. Bear the full extractive weight of the constraint.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_fighters, payer,
    powerless, biographical, trapped, global).

% Non-state fighters captured during hostilities. Held without GC III protections: no guaranteed humane treatment standards, no prohibition on adverse distinction, no right to repatriation. Subject to domestic criminal prosecution, often in military or special courts with reduced due process. Their detention status is entirely at the captor state's discretion. No exit — they are physically detained and legally classified by the constraint.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detained_insurgents, payer,
    powerless, immediate, trapped, global).

% Organized armed groups fighting colonial domination, alien occupation, or racist regimes (AP I Art 1(4) wars of national liberation). Under this reading, they are denied combatant status despite meeting functional criteria (command structure, distinctive sign, open arms, law of war compliance). They are structurally excluded from the POW framework that AP I would extend to them. Their political identity as liberation fighters is locked to the conflict — they cannot exit the classification without abandoning their cause. They bear high extraction (criminalization) with slightly more organizational resistance capacity than isolated insurgents.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, national_liberation_combatants, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, national_liberation_combatants, excluded).

% ICJ, ICC, and regional human rights courts adjudicate combatant status disputes and review detention legality. They interpret GC III, AP I, and customary IHL but lack direct enforcement power over state prosecutorial practice. Their judgments create interpretive pressure but do not override domestic criminal jurisdiction. They see the full structural picture but cannot change the constraint's operation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_courts, observer,
    institutional, generational, analytical, global).

% International Committee of the Red Cross monitors detention conditions, visits prisoners, and advocates for functional protections (Common Article 3 minimums) regardless of status. It operates in the gap between the state-centric reading's formal exclusions and the functional reality of detention. It has analytical exit but no power to alter the legal classification. Its presence is the primary resistance vector against the constraint's full suppression.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, state_prosecutorial_authorities).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, bright-line legal classification distinguishing lawful combatants (state militaries meeting Article 4 criteria) from unlawful participants in hostilities, enabling targeting rules, detention regimes, and prosecutorial clarity in interstate armed conflict.
% TRANSFER_FUNCTION: Transfers legal immunity from non-state fighters to state authorities: non-state fighters lose POW protections and become prosecutable under domestic law; state militaries retain full immunity; state prosecutorial authorities gain jurisdiction over a category of persons (enemy fighters) who would otherwise be immune.
% ABSENT_VOICES: Non-state fighters and national liberation combatants are structurally excluded from the treaty-making and interpretive processes that define combatant status. They would object to their criminalization but have no seat at the diplomatic table. Civilian populations in non-international conflicts are also absent — they bear the humanitarian consequences when the constraint incentivizes states to deny protections to all non-state actors.
% DISAPPEARANCE_RATIONALE: If the state-centric combatant status definition vanished overnight, non-state fighters would immediately claim POW protections or at least functional equivalents; states would lose automatic prosecutorial jurisdiction over enemy fighters; detention regimes would have to default to Common Article 3 or human rights law minimums; the legal architecture of 'unlawful combatant' would collapse. The international legal order around detention and targeting would reorganize around functional criteria rather than formal state membership.
% FOUNDING_PROBLEM: 1949 diplomatic conference: distinguishing lawful combatants from francs-tireurs and civilians in interstate war to protect civilian populations from the consequences of irregular warfare and to provide clear categories for detention and targeting.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and prosecutorial authorities attest the problem remains live — irregular warfare persists and clear categories are needed. ICRC, international courts, and AP I parties attest the founding problem is substantially transformed — modern conflicts are predominantly non-international, the state/non-state binary maps poorly, and the Article 4 criteria exclude functionally identical fighters from protections. No disinterested third party corroborates the founding problem as originally framed without qualification.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint operates as a snare: it presents itself as a neutral legal classification (Article 4 criteria) but structurally extracts legal immunity from non-state actors and transfers prosecutorial power to states. The coordination function (clear status categories for detention and targeting) is real but thin — the same clarity could be achieved by a functional definition. The extraction is the criminalization of non-state participation in hostilities, which serves state monopoly on violence. Suppression is high because the exclusion is maintained by active prosecutorial practice, not passive neglect. The low theater ratio reflects that Article 4 is applied as-written; there is little performative gap between the rule's text and its operation. Resistance is significant (0.73) from non-state actors, ICRC advocacy, and AP I parties, but has not shifted the core exclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat (beneficiary, institutional power, arbitrage exit), the constraint is a rope — it provides clear coordination of who qualifies for protections and who may be targeted. From the non-state fighter seat (victim, powerless, trapped), the same constraint is a snare — it denies protections available to functionally identical actors and enables prosecution. The engine computes this divergence from the structural data: beneficiaries have d near 0 (full beneficiary), victims have d near 1 (full target). The national liberation combatant seat (organized but trapped by the Article 1(4) exclusion) experiences the constraint as a snare with slightly higher resistance capacity but no effective exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: state_militaries and state_prosecutorial_authorities — both institutional actors with arbitrage-grade exit (they write and enforce the rules). They collect the constraint's gains: legal immunity for state forces, prosecutorial jurisdiction over non-state actors. Victims: non_state_fighters, detained_insurgents, national_liberation_combatants — all powerless or organized but trapped (no exit from the legal classification; identity as 'fighter' is locked to the conflict). The prosecutorial authority is the agenda_setter (administers the domestic law enforcement). The ICRC and international courts are observers (analytical exit). No directionality overrides needed — the beneficiary/victim structure plus exit options derives correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1949): distinguishing lawful combatants from francs-tireurs and civilians in interstate war to protect civilians and provide clear detention categories. That problem is contested — interstate war is rarer, non-international conflicts dominate, and the Article 4 criteria map poorly to modern asymmetric conflicts. The constraint persists not because the founding problem is live, but because it secures state monopoly on legitimate violence and prosecutorial convenience. This is a resolved mandatrophy: the arrangement has outlived its coordination function but persists by extracting from those it classifies as outside the law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is the combatant status definition a single natural legal category, or does it instantiate multiple structurally distinct constraints depending on which reading governs?',
    'Decompose the kernel into separate constraint stories per reading; verify ε differs materially across readings (state-centric reading: high ε for non-state fighters; functional reading: low ε for all detainees). If ε differs, the kernel is a family of constraints, not one constraint with measurement variance.',
    'If the kernel decomposes, the state-centric reading is one constraint in a family with national_liberation_reading and functional_protection_reading, linked by network.affects_constraints. This story is that family member.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Whether the combatant status kernel is a single constraint or a constraint family with per-reading ε').

omega_variable(
    suppression_mechanism_in_i_hl,
    'Is the suppression of non-state fighters'' POW claims structural (treaty text, state practice, prosecutorial power) or internalized (non-state actors accepting criminalization as legitimate)?',
    'Post-conflict legal trajectory: if non-state fighters continue to claim POW status and resist domestic prosecution after conflict ends, suppression has structural and internalized components. Track whether resistance persists after the extractive mechanism (state prosecutorial power) is removed.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the criminalization with them after conflict. This affects resistance measurements and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_i_hl, empirical, 'Structural vs. internalized suppression in the combatant status definition').

omega_variable(
    article_4_vs_ap1_art1_4_tension,
    'Does the state-centric reading''s reliance on Article 4 GC III structurally foreclose AP I Article 1(4) national liberation movements, or do the two readings coexist in state practice?',
    'Examine state practice and jurisprudence: do states that reject AP I Article 1(4) also treat national liberation fighters as criminals under domestic law (forecloses), or do they extend some protections without conceding combatant status (coexists_with/influences)?',
    'Determines the reading_relations between state_centric_reading and national_liberation_reading in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_vs_ap1_art1_4_tension, conceptual, 'Whether state-centric reading forecloses national liberation reading or they coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combatant_status_state_centric_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement(combatant_status_state_centric_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(combatant_status_state_centric_tr_t1990, combatant_status_definition__state_centric_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(combatant_status_state_centric_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(combatant_status_state_centric_tr_t2010, combatant_status_definition__state_centric_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(combatant_status_state_centric_tr_t2020, combatant_status_definition__state_centric_reading, theater_ratio, 2020, 0.12).

% Extraction over time
narrative_ontology:measurement(combatant_status_state_centric_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.65).
narrative_ontology:measurement(combatant_status_state_centric_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.72).
narrative_ontology:measurement(combatant_status_state_centric_be_t1990, combatant_status_definition__state_centric_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(combatant_status_state_centric_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.82).
narrative_ontology:measurement(combatant_status_state_centric_be_t2010, combatant_status_definition__state_centric_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(combatant_status_state_centric_be_t2020, combatant_status_definition__state_centric_reading, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(combatant_status_state_centric_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.78).
narrative_ontology:measurement(combatant_status_state_centric_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.85).
narrative_ontology:measurement(combatant_status_state_centric_su_t1990, combatant_status_definition__state_centric_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(combatant_status_state_centric_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.91).
narrative_ontology:measurement(combatant_status_state_centric_su_t2010, combatant_status_definition__state_centric_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(combatant_status_state_centric_su_t2020, combatant_status_definition__state_centric_reading, suppression_requirement, 2020, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, pow_protections_scope).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, domestic_prosecution_of_combatants).

% DUAL FORMULATION NOTE:
% The combatant_status_definition kernel decomposes into three constraint stories: state_centric_reading (this file), national_liberation_reading, functional_protection_reading. They share the same referent (GC III Article 4 + AP I Art 1(4) + CA3) but author different ε values and different beneficiary/victim structures because they instantiate structurally distinct claims about who qualifies for combatant status. The state-centric reading is upstream — it is the default framework that the other two readings react against or qualify.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
