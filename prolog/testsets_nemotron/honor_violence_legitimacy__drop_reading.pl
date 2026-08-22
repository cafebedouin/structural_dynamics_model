% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling Legitimacy — Drop Reading (External Cost Decline)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the 'drop reading' of the
 *   honor_violence_legitimacy kernel: dueling remained formally legitimate
 *   within elite honor codes and military regulations throughout the long
 *   19th century (1780–1880), but its practice frequency dropped sharply due
 *   to external costs — legal prosecution risk, changing public opinion,
 *   professionalization of officer corps, and the rise of alternative
 *   status-dispute mechanisms (courts, press, politics). The constraint did
 *   not lose its conceptual availability; it became practically rare while
 *   remaining thinkable. This is structurally distinct from the 'contraction
 *   reading' (honor itself was redefined to exclude violence) and the
 *   'composite reading' (both mechanisms operated simultaneously). The drop
 *   reading asserts that the coordination function (elite dispute management)
 *   persisted conceptually but the extraction function (mortality risk borne
 *   by participants) became unsustainable as external enforcement costs rose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.38).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.22).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling Legitimacy — Drop Reading (External Cost Decline)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '97c6a1fc-3c99-4ca5-a04f-3746e16af235').
narrative_ontology:cs_kernel_codification('97c6a1fc-3c99-4ca5-a04f-3746e16af235', distributed).
narrative_ontology:cs_authority_grounding('97c6a1fc-3c99-4ca5-a04f-3746e16af235', practice).
narrative_ontology:cs_interpretation_layer_present('97c6a1fc-3c99-4ca5-a04f-3746e16af235').
narrative_ontology:cs_reading_relation('97c6a1fc-3c99-4ca5-a04f-3746e16af235', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('97c6a1fc-3c99-4ca5-a04f-3746e16af235', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('97c6a1fc-3c99-4ca5-a04f-3746e16af235', foundational, honor_legitimacy_persists_despite_practice_decline).
narrative_ontology:cs_axiom_status(honor_legitimacy_persists_despite_practice_decline, holdable).
narrative_ontology:cs_axiom_grounding('97c6a1fc-3c99-4ca5-a04f-3746e16af235', honor_legitimacy_persists_despite_practice_decline, conventional).
narrative_ontology:cs_axiom('97c6a1fc-3c99-4ca5-a04f-3746e16af235', foundational, external_costs_drive_practice_frequency_not_conceptual_availability).
narrative_ontology:cs_axiom_status(external_costs_drive_practice_frequency_not_conceptual_availability, holdable).
narrative_ontology:cs_axiom_grounding('97c6a1fc-3c99-4ca5-a04f-3746e16af235', external_costs_drive_practice_frequency_not_conceptual_availability, empirically_contingent).
narrative_ontology:cs_reference_frame('97c6a1fc-3c99-4ca5-a04f-3746e16af235', classical_honor_violence_legitimacy).
narrative_ontology:cs_drift_state('97c6a1fc-3c99-4ca5-a04f-3746e16af235', late_19th_century, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97c6a1fc-3c99-4ca5-a04f-3746e16af235', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, aristocratic_honor_claimants).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, judicial_authorities_upholding_law).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, families_of_duelists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, lower_status_challengers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the landed aristocracy and high bourgeoisie who claim the right to demand satisfaction for insults. They benefit from the constraint because it preserves their monopoly on honor recognition and allows them to settle disputes without legal process. Their exit is constrained: they cannot abandon honor claims without losing status, but they increasingly avoid duels due to legal risk and social pressure.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, aristocratic_honor_claimants, beneficiary,
    powerful, biographical, constrained, national).

% Professional officer corps that administers dueling codes, courts of honor, and internal discipline. They set the procedural rules that make dueling 'legitimate' rather than murder. They benefit institutionally from maintaining a distinct military honor sphere separate from civilian courts. Their exit options are strong: they can shift to purely legal/administrative discipline without personal cost, and many armies do so during this period.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, military_officer_corps, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges and prosecutors who formally prohibit dueling but often decline to prosecute when 'honor' is satisfied. They benefit from the constraint by maintaining legal authority while accommodating social power — the constraint lets them appear to enforce the law while actually managing elite conflict. Their exit is arbitrage-grade: they can tighten enforcement at any time with institutional support.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, judicial_authorities_upholding_law, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, judicial_authorities_upholding_law, beneficiary).

% Men who actually fight duels — principally junior officers, aristocratic younger sons, and professionals. They bear the extraction: risk of death, injury, legal prosecution, family ruin. Their exit is trapped: refusing a challenge destroys their honor and social standing; accepting risks their life. The constraint's legitimacy makes refusal structurally impossible for them.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dueling_participants, payer,
    moderate, immediate, trapped, local).

% Wives, children, parents, and dependents of duelists who bear financial ruin, social stigma, and emotional trauma when a breadwinner dies or is imprisoned. They have no voice in the honor calculus — the constraint treats them as externalities. Their exit is trapped: they cannot opt out of the kinship obligations that make them hostages to the duelist's honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, families_of_duelists, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__drop_reading, families_of_duelists, excluded).

% Men of lower social standing who challenge or are challenged by aristocrats. The constraint extracts asymmetrically: they face the same mortal risk but gain less honor recognition, and courts treat their participation more harshly. Their exit is trapped — they cannot decline without confirming their dishonor, but acceptance offers no proportional reward.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, lower_status_challengers, payer,
    powerless, immediate, trapped, local).

% Analytical seat observing the constraint's structural evolution across the century. Sees the coordination function (elite dispute management) and extraction function (mortality externalized to participants and families) operating simultaneously. No personal stake in the constraint's persistence.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, historical_sociologist_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bounded, ritualized mechanism for elite men to settle status disputes without uncontrolled violence or legal process, preserving the honor hierarchy that structures their social and political world.
% TRANSFER_FUNCTION: Moves the risk of death, injury, legal prosecution, and family ruin from the honor-claiming elite (who initiate and benefit from the system) onto the duelists themselves and their dependents, while the institutional agenda-setters (officer corps, judiciary) maintain plausible deniability.
% ABSENT_VOICES: Women of the affected families (wives, mothers, sisters) who bore the material consequences but were structurally excluded from the honor calculus; common soldiers and servants who served as seconds or witnesses without honor standing; religious authorities who condemned dueling but lacked enforcement power.
% DISAPPEARANCE_RATIONALE: If the dueling legitimacy constraint vanished overnight, elite dispute resolution would shift to courts, press campaigns, and political maneuvering — the honor hierarchy would persist but its enforcement mechanism would change. The families of duelists would no longer be hostages to ritual violence. The officer corps would lose its distinct honor jurisdiction.
% FOUNDING_PROBLEM: In pre-state and early-state societies, elite men needed a way to settle status disputes that state courts could not or would not adjudicate — particularly insults to personal reputation that had no legal remedy but could destroy social standing.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary dueling codes and military regulations attest the founding problem was live. Historians of the judicialization of honor (e.g., Robert Nye, Ute Frevert) argue the problem was substantially solved by the mid-19th century through libel law, press regulation, and professional disciplinary systems — the constraint persisted as rent extraction after its coordination function atrophied. No corroboration from outside the benefiting parties for the claim that the founding problem remained live throughout the period.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38) reflects that the constraint extracts mortality risk and legal jeopardy from participants while the beneficiaries (aristocratic claimants, officer corps) bear minimal direct cost. The rise from 0.25 to 0.38 over the century tracks the increasing mismatch between the constraint's formal legitimacy and its practical suppression — as dueling becomes rarer, each instance carries higher scrutiny and the institutional agenda-setters extract more legitimacy rent per duel. Suppression (0.22) is moderate: the constraint is not actively enforced against participants (prosecution is sporadic), but the legal prohibition creates a background threat that shapes behavior. Theater ratio (0.15) is low but rising: the ritual apparatus (seconds, codes, courts of honor) is maintained but increasingly performative as actual duels decline. Accessibility collapse (0.42) is moderate — alternatives (courts, press) exist but are not fully accessible to all honor claimants. Resistance (0.35) reflects sporadic legal challenges and public criticism but no organized anti-dueling movement.
 *
 * PERSPECTIVAL GAP:
 *   From the officer corps' seat, the constraint is a functioning coordination mechanism they administer — a rope. From the duelist's seat, it is a snare: they cannot exit, they bear the costs, and the coordination benefit accrues to others. From the judicial authority's seat, it is a tangled rope: they coordinate by tolerating it (managing elite conflict) but extract legitimacy rent from the tolerance. The engine computes these divergences from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic honor claimants and military officer corps are structural beneficiaries (d near 0.1–0.2): they collect the coordination benefit (dispute resolution) without bearing the mortality cost. Judicial authorities are dual-positioned: agenda-setters who could suppress the constraint but benefit from its managed tolerance (d ~0.3). Duels participants and families are targets (d ~0.8–0.9): trapped by honor logic, bearing asymmetric extraction. Lower-status challengers are maximally targeted (d ~0.95): same mortality risk, less honor return, harsher legal treatment. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows clear mandatrophy dynamics: its founding problem (elite dispute resolution absent legal remedies) was substantially solved by mid-century through libel law, professional discipline, and political institutions, but the constraint persisted because the officer corps and aristocratic claimants benefited from maintaining a distinct honor jurisdiction. The rising extractiveness and theater ratio track this atrophy — the coordination function becomes increasingly performative while the extraction (mortality risk externalized to participants) continues. The constraint was not a snare from the start (it solved a real problem) but became one as the problem disappeared and the arrangement persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_boundary,
    'Did the conceptual legitimacy of dueling genuinely persist (drop reading) or did honor itself contract to exclude violence (contraction reading)?',
    'Analyze honor discourse in military codes, dueling manuals, and elite correspondence across the period: if the vocabulary of ''honor requiring blood'' persists while practice drops, drop reading is supported; if the vocabulary itself shifts to non-violent honor, contraction reading is supported.',
    'If contraction reading is correct, the constraint''s accessibility_collapse should be higher (alternatives not just practically unavailable but conceptually unthinkable) and the constraint type shifts toward mountain (conceptual inevitability). If drop reading is correct, accessibility_collapse stays moderate and the constraint remains tangled_rope (coordination function persists conceptually).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_boundary, conceptual, 'Whether the kernel''s conceptual structure contracted or only its practice frequency dropped.').

omega_variable(
    composite_overdetermination,
    'Did external costs and conceptual redefinition operate simultaneously and indistinguishably (composite reading), or can their effects be temporally separated?',
    'Temporal decomposition: if legal prosecution risk rises before honor vocabulary shifts, drop leads; if vocabulary shifts before legal risk rises, contraction leads; if they move in lockstep, composite is supported.',
    'If composite is correct, this constraint story (drop_reading) captures only half the causal structure — the extraction metrics would conflate two mechanisms. The engine would need to model the composite as a distinct constraint with its own stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_overdetermination, empirical, 'Whether the drop and contraction mechanisms are separable or overdetermined.').

omega_variable(
    family_suppression_mechanism,
    'Is the suppression experienced by families of duelists structural (legal/economic dependency) or internalized (honor ideology makes them complicit)?',
    'Post-duel trajectory analysis: if families of killed duelists petition for prosecution, seek civil remedies, or publicly condemn dueling, suppression is structural; if they honor the duelist''s memory and enforce the code on surviving sons, suppression is internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (0.22) suggests — the victims carry the suppression with them. This would increase the effective extraction on the victim seats and strengthen the snare/tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_suppression_mechanism, empirical, 'Structural vs. internalized suppression mechanism for victim stakeholders.').

omega_variable(
    reading_relations_ontology,
    'What is the structural relationship between this drop_reading and its sibling readings — do they foreclose, coexist with, or influence each other?',
    'Analyze whether a single party could hold both the drop and contraction readings simultaneously. If the core premises are logically compatible (practice dropped AND honor contracted), they coexist. If one premise makes the other impossible (honor cannot both persist and contract), they foreclose. If one creates pressure on the other without logical exclusion, they influence.',
    'Determines cs_structure.reading_relations values. Forecloses would mean the kernel admits only one live reading at a time. Coexists_with means multiple readings are simultaneously held by different factions. Influences means the drop reading''s external-cost narrative creates pressure on the contraction reading''s conceptual narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relations_ontology, conceptual, 'Structural relationship between drop_reading and sibling readings in the honor_violence_legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1780, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__drop_reading, theater_ratio, 1780, 0.08).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__drop_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__drop_reading, theater_ratio, 1820, 0.12).
narrative_ontology:measurement(hono_tr_t1840, honor_violence_legitimacy__drop_reading, theater_ratio, 1840, 0.13).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__drop_reading, theater_ratio, 1860, 0.14).
narrative_ontology:measurement(hono_tr_t1880, honor_violence_legitimacy__drop_reading, theater_ratio, 1880, 0.15).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__drop_reading, base_extractiveness, 1780, 0.25).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__drop_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__drop_reading, base_extractiveness, 1820, 0.33).
narrative_ontology:measurement(hono_be_t1840, honor_violence_legitimacy__drop_reading, base_extractiveness, 1840, 0.36).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__drop_reading, base_extractiveness, 1860, 0.38).
narrative_ontology:measurement(hono_be_t1880, honor_violence_legitimacy__drop_reading, base_extractiveness, 1880, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__drop_reading, suppression_requirement, 1780, 0.15).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__drop_reading, suppression_requirement, 1800, 0.18).
narrative_ontology:measurement(hono_su_t1820, honor_violence_legitimacy__drop_reading, suppression_requirement, 1820, 0.2).
narrative_ontology:measurement(hono_su_t1840, honor_violence_legitimacy__drop_reading, suppression_requirement, 1840, 0.21).
narrative_ontology:measurement(hono_su_t1860, honor_violence_legitimacy__drop_reading, suppression_requirement, 1860, 0.22).
narrative_ontology:measurement(hono_su_t1880, honor_violence_legitimacy__drop_reading, suppression_requirement, 1880, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Part of the honor_violence_legitimacy kernel family. The drop_reading isolates the external-cost mechanism (practice frequency drops, conceptual availability persists). The contraction_reading isolates the conceptual-redefinition mechanism (honor itself excludes violence). The composite_reading models their simultaneous operation. Each reading has distinct ε values and stakeholder structures: drop_reading has moderate extractiveness with trapped participants; contraction_reading has lower extractiveness (no practice to extract from) but higher accessibility_collapse; composite_reading has higher extractiveness (both mechanisms extracting) and different temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, institutional, 0.3).
constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, powerful, 0.15).
constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, moderate, 0.85).
constraint_indexing:directionality_override(honor_violence_legitimacy__drop_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
