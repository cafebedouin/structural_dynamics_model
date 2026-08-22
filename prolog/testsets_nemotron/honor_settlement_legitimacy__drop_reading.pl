% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor Settlement Legitimacy — Drop Reading (Fringe Dueling Persistence)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The drop_reading of honor_settlement_legitimacy captures the persistence
 *   of dueling as a fringe practice in specific geographic and social niches
 *   (rural Southern US, frontier military posts, aristocratic European
 *   officer corps) after the mainstream culture had contracted around
 *   legal-bureaucratic settlement. The constraint is the *legitimacy claim*
 *   that honor disputes require violent personal settlement — a claim that
 *   has lost general currency but retains local normative force. The
 *   reading's structural delta: honor culture remains a live option in
 *   niches; dueling is suppressed by state law and majority opinion but not
 *   eliminated from the normative repertoire of adherents. This is a
 *   scaffold: the coordination function (settling honor disputes without
 *   state courts) was meant to be transitional toward legal order, but the
 *   sunset clause (state monopoly on violence) has been only partially
 *   enforced in these niches. The claimed_type is scaffold; the metrics show
 *   rising extractiveness and theater as the coordination function atrophies
 *   and the practice becomes increasingly performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.32).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.68).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, scaffold).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor Settlement Legitimacy — Drop Reading (Fringe Dueling Persistence)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).
narrative_ontology:has_sunset_clause(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'e9f0b35c-dd5b-41e3-a542-39fe383a46e9').
narrative_ontology:cs_kernel_codification('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', fixed_text).
narrative_ontology:cs_authority_grounding('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', lineage).
narrative_ontology:cs_interpretation_layer_present('e9f0b35c-dd5b-41e3-a542-39fe383a46e9').
narrative_ontology:cs_reading_relation('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', honor_settlement_legitimacy__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', foundational, honor_remains_live_in_niches).
narrative_ontology:cs_axiom_status(honor_remains_live_in_niches, holdable).
narrative_ontology:cs_axiom_grounding('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', honor_remains_live_in_niches, conventional).
narrative_ontology:cs_axiom('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', foundational, state_law_illegitimate_for_honor).
narrative_ontology:cs_axiom_status(state_law_illegitimate_for_honor, holdable).
narrative_ontology:cs_axiom_grounding('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', state_law_illegitimate_for_honor, deontological).
narrative_ontology:cs_reference_frame('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', aristocratic_frontier_honor_order).
narrative_ontology:cs_drift_state('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', late_state_incorporation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e9f0b35c-dd5b-41e3-a542-39fe383a46e9', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, rural_gentry_families).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, frontier_community_elders).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, military_officer_corps_traditionalists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, younger_sons_disenfranchised).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, women_in_honor_households).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, mercantile_professional_new_entrants).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, personal_honor_requires_violent_defense).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, legal_courts_cannot_adjudicate_honor).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, blood_price_settles_moral_debt).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the honor code as the governing norm for dispute settlement among their class. They host duels, arbitrate challenges, and define what counts as an insult. Their authority derives from lineage and landholding; exit from the honor frame would mean surrendering the social capital that distinguishes them from merchant-planters and state officials. They are identity-locked: the honor code constitutes their self-concept and class position.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, rural_gentry_families, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, rural_gentry_families, beneficiary).

% In territories where state courts are distant or distrusted, elders administer honor settlements as the primary dispute resolution mechanism. They benefit from the coordination function (order without state apparatus) but are constrained by territorial incorporation — as state law advances, their exit from the honor frame becomes possible but costly (loss of local authority). They are not identity-locked; their commitment is instrumental to governance vacuum.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, frontier_community_elders, agenda_setter,
    moderate, biographical, constrained, local).

% Officer corps in several European and American armies maintain dueling as a ritual of professional honor. The practice coordinates status hierarchy and courage signaling within the corps. They are institutional beneficiaries: the constraint structures their professional identity and promotion logic. Exit is identity-locked — an officer who refuses a challenge loses the corps' trust; the institution has 'become' its honor code. State military law prohibits dueling but enforcement is selective.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, military_officer_corps_traditionalists, beneficiary,
    institutional, biographical, identity_locked, national).

% Excluded from inheritance and honor-standing, younger sons must prove worth through dueling or accept permanent subordination. They bear the mortality risk and the cost of deference. Exit is trapped: leaving the niche means abandoning family, land, and the only status ladder available. They have no voice in the honor code's administration.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, younger_sons_disenfranchised, payer,
    powerless, biographical, trapped, local).

% Bear the consequences of male honor violence (widowhood, economic ruin, social stigma) and enforce honor norms on sons/daughters to maintain household standing. They cannot duel but are bound by the code's logic. Exit is trapped: patriarchal property and custody laws, plus cultural internalization, make leaving the honor frame nearly impossible. They are the constraint's silent substrate.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, women_in_honor_households, payer,
    powerless, biographical, trapped, local).

% Merchants, lawyers, and professionals entering honor-dominated regions must either adopt the code (duel to defend commercial reputation) or accept exclusion from local networks. They pay the 'honor tax' in risk and conformity. Exit is constrained: they can relocate to legal-bureaucratic zones but lose regional market access. They are the leading edge of the sunset clause — their presence erodes the niche.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, mercantile_professional_new_entrants, payer,
    moderate, biographical, constrained, regional).

% Prosecute dueling as murder or assault; the constraint's persistence measures their enforcement failure in niches. They view the honor code as an obstacle to state monopoly on violence. Their analytical seat sees the full structure: the coordination function (local order), the extraction (victim subordination), and the sunset dynamic (state capacity advancing).
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).

% Document the fringe practice as a living cultural system. Their analytical seat is external but immersive — they see the internal logic beneficiaries and victims experience. They do not collect from the constraint; their presence slightly increases suppression (documentation aids prosecution) but also preserves the practice's record.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_ethnographers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles honor disputes within closed communities without recourse to state courts, which adherents view as illegitimate for honor matters. Coordinates status hierarchy, courage signaling, and dispute closure through ritualized violence.
% TRANSFER_FUNCTION: Moves risk of death/injury and deference-costs from agenda_setters (elders, officers, gentry) to payers (younger sons, women, newcomers). Moves authority to define insult and satisfaction from state law to local honor administrators.
% ABSENT_VOICES: The dead (those killed in duels that the drop_reading claims 'settled' disputes), the permanently maimed, and the women and younger sons who internalized the code so completely they never articulated objection. Also absent: state authorities in niches where enforcement is nominal — they are physically absent, not merely excluded.
% DISAPPEARANCE_RATIONALE: If the fringe honor code vanished overnight, rural gentry would lose their primary dispute-resolution authority; younger sons and women would lose the structure that subordinates them but also the only status ladder they know; frontier elders would face governance vacuum; military traditionalists would lose the ritual that structures officer identity. The niches would reorganize — some toward state law, some toward new informal codes.
% FOUNDING_PROBLEM: In stateless or weak-state frontier/aristocratic zones, no trusted impartial tribunal existed to settle disputes over reputation, property boundaries, and personal insult. Violent personal settlement was the only mechanism that commanded compliance from all parties.
% FOUNDING_PROBLEM_CORROBORATION: State legal authorities and mercantile newcomers attest the founding problem is dead — courts now function, reputation markets substitute for honor. Rural gentry elders and military traditionalists attest it is live — courts cannot adjudicate honor, only peers can. No neutral corroboration exists; the dispute is the kernel contest itself.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).
:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) is moderate: the constraint extracts compliance from younger sons, women, and outsiders who must defer to honor logic they don't endorse, but the fringe scale limits total extraction. Suppression (0.68) is high because state law and majority culture actively prosecute and stigmatize dueling — persistence requires active resistance to suppression. Theater ratio (0.22) is rising: an increasing share of duels are performative (first blood, prescribed rituals) rather than lethal settlement, indicating the coordination function is decaying. Accessibility collapse (0.45) is moderate: legal courts exist and are accessible, but honor adherents treat them as illegitimate for honor matters — alternatives are cognitively collapsed, not structurally absent. Resistance (0.41) is moderate: the constraint meets resistance from state authorities and modernizing elites, but fringe adherence is genuine, not coerced.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (rural gentry elder), the constraint is a living coordination mechanism — the only way to settle honor without surrendering to a legal system they view as alien. From the victim seat (younger son), it is an extractive trap — risk death or accept subordination. From the observer seat (state prosecutor), it is a suppressed anachronism. The engine computes these divergences from the structural data; the claimed scaffold type reflects the authoring seat's judgment that the coordination function was transitional and its sunset is underway but incomplete.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural gentry families and frontier elders are beneficiaries (agenda_setters): they administer the honor code, settle disputes on their terms, and extract deference. Military traditionalists are secondary beneficiaries: the code structures officer identity and promotion. Younger sons, women in honor households, and mercantile newcomers are victims (payers): they bear the risk of violence, the cost of deference, and exclusion from honor-standing without the power to shape the code. The directionality derivation from beneficiary/victim + exit options captures this: beneficiaries have constrained exit (identity_locked to honor identity), victims have trapped or constrained exit depending on niche.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate was: settle honor disputes without state courts. That problem is largely dead (state courts function, majority culture uses them). The arrangement persists in niches where the mandate is contested — adherents claim the problem is live (legal courts cannot adjudicate honor). The mandatrophy is unresolved: the constraint's coordination function has atrophied in the mainstream but the sunset clause (state monopoly) is only partially enforced in niches. This creates the scaffold classification: transitional support whose transition is stalled in pockets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the drop_reading a genuine structural reading of the honor_settlement_legitimacy kernel, or a residual observation mistaken for a stable reading?',
    'Trace whether the fringe practitioners articulate a coherent normative framework inheriting from the kernel''s authority structure, or merely perform vestigial rituals without interpretive continuity.',
    'If no interpretive continuity, the drop_reading collapses into a piton observation of the composite_reading''s aftermath — not a live kernel reading. This reclassifies the constraint from scaffold to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether fringe dueling constitutes a live reading of the honor kernel or mere residue').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dueling in fringe niches structural (legal prosecution, social ostracism) or internalized (practitioners'' own doubt about legitimacy)?',
    'Compare suppression trajectories in niches with active legal enforcement vs. niches where law is nominal but practitioners still decline to duel — persistent suppression without legal pressure indicates internalization.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure; the fringe practitioners carry the suppression with them, making the scaffold''s sunset irreversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in residual honor niches').

omega_variable(
    coordination_extraction_boundary,
    'Does the fringe dueling practice still coordinate dispute resolution among adherents, or has it become pure performative extraction (status signaling without settlement function)?',
    'Track outcomes of fringe duels: do they actually terminate disputes and restore social standing, or do they generate new grievances requiring further duels? Settlement rate vs. escalation rate.',
    'If coordination function collapsed (settlement rate near zero), the scaffold''s justification is gone — the constraint is piton, not scaffold. If coordination persists, scaffold classification holds for the niche.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether fringe dueling retains coordination function or has become pure theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hono_tr_t25, honor_settlement_legitimacy__drop_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__drop_reading, theater_ratio, 50, 0.16).
narrative_ontology:measurement(hono_tr_t75, honor_settlement_legitimacy__drop_reading, theater_ratio, 75, 0.19).
narrative_ontology:measurement(hono_tr_t90, honor_settlement_legitimacy__drop_reading, theater_ratio, 90, 0.21).
narrative_ontology:measurement(hono_tr_t100, honor_settlement_legitimacy__drop_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hono_be_t25, honor_settlement_legitimacy__drop_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__drop_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(hono_be_t75, honor_settlement_legitimacy__drop_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement(hono_be_t90, honor_settlement_legitimacy__drop_reading, base_extractiveness, 90, 0.31).
narrative_ontology:measurement(hono_be_t100, honor_settlement_legitimacy__drop_reading, base_extractiveness, 100, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hono_su_t25, honor_settlement_legitimacy__drop_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__drop_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hono_su_t75, honor_settlement_legitimacy__drop_reading, suppression_requirement, 75, 0.64).
narrative_ontology:measurement(hono_su_t90, honor_settlement_legitimacy__drop_reading, suppression_requirement, 90, 0.67).
narrative_ontology:measurement(hono_su_t100, honor_settlement_legitimacy__drop_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.1).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, state_monopoly_violence_enforcement).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel decomposes into three readings with distinct ε and beneficiary/victim structures. The drop_reading has the lowest extractiveness (fringe scale) but highest theater_ratio (performative decay). The contraction_reading has near-zero extractiveness (cognitive unthinkability eliminates the constraint). The composite_reading has moderate extractiveness (mainstream transition costs). All three share the vindicated_proposition 'personal_honor_requires_violent_defense' but differ in whether that proposition remains normatively active.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, moderate, 0.75).
constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
