% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction via Dueling (Decline Reading)
 *   domain: legal/social/normative
 *
 * SUMMARY:
 *   Dueling persisted from the 16th through 19th centuries at declining
 *   frequency until it became fringe. This reading instantiates the
 *   constraint as a practice that weakens structurally but remains
 *   conceptually available — a Piton in institutional terms. The
 *   honor-satisfaction mechanism was real and functional in its founding
 *   (settling disputes outside law), but by the interval's end the founding
 *   problem was dead (law and reputation systems had solved it), enforcement
 *   costs were rising (state prosecution became less selective), social cost
 *   was accumulating (cultural stigma, insurance penalties, exile), and
 *   participation was declining (fewer aristocrats willing to risk it). Yet
 *   the mechanism persisted because exit meant admitting the honor doctrine
 *   was constructed, not natural — identity fusion kept some participants
 *   locked in even as the constraint became mostly theater: aristocrats
 *   defending honor verbally while actual power consolidated in bourgeois
 *   institutions.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_claimants: beneficiaries and gatekeepers; high power, identity-locked exit, declining participation but stubborn persistence
 *   - state_legal_authority: agenda-setter; selective enforcement and selective non-enforcement create the cost gradient
 *   - bourgeois_merchants_and_professionals: organized payers and alternative-path-builders; mobile exit undermines the constraint's salience
 *   - common_law_subjects: general payers; diffuse costs from legal uncertainty and double standards
 *   - lower_classes: excluded; bear costs without participation rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.68).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction via Dueling (Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "legal/social/normative").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, 'b306f687-be86-442a-a0fc-76579858bf02').
narrative_ontology:cs_kernel_codification('b306f687-be86-442a-a0fc-76579858bf02', distributed).
narrative_ontology:cs_authority_grounding('b306f687-be86-442a-a0fc-76579858bf02', lineage).
narrative_ontology:cs_interpretation_layer_present('b306f687-be86-442a-a0fc-76579858bf02').
narrative_ontology:cs_reading_relation('b306f687-be86-442a-a0fc-76579858bf02', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('b306f687-be86-442a-a0fc-76579858bf02', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('b306f687-be86-442a-a0fc-76579858bf02', foundational, honor_persists_at_declining_frequency).
narrative_ontology:cs_axiom_status(honor_persists_at_declining_frequency, holdable).
narrative_ontology:cs_axiom_grounding('b306f687-be86-442a-a0fc-76579858bf02', honor_persists_at_declining_frequency, empirically_contingent).
narrative_ontology:cs_axiom('b306f687-be86-442a-a0fc-76579858bf02', foundational, practice_decay_via_cost_accumulation).
narrative_ontology:cs_axiom_status(practice_decay_via_cost_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('b306f687-be86-442a-a0fc-76579858bf02', practice_decay_via_cost_accumulation, instrumental).
narrative_ontology:cs_reference_frame('b306f687-be86-442a-a0fc-76579858bf02', aristocratic_satisfaction_sovereignty).
narrative_ontology:cs_drift_state('b306f687-be86-442a-a0fc-76579858bf02', late_nineteenth_century_enforcement_intensification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b306f687-be86-442a-a0fc-76579858bf02', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, common_law_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, bourgeois_merchants_and_professionals).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_supremacy).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, satisfaction_through_violence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the claim that personal honor violations require violent satisfaction — dueling. They frame this as defending a non-negotiable status marker and personal sovereignty. As enforcement costs rise and social sanction increases, they continue to participate at declining rates, treating participation as proof of continued membership in the honor-bound class. The constraint persists because exit means admitting the honor doctrine was always constructed, not natural.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants, agenda_setter).

% Bear the costs of the dueling constraint indirectly: loss of productive members of society (death, injury, emigration to avoid prosecution), legal uncertainty (duelists prosecuted or pardoned arbitrarily), and the maintenance of a separate moral order that contradicts civil law. They cannot escape the constraint's operation without leaving the jurisdiction or accepting social subordination.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, common_law_subjects, payer,
    moderate, biographical, constrained, national).

% Enforces prohibitions against dueling through criminal law while tolerating selective non-enforcement or lenient prosecution of aristocratic participants. Maintains ambiguous legitimacy: publicly opposed to dueling, privately protective of honor-class prerogatives. The enforcement machinery creates the cost gradient that drives declining participation without eliminating the practice entirely.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Navigate a constraint they never fully adopted: they adopt some honor codes but strategically avoid dueling as economically irrational. They accumulate capital and social mobility outside the dueling system, gradually making the aristocratic honor standard obsolete in the domains where real power increasingly concentrates. Their mobility and external valuation of status undermine the constraint's salience.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_merchants_and_professionals, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, bourgeois_merchants_and_professionals, observer).

% Are excluded from the honor system entirely — dueling is a privilege-right of the powerful, not available to them. They bear costs (loss of family members to duel-related deaths, distortion of law toward aristocratic interests) without the option of participation or remedy. Their structural exclusion is what the honor system requires.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, lower_classes, excluded,
    powerless, immediate, trapped, local).

% Produce arguments that honor should be satisfied through courts and reputation, not violence. They articulate the emerging norm that violence is incompatible with civility, and frame dueling as a relic. Their authority gradually shifts what constitutes legitimate satisfaction, making dueling less defensible even to participants.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, jurists_and_legal_reformers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_claimants).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dueling coordinated the settlement of honor disputes outside formal courts, providing a private remedy for wrongs (slights, insults, accusations) that the common law considered non-actionable or too costly to litigate. It solved a coordination problem: aristocrats needed a way to defend reputation and deter injury that formal law did not provide.
% TRANSFER_FUNCTION: Transfers the authority to inflict punishment and determine satisfaction from the state to individual honor-claimants, with the cost borne by the challenged party (risk of death or injury, legal jeopardy, exile). Moves the validation of honor from legal or commercial proof to lethal combat.
% ABSENT_VOICES: Lower classes and enslaved peoples are structurally excluded — they have no participation right and no voice in the honor system, yet bear costs from its operation (loss of productive labor, legal double standards, normalization of elite violence). They would contest the premise that honor is indivisible from violence and blood, but have no seat in the conversation the constraint operates within.
% DISAPPEARANCE_RATIONALE: If dueling vanished as a legitimate satisfaction mechanism, aristocratic honor-defense would shift to alternative channels: courts, public reputation, social ostracism, and financial remedies. The constraint's disappearance would force a reorganization of how status is defended and maintained, collapsing the separate moral order that dueling anchored.
% FOUNDING_PROBLEM: Aristocratic honor and sovereignty required private remedy for affronts that feudal and early modern law did not adequately address. Honor was understood as indivisible from the capacity to respond to violence with violence; formal law and monetary compensation were seen as insufficient.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and jurists working from the 18th century onward attest that the founding problem (inadequacy of law as honor remedy) was substantially solved by reforms making courts available for defamation, establishing insurance mechanisms for dueling death, and shifting cultural valuation away from blood-satisfaction toward reputation. The aristocratic honor claimants themselves remain the primary attestors of the problem's continued relevance — a self-interested claim with no corroboration from non-beneficiary sources.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.82) because the constraint transfers authority and punishment to honor-claimants, extracting compliance and social deference from everyone else. It declines monotonically to 0.68, driven by rising enforcement cost (state prosecution), social sanction (reputation damage, insurance penalties), and alternative paths (bourgeois mobility). The decline flattens around time=20 because a hard core of identity-locked participants remains: they cannot exit without admitting the doctrine was constructed. Theater_ratio rises symmetrically: from 0.18 (mostly functional honor-defense) to 0.42 (mostly performative display of continued membership in the honor class). Suppression_requirement follows enforcement intensity: it rises as state prosecution becomes more consistent and less selective, then declines slightly as mass compliance is achieved through cultural shift rather than active enforcement. The leveled coercion grid shows individual-level stakes declining (honor-claimants face personal legal risk, bourgeois alternatives proliferate) while structural and organizational suppression intensifies (law gets clearer, enforcement machinery institutionalizes). Resistance rises across all levels as alternatives become visible and the constraint's constructedness becomes undeniable.
 *
 * PERSPECTIVAL GAP:
 *   Aristocratic honor-claimants perceive the constraint as a permanent feature of civilization — honor *is* the capacity to respond to violence with violence — and frame participation decline as temporary weakness, not structural decay. State legal authority perceives the constraint as a problem to be managed through selective enforcement, not fundamentally challenged. Bourgeois merchants perceive it as irrational and obsolete, something they strategically avoid. From the bounded payer perspective (common law subjects, lower classes), the constraint persists as a legal double standard and a marker of their subordination. The engine computes these divergences from the power and exit data: identity-locked participants (aristocrats) should compute as higher-d targets despite their beneficiary role because exit is blocked; mobile participants (bourgeoisie) should compute as lower-d targets because they have arbitrage options; and the agenda-setter (state) should compute as beneficiary-adjacent despite formal opposition because selective enforcement channels the gains to an honor-class alliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic honor-claimants are the structural beneficiaries: they collect the authority to determine satisfaction and the social deference that flows from the honor system. However, their exit is identity_locked — admitting the doctrine is constructed collapses their self-concept — so they also function as targets (high d) trapped in the system despite benefiting. Their dual position is captured via secondary_role and directionality_override if needed. State legal authority is the agenda-setter: it maintains selective enforcement that allows the system to persist while publicly opposing it. Bourgeois participants are payers (they navigate social pressure to participate) with mobile exit (they shift capital and power to non-dueling channels), so they have lower directionality values and can escape. Common law subjects and lower classes are pure payers with constrained or trapped exit, so they bear high d values. The grid shows how suppression and stakes_inflation work differently at different levels: at the individual level, personal enforcement (threats, prosecution) is aggressive; at the class level, cultural pressure and insurance penalties accumulate; at the structural level, law becomes clearer and more binding.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy paradox by showing the founding problem as objectively dead (law and reputation systems solve it) while the constraint persists at declining frequency. The constraint is a Piton: the cost to fix exceeds what any single seat bears, so it lingers through inertia and theater. The agenda-setter (state) could end it with clear law and consistent enforcement, but selective enforcement keeps the system ambiguous. The beneficiaries (aristocrats) have invested their identity in it and cannot exit cognitively. The payers lack concentrated power to force reform. The constraint persists not because it solves a live problem, but because the exit cost (identity collapse, admission of constructedness, loss of authority) is unbearable for the beneficiaries and the fix cost (political capital, legal clarity, social enforcement) is high enough that no payer coalition pays to reform it. Theater rises as the constraint becomes less functional — dueling becomes a ritual of aristocratic self-identification rather than a real satisfaction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_depth,
    'For aristocratic honor-claimants, how deep is the identity fusion binding them to the dueling doctrine? Is it professional identity (career path dependence on honor-class status), relational identity (self-concept constituted through honor relationships with peers), ideological identity (worldview that makes violence non-negotiable), or institutional identity (the social structure has ''become'' their function)?',
    'Qualitative historical analysis of letters, diaries, and testimonies from declining-period participants; comparison of exit patterns when external pressures mount (exile vs. participation despite costs); observation of whether identity-locked participants maintain the doctrine even when law enforcement becomes severe.',
    'Deeper fusion means higher effective d (more trapped) and longer constraint persistence. If fusion is shallow, participants should exit rapidly as costs rise; if deep, they persist theatrically and the constraint lingers. Classification of this reading''s piton status depends partly on fusion depth — a constraint sustained by identity fusion differs structurally from one sustained by rational lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, empirical, 'Depth of identity fusion in honor-class persistence').

omega_variable(
    state_enforcement_selectivity_mechanism,
    'Was selective enforcement of dueling prohibitions a deliberate political choice (state protecting aristocratic clients while maintaining law''s formal supremacy) or a practical consequence of proof and jurisdiction challenges?',
    'Archival study of prosecution rates stratified by defendant class; records of executive pardons or commutations; comparison across jurisdictions with different enforcement intensities; testimony from prosecutors and judges about case selection.',
    'If deliberate, the state is structurally complicit in maintaining the constraint — the agenda-setter role is genuine. If practical, enforcement is selectively light by accident, and the state''s stated opposition to dueling is less hollow. This affects whether the state is classified as beneficiary-adjacent or genuinely neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_selectivity_mechanism, empirical, 'Whether state selective enforcement was deliberate or structural').

omega_variable(
    composite_versus_decline_mechanism_partition,
    'Did the decline result from a SINGLE dominant mechanism (enforcement cost accumulation, driving frequency down monotonically) or MULTIPLE simultaneous mechanisms (state monopoly, bourgeois norm-shift, insurance category-jump, legal reform)?',
    'Time-series decomposition: does base_extractiveness follow a smooth decay curve (single mechanism) or show inflection points matching specific interventions (multiple mechanisms)? Do different regions with different legal reforms show different decay patterns?',
    'If single mechanism (decline reading is correct), the constraint weakens as cost accumulates but remains conceptually available. If multiple mechanisms (composite reading is correct), the mechanisms interact and the constraint might bifurcate into regional variants. The classification depends on whether we model monotonic decline or branching divergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(composite_versus_decline_mechanism_partition, conceptual, 'Whether decline is driven by one dominant mechanism or multiple interacting mechanisms').

omega_variable(
    suppression_internalization_boundary,
    'How much of the measured suppression in the later interval is externally enforced (legal machinery, police, courts) versus internalized (participants have absorbed the cultural norm that violence is uncivilized, identity damage has become self-inflicted)?',
    'Post-exit suppression trajectory: if suppression persists after the legal mechanism is removed (e.g., in exile or in jurisdictions with weak enforcement), reclassify as internalized. Comparison of suppression curves across high-enforcement and low-enforcement regions.',
    'If suppression is mostly external, removing legal enforcement would allow rapid escape. If internalized, even legal decriminalization would not end participation because the internalized shame is the true suppressive force. This affects the fix cost calculation and the persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    honor_doctrine_natural_vs_constructed,
    'Was the honor doctrine ever a natural law or irreducible human fact, or was it always a constructed social choice? The beneficiaries claim it is natural; the decline reading implicitly shows it as constructed (because it dies when alternatives work). Which reading of the kernel''s legitimacy is defensible?',
    'Comparative anthropology and history: do honor-satisfaction requirements appear universally across cultures, or only in stratified societies with aristocratic power? If cultural, is the constraint a false summit claiming naturality? If genuinely natural, why does it decay when suppression and cost rise, rather than persist as mountains do?',
    'If the honor doctrine is a false summit (natural-law claim on constructed mechanism), the constraint should be reclassified by FSM rules and carries an omega documenting the claim/metric divergence. If genuinely natural, the rise in resistance and decline in accessibility_collapse are anomalous. This omega routes the epistemological contest about the kernel''s status through the apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honor_doctrine_natural_vs_constructed, conceptual, 'Whether honor-satisfaction is natural law or constructed doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t3, honor_satisfaction_mechanism__decline_reading, theater_ratio, 3, 0.22).
narrative_ontology:measurement_basis(hono_tr_t3, observed).
narrative_ontology:measurement(hono_tr_t6, honor_satisfaction_mechanism__decline_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(hono_tr_t6, observed).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_mechanism__decline_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_mechanism__decline_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t25, honor_satisfaction_mechanism__decline_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(hono_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t3, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 3, 0.79).
narrative_ontology:measurement_basis(hono_be_t3, observed).
narrative_ontology:measurement(hono_be_t6, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement_basis(hono_be_t6, observed).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t25, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(hono_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t3, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement_basis(hono_su_t3, observed).
narrative_ontology:measurement(hono_su_t6, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(hono_su_t6, observed).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t25, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hono_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(class), 25, 0.52).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 0, 0.71).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(individual), 25, 0.54).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 0, 0.78).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(organizational), 25, 0.58).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 0, 0.81).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_mechanism__decline_reading, accessibility_collapse(structural), 25, 0.61).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_mechanism__decline_reading, resistance(class), 0, 0.51).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_mechanism__decline_reading, resistance(class), 25, 0.65).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_mechanism__decline_reading, resistance(individual), 0, 0.38).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_mechanism__decline_reading, resistance(individual), 25, 0.62).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_mechanism__decline_reading, resistance(organizational), 25, 0.68).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_mechanism__decline_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_mechanism__decline_reading, resistance(structural), 25, 0.58).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 0, 0.82).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_mechanism__decline_reading, stakes_inflation(class), 25, 0.65).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 0, 0.85).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_mechanism__decline_reading, stakes_inflation(individual), 25, 0.72).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 0, 0.88).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_mechanism__decline_reading, stakes_inflation(organizational), 25, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 0, 0.79).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_mechanism__decline_reading, stakes_inflation(structural), 25, 0.58).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_mechanism__decline_reading, suppression(class), 0, 0.66).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_mechanism__decline_reading, suppression(class), 25, 0.68).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_mechanism__decline_reading, suppression(individual), 0, 0.64).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_mechanism__decline_reading, suppression(individual), 25, 0.76).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_mechanism__decline_reading, suppression(organizational), 25, 0.71).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_mechanism__decline_reading, suppression(structural), 0, 0.54).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_mechanism__decline_reading, suppression(structural), 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% The kernel 'honor_satisfaction_mechanism' is instantiated by three constraint stories, each modeling a different reading of what happened to dueling. The decline_reading (this file) models persistent practice at declining frequency driven by enforcement cost and social sanction accumulation. The composite_reading models multiple simultaneous mechanisms (state monopoly, bourgeois norms, insurance, category-shift). The contraction_reading models cognitive collapse — the mechanism becoming categorically unthinkable. All three readings describe aspects of the historical record; they differ in which mechanism is treated as primary and how the terminal state is characterized. The decline reading influences the contraction reading: as frequency drops and stigma accumulates, the psychological plausibility of the category deteriorates, making contraction possible downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
