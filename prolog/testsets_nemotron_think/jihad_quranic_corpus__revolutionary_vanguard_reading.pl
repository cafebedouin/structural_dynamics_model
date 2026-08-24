% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Jihad as Individual Obligation (Fard 'Ayn) Against Apostate Rulers and Occupiers
 *   domain: religious/political/theological
 *
 * SUMMARY:
 *   This constraint story captures the revolutionary vanguard reading of the
 *   jihad_quranic_corpus kernel: the claim that jihad is an immediate
 *   individual obligation (fard 'ayn) against apostate rulers and foreign
 *   occupiers, bypassing the classical requirement for state/imam
 *   authorization through takfir (excommunication) declarations and emergency
 *   jurisprudence (fiqh al-nawazil). The doctrine reclassifies Muslim rulers
 *   who do not implement sharia as apostates, reclassifies civilians living
 *   under such rule as combatants via collective guilt, and authorizes any
 *   individual Muslim to kill them without centralized command. The
 *   coordination story is defense of Islam and liberation of Muslim lands;
 *   the extraction is asymmetrical violence against declared enemies with no
 *   exit for targets. The engine will compute per-seat classifications from
 *   the structural asymmetry: ideologues and takfir authorities as
 *   agenda-setters with identity-locked exit; recruits as dual-positioned
 *   beneficiaries/payers with constrained exit; declared apostates,
 *   occupiers, and civilians as trapped payers; classical scholars as
 *   excluded; state security as institutional payers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.9).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Jihad as Individual Obligation (Fard 'Ayn) Against Apostate Rulers and Occupiers").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political/theological").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'df16892f-acfa-4494-ab5b-6b727ef99e3d').
narrative_ontology:cs_kernel_codification('df16892f-acfa-4494-ab5b-6b727ef99e3d', formalized).
narrative_ontology:cs_authority_grounding('df16892f-acfa-4494-ab5b-6b727ef99e3d', lineage).
narrative_ontology:cs_interpretation_layer_present('df16892f-acfa-4494-ab5b-6b727ef99e3d').
narrative_ontology:cs_reading_relation('df16892f-acfa-4494-ab5b-6b727ef99e3d', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('df16892f-acfa-4494-ab5b-6b727ef99e3d', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_axiom('df16892f-acfa-4494-ab5b-6b727ef99e3d', foundational, fard_ayn_immediate_obligation).
narrative_ontology:cs_axiom_status(fard_ayn_immediate_obligation, holdable).
narrative_ontology:cs_axiom_grounding('df16892f-acfa-4494-ab5b-6b727ef99e3d', fard_ayn_immediate_obligation, deontological).
narrative_ontology:cs_axiom('df16892f-acfa-4494-ab5b-6b727ef99e3d', foundational, takfir_authorizes_bypass_state_authority).
narrative_ontology:cs_axiom_status(takfir_authorizes_bypass_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('df16892f-acfa-4494-ab5b-6b727ef99e3d', takfir_authorizes_bypass_state_authority, deontological).
narrative_ontology:cs_axiom('df16892f-acfa-4494-ab5b-6b727ef99e3d', foundational, collective_guilt_justifies_civilian_targeting).
narrative_ontology:cs_axiom_status(collective_guilt_justifies_civilian_targeting, holdable).
narrative_ontology:cs_axiom_grounding('df16892f-acfa-4494-ab5b-6b727ef99e3d', collective_guilt_justifies_civilian_targeting, deontological).
narrative_ontology:cs_reference_frame('df16892f-acfa-4494-ab5b-6b727ef99e3d', classical_jihad_framework).
narrative_ontology:cs_drift_state('df16892f-acfa-4494-ab5b-6b727ef99e3d', contemporary_revolutionary_vanguard_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('df16892f-acfa-4494-ab5b-6b727ef99e3d', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_ideologues).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_recruits).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, declared_apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_deemed_combatants).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_scholars_rejecting_doctrine).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslims_pressured_by_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_recruits).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, state_security_apparatus).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_jurisprudence_overrides_classical_safeguards).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, individual_obligation_bypasses_state_authority).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, collective_guilt_justifies_civilian_targeting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulate and propagate the doctrine that jihad is an immediate individual obligation against apostate rulers and occupiers. They declare takfir, authorize operations, and gain authority, recruitment, and material support from the doctrine's acceptance. Their identity is fused with the doctrine; exit means abandoning their life's work and theological self-conception.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_ideologues, agenda_setter,
    organized, generational, identity_locked, global).

% Religious figures who issue takfir declarations against rulers and populations. They gain religious authority, followers, and sometimes material resources from their role as gatekeepers of who is Muslim and who is apostate. Their credibility depends entirely on the doctrine's acceptance; they cannot retract without losing all standing.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_authorities, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_authorities, beneficiary).

% Individuals who accept the doctrine and mobilize for violent action. They gain identity, purpose, community, and promise of martyrdom/reward. They pay with their lives, freedom, and often their families' safety. Exit is constrained by social pressure, fear of being declared apostate themselves, and loss of the identity framework the doctrine provides.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_recruits, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_recruits, payer).

% Muslim rulers declared apostate by vanguard authorities. They face assassination attempts, insurgency, and delegitimization. They cannot exit the category once declared; the only options are crushing the vanguard militarily or being removed. Their power is high but the constraint targets them existentially.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, declared_apostate_rulers, payer,
    powerful, immediate, trapped, national).

% Foreign military forces in Muslim lands targeted by the doctrine. They bear the cost of asymmetric warfare, casualties, and political pressure. Unlike other victims, they have genuine exit options (withdrawal), which the doctrine seeks to make politically costly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    powerful, biographical, mobile, global).

% Civilian populations in conflict zones declared combatants via collective guilt (living under apostate rule, not emigrating, not actively resisting). They bear the extraction of violence with no agency, no voice in the doctrinal debate, and no exit — displacement is often impossible or equally deadly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_deemed_combatants, payer,
    powerless, immediate, trapped, local).

% Traditional jurists and institutions (e.g., Al-Azhar, major madrasas) who maintain classical safeguards: jihad as collective obligation requiring state authorization, proportionality, non-combatant immunity. They are structurally excluded from the vanguard's epistemic framework; their objections are dismissed as collaboration with apostates. They retain institutional power but lose doctrinal authority over the mobilized.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_scholars_rejecting_doctrine, excluded,
    institutional, generational, constrained, global).

% Ordinary Muslims who reject the vanguard's theology but face social pressure, takfir threats, and state suspicion. They pay through coerced conformity, fear of denunciation, and collateral damage from state counter-terrorism. Exit requires public theological dissent which invites vanguard retaliation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslims_pressured_by_doctrine, payer,
    moderate, biographical, constrained, global).

% State intelligence and military forces combating vanguard mobilization. They bear enormous resource costs, casualties, and political blowback. The constraint forces them into permanent counter-insurgency; exit would mean regime collapse. They are payers to both the vanguard's violence and the political necessity of responding.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, state_security_apparatus, payer,
    institutional, biographical, constrained, national).

% International humanitarian law bodies, UN mechanisms, and courts that classify the doctrine's operations as war crimes and crimes against humanity. They document, condemn, and occasionally prosecute but lack enforcement power against non-state actors. Their analytical seat sees the full structural violation of jus in bello.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, international_legal_order, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes individuals for immediate violent action against declared enemies of Islam, providing a theological justification and identity framework that bypasses classical authority structures and enables decentralized operational tempo.
% TRANSFER_FUNCTION: Moves life, property, and security from those declared apostate, occupying forces, and civilians deemed combatants to the revolutionary vanguard project; moves religious and political authority from classical scholars and state institutions to takfir-declaring ideologues and operational commanders.
% ABSENT_VOICES: Classical jurists maintaining traditional safeguards (proportionality, non-combatant immunity, state authorization), moderate Muslim communities who reject takfir methodology, civilians in conflict zones who bear collective guilt designations, international humanitarian law practitioners — all structurally excluded from the doctrinal formulation and its operationalization.
% DISAPPEARANCE_RATIONALE: This doctrine is the primary mobilization engine for decentralized jihad. If it vanished overnight, individuals would revert to the classical requirement of state/imam authorization for offensive jihad, dramatically reducing operational tempo and fragmentation. Classical authority would reassert doctrinal control; state security apparatus would shift from counter-insurgency to conventional threats; civilian populations would lose the collective guilt designation.
% FOUNDING_PROBLEM: The perceived failure of Muslim states to implement Islamic law, the presence of foreign occupiers in Muslim lands, and the classical jurisprudential requirement for centralized authority (imam/caliph) that left individuals without recognized recourse against rulers deemed un-Islamic.
% FOUNDING_PROBLEM_CORROBORATION: Classical scholars (Al-Azhar, traditional madrasas, state religious establishments) attest the classical framework addresses the problem through established channels; human rights organizations and international legal bodies attest the vanguard's solution creates worse harms (indiscriminate violence, civilian targeting, state collapse); no neutral third party corroborates the vanguard's reading as the sole or necessary solution — the corroboration for the vanguard's framing comes exclusively from within the benefiting parties.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is extreme (0.85) because the constraint transfers life and security from declared targets to the vanguard project via lethal violence, with the transfer justified by theological innovation. Suppression is near-total (0.9) because takfir creates a category with no exit — once declared apostate, the target cannot recant, emigrate, or negotiate; the only resolution is death or victory. Theater ratio is moderate (0.3): the religious framing provides genuine motivational coordination for recruits (not pure performance), but the takfir mechanism and collective guilt doctrine are innovations that serve extraction more than theological necessity. Accessibility collapse is high (0.85): accepting the premise that rulers are apostates and classical safeguards are suspended collapses all alternative frameworks — classical fiqh, international law, quietist quietism. Resistance is high (0.8) from classical institutions, states, and moderate communities, but the constraint's decentralized structure makes it resilient to decapitation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (ideologues, takfir authorities) experience the constraint as genuine coordination — a theological breakthrough that solves the problem of individual agency under apostate rule. The payer seats (declared apostates, civilians, state security) experience it as pure extraction backed by lethal coercion. The recruit seat experiences it as both: coordination that gives meaning, extraction that takes their lives. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Ideologues and takfir authorities are structural beneficiaries (d ≈ 0.1): they collect authority, recruits, and resources from the doctrine's operation. Recruits are near-symmetric (d ≈ 0.5): they gain identity and purpose but pay with high mortality risk and constrained exit. Declared apostate rulers and civilians deemed combatants are full targets (d ≈ 1.0): trapped, identity-locked in the target category, bearing lethal extraction with no recourse. Occupying forces are mobile targets (d ≈ 0.8): they bear extraction but have genuine exit (withdrawal). Classical scholars are excluded (d undefined by derivation): they would be payers if the doctrine succeeds (loss of authority) but are structurally outside the constraint's direct operation. State security are institutional payers (d ≈ 0.7): they bear costs but cannot exit without regime collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classical jihad constraint (state-authorized, collective obligation, with safeguards) has undergone mandatrophy: its original function (mobilizing defense of the community under legitimate authority) has atrophied as states failed to fulfill the obligation, creating a vacuum. The revolutionary vanguard reading fills the vacuum but mutates the function: it retains the label 'jihad' and the mobilization power while discarding the safeguards that made the classical constraint a rope/tangled_rope with bounded extraction. The mandate (defend Islam) is real; the current arrangement (individual obligation via takfir) is a snare-like mutation that extracts from the very community it claims to defend. The mandatrophy is unresolved — the vacuum persists and the mutation spreads.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_mechanism_legitimacy,
    'Is the takfir mechanism deployed by this reading a genuine theological tool with classical precedent, or a doctrinal innovation engineered to authorize political violence against Muslim rulers?',
    'Comparative analysis of classical takfir jurisprudence (conditions, authorities, safeguards) versus vanguard usage (who declares, on what grounds, with what procedural protections). Historical investigation of whether pre-modern jurists authorized individual Muslims to kill rulers declared apostate without judicial process.',
    'If the mechanism is a genuine classical tool, the constraint''s coordination function has deeper roots and the extraction may be partially bounded by classical procedure. If it is a modern innovation for political violence, the constraint is a snare wearing a rope''s clothing — the coordination story is cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(takfir_mechanism_legitimacy, conceptual, 'Whether takfir in this reading is classical continuity or modern innovation for extraction.').

omega_variable(
    coordination_extraction_boundary,
    'Does the mobilization framework provide genuine coordination value to recruits (identity, purpose, community, spiritual meaning) that is separable from the extraction inflicted on targets, or is the entire structure a mechanism for extracting violence from recruits themselves?',
    'Longitudinal study of recruit trajectories: do those who join gain measurable psychosocial benefits (belonging, meaning, status) independent of operational outcomes? Comparison with non-violent identity-coordination groups (e.g., Sufi orders, dawa movements).',
    'If coordination is genuine and separable, the constraint is tangled_rope (hybrid). If recruits are themselves extracted (lives wasted for ideologues'' gain with no real benefit), the constraint is a snare at all seats. The current metrics assume hybrid; this omega flags the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the mobilization coordination is genuine benefit to recruits or extraction from them.').

omega_variable(
    collective_guilt_theological_basis,
    'Does the doctrine of collective guilt (civilians become combatants by residing under apostate rule without emigrating or resisting) have any basis in classical Islamic jurisprudence, or is it a total innovation for civilian targeting?',
    'Survey of classical fiqh on dar al-harb/dar al-islam, hijra (emigration) obligation, and civilian immunity (non-combatant protections in Islamic law of war). Determine whether any pre-modern jurist extended combatant status to non-combatant populations via collective guilt.',
    'If classical basis exists, the civilian targeting has some coordination internal to the tradition (however repugnant to modern norms). If it is pure innovation, the civilian extraction is entirely unbounded by any coordinating framework — a snare element within the tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_guilt_theological_basis, empirical, 'Whether collective guilt doctrine has classical precedent or is modern innovation for extraction.').

omega_variable(
    kernel_reading_relationship,
    'This constraint is one reading of the contested jihad_quranic_corpus kernel. How does its structural relationship to sibling readings affect its classification stability?',
    'Track whether the defensive_spiritual_reading''s continued institutional dominance (Al-Azhar, major states) constrains the vanguard reading''s recruitment pool, or whether the expansionist_legalist_reading''s state-sponsored variants (e.g., historical caliphates, modern state jihad) compete for the same mobilizable population. Monitor for foreclosure events: if a major authority formally adopts one reading as exclusive, does it structurally eliminate the others?',
    'If the defensive reading forecloses this reading institutionally, the constraint''s beneficiary base shrinks. If the expansionist reading coexists and competes, the vanguard reading must escalate extraction to differentiate. The reading_relations and drift_state in cs_structure capture this; this omega flags the empirical uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relationship, conceptual, 'Structural relationship to sibling readings of the same kernel and its effect on classification stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 32, 0.84).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t8, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t16, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 16, 0.85).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t24, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 24, 0.88).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t32, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(jihad_quranic_corpus__revolutionary_vanguard_reading_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.08).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jihad_constraint).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, state_monopoly_on_violence_constraint).

% DUAL FORMULATION NOTE:
% This constraint is the revolutionary_vanguard_reading of the jihad_quranic_corpus kernel. It decomposes the colloquial label 'jihad' into a structurally distinct claim: immediate individual obligation against apostate rulers via takfir and emergency jurisprudence. The defensive_spiritual_reading and expansionist_legalist_reading are separate constraints with different ε values, beneficiary/victim structures, and coordination functions. All three form a constraint family linked by the shared kernel. This reading's ε (0.85) is substantially higher than the defensive reading's (near 0) and the expansionist reading's (moderate, ~0.4-0.5) because it eliminates all classical safeguards and authorizes decentralized lethal violence against co-religionists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, institutional, 0.85).
constraint_indexing:directionality_override(jihad_quranic_corpus__revolutionary_vanguard_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
