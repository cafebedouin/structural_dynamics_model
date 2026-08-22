% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Primary Constraint on Death-with-Dignity
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The sanctity-of-life reading treats human life as possessing intrinsic,
 *   transcendent value that forbids intentional termination regardless of
 *   consent. This constraint operates as a legal prohibition on assisted
 *   dying and euthanasia, enforced through criminal law and medical
 *   licensing. Its stated function is protecting vulnerable populations from
 *   coercion and preserving the moral order that treats life as inviolable.
 *   However, as legalization spreads in peer jurisdictions (Netherlands,
 *   Belgium, Canada, Oregon, etc.) and empirical data accumulates on both
 *   regimes' outcomes, the constraint's operation increasingly appears to
 *   extract from the very populations it claims to protect — terminally ill
 *   elderly who endure prolonged suffering, disabled persons whose lives are
 *   treated as inherently burdensome, and economically vulnerable patients
 *   who face pressure toward cost-saving death in systems with strained
 *   resources. The theater ratio rises as palliative care infrastructure
 *   expands but remains inaccessible to many, and the prohibition's
 *   enforcement apparatus (criminal sanctions, licensing discipline) targets
 *   clinicians and families rather than the structural conditions that create
 *   vulnerability.
 *
 * KEY AGENTS:
 *   - moral_order_community: Primary beneficiary (institutional/analytical) — religious institutions, bioethics establishments, legislative coalitions that derive authority from maintaining the prohibition
 *   - terminally_ill_elderly: Primary victim (powerless/trapped) — denied control over dying process, forced to endure suffering the constraint declares inviolable
 *   - disabled_persons: Primary victim (powerless/identity_locked) — subject to assumptions about quality of life that the constraint reinforces while denying them exit
 *   - economically_vulnerable_patients: Primary victim (powerless/constrained) — face structural pressure toward death in resource-constrained systems while being denied legal pathways
 *   - palliative_care_clinicians: Secondary actor (organized/constrained) — provide genuine coordination function (suffering relief) but operate within constraint that limits their tools
 *   - legalization_advocates: Excluded (moderate/mobile) — would challenge constraint but are structurally excluded from policy-making in prohibition jurisdictions
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across jurisdictions and readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.72).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Primary Constraint on Death-with-Dignity").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'd20f4d70-45e8-422a-b0f7-12d4c325a851').
narrative_ontology:cs_kernel_codification('d20f4d70-45e8-422a-b0f7-12d4c325a851', fixed_text).
narrative_ontology:cs_authority_grounding('d20f4d70-45e8-422a-b0f7-12d4c325a851', lineage).
narrative_ontology:cs_interpretation_layer_present('d20f4d70-45e8-422a-b0f7-12d4c325a851').
narrative_ontology:cs_reading_relation('d20f4d70-45e8-422a-b0f7-12d4c325a851', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d20f4d70-45e8-422a-b0f7-12d4c325a851', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('d20f4d70-45e8-422a-b0f7-12d4c325a851', foundational, life_intrinsically_valuable).
narrative_ontology:cs_axiom_status(life_intrinsically_valuable, holdable).
narrative_ontology:cs_axiom_grounding('d20f4d70-45e8-422a-b0f7-12d4c325a851', life_intrinsically_valuable, deontological).
narrative_ontology:cs_axiom('d20f4d70-45e8-422a-b0f7-12d4c325a851', foundational, intentional_killing_always_wrong).
narrative_ontology:cs_axiom_status(intentional_killing_always_wrong, holdable).
narrative_ontology:cs_axiom_grounding('d20f4d70-45e8-422a-b0f7-12d4c325a851', intentional_killing_always_wrong, deontological).
narrative_ontology:cs_axiom('d20f4d70-45e8-422a-b0f7-12d4c325a851', secondary, consent_does_not_justify_killing).
narrative_ontology:cs_axiom_status(consent_does_not_justify_killing, holdable).
narrative_ontology:cs_axiom_grounding('d20f4d70-45e8-422a-b0f7-12d4c325a851', consent_does_not_justify_killing, deontological).
narrative_ontology:cs_reference_frame('d20f4d70-45e8-422a-b0f7-12d4c325a851', classical_medical_ethics_prohibition).
narrative_ontology:cs_drift_state('d20f4d70-45e8-422a-b0f7-12d4c325a851', contemporary_legalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d20f4d70-45e8-422a-b0f7-12d4c325a851', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_elderly).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_persons).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, economically_vulnerable_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_clinicians).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, palliative_care_clinicians).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, intrinsic_value_of_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious institutions, bioethics commissions, and legislative coalitions that derive doctrinal authority and political identity from maintaining the prohibition on intentional life-termination. Their institutional identity is fused with the constraint — abandoning it would dissolve the coherence of their moral framework. They collect status, mobilization capacity, and institutional relevance from the constraint's maintenance. Exit would require reconstituting their entire authority structure.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_community, beneficiary,
    institutional, generational, identity_locked, global).

% Individuals with terminal diagnoses who face prolonged dying processes the constraint forbids them from abbreviating. They cannot access assisted dying in prohibition jurisdictions; suicide is technically available but physically difficult and socially stigmatized; treatment refusal leads to potentially worse suffering; palliative sedation is inconsistently available. Their exit options are structurally blocked by the constraint's enforcement apparatus (criminal law, medical licensing).
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_elderly, payer,
    powerless, immediate, trapped, national).

% Persons with disabilities who face the constraint's dual operation: (1) the sanctity framework treats their lives as inherently valuable in a way that denies their autonomy to assess their own suffering, and (2) they are the population most vulnerable to coercion if legalization occurs, yet the constraint uses this vulnerability to justify denying everyone exit. Their identity is often constituted through the relational context the constraint reinforces (dependence, burden narratives), making exit from the constraint's logic identity-threatening.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_persons, payer,
    powerless, biographical, identity_locked, global).

% Patients in resource-constrained healthcare systems who face structural pressure toward cost-saving death (implicit rationing, family financial burden, institutional incentives) while being denied legal, regulated pathways for assisted dying. The constraint protects them from hypothetical coercion under legalization but does not address the actual coercion of poverty and system strain. Their exit is constrained by both the prohibition and the material conditions it ignores.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, economically_vulnerable_patients, payer,
    powerless, immediate, constrained, national).

% Clinicians who provide genuine suffering relief within the constraint's boundaries. They benefit from professional recognition, trust, and a clear scope of practice the constraint enables. They also pay a cost: they cannot offer assisted dying even when patients request it and palliative options are exhausted, creating moral distress and professional limitation. Their exit is constrained by licensing, institutional policy, and professional identity.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_clinicians, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, palliative_care_clinicians, payer).

% Advocates, scholars, and clinicians who argue for legalized assisted dying with safeguards. They are structurally excluded from policy-making in prohibition jurisdictions — their testimony is heard but not determinative, their evidence is contested on doctrinal grounds, and their proposed safeguards are treated as insufficient by the constraint's guardians. They can migrate to legalization jurisdictions (mobile exit) but cannot change the constraint from within.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legalization_advocates, excluded,
    moderate, biographical, mobile, global).

% The comparative bioethics / legal philosophy seat that observes the constraint's operation across jurisdictions and readings. Neither collects nor pays; sees the full structure including the kernel's three readings and their empirical differentials.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared moral boundary against intentional killing that coordinates social trust in medicine, protects the vulnerable from being treated as burdens, and preserves the heuristic that life is inviolable — a coordination problem about the symbolic and practical foundations of medical ethics and law.
% TRANSFER_FUNCTION: Moves autonomy, relief from suffering, and control over dying from vulnerable populations (terminally ill elderly, disabled persons, economically vulnerable patients) to the moral_order_community (religious institutions, bioethics establishments, legislative coalitions) in the form of doctrinal coherence, institutional authority, and political mobilization.
% ABSENT_VOICES: The terminally ill who would choose assisted dying if legal — they are dead or silenced by the constraint. The disabled persons who support legalization with safeguards — they are excluded by the constraint's claim to speak for their protection. The clinicians who would provide assisted dying — they are disciplined into silence. These voices are absent because the constraint's enforcement apparatus (criminal law, licensing, professional discipline) structurally excludes them from the policy conversation.
% DISAPPEARANCE_RATIONALE: If the sanctity prohibition vanished overnight, jurisdictions would rapidly enact legalized assisted dying frameworks (as seen in every peer jurisdiction that has debated this). Medical practice would incorporate assisted dying as a regulated option. Palliative care would expand as a complementary (not substitute) service. The moral_order_community would lose its primary bioethical mobilization target. Vulnerable populations would gain legal exit pathways but face new coercion risks requiring active safeguard enforcement. The world rearranges substantially.
% FOUNDING_PROBLEM: The prohibition was built to solve the problem of state-sanctioned killing (eugenics, involuntary euthanasia, medical abuse of vulnerable populations) by establishing an absolute boundary: medicine heals, never kills. This absolute boundary was the coordination solution to the horror of medicalized murder.
% FOUNDING_PROBLEM_CORROBORATION: The moral_order_community attests the founding problem remains live (citing ongoing risks of coercion, slippery slopes, and the symbolic importance of the healing/killing boundary). Legalization advocates and empirical researchers from legalized jurisdictions attest the founding problem is substantially addressed by safeguarded frameworks (citing Dutch, Belgian, Canadian, Oregon data showing no increase in non-voluntary euthanasia, stable palliative care, and effective oversight). No neutral corroborating source outside both camps has definitive authority — the dispute is structural.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects the constraint's net transfer: it imposes substantial suffering costs on vulnerable populations (prolonged dying, loss of autonomy, financial burden) while the claimed protection benefit (preventing coercion) is empirically contested and partially achieved by legalized regimes with safeguards. Suppression (0.72) is high because the constraint actively forecloses legal exit pathways — criminalization of assistance, licensing penalties for clinicians, institutional barriers to palliative sedation — and maintains these through state enforcement. Theater ratio (0.31) captures the growing gap between the constraint's proclaimed protection function and its actual operation: palliative care expansion is real but uneven, and the prohibition's enforcement increasingly targets marginal cases (compassionate assistance by family) rather than systemic coercion. Accessibility collapse (0.68) reflects that alternatives (suicide, treatment refusal, palliative sedation) exist but are structurally inadequate for many — the constraint collapses the space of dignified dying options without providing equivalent relief. Resistance (0.61) is substantial and growing: public opinion shifts toward legalization, professional bodies revise positions, and constitutional challenges mount.
 *
 * PERSPECTIVAL GAP:
 *   From the moral_order_community seat, the constraint is a rope (genuine coordination protecting the vulnerable, minimal coercive overhead). From the vulnerable population seats, it is a snare (pure extraction of autonomy and relief, enforced by criminal law). From the palliative clinician seat, it is a tangled_rope (coordination function real but extraction present via tool restriction). The engine computes this divergence from the structural data — the authored claim (snare) reflects the aggregate structural assessment from the analytical observer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The moral_order_community (religious institutions, bioethics commissions, legislative coalitions) sits at the beneficiary end: they derive institutional authority, doctrinal coherence, and political mobilization from maintaining the prohibition — their d is near 0.0 (subsidy). Vulnerable populations (terminally ill elderly, disabled persons, economically vulnerable patients) sit at the target end: they bear the suffering costs, loss of autonomy, and structural vulnerability the constraint imposes — their d is near 1.0. Palliative care clinicians sit near symmetric (d ~ 0.5): they gain professional scope and trust from providing care within the constraint, but lose tools (assisted dying) that some patients request. Legalization advocates are excluded (d not computed — they are not governed by the constraint's benefits). The analytical observer sits at d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate (protect life from intentional termination) remains formally intact, but its protective function has atrophied relative to its coercive prolongation of suffering. Legalized regimes demonstrate that protection against coercion can coexist with autonomy-respecting exit pathways — the empirical evidence undercuts the necessity claim. The constraint persists because the moral_order_community's institutional identity is fused with the prohibition (identity_locked maintenance), not because the protection function requires total prohibition. This is mandatrophy: the mandate has outlived its function but the constraint remains via institutional inertia and identity fusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested dignified_death kernel, and what would the sibling readings change structurally?',
    'Cross-reading structural comparison: the autonomy_primary reading centers self-determination as the dignity locus (victim = suffering individual denied control; beneficiary = individual autonomy), the relational_autonomy reading distributes authority across patient-family-clinician (victim = individual whose relational context is overridden; beneficiary = relational integrity). This sanctity_primary reading centers intrinsic life value (victim = vulnerable populations subject to coercion/pressure under legalization; beneficiary = moral order/community). The three readings instantiate different constraints with different ε, victim sets, and claimed types.',
    'Confirms this is a single ε-invariant constraint (this reading) rather than a blended account. The kernel structure routes committer content to omegas rather than standard fields per Rule 2.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee frame: this constraint is the sanctity_primary reading of the dignified_death kernel').

omega_variable(
    coercion_vs_protection_boundary,
    'Where does the protection norm against killing become coercive prolongation of suffering for vulnerable populations?',
    'Empirical study of jurisdictions with and without legalization: track rates of non-voluntary euthanasia, palliative sedation practices, and reported pressure on vulnerable groups. Compare outcomes for elderly, disabled, and poor patients under sanctity-only vs. legalized regimes.',
    'If protection function demonstrably prevents coercion without prolonging suffering, the constraint leans toward rope/tangled_rope. If it systematically prolongs suffering for vulnerable groups while failing to prevent covert coercion, snare classification is reinforced. This is the core structural ambiguity for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Whether the sanctity constraint''s protection function is real or cover for extraction').

omega_variable(
    moral_order_as_beneficiary,
    'Does ''moral order / community'' function as a genuine beneficiary that collects rents from the constraint, or is it a vindicated proposition wrongly categorized?',
    'Trace institutional beneficiaries: religious organizations with doctrinal authority, bioethics commissions with sanctity mandates, legislative coalitions that maintain prohibition. Determine whether these actors extract resources, status, or control from the constraint''s operation.',
    'If identifiable institutional actors benefit materially from maintaining the prohibition, the beneficiary declaration is structurally grounded. If ''moral order'' is only a doctrinal abstraction with no institutional capture, it belongs in vindicated_propositions only and the constraint may lack a concentrated beneficiary (piton candidate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_order_as_beneficiary, empirical, 'Whether moral_order_community is a real beneficiary actor or a doctrinal placeholder').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1990, dignified_death__sanctity_primary, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__sanctity_primary, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__sanctity_primary, theater_ratio, 2010, 0.26).
narrative_ontology:measurement(dign_tr_t2015, dignified_death__sanctity_primary, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(dign_tr_t2020, dignified_death__sanctity_primary, theater_ratio, 2020, 0.3).
narrative_ontology:measurement(dign_tr_t2025, dignified_death__sanctity_primary, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(dign_be_t1990, dignified_death__sanctity_primary, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(dign_be_t2000, dignified_death__sanctity_primary, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(dign_be_t2010, dignified_death__sanctity_primary, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(dign_be_t2015, dignified_death__sanctity_primary, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(dign_be_t2020, dignified_death__sanctity_primary, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(dign_be_t2025, dignified_death__sanctity_primary, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1990, dignified_death__sanctity_primary, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(dign_su_t2000, dignified_death__sanctity_primary, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(dign_su_t2010, dignified_death__sanctity_primary, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(dign_su_t2015, dignified_death__sanctity_primary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(dign_su_t2020, dignified_death__sanctity_primary, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(dign_su_t2025, dignified_death__sanctity_primary, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, palliative_care_access).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, medical_conscience_protection).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three constraint stories: sanctity_primary (this story, snare, ε≈0.58, victim=vulnerable populations), autonomy_primary (rope/tangled_rope, ε≈0.25, victim=suffering individual denied autonomy), relational_autonomy (scaffold/tangled_rope, ε≈0.35, victim=individual whose relational context is overridden). They share the same empirical referent (death-with-dignity policy space) but instantiate different constraints with different ε, victim sets, and coordination functions. This decomposition follows the ε-invariance principle: each reading is a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__sanctity_primary, institutional, 0.15).
constraint_indexing:directionality_override(dignified_death__sanctity_primary, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
