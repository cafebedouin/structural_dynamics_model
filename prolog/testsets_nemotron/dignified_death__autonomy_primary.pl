% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Right to Determined Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The autonomy-primary reading of dignified death asserts that a suffering
 *   individual's self-determination is the final authority over the timing
 *   and method of their death. This reading instantiates a constraint that
 *   transfers decision authority from the state and medical profession to the
 *   patient, but does so through a gatekept regime — eligibility criteria,
 *   waiting periods, medical assessments, and procedural safeguards. The
 *   constraint is structurally a tangled rope: it solves a genuine
 *   coordination problem (replacing uncontrolled suicide with a witnessed,
 *   regulated process) while simultaneously extracting from those denied exit
 *   by the very eligibility criteria that make the regime politically viable.
 *   The state prohibition apparatus and medical gatekeepers administer the
 *   constraint; the autonomous suffering individual is the named beneficiary;
 *   those who fall outside eligibility (the 'denied exit' cohort) are the
 *   victims. The constraint has evolved from near-total prohibition (high
 *   extraction, high suppression) toward liberalized regimes in some
 *   jurisdictions, but the gatekeeping structure persists and in some
 *   contexts is tightening — the measurement series shows a U-shaped
 *   extraction trajectory as new eligibility restrictions are added to
 *   expanding regimes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.7).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Right to Determined Death").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '88dfa7a2-0027-4e92-adad-83071a9ab96d').
narrative_ontology:cs_kernel_codification('88dfa7a2-0027-4e92-adad-83071a9ab96d', distributed).
narrative_ontology:cs_authority_grounding('88dfa7a2-0027-4e92-adad-83071a9ab96d', extraction).
narrative_ontology:cs_interpretation_layer_present('88dfa7a2-0027-4e92-adad-83071a9ab96d').
narrative_ontology:cs_reading_relation('88dfa7a2-0027-4e92-adad-83071a9ab96d', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_reading_relation('88dfa7a2-0027-4e92-adad-83071a9ab96d', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('88dfa7a2-0027-4e92-adad-83071a9ab96d', foundational, self_determination_supremacy_over_life).
narrative_ontology:cs_axiom_status(self_determination_supremacy_over_life, holdable).
narrative_ontology:cs_axiom_grounding('88dfa7a2-0027-4e92-adad-83071a9ab96d', self_determination_supremacy_over_life, deontological).
narrative_ontology:cs_axiom('88dfa7a2-0027-4e92-adad-83071a9ab96d', secondary, state_prohibition_of_assisted_death_is_illegitimate_coercion).
narrative_ontology:cs_axiom_status(state_prohibition_of_assisted_death_is_illegitimate_coercion, holdable).
narrative_ontology:cs_axiom_grounding('88dfa7a2-0027-4e92-adad-83071a9ab96d', state_prohibition_of_assisted_death_is_illegitimate_coercion, deontological).
narrative_ontology:cs_reference_frame('88dfa7a2-0027-4e92-adad-83071a9ab96d', autonomy_primary_legitimacy_baseline).
narrative_ontology:cs_drift_state('88dfa7a2-0027-4e92-adad-83071a9ab96d', contemporary_liberalized_regimes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88dfa7a2-0027-4e92-adad-83071a9ab96d', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_suffering_individual).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, individuals_denied_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_gatekeepers).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, bodily_self_ownership).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, autonomy_as_primary_moral_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person facing intolerable suffering who claims the right to determine the timing and method of their death. Their identity is fused with the autonomy claim — the right to self-determine is constitutive of their self-concept. Exit from the constraint means either exercising the right (death) or being prevented (prolonged suffering). They cannot 'leave' the situation without resolving the core claim.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_suffering_individual, beneficiary,
    moderate, biographical, identity_locked, national).

% People who seek a self-determined death but are legally or medically prevented — by prohibition, eligibility criteria, or institutional refusal. They bear the full cost of the constraint: prolonged suffering against their will, loss of control over their final days, and the psychological burden of knowing exit is denied. No meaningful exit options exist; the constraint is enforced by the state and medical gatekeepers.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, individuals_denied_exit, payer,
    powerless, immediate, trapped, national).

% The legal and regulatory framework (criminal law, medical licensing, health policy) that prohibits or restricts assisted death. It sets eligibility criteria, enforces prohibitions, and administers the gatekeeping process. The apparatus benefits from the constraint's persistence — it maintains the state's monopoly over life-ending decisions and avoids the institutional complexity of a regulated regime. It can modify the constraint (legislative reform) but faces high political costs.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_prohibition_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Physicians, ethics committees, and health systems that administer eligibility assessments and can authorize or refuse assisted death. They hold professional authority over the process and are structurally positioned as the constraint's operators. They benefit from professional control and moral clarity of the prohibitionist stance, but face role strain when patients request exit. Their exit from the constraint is constrained by professional identity, legal liability, and institutional policy.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_gatekeepers, beneficiary).

% Relatives and close relations of the suffering individual who would be affected by the death but have no formal decision authority under the autonomy-primary reading. They bear witness to the suffering and its prolongation, may carry moral injury from the outcome, but are structurally excluded from the authority relation the constraint centers. Their voices enter only informally or through surrogate decision-making when the patient loses capacity.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, family_and_intimates, excluded,
    moderate, biographical, constrained, local).

% Academic and clinical ethicists who analyze the constraint's normative structure, empirical effects, and institutional dynamics. They do not bear the constraint's costs or collect its benefits directly, but their discourse shapes the legitimacy conditions under which the constraint operates or is reformed.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a socially legitimate pathway for self-determined death that replaces chaotic, isolated, or violent self-harm with a regulated, witnessed, and documented process — solving the problem of uncontrolled suicide and its collateral harms to survivors and responders.
% TRANSFER_FUNCTION: Transfers the authority to determine death's timing and method from the state/prohibition apparatus to the autonomous suffering individual, while transferring the burden of gatekeeping, assessment, and procedural compliance to medical gatekeepers. The state retains the power to define eligibility but loses the absolute veto.
% ABSENT_VOICES: Future potential sufferers who cannot yet speak (the not-yet-ill, the young), whose interests in a world where self-determined death is normalized are not represented in current debate. Also absent: those who would choose life but fear becoming burdens if the autonomy norm becomes a social expectation — the 'right to die' becoming a 'duty to die' is a structural risk not voiced by current parties.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary constraint vanished overnight — i.e., absolute prohibition returned — individuals currently accessing assisted death would lose that option, suffering would be prolonged against will for some, and the regulatory apparatus would revert to blanket criminalization. The world rearranges: medical practice, palliative care investment, legal precedent, and public discourse all reorient around the prohibition baseline. Conversely, if the constraint were fully instantiated, the prohibition apparatus would be dismantled or radically transformed.
% FOUNDING_PROBLEM: The historical problem of uncontrolled, isolated, often violent suicide among the terminally and chronically suffering — and the concomitant problem of medical paternalism overriding competent patients' explicit wishes to die. The autonomy-primary reading was built to solve both: replace chaotic death with ordered self-determination, and replace medical override with patient authority.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (autonomous suffering individuals, right-to-die organizations, some bioethicists) attest the founding problem remains live — suffering individuals are still denied exit, and medical paternalism persists. Opponents (sanctity-primary advocates, some medical associations, religious bodies) attest the founding problem is substantially addressed by modern palliative care and that the autonomy reading creates new problems (vulnerability, slippery slope). Legislative testimony, court records, and empirical studies from outside the beneficiary set document both positions.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) reflects the constraint's dual character: it enables exit for some (low extraction for the eligible) while actively denying it to others (high extraction for the ineligible). The aggregate is driven by the denied-exit cohort's size and the severity of their prolonged suffering. Suppression (0.7) is high because the constraint's persistence depends on active enforcement — criminal sanctions, license revocation, institutional policies that prevent even willing clinicians from acting. Theater ratio (0.25) captures the gap between the regime's stated purpose (protecting the vulnerable) and its operational reality (the safeguards themselves function as barriers that extract time, dignity, and autonomy from the very people they claim to protect). Accessibility collapse (0.3) is moderate — alternatives (travel to permissive jurisdictions, voluntary stopping of eating and drinking, unassisted suicide) exist but are costly, dangerous, or incomplete. Resistance (0.65) is substantial: the denied-exit cohort resists through litigation, civil disobedience, and political advocacy; medical gatekeepers resist through conscientious objection and procedural foot-dragging.
 *
 * PERSPECTIVAL GAP:
 *   From the autonomous individual's seat, the constraint is a rope — it coordinates their exit with medical and legal recognition, solving the problem of dying alone and in legal jeopardy. From the denied-exit individual's seat, it is a snare — the coordination story (safeguards protect the vulnerable) is cover for extraction (their suffering is prolonged to maintain the regime's political viability). From the state apparatus seat, it is a scaffold — the regime is transitional, meant to manage the shift from prohibition to regulated access, but the transition has stalled. From the medical gatekeeper seat, it is a piton — the assessment rituals persist theatrically while the substantive function (preventing 'wrongful' deaths) has atrophied in jurisdictions where eligibility has expanded. The engine computes these per-seat types from the structural data; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The autonomous suffering individual is the structural beneficiary (d low) — the constraint exists to serve their claim, and when it works, they collect the full benefit. But their exit is identity-locked: the autonomy claim is constitutive of their self-concept in this situation; they cannot 'choose' to not care about the constraint without abandoning the self-understanding that makes them the beneficiary. Individuals denied exit are full targets (d near 1.0) — they bear the constraint's extraction with no offsetting benefit, and their exit options are trapped (legal prohibition, medical refusal, physical incapacity). The state prohibition apparatus sits near the beneficiary end (d low) despite its agenda-setter role: it collects institutional simplicity and moral clarity from the prohibition, and its exit (legislative reform) is arbitrage-grade — it can change the law but chooses not to because the political cost exceeds the benefit. Medical gatekeepers are structurally ambiguous: they administer the constraint (agenda-setter) but also benefit from professional control (beneficiary), while facing role strain and liability risk (payer-like costs). Their exit is constrained — they can leave the profession or jurisdiction, but at high professional identity cost. Family and intimates are excluded: they bear witness costs but have no authority. Bioethics scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (uncontrolled suicide + medical paternalism) is contested as live or substantially solved. If the founding problem is dead — modern palliative care addresses suffering, and medical paternalism has been displaced by informed consent — then the constraint persists as mandatrophy: the autonomy norm has outlived the problem it was built to solve, but the gatekeeping machinery remains because no actor bears enough cost to dismantle it and the state benefits from the status quo. If the founding problem is live (denied exit persists, palliative care has limits), then the constraint remains functionally justified but extractively implemented — a tangled rope where the coordination function is real but the eligibility criteria extract from those they exclude. The classification prevents mislabeling: calling it a pure snare would miss the genuine coordination for the eligible; calling it a pure rope would miss the asymmetric extraction on the denied. The tangled rope classification captures both.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Are the eligibility criteria (terminal illness prognosis, capacity assessment, waiting periods) structurally necessary for the coordination function (safe, witnessed, documented death), or do they function as extraction mechanisms that deny exit to the ineligible while legitimizing the regime?',
    'Compare outcomes in jurisdictions with minimal criteria (e.g., Netherlands'' ''unbearable suffering'' standard) vs. restrictive criteria (e.g., US states'' 6-month prognosis requirement). If coordination outcomes (safety, lack of abuse) hold under minimal criteria, the restrictive criteria are extractive barriers, not coordination necessities.',
    'If criteria are extractive, the constraint''s ε is higher than the coordination function alone would justify, and the denied-exit cohort is a structural victim set, not a safety residual. If criteria are necessary, the extraction is the price of coordination and the victim set is an unavoidable tragedy, not a structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the gatekeeping structure is coordination infrastructure or extraction machinery.').

omega_variable(
    internalized_suppression_ambiguity,
    'For individuals denied exit, is the suppression they experience primarily structural (legal prohibition, medical refusal) or partially internalized (absorbing the message that their suffering is not ''qualifying,'' that their desire to die is a symptom of illness rather than a rational choice)?',
    'Longitudinal qualitative study of denied-exit individuals: track whether the experience of denial changes their self-understanding of their suffering and their right to determine its end. Compare with those who access the regime — does the assessment process itself reshape the autonomy claim?',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — the denied carry the constraint with them after the formal denial. This would increase ε for the denied-exit seat and strengthen the snare-like character from that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the denied-exit cohort.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (autonomy_primary) of the contested kernel ''dignified_death''. The sibling readings are sanctity_primary and relational_autonomy. Where exactly is the structural disagreement located — in the victim set, the beneficiary structure, the coordination function, or the authority grounding?',
    'Structural decomposition of each reading''s constraint story: compare beneficiary/victim declarations, coordination_function, transfer_function, and authority_grounding across the three readings. The disagreement locus is the element that differs most fundamentally.',
    'If the disagreement is in victim set (who counts as harmed), the kernel contest is about the scope of moral concern. If in authority grounding (who decides), it is about the locus of legitimacy. If in coordination function (what problem is solved), it is about the empirical world the constraint responds to. Different loci imply different resolution pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: this reading''s structural position within the dignified_death kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1990, dignified_death__autonomy_primary, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__autonomy_primary, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(dign_tr_t2010, dignified_death__autonomy_primary, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(dign_tr_t2015, dignified_death__autonomy_primary, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(dign_tr_t2020, dignified_death__autonomy_primary, theater_ratio, 2020, 0.25).
narrative_ontology:measurement(dign_tr_t2025, dignified_death__autonomy_primary, theater_ratio, 2025, 0.26).
narrative_ontology:measurement(dign_tr_t2030, dignified_death__autonomy_primary, theater_ratio, 2030, 0.25).

% Extraction over time
narrative_ontology:measurement(dign_be_t1990, dignified_death__autonomy_primary, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(dign_be_t2000, dignified_death__autonomy_primary, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(dign_be_t2010, dignified_death__autonomy_primary, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(dign_be_t2015, dignified_death__autonomy_primary, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(dign_be_t2020, dignified_death__autonomy_primary, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(dign_be_t2025, dignified_death__autonomy_primary, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement(dign_be_t2030, dignified_death__autonomy_primary, base_extractiveness, 2030, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1990, dignified_death__autonomy_primary, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(dign_su_t2000, dignified_death__autonomy_primary, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(dign_su_t2010, dignified_death__autonomy_primary, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(dign_su_t2015, dignified_death__autonomy_primary, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(dign_su_t2020, dignified_death__autonomy_primary, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(dign_su_t2025, dignified_death__autonomy_primary, suppression_requirement, 2025, 0.69).
narrative_ontology:measurement(dign_su_t2030, dignified_death__autonomy_primary, suppression_requirement, 2030, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, palliative_care_access_regime).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, mental_health_involuntary_commitment).

% DUAL FORMULATION NOTE:
% This story is one member of the dignified_death constraint family (kernel_id: dignified_death). The three readings instantiate three distinct constraints with different ε values, beneficiary/victim structures, and claimed types. autonomy_primary = tangled_rope (ε~0.55, gatekept autonomy). sanctity_primary = snare (ε~0.8, absolute prohibition extracts from all sufferers). relational_autonomy = scaffold (ε~0.35, transitional triad-based process with sunset toward full autonomy). They are linked by network.affects_constraints and share the kernel_context declaration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__autonomy_primary, institutional, 0.2).
constraint_indexing:directionality_override(dignified_death__autonomy_primary, powerless, 0.95).
constraint_indexing:directionality_override(dignified_death__autonomy_primary, moderate, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
