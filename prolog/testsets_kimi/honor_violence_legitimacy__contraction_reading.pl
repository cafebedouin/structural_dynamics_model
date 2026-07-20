% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor-Violence Legitimacy: Contraction Reading
 *   domain: historical_sociology_legal_anthropology
 *
 * SUMMARY:
 *   In early modern Europe, the concept of honor underwent a structural
 *   redefinition that expelled lethal violence from its core. Where honor had
 *   previously licensed and sometimes demanded violent response to insult,
 *   the contraction reading holds that honor was reconceptualized around
 *   civil, professional, and moral virtues. Dueling did not merely become
 *   illegal or costly; it became structurally unthinkable as a legitimate
 *   honor response. This constraint story models the new honor code as a
 *   commitment system kernel reading: the kernel 'honor' was read through
 *   contraction, making violence cognitively inaccessible within the honor
 *   frame. The reading competes with the 'drop' reading (external costs alone
 *   caused decline) and the 'composite' reading (both mechanisms operated).
 *
 * KEY AGENTS:
 *   - commercial_bourgeoisie (moderate/constrained): Primary beneficiary â gains honorable status without martial prowess.
 *   - centralizing_state (institutional/arbitrage): Agenda-setter â enforces legal prohibition and claims violence monopoly.
 *   - military_aristocracy (powerful/identity_locked): Primary target â identity fused to old honor-violence nexus, bears cultural dispossession.
 *   - provincial_gentry (moderate/constrained): Secondary target â lacks capital to excel in new honor economy.
 *   - salon_intellectuals (moderate/mobile): Beneficiary â produces and arbitrates new honor discourse.
 *   - legal_apparatus (institutional/analytical): Agenda-setter â codifies and adjudicates the new non-violent order.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.48).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.52).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor-Violence Legitimacy: Contraction Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology_legal_anthropology").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc').
narrative_ontology:cs_kernel_codification('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', distributed).
narrative_ontology:cs_authority_grounding('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', practice).
narrative_ontology:cs_interpretation_layer_present('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc').
narrative_ontology:cs_reading_relation('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', honor_violence_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', foundational, honor_excludes_violence).
narrative_ontology:cs_axiom_status(honor_excludes_violence, holdable).
narrative_ontology:cs_axiom_grounding('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', honor_excludes_violence, conventional).
narrative_ontology:cs_axiom('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', foundational, civil_virtue_supersedes_martial_prowess).
narrative_ontology:cs_axiom_status(civil_virtue_supersedes_martial_prowess, holdable).
narrative_ontology:cs_axiom_grounding('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', civil_virtue_supersedes_martial_prowess, conventional).
narrative_ontology:cs_reference_frame('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', martial_honor_legitimacy).
narrative_ontology:cs_drift_state('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', post_enlightenment_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('22d977fa-f6aa-4e2a-bb6a-5a9b1e37f9fc', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, centralizing_state).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, salon_intellectuals).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, military_aristocracy).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, provincial_gentry).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, bourgeois_cultural_hegemony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains honorable status through commercial integrity and professional competence rather than martial prowess; the redefinition legitimizes their economic activity as compatible with honor, while previously such activity was considered degrading to a gentleman.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, commercial_bourgeoisie, beneficiary,
    moderate, generational, constrained, national).

% Promotes legal prohibitions on dueling and claims monopoly on legitimate violence; benefits from reduced private lethal conflict that challenged judicial authority and state sovereignty.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, centralizing_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Produce and disseminate the new honor discourse; gain status as arbiters of civilized conduct, though they do not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, salon_intellectuals, beneficiary,
    moderate, biographical, mobile, national).

% Bears the cost of the honor redefinition as their traditional martial valor becomes culturally devalued; their identity is fused to the old honor-violence nexus, making exit psychologically and socially costly even as dueling is legally prohibited and socially ridiculed.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, military_aristocracy, payer,
    powerful, generational, identity_locked, national).

% Caught between old and new codes; lacking the cultural capital to excel in the new professional honor economy, they face declining social standing as provincial martial traditions are stigmatized.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, provincial_gentry, payer,
    moderate, biographical, constrained, regional).

% Codifies anti-dueling legislation and adjudicates honor disputes through legal rather than martial means; maintains the enforcement infrastructure that gives teeth to the conceptual redefinition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of lethal honor disputes by removing violence from the legitimate response set, enabling commercial and professional society to function without the threat of death over insults or commercial rivalry.
% TRANSFER_FUNCTION: Moves social standing and cultural legitimacy from martial elites to professional and commercial classes by redefining the criteria of honorable conduct from martial prowess to civic and moral virtue.
% ABSENT_VOICES: Provincial duelists, traditional martial educators, and the seconds and surgeons who served the dueling economy are excluded from the salons and state commissions that redefined honor; their objections survive only in private correspondence, local resistance, and underground dueling cultures.
% DISAPPEARANCE_RATIONALE: If the exclusion of violence from honor vanished, dueling would re-enter the legitimate response set, the state's monopoly on violence would be challenged by private lethal dispute resolution, and the social architecture of professional and commercial status would destabilize as martial prowess regained honor-value and the bourgeoisie lost its cultural footing.
% FOUNDING_PROBLEM: Private lethal honor disputes created cycles of violence that destabilized early modern states, consumed elite male populations, and endangered the emerging commercial-professional classes who lacked martial training and could not safely participate in honor culture.
% FOUNDING_PROBLEM_CORROBORATION: State archives document anti-dueling legislation motives; however, the 'cycles of violence' narrative is primarily attested by the centralizing state and bourgeois commentators themselves. Military aristocratic correspondence contests the severity of the problem, suggesting the founding narrative served state-building and class interests. Independent ecclesiastical records from outside the benefiting parties provide mixed corroboration, condemning both dueling and the bourgeois materialism that supplanted it.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High accessibility_collapse (0.88) reflects the 'structurally unthinkable' quality central to this reading: once the redefinition is accepted, dueling is not merely prohibited but conceptually unavailable as an honor response. Moderate extractiveness (0.48) because the same redefinition that coordinates professional society strips martial elites of cultural capital. Suppression (0.52) is moderate: active enforcement (legal prohibition, social ostracism, salon ridicule) was necessary during the transition, though successful internalization later reduced the raw enforcement burden. Theater ratio (0.25) is low because the coordination function (reduced lethal violence, stable commercial society) remains genuine and is not performative maintenance. Resistance (0.42) comes from aristocratic holdouts and provincial traditions. The measurement series tracks the transition: extraction rises as the new code consolidates, theater slowly rises as enforcement ritualizes, and suppression follows an inverse-U pattern â peaking during the transition phase when active enforcement was most needed, then declining as the norm became internalized.
 *
 * PERSPECTIVAL GAP:
 *   The state and commercial bourgeoisie experience the constraint as genuine coordination that civilizes society and enables commerce without the shadow of the sword. The military aristocracy experiences the identical structure as cultural expropriation â their habitus, encoded in the old honor-violence nexus, is rendered illegitimate and ridiculous. The engine computes this divergence from the structural data: identical constraint, opposed beneficiary/victim declarations, and identity-locked exit for the target seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The commercial bourgeoisie, centralizing state, and salon intellectuals are structural beneficiaries of the new honor code (low d): they gain legitimate status, monopoly on violence, and discursive authority respectively. The military aristocracy and provincial gentry are targets (high d): their identity is locked to the old code, so the new constraint extracts by devaluing their core cultural capital. The legal apparatus sits near symmetric: it enforces but does not personally collect the cultural gains. The divergence between the state/bourgeois seat and the aristocratic seat is the central structural asymmetry: the same constraint reads as civilization to one and dispossession to the other.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (Snare) by preserving the genuine coordination function: reduced lethal violence is a real public good that solved a real collective-action problem. It prevents mislabeling as pure coordination (Rope) by acknowledging the asymmetric cost borne by identity-locked martial elites whose cultural capital was devalued through the same structural move that coordinated the bourgeoisie. If the constraint were a Snare, the violence-reduction narrative would be cover; if it were a Rope, the aristocratic cost would be negligible or symmetric. Neither is descriptively true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locked_suppression,
    'Is the aristocratic adherence to dueling a free choice or an identity-lock that makes exit impossible without psychological dissolution?',
    'Historical biographical analysis: compare aristocrats who successfully adapted to the new honor code versus those who continued dueling despite legal penalties, measuring the correlation between identity-fusion metrics and behavioral persistence.',
    'If identity-locked, effective extraction is higher than the structural measure suggests â the constraint extracts not just status but selfhood, and the aristocratic seat sits nearer the full-target end of the directionality spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression, empirical, 'Whether aristocratic resistance represents choice or identity fusion.').

omega_variable(
    coordination_extraction_separability,
    'Could the violence-reduction benefit have been achieved without the class-cultural dispossession of martial elites?',
    'Comparative historical analysis of societies where state monopoly on violence was achieved without redefining honor to exclude violence from the virtue set (e.g., certain East Asian or Scandinavian cases).',
    'If yes, the extraction is separable from the coordination and the constraint is more extractive than necessary; if no, the cultural redefinition was structurally necessary for the coordination outcome.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in this case.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social ostracism) or internalized (the unthinkability itself)?',
    'Post-decriminalization suppression trajectory: in jurisdictions that decriminalized dueling, did rates remain low due to internalized norms, or did covert dueling resurface once legal penalties were removed?',
    'If internalized, the constraint''s effective suppression exceeds the structural measure â the target carries the suppression internally after external enforcement eases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_contr_tr_t0, honor_violence_legitimacy__contraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(honor_contr_tr_t10, honor_violence_legitimacy__contraction_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(honor_contr_tr_t20, honor_violence_legitimacy__contraction_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(honor_contr_tr_t30, honor_violence_legitimacy__contraction_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(honor_contr_tr_t40, honor_violence_legitimacy__contraction_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(honor_contr_tr_t50, honor_violence_legitimacy__contraction_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(honor_contr_be_t0, honor_violence_legitimacy__contraction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(honor_contr_be_t10, honor_violence_legitimacy__contraction_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(honor_contr_be_t20, honor_violence_legitimacy__contraction_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(honor_contr_be_t30, honor_violence_legitimacy__contraction_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(honor_contr_be_t40, honor_violence_legitimacy__contraction_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(honor_contr_be_t50, honor_violence_legitimacy__contraction_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(honor_contr_su_t0, honor_violence_legitimacy__contraction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(honor_contr_su_t10, honor_violence_legitimacy__contraction_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(honor_contr_su_t20, honor_violence_legitimacy__contraction_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(honor_contr_su_t30, honor_violence_legitimacy__contraction_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(honor_contr_su_t40, honor_violence_legitimacy__contraction_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(honor_contr_su_t50, honor_violence_legitimacy__contraction_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one component of the honor_violence_legitimacy kernel family. The contraction reading isolates the conceptual-redefinition mechanism, while the drop reading isolates the external-cost mechanism, and the composite reading combines them. Each carries a distinct epsilon and classification per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
