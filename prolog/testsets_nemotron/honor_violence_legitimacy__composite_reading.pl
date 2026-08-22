% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor Violence Legitimacy (Composite: External Costs + Conceptual Contraction)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the decline of dueling and honor violence
 *   legitimacy in Western societies (c. 1780–1900) as a composite reading of
 *   the contested kernel 'honor_violence_legitimacy'. The composite reading
 *   asserts that two mechanisms operated simultaneously: (1) a 'drop'
 *   mechanism — external costs imposed by state legal monopoly
 *   (criminalization, professional consequences, social ostracization) making
 *   honor violence practically costly; and (2) a 'contraction' mechanism —
 *   conceptual redefinition of honor itself to exclude violence, driven by
 *   bourgeois moral reform, professionalization of military/legal elites, and
 *   the rise of legal formalism. These mechanisms had different victim sets:
 *   the drop mechanism primarily extracted from aristocratic officers and
 *   traditional gentry who continued dueling; the contraction mechanism
 *   extracted from honor culture adherents more broadly (including women who
 *   enforced honor norms domestically) by redefining what counted as
 *   honorable. The contraction edge made the drop mechanism insufficient
 *   alone — even where legal penalties were weak, the conceptual shift
 *   delegitimized the practice from within. This reading is ONE instantiation
 *   of the kernel; sibling readings (drop_reading, contraction_reading)
 *   isolate each mechanism as the primary driver.
 *
 * KEY AGENTS:
 *   - state_legal_monopoly: Primary agenda_setter (institutional/generational/arbitrage/global) — imposes criminal penalties, controls professional licensing, defines legitimate violence
 *   - emerging_professional_classes: Primary beneficiary (organized/biographical/arbitrage/national) — lawyers, bureaucrats, officers who gained status from honor's redefinition as professional competence
 *   - bourgeois_moral_reformers: Secondary beneficiary (organized/generational/mobile/continental) — campaigned for honor's redefinition as civic virtue, gained cultural authority
 *   - aristocratic_officer_class: Primary victim (powerful/biographical/constrained/national) — lost duel-based honor system that structured military advancement and social standing
 *   - traditional_gentry_dueling_participants: Primary victim (moderate/biographical/trapped/regional) — faced criminalization without alternative status pathways
 *   - honor_culture_adherents: Secondary victim (organized/generational/identity_locked/continental) — internalized the redefinition; honor became incompatible with violence by cultural osmosis
 *   - women_in_honor_enforcement_roles: Secondary victim (powerless/generational/identity_locked/local) — lost domestic authority as honor enforcers when honor was redefined to exclude violence
 *   - competition_authorities: Observer (institutional/generational/analytical/national) — not directly present historically; analytical seat for modern classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.78).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.62).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor Violence Legitimacy (Composite: External Costs + Conceptual Contraction)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'd0575c6a-b8b0-4fb9-be02-95b539d7c624').
narrative_ontology:cs_kernel_codification('d0575c6a-b8b0-4fb9-be02-95b539d7c624', distributed).
narrative_ontology:cs_authority_grounding('d0575c6a-b8b0-4fb9-be02-95b539d7c624', practice).
narrative_ontology:cs_interpretation_layer_present('d0575c6a-b8b0-4fb9-be02-95b539d7c624').
narrative_ontology:cs_reading_relation('d0575c6a-b8b0-4fb9-be02-95b539d7c624', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('d0575c6a-b8b0-4fb9-be02-95b539d7c624', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('d0575c6a-b8b0-4fb9-be02-95b539d7c624', foundational, honor_violence_decline_overdetermined).
narrative_ontology:cs_axiom_status(honor_violence_decline_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('d0575c6a-b8b0-4fb9-be02-95b539d7c624', honor_violence_decline_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('d0575c6a-b8b0-4fb9-be02-95b539d7c624', foundational, contraction_mechanism_necessary_for_drop_efficacy).
narrative_ontology:cs_axiom_status(contraction_mechanism_necessary_for_drop_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('d0575c6a-b8b0-4fb9-be02-95b539d7c624', contraction_mechanism_necessary_for_drop_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('d0575c6a-b8b0-4fb9-be02-95b539d7c624', aristocratic_honor_violence_order).
narrative_ontology:cs_drift_state('d0575c6a-b8b0-4fb9-be02-95b539d7c624', bourgeois_legal_order_consolidated, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d0575c6a-b8b0-4fb9-be02-95b539d7c624', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, emerging_professional_classes).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, bourgeois_moral_reformers).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, aristocratic_officer_class).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, traditional_gentry_dueling_participants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_in_honor_enforcement_roles).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, civilian_supremacy_over_military_honor).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, legal_formalism_as_civilizing_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposes criminal penalties on dueling, controls military/legal professional licensing, defines legitimate violence through state courts and police. Gains monopoly on violence adjudication and extraction via legal fees, state legitimacy, and bureaucratic control. Exit is arbitrary — the state IS the constraint's enforcer.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_monopoly, agenda_setter,
    institutional, generational, arbitrage, global).

% Lawyers, bureaucrats, medical officers, engineers gain status and gatekeeping authority as honor is redefined from 'personal violence readiness' to 'professional competence and civic reliability'. They collect rents from professional licensing and state employment. Exit is high — they can move across professions or nations — but they benefit from the constraint's maintenance.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, emerging_professional_classes, beneficiary,
    organized, biographical, arbitrage, national).

% Middle-class reformers (temperance, anti-dueling societies, legal reformers) campaign to redefine honor as civic virtue, self-control, and legal compliance. Gain cultural authority, political influence, and alignment with state power. Exit is mobile — they could retreat to private life — but they are ideologically invested in the redefinition.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, bourgeois_moral_reformers, beneficiary,
    organized, generational, mobile, continental).

% Aristocratic military officers for whom dueling was a career requirement and status maintenance mechanism. Face criminal prosecution, cashiering, social ostracization if they duel; loss of honor-status if they don't. Constrained exit: can emigrate (some did to Americas), submit to new professional standards, or resist and be destroyed. No arbitrage — their capital (lineage, honor capital) is non-transferable.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_officer_class, payer,
    powerful, biographical, constrained, national).

% Rural/semi-rural gentry for whom dueling was local dispute resolution and status signaling. Lack professional alternatives, political connections, or cultural capital to navigate new system. Trapped: legal penalties reach them, but they have no pathway into the new professional/bourgeois order. Their honor culture is their only social currency, and it is being demonetized.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, traditional_gentry_dueling_participants, payer,
    moderate, biographical, trapped, regional).

% Broader populations (including urban artisans, student corps, regional elites) who internalized honor-violence norms. The contraction mechanism redefines honor so that violence becomes dishonorable — they experience this as moral awakening, not coercion. Identity-locked: their self-concept is constituted through the honor framework; they cannot 'exit' the redefinition because it rewrites their identity from within. They pay by losing the coherence of their moral world.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_culture_adherents, payer,
    organized, generational, identity_locked, continental).

% Women (mothers, sisters, wives) who enforced honor norms domestically — policing male relatives' reputation, managing shame/honor dynamics, arranging marriages to restore honor. When honor is redefined to exclude violence, their domestic authority evaporates: no new role replaces 'honor enforcer'. Identity-locked in the domestic sphere; trapped in gender role that the constraint hollows out without replacing. They bear diffuse costs (loss of authority, increased vulnerability) with zero extraction capture.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_in_honor_enforcement_roles, payer,
    powerless, generational, identity_locked, local).

% Analytical seat observing the composite mechanism from outside the historical moment. Sees both drop and contraction mechanisms, their coupling, and their distinct victim sets. No material stake; exit is analytical (can change frameworks). Provides the classification that the historical actors could not.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes state monopoly on legitimate violence and professional standardization by replacing personal honor violence with legal adjudication and meritocratic gatekeeping — a genuine coordination problem (ending private vengeance cycles, creating predictable professional hierarchies).
% TRANSFER_FUNCTION: Moves status, authority, material rewards, and moral legitimacy from aristocratic/gentry honor culture (dueling participants, honor enforcers) to state legal monopoly and emerging professional classes (lawyers, bureaucrats, officers, reformers). The drop mechanism transfers via legal penalties; the contraction mechanism transfers via conceptual redefinition of what counts as honorable.
% ABSENT_VOICES: Colonial subjects and non-Western honor cultures subjected to the same conceptual redefinition via imperial imposition — they would object to the universalization of the 'honor without violence' model but were structurally excluded from the metropolitan reform conversation. Also: women in honor enforcement roles (partially represented in stakeholders) — their specific loss of domestic authority was rarely articulated in reform debates.
% DISAPPEARANCE_RATIONALE: If the composite constraint vanished overnight (both legal penalties and conceptual redefinition reversed), dueling and honor violence would not simply return — the material conditions (professional armies, state courts, bourgeois morality) are gone. But the world would rearrange: new forms of status violence, reputation markets, and extra-legal dispute resolution would emerge to fill the coordination vacuum. The constraint's beneficiaries (state, professions) would lose their monopoly; its victims (honor culture adherents) would not regain their lost world.
% FOUNDING_PROBLEM: Early modern states faced endemic private violence (dueling, feuding, vendetta) that undermined state monopoly on violence, professional military effectiveness, and commercial reliability. The honor system provided a decentralized coordination mechanism for violence limitation, but it was aristocratic, unpredictable, and incompatible with bureaucratic statehood.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (private violence undermining state monopoly) is dead — modern states have effective monopoly on violence, professional militaries don't duel, commercial disputes use courts. Corroboration from outside beneficiary set: military historians (van Creveld, Keegan) document the professionalization of officer corps as solving the dueling problem; legal historians (Foucault, Elias) document the civilizing process as endogenous to state formation, not a reformer project. No living participant in the honor culture attests the problem is live — the culture is gone.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transferred status, authority, and material rewards from honor-culture participants to state and professional elites — but not total (hence not snare) because the coordination function (state monopoly on violence, professional standardization) was genuine and benefited society broadly. Suppression (0.62) is moderate: legal penalties were real but uneven; the deeper suppression was conceptual — the contraction mechanism made honor violence unthinkable, not just illegal. Theater ratio (0.41) is significant: by 1900, honor codes persisted as ceremonial forms (military academies, student fraternities) while the violence they once regulated was gone — the constraint's coordination function atrophied, leaving performative maintenance. Accessibility collapse (0.58) and resistance (0.34) reflect a constraint that met organized resistance early (aristocratic officers) but saw resistance collapse as the contraction mechanism took hold — alternatives didn't just disappear; they were conceptually erased.
 *
 * PERSPECTIVAL GAP:
 *   The state/professional beneficiary seats experience this as rope/scaffold (genuine coordination, transitional justification). The aristocratic officer and gentry victim seats experience it as snare (targeted extraction, identity destruction). The honor culture adherent seat experiences it as mountain-like inevitability (conceptual redefinition feels like natural law). The women's seat experiences it as piton (loss of domestic authority without replacement, theatrical maintenance of 'honor' discourse). The engine computes this divergence from the structural data — the composite reading's claim of tangled_rope captures the hybrid reality but no single seat sees it that way.
 *
 * DIRECTIONALITY LOGIC:
 *   State legal monopoly and professional classes are structural beneficiaries (d ~ 0.15): they gained monopoly control, professional gatekeeping, and status from the constraint's operation. Bourgeois reformers are moderate beneficiaries (d ~ 0.3): cultural authority gained, but less material extraction. Aristocratic officers and gentry are primary targets (d ~ 0.85): they bore criminal penalties, career destruction, and status loss with constrained exit (emigration or submission). Honor culture adherents are identity-locked targets (d ~ 0.9): the contraction mechanism fused their identity with the redefined honor concept, making exit from the constraint psychologically impossible. Women in enforcement roles are trapped (d ~ 0.95): no exit from the domestic sphere where honor was enforced, no voice in its redefinition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing state monopoly on violence and professional standardization) is live — modern states still require these. But the constraint's specific form (criminalizing dueling, redefining honor) is dead as a founding solution: dueling is gone, honor is redefined. The arrangement persists as mandate without function — the legal prohibitions remain, the conceptual redefinition is complete, but the extraction machinery (professional gatekeeping, status allocation) continues. This is mandatrophy: the mandate (state monopoly, professional standards) has outlived the specific constraint form. The composite reading captures this by showing both mechanisms — drop (legal) and contraction (conceptual) — where contraction is the mandatrophic edge: it solved the coordination problem by eliminating the very concept that needed coordinating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the composite reading a single constraint with two mechanisms, or a constraint family of two linked constraints (drop + contraction) that should be modeled separately?',
    'Test whether the drop and contraction mechanisms have distinct victim sets, distinct enforcement logics, and distinct temporal profiles. If they share the same enforcement infrastructure and victim set, composite is appropriate; if distinct, decompose per ε-invariance.',
    'If decomposable, two constraints with different ε values and classifications; if composite, one constraint with compound extraction profile. The engine''s network coupling analysis will detect either structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the composite reading instantiates one constraint or a constraint family').

omega_variable(
    extraction_attribution_ambiguity,
    'How much of the measured extraction is attributable to the drop mechanism (external costs) versus the contraction mechanism (conceptual redefinition)?',
    'Comparative historical analysis of jurisdictions where only one mechanism operated (e.g., early abolition of dueling via legal penalty vs. cultural shift without legal change). Disaggregate ε into ε_drop and ε_contraction.',
    'If drop mechanism dominates, the constraint is primarily state enforcement extracting from honor culture; if contraction dominates, it''s primarily conceptual displacement. Changes the primary beneficiary and victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_attribution_ambiguity, empirical, 'Attribution of extraction between external cost imposition and conceptual redefinition').

omega_variable(
    contingency_of_contraction,
    'Was the conceptual redefinition of honor (contraction) a necessary consequence of modernization, or a contingent outcome of specific reform movements?',
    'Cross-national comparison of honor concept trajectories: did societies with similar modernization paths but different reform movements exhibit different contraction patterns?',
    'If necessary, contraction reads as Mountain-like inevitability (low ε for that component); if contingent, it reads as engineered displacement (high ε). Affects the composite''s overall classification and mandatrophy status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingency_of_contraction, conceptual, 'Whether honor''s conceptual contraction was structurally necessary or politically engineered').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 1780, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1780, honor_violence_legitimacy__composite_reading, theater_ratio, 1780, 0.12).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1800, honor_violence_legitimacy__composite_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1820, honor_violence_legitimacy__composite_reading, theater_ratio, 1820, 0.24).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1840, honor_violence_legitimacy__composite_reading, theater_ratio, 1840, 0.31).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1860, honor_violence_legitimacy__composite_reading, theater_ratio, 1860, 0.36).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1880, honor_violence_legitimacy__composite_reading, theater_ratio, 1880, 0.39).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_tr_t1900, honor_violence_legitimacy__composite_reading, theater_ratio, 1900, 0.41).

% Extraction over time
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1780, honor_violence_legitimacy__composite_reading, base_extractiveness, 1780, 0.35).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1800, honor_violence_legitimacy__composite_reading, base_extractiveness, 1800, 0.42).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1820, honor_violence_legitimacy__composite_reading, base_extractiveness, 1820, 0.51).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1840, honor_violence_legitimacy__composite_reading, base_extractiveness, 1840, 0.58).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1860, honor_violence_legitimacy__composite_reading, base_extractiveness, 1860, 0.65).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1880, honor_violence_legitimacy__composite_reading, base_extractiveness, 1880, 0.71).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_be_t1900, honor_violence_legitimacy__composite_reading, base_extractiveness, 1900, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1780, honor_violence_legitimacy__composite_reading, suppression_requirement, 1780, 0.28).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1800, honor_violence_legitimacy__composite_reading, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1820, honor_violence_legitimacy__composite_reading, suppression_requirement, 1820, 0.42).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1840, honor_violence_legitimacy__composite_reading, suppression_requirement, 1840, 0.49).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1860, honor_violence_legitimacy__composite_reading, suppression_requirement, 1860, 0.54).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1880, honor_violence_legitimacy__composite_reading, suppression_requirement, 1880, 0.58).
narrative_ontology:measurement(honor_violence_legitimacy__composite_reading_su_t1900, honor_violence_legitimacy__composite_reading, suppression_requirement, 1900, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, state_monopoly_violence_consolidation).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, professional_licensing_regimes).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, bourgeois_moral_reform_movements).

% DUAL FORMULATION NOTE:
% Kernel 'honor_violence_legitimacy' decomposes into three constraint stories: (1) drop_reading — external costs only; (2) contraction_reading — conceptual redefinition only; (3) composite_reading (this story) — both mechanisms simultaneously. The composite has higher ε (0.78) than either sibling because it captures the synergistic extraction: drop mechanism's legal penalties + contraction mechanism's conceptual erasure. The drop and contraction readings have distinct victim sets and temporal profiles; this composite reading models their interaction. Per ε-invariance, these are separate constraints with separate ε values, linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, institutional, 0.12).
constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, organized, 0.28).
constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerful, 0.82).
constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, moderate, 0.88).
constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerless, 0.93).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
