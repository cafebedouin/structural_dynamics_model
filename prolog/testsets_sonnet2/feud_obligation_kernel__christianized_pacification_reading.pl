% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Ecclesiastical Prohibition of Blood-Feud as Divine-Law Violation (Christianized Pacification Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   Medieval Christian ecclesiastical authorities, later joined by
 *   consolidating royal courts, declared blood-feud vengeance a violation of
 *   divine law: only God, and those to whom God delegates authority (bishops
 *   wielding spiritual sanction, kings wielding the 'king's peace'), may
 *   legitimately authorize lethal violence. Feud participants who continued
 *   customary vengeance faced excommunication, denial of sacraments and
 *   burial rites, and eventually secular prosecution. The doctrine
 *   simultaneously curbed some cycles of retaliatory killing and installed
 *   the Church and allied crown as the obligatory intermediaries for any
 *   lethal dispute, collecting penitential fees, judicial fines, and expanded
 *   jurisdictional reach in the process.
 *
 * KEY AGENTS:
 *   - ecclesiastical_authority: primary agenda-setter and beneficiary — gains interpretive monopoly on legitimate violence
 *   - allied_royal_courts: secondary beneficiary — absorbs feud jurisdiction into royal fines and consolidated territorial authority
 *   - kin_group_avengers and kin_group_targets: primary targets — caught between honor obligation and spiritual/legal sanction
 *   - feuding_lineages_generally: declared universally in spiritual peril by participation in the customary system
 *   - customary_law_specialists: displaced experts, excluded from the reframing
 *   - modern_legal_historians: analytical observers assessing genuine reform versus institutional capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.58).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.81).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Ecclesiastical Prohibition of Blood-Feud as Divine-Law Violation (Christianized Pacification Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '1a279670-a4ff-48aa-a46d-4caac1e0a59f').
narrative_ontology:cs_kernel_codification('1a279670-a4ff-48aa-a46d-4caac1e0a59f', formalized).
narrative_ontology:cs_authority_grounding('1a279670-a4ff-48aa-a46d-4caac1e0a59f', lineage).
narrative_ontology:cs_interpretation_layer_present('1a279670-a4ff-48aa-a46d-4caac1e0a59f').
narrative_ontology:cs_reading_relation('1a279670-a4ff-48aa-a46d-4caac1e0a59f', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1a279670-a4ff-48aa-a46d-4caac1e0a59f', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('1a279670-a4ff-48aa-a46d-4caac1e0a59f', foundational, vengeance_belongs_to_god_alone).
narrative_ontology:cs_axiom_status(vengeance_belongs_to_god_alone, holdable).
narrative_ontology:cs_axiom_grounding('1a279670-a4ff-48aa-a46d-4caac1e0a59f', vengeance_belongs_to_god_alone, theological).
narrative_ontology:cs_axiom('1a279670-a4ff-48aa-a46d-4caac1e0a59f', foundational, legitimate_violence_requires_delegated_authority).
narrative_ontology:cs_axiom_status(legitimate_violence_requires_delegated_authority, holdable).
narrative_ontology:cs_axiom_grounding('1a279670-a4ff-48aa-a46d-4caac1e0a59f', legitimate_violence_requires_delegated_authority, theological).
narrative_ontology:cs_reference_frame('1a279670-a4ff-48aa-a46d-4caac1e0a59f', divine_monopoly_on_legitimate_violence).
narrative_ontology:cs_drift_state('1a279670-a4ff-48aa-a46d-4caac1e0a59f', high_medieval_royal_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a279670-a4ff-48aa-a46d-4caac1e0a59f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, allied_royal_courts).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, kin_group_avengers).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, kin_group_targets).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_lineages_generally).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, penitential_clergy).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, vengeance_belongs_to_god_alone).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, legitimate_violence_requires_delegated_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines vengeance as sin, threatens excommunication and denial of sacraments to feud participants, and establishes penitential tariffs and sanctuary/asylum privileges as the sanctioned channel for resolving killings. Gains an interpretive monopoly on what counts as legitimate versus illegitimate violence, and expands its jurisdiction into disputes previously settled entirely within kin networks. Bears none of the killing risk itself and cannot be feuded against without triggering the very sanctions it controls.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority, beneficiary).

% Adopts the Church's framing to argue that only the crown may authorize lawful violence (the king's peace), using ecclesiastical condemnation of private vengeance to justify absorbing feud jurisdiction into royal courts and fines payable to the crown. Gains consolidated territorial authority and a revenue stream from wergild-adjacent judicial fees that previously stayed within kin networks.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, allied_royal_courts, beneficiary,
    institutional, generational, arbitrage, national).

% Bound by kin honor obligation to avenge a killing, but now faces excommunication, denial of burial rites, and civil penalty if they act on that obligation through traditional means. Caught between the social cost of failing kin duty and the spiritual/legal cost of fulfilling it; exit requires either accepting Church-mediated composition (wergild/penance) or risking total social and spiritual exclusion.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, kin_group_avengers, payer,
    moderate, biographical, constrained, local).

% The original killer's kin, who under customary feud law expected the matter closed through composition or continued violence; now subject to unpredictable ecclesiastical and royal intervention that can reopen, redirect, or monetize the dispute through tariffs and fines they did not negotiate, with sanctuary offering only partial and conditional protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, kin_group_targets, payer,
    moderate, biographical, constrained, local).

% All parties embedded in the customary feud system are declared to be in spiritual peril simply by participating in it, whether as avenger, target, or bystander kin who support either side. Their entire prior mechanism for restoring honor and order is reclassified as sin, collapsing the legitimacy of the practices that previously governed their security without offering full substitute protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feuding_lineages_generally, payer,
    powerless, generational, trapped, regional).

% Administers confession, penance schedules, and reconciliation rituals for feud participants, positioning the local church as the indispensable intermediary for resolving killings. Gains standing, tithes, and social authority as the party through whom peace must be brokered.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, penitential_clergy, agenda_setter,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, penitential_clergy, beneficiary).

% Elders and lawspeakers who previously adjudicated feud composition under customary law are structurally sidelined as the Church and crown reclassify their entire domain of expertise as either sinful or jurisdictionally subordinate. They are not consulted in the theological reframing and have no forum to contest it on its own terms.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, customary_law_specialists, excluded,
    moderate, biographical, trapped, local).

% Studies charters, penitentials, and court records to assess whether the Christianized pacification narrative reflects genuine moral reform, institutional capture of dispute-resolution authority, or both simultaneously.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, modern_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, theologically grounded channel (confession, penance, sanctuary, ecclesiastical/royal adjudication) for resolving lethal disputes that would otherwise cycle through retaliatory killing, reducing overall bloodshed by substituting monetary and spiritual settlement for continued violence.
% TRANSFER_FUNCTION: Moves the authority to declare violence legitimate from kin networks to Church and crown, and moves material resources (penitential fees, wergild shares, judicial fines, tithes) from feuding lineages toward ecclesiastical and royal treasuries; also moves spiritual standing away from anyone who continues customary vengeance.
% ABSENT_VOICES: Customary law specialists (elders, lawspeakers) whose entire domain of practice is reclassified as sin or subordinated to royal jurisdiction are not party to the theological reframing; women and dependents within feuding kin groups, who bear consequences of both continued violence and Church-imposed settlements, are rarely represented in the charters and penitentials that record this history.
% DISAPPEARANCE_RATIONALE: If the prohibition and its penitential/jurisdictional apparatus vanished, kin groups would likely revert to negotiating composition and retaliation through customary law specialists without ecclesiastical or royal mediation; Church and crown would lose a major lever for territorial and moral authority, and the flow of tariffs, fines, and tithes tied to feud resolution would cease.
% FOUNDING_PROBLEM: Endemic cycles of retaliatory killing between kin groups destabilized regions where no central authority could reliably prevent or punish violence, producing chronic insecurity, economic disruption, and a demand for some mechanism to end escalating vendettas.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chroniclers and allied royal charters attest the problem was real and that the prohibition curbed bloodshed. Independent legal historians studying surviving customary law records and penitential registers note that composition mechanisms under customary law were already functioning to limit feud escalation before Christianization, and that the Church's intervention substantially expanded its own jurisdiction and revenue alongside any genuine pacification effect — corroboration for the founding-problem's continued necessity comes primarily from the benefiting institutions themselves.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the steady growth of penitential tariffs, sanctuary fees, and royal fines layered onto feud resolution as the doctrine matured from purely spiritual condemnation into a full jurisdictional and revenue apparatus. Suppression (0.81) is high and rises over the interval because enforcement escalates from social/spiritual pressure (excommunication threat) to coordinated ecclesiastical-royal prosecution — this is a raw structural property, not scaled by scope, reflecting how thoroughly alternative dispute-resolution channels were foreclosed. Theater ratio stays comparatively low (0.22) because the underlying function — reducing lethal cycles — is substantially real, not merely performed, even as the apparatus around it grows extractive. Accessibility collapse (0.62) and resistance (0.71) reflect that customary alternatives did not vanish completely (feud persisted underground in many regions for centuries) but met genuine, escalating suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical/royal agenda-setter seat, this looks like Tangled Rope shading toward legitimate coordination: a genuine reduction in retaliatory killing achieved through institutional intervention. From the kin-group payer seats, the same structure computes closer to Snare: their prior (imperfect but functioning) dispute-resolution mechanism is criminalized and replaced with one they did not design, cannot exit, and must pay into. The engine computes both seats from the same structural data — this divergence is the intended measurement, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authority and allied royal courts sit at the beneficiary end: they set the terms of legitimate violence, collect fees and fines, and cannot be feuded against without triggering the sanctions they administer — d near 0. Kin group avengers, kin group targets, and feuding lineages generally sit near the target end: trapped or constrained exit, bearing both the original honor-obligation cost and the new spiritual/legal cost layered atop it — d near 1. Per this reading's expected structural delta, ALL feud participants (not just one side) enter the victim set, because the theological claim is that continued participation in customary vengeance itself constitutes spiritual peril regardless of which side of the feud an agent stands on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (endemic retaliatory killing) may have been substantially real at the outset, but the corroboration is asymmetric: it comes primarily from the institutions that benefited from claiming it. If customary composition mechanisms were already functioning adequately (as the stateless_coordination_reading argues), then the christianized_pacification apparatus is less a fix for a live problem than an extraction of jurisdiction and revenue riding on a genuine but partial pacification effect — classifying this as tangled_rope rather than pure rope or pure snare prevents both over-crediting the doctrine as pure coordination and dismissing its real, if partial, reduction of lethal cycles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_theological_vs_material,
    'Is the prohibition on blood-feud best understood as a genuine theological/moral development (vengeance properly belongs to God) or as an institutional capture of dispute-resolution authority and its associated revenue (the extraction_cycle_reading and stateless_coordination_reading''s shared skepticism)?',
    'Comparative analysis of regions where feud persisted despite ecclesiastical condemnation versus regions where it was substantially replaced by Church/royal adjudication, correlated with the material benefit (fees, fines, land grants) accruing to intervening institutions in each region.',
    'If theological development dominates, this reading''s beneficiary declarations for ecclesiastical_authority should be read as incidental rather than causal; if institutional capture dominates, the coordination function claimed here is closer to cover story than substance, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence_theological_vs_material, conceptual, 'Whether the divine-law framing is genuine doctrine or extraction cover — the central fault line between this reading and its two siblings.').

omega_variable(
    sibling_reading_structural_delta,
    'Where exactly does this reading''s beneficiary/victim structure diverge from the stateless_coordination_reading (which treats feud itself, not its prohibition, as the functioning coordination mechanism) and the extraction_cycle_reading (which treats feud itself as the extraction)?',
    'This is Rule 2 routing: the committer structure is that this reading places the CHURCH in the beneficiary set and ALL feud participants (regardless of side) in the victim set via spiritual peril — a structural move neither sibling reading makes, since both siblings treat the feud system itself (not its prohibition) as the object of analysis.',
    'If the stateless_coordination_reading is correct that feud was adequate self-enforcing deterrence, then this reading''s ε (extraction of the prohibition regime) should be read as extraction imposed ON TOP OF a working system, not extraction removing extraction. If the extraction_cycle_reading is correct that feud itself was destructively extractive, then this reading''s prohibition regime could be read as a partial remedy whose own extraction is smaller net-net than what it replaced — but that comparison is out of scope for THIS story''s ε, which is fixed to the standing prohibition arrangement per the ε-referent rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents where the three kernel readings structurally diverge in beneficiary/victim assignment, per Rule 2.').

omega_variable(
    complete_suppression_feasibility,
    'Did the Church and allied crowns actually achieve the complete suppression of feud vengeance that the penitential discipline sought, or did customary practice persist in parallel (underground feud, informal composition) for centuries after formal prohibition?',
    'Court records, penitential registers, and chronicle accounts documenting continued feud activity after formal Christianization in specific regions (e.g., parts of early medieval Iceland, the Balkans, Corsica) versus regions where suppression appears more complete.',
    'If suppression was never complete, accessibility_collapse should be read as regionally variable rather than uniform, and the constraint''s actual effective suppression is lower than the doctrine''s stated ambition — this bears on whether later measurement points overstate suppression_requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complete_suppression_feasibility, empirical, 'Whether the sought complete suppression via penitential discipline was actually achieved or remained aspirational.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(feud_tr_t120, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 120, 0.16).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 200, 0.19).
narrative_ontology:measurement(feud_tr_t280, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 280, 0.21).
narrative_ontology:measurement(feud_tr_t400, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 400, 0.22).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(feud_be_t120, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 120, 0.45).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 200, 0.51).
narrative_ontology:measurement(feud_be_t280, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 280, 0.55).
narrative_ontology:measurement(feud_be_t400, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(feud_su_t120, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 200, 0.74).
narrative_ontology:measurement(feud_su_t280, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 280, 0.78).
narrative_ontology:measurement(feud_su_t400, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 400, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of feud_obligation_kernel. christianized_pacification_reading (this story) treats the ecclesiastical/royal prohibition regime as the object of analysis, with Church and crown as beneficiaries and all feud participants as victims via spiritual peril. stateless_coordination_reading treats the feud system itself as a self-enforcing coordination mechanism (near-Rope, low ε) functioning adequately absent central authority. extraction_cycle_reading treats the feud system itself as destructively extractive (high ε) independent of theological framing, preventing territorial consolidation. All three share the same underlying historical kernel — the normative status of blood-feud and who holds legitimate authority over lethal violence — but instantiate structurally distinct constraints with different objects, different ε referents, and different beneficiary/victim sets. Per the ε-invariance principle, these are linked via network edges rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
