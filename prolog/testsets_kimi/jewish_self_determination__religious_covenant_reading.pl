% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Religious Covenant Reading of Jewish Territorial Self-Determination
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This constraint story models the religious_covenant_reading of the
 *   jewish_self_determination kernel: the claim that Jewish territorial
 *   sovereignty in the Land of Israel derives from a divine covenant, making
 *   it a religious obligation independent of secular political frameworks.
 *   While presented by adherents as immutable divine command (a mountain
 *   within the theological frame), its operationalization through Israeli
 *   state institutions, settlement policy, and coalition politics makes it
 *   structurally a tangled rope â genuine identity coordination for the
 *   believing community entangled with asymmetric extraction from Palestinian
 *   communities and secular Israeli citizenry. The constraint forecloses
 *   secular territorial negotiation by treating land as sacred and
 *   non-negotiable.
 *
 * KEY AGENTS:
 *   - religious_zionist_parties (agenda_setter / institutional / constrained exit): Translate theology into state policy and control coalition leverage.
 *   - settlement_enterprise (beneficiary / organized / constrained exit): Receives land, subsidies, and military protection.
 *   - religious_zionist_movement (beneficiary / organized / identity_locked): Ideological beneficiary whose communal identity is fused with the territorial project.
 *   - secular_israeli_citizenry (payer / moderate / constrained): Bears fiscal, military, and diplomatic costs of foreclosed negotiation.
 *   - palestinian_communities (payer / powerless / trapped): Lose land, movement, and self-determination under permanent religious-state control.
 *   - diaspora_jewish_communities (excluded / organized / mobile): Reject the covenantal reading but are excluded from the framework.
 *   - international_peace_brokers (observer / institutional / analytical): Attempt secular diplomatic intervention from outside the theological logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.78).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Religious Covenant Reading of Jewish Territorial Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political philosophy / nationalism studies / postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '2673a10b-853a-4313-ab4e-64c56924a4e6').
narrative_ontology:cs_kernel_codification('2673a10b-853a-4313-ab4e-64c56924a4e6', fixed_text).
narrative_ontology:cs_authority_grounding('2673a10b-853a-4313-ab4e-64c56924a4e6', lineage).
narrative_ontology:cs_interpretation_layer_present('2673a10b-853a-4313-ab4e-64c56924a4e6').
narrative_ontology:cs_reading_relation('2673a10b-853a-4313-ab4e-64c56924a4e6', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('2673a10b-853a-4313-ab4e-64c56924a4e6', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('2673a10b-853a-4313-ab4e-64c56924a4e6', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2673a10b-853a-4313-ab4e-64c56924a4e6', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('2673a10b-853a-4313-ab4e-64c56924a4e6', foundational, territorial_sovereignty_as_divine_command).
narrative_ontology:cs_axiom_status(territorial_sovereignty_as_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('2673a10b-853a-4313-ab4e-64c56924a4e6', territorial_sovereignty_as_divine_command, theological).
narrative_ontology:cs_axiom('2673a10b-853a-4313-ab4e-64c56924a4e6', foundational, secular_negotiation_theologically_illegitimate).
narrative_ontology:cs_axiom_status(secular_negotiation_theologically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2673a10b-853a-4313-ab4e-64c56924a4e6', secular_negotiation_theologically_illegitimate, theological).
narrative_ontology:cs_reference_frame('2673a10b-853a-4313-ab4e-64c56924a4e6', divine_covenantal_land_grant).
narrative_ontology:cs_drift_state('2673a10b-853a-4313-ab4e-64c56924a4e6', post_1967_territorial_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2673a10b-853a-4313-ab4e-64c56924a4e6', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_communities).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_israeli_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control ministries and coalition leverage in Israeli government, translating covenantal theology into state policy on settlement, land allocation, and territorial negotiation. Their political survival depends on maintaining the religious-national alliance, making exit from the covenantal frame electorally and ideologically costly.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_parties, agenda_setter,
    institutional, generational, constrained, national).

% Receives state subsidies, military protection, and legal endorsement for territorial expansion in contested areas. The enterprise depends on the covenantal claim to justify land allocation that secular frameworks would classify as illegal or negotiable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, constrained, regional).

% Draws ideological vindication and communal purpose from the state enforcement of a divine promise. Membership and identity are constituted through participation in the settlement project and theological opposition to territorial compromise; exit would require apostasy from a core communal narrative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary,
    organized, civilizational, identity_locked, national).

% Bears the tax burden, military service costs, and international isolation produced by the settlement project and foreclosed diplomatic options. Secular peace frameworks are structurally blocked by coalition theology, leaving electoral or emigration as the only exits.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_israeli_citizenry, payer,
    moderate, biographical, constrained, national).

% Lose land access, freedom of movement, and political self-determination as the covenantal claim justifies permanent Israeli control over territory they inhabit. No exit from military administration or settlement expansion; their presence is framed as demographic threat rather than co-sovereign population.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_communities, payer,
    powerless, generational, trapped, local).

% Include large constituencies who reject territorial sovereignty as a Jewish value or prioritize diaspora flourishing. Their voices are excluded from the Israeli state framework that claims to act in their name under the covenantal reading.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities, excluded,
    organized, generational, mobile, global).

% Attempt to advance secular territorial compromise through diplomatic frameworks. They observe the constraint from outside the theological logic and are treated as illegitimate interveners by the covenantal framework.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_peace_brokers, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish territorial presence around a unified theological justification that transcends secular political negotiation, resolving (for adherents) the problem of legitimacy without recourse to contingent international agreement.
% TRANSFER_FUNCTION: Moves territorial control from secular diplomatic negotiability to religiously mandated settlement and state enforcement, transferring land access from Palestinian residents and secular compromise frameworks to the settlement enterprise and religious state institutions.
% ABSENT_VOICES: Diasporist Jewish communities who reject territorial sovereignty as a Jewish value; Palestinian refugees and interior communities whose claims are structurally excluded by the divine-covenant framing; secular Israeli peace architects whose two-state proposals are ruled theologically illegitimate.
% DISAPPEARANCE_RATIONALE: If the divine-covenant constraint vanished, the religious Zionist settlement project would lose its primary legitimating architecture; Israeli territorial politics would shift toward secular nationalist, liberal, or civic-territorial frames, and the current coalition structures binding religious parties to land policy would destabilize.
% FOUNDING_PROBLEM: The problem of Jewish state legitimacy in a contested land without a secure secular-national consensus, addressed by grounding territorial claim in an immutable divine promise that requires no external validation.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist theologians and parties attest the problem is live. Secular Israeli historians, Palestinian scholars, and international law practitioners attest the founding problem was always a political choice to deploy theology for territorial expansion, and that the arrangement now perpetuates a conflict it was meant to resolve; no independent corroboration exists outside the benefiting parties that the divine covenant required this specific state form.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.82) is high because the constraint transfers territorial control and political authority from secular negotiable frameworks and Palestinian inhabitants to a state-backed settlement enterprise. Suppression (0.78) is high because the covenantal claim forecloses compromise alternatives and requires active state enforcement (military, legal, bureaucratic) to maintain territorial control against both internal secular dissent and Palestinian resistance. Theater_ratio (0.46) is moderate-to-high: while theological commitment is genuine for adherents, state institutions increasingly performatively maintain the covenantal frame to justify policies whose material motivations are demographic and strategic. Accessibility_collapse (0.68) reflects that within the Israeli polity, secular alternatives to permanent control have been substantially marginalized but not fully erased. Resistance (0.75) is high due to persistent Palestinian opposition, secular Israeli dissent, and international pressure.
 *
 * PERSPECTIVAL GAP:
 *   Religious Zionist seats experience the constraint as a mountain â a divine command that would persist regardless of political circumstance, with near-zero perceived extraction. Palestinian and secular Israeli seats experience the same structure as a snare or tangled rope: state-enforced extraction that suppresses alternatives and requires active coercion to maintain. The engine computes this divergence from identical structural data; the claim/metric independence rule preserves the gap rather than reconciling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious_zionist_movement, settlement_enterprise) sit near the full-beneficiary end: the constraint subsidizes their ideological purpose and material expansion with state power. Agenda-setters (religious_zionist_parties) also sit near the beneficiary end though they administer enforcement. Payers (palestinian_communities, secular_israeli_citizenry) sit near the full-target end: they bear the costs of foreclosed negotiation and territorial dispossession. Palestinian communities are at the extreme target end due to powerlessness and trapped exit; secular Israelis are slightly less extreme due to moderate power and constrained but not fully trapped exit. Excluded diaspora communities and international observers sit outside the primary directionality derivation, with diaspora communities mobile and observers analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Jewish collective legitimacy in a contested land â has arguably been solved or transformed by state power and international recognition, yet the constraint persists because it has been captured by a beneficiary constellation (religious parties, settlement enterprise) whose interests are served by its continuation. The R5 genealogy interview records this as contested: beneficiaries claim the problem is live (security, divine command), while corroborating observers outside the beneficiary set argue the arrangement perpetuates a conflict it was built to resolve. This mismatch prevents mislabeling the constraint as pure coordination (a rope for Jewish self-preservation) by documenting that the founding justification is contested and the current function is extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_political_instrument,
    'Is the covenantal territorial claim a genuine theological commitment with independent normative force, or a political instrument leveraging religious vocabulary for territorial expansion?',
    'Historical-sociological analysis of theological development versus political mobilization timelines; examine whether the covenantal reading predates the territorial project or was retrofitted to it.',
    'If retrofitted, the constraint is a snare or tangled rope using identity_coordination as cover; if genuinely pre-existing and theologically central, the classification leans toward a commitment-system mountain with high accessibility collapse for believers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_vs_political_instrument, empirical, 'Whether the religious covenant is genuine theology or political instrument.').

omega_variable(
    framework_effective_epsilon,
    'Does the contested status of the divine-covenant framework raise the constraint''s effective extraction, or does the internal absoluteness of the claim neutralize contestation for adherents?',
    'Measure policy divergence between religious Zionist constituencies and the general Israeli public on territorial compromise; if the religious constituency is immune to cost signals that shift secular opinion, the internal mountain framing dominates effective behavior.',
    'If internal framing dominates, the constraint operates as a mountain for believers and a snare for non-believers, producing extreme seat divergence; if contestation raises epsilon uniformly, it operates as a standard tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framework_effective_epsilon, conceptual, 'How framework contestation modulates extraction across seats.').

omega_variable(
    suppression_internalization_secular_public,
    'Has the secular Israeli public internalized the religious-covenant constraint as a background condition, or is their compliance purely structural (coalition politics, military discipline)?',
    'Track public opinion and voting behavior on territorial compromise over time; if secular opinion shifts toward religious framing even without coercion, suppression is partially internalized.',
    'Internalized suppression would raise effective extraction and resistance costs for reform; structural-only suppression leaves room for secular political realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_secular_public, empirical, 'Structural versus internalized suppression for secular constituencies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__religious_covenant_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__religious_covenant_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__religious_covenant_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__religious_covenant_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__religious_covenant_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__religious_covenant_reading, theater_ratio, 50, 0.46).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__religious_covenant_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__religious_covenant_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__religious_covenant_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__religious_covenant_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__religious_covenant_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__religious_covenant_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__religious_covenant_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__religious_covenant_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__religious_covenant_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__religious_covenant_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__religious_covenant_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__religious_covenant_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_self_determination kernel. The religious_covenant_reading instantiates a divine-command framing that structurally diverges from liberal-nationalist, indigenous-return, diasporist, and settler-colonial readings. Decomposition is required because the epsilon and beneficiary/victim structure of this reading differ fundamentally from its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
