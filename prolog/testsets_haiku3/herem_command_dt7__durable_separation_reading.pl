% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Divine Mandate: Durable Separation Reading
 *   domain: religious/ethical/metaphysical
 *
 * SUMMARY:
 *   The durable_separation_reading interprets Deuteronomy's herem command
 *   (total devotion/separation from designated outsiders, including marital
 *   prohibition, resource non-sharing, and in conquest contexts, elimination)
 *   as encoding a timeless divine mandate for identity preservation through
 *   categorical separation. This reading holds that the command's force does
 *   not attenuate with historical circumstance: the covenant community
 *   remains perpetually bound to maintain boundaries against outsider
 *   contamination. The reading benefits the institutional authority that
 *   maintains it by vindicating separation as divinely mandated rather than
 *   culturally contingent. It extracts autonomy from mixed-boundary agents
 *   (who must sever chosen relationships) and from non-covenant outsiders
 *   (who bear the cost of perpetual exclusion). The claim/metric independence
 *   is deliberate: the reading is CLAIMED as rope (genuine coordination
 *   function: preserving group identity) while the authored metrics describe
 *   substantially extractive operation (high extractiveness, high
 *   suppression, rising theater ratio indicating increasing performative
 *   maintenance of boundaries as enforcement costs rise).
 *
 * KEY AGENTS:
 *   - covenant_community_elect: Benefits from categorical identity vindication; administers enforcement (institutional power, identity-locked, civilizational horizon)
 *   - non_covenant_outsiders: Face perpetual exclusion and authorized elimination; bear the extraction (powerless, trapped, universal scope)
 *   - mixed_boundary_agents: Bear forced severance of chosen relationships; face social death for boundary violation (moderate power, identity-locked, biographical horizon)
 *   - interpretive_authority_lineage: Derives institutional authority from maintaining the reading's transmission; survival depends on its plausibility (institutional, analytical exit, civilizational horizon)
 *   - rival_theological_readings: Structurally excluded from the covenant community's canonical process (organized, constrained exit, universal scope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.88).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Divine Mandate: Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious/ethical/metaphysical").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'cb84ccbb-743b-4726-a66b-620b2a0457eb').
narrative_ontology:cs_kernel_codification('cb84ccbb-743b-4726-a66b-620b2a0457eb', fixed_text).
narrative_ontology:cs_authority_grounding('cb84ccbb-743b-4726-a66b-620b2a0457eb', lineage).
narrative_ontology:cs_interpretation_layer_present('cb84ccbb-743b-4726-a66b-620b2a0457eb').
narrative_ontology:cs_reading_relation('cb84ccbb-743b-4726-a66b-620b2a0457eb', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb84ccbb-743b-4726-a66b-620b2a0457eb', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('cb84ccbb-743b-4726-a66b-620b2a0457eb', foundational, herem_encodes_timeless_categorical_mandate).
narrative_ontology:cs_axiom_status(herem_encodes_timeless_categorical_mandate, holdable).
narrative_ontology:cs_axiom_grounding('cb84ccbb-743b-4726-a66b-620b2a0457eb', herem_encodes_timeless_categorical_mandate, deontological).
narrative_ontology:cs_axiom('cb84ccbb-743b-4726-a66b-620b2a0457eb', secondary, outsider_boundary_maintenance_divinely_obligatory).
narrative_ontology:cs_axiom_status(outsider_boundary_maintenance_divinely_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('cb84ccbb-743b-4726-a66b-620b2a0457eb', outsider_boundary_maintenance_divinely_obligatory, theological).
narrative_ontology:cs_reference_frame('cb84ccbb-743b-4726-a66b-620b2a0457eb', eternal_covenant_separation_framework).
narrative_ontology:cs_drift_state('cb84ccbb-743b-4726-a66b-620b2a0457eb', contemporary_pluralist_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb84ccbb-743b-4726-a66b-620b2a0457eb', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_elect).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, mixed_boundary_agents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community defined by covenant obedience. Benefits from the herem command by securing categorical identity boundaries against contamination through intermarriage, resource-sharing, and social fusion with outsiders. The command vindicates their separateness as divinely mandated rather than ethically contingent. Administers the enforcement through doctrinal interpretation, membership adjudication, and sanction of violators.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_elect, beneficiary,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_elect, agenda_setter).

% All persons outside the covenant boundary. Bear the cost of exclusion from membership, inheritance, alliance, and sanctuary. The reading construes them as perpetual contamination threats whose very proximity endangers covenant holiness. No exit from outsider status except conversion (which requires abandonment of prior identity and kinship). Under herem logic, their elimination or perpetual subordination is divinely authorized when they resist incorporation on the covenant's terms.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer,
    powerless, civilizational, trapped, universal).

% Covenant members who marry outsiders, adopt outsider practices, or maintain kinship ties across the boundary. Face enforcement action (divorce, expulsion, social death) to maintain boundary purity. Their existence threatens the categorical separation the herem command polices. They are excluded from community deliberation about their own status (the covenant community adjudicates without their voice).
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, mixed_boundary_agents, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, mixed_boundary_agents, excluded).

% Religious scholars, councils, and institutional interpreters who maintain the reading's transmission and application. Derive authority from claiming fidelity to timeless divine mandate. Adjudicate boundary cases, pronounce on who qualifies as 'other,' and legitimize enforcement actions. Their institutional survival depends on the reading's plausibility.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, interpretive_authority_lineage, agenda_setter,
    institutional, civilizational, analytical, universal).

% Alternative readings (allegorical, supersessionist, universalist) that reframe herem as non-durable, spiritualized, or morally overridden. Structurally excluded from the covenant community's interpretive process. Their advocates within the community face marginalization or expulsion. Their existence as live intellectual options demonstrates the reading is not naturally inevitable.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, rival_theological_readings, excluded,
    organized, civilizational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_elect).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes group identity and prevents assimilation-through-intermarriage that would erode the covenant community's boundaries and theological distinctiveness. Provides a categorical framework for determining who belongs and who does not, without which group cohesion across generations would depend on voluntary preference rather than binding mandate.
% TRANSFER_FUNCTION: Moves autonomy from mixed-boundary agents and non-outsiders to the covenant community: the right to marry whom one wishes is transferred to the collective (which forbids exogamy); the right to maintain kinship and trade ties with outsiders is transferred to the community (which severs them); the right to exist unmolested as an outsider is transferred to the community (which authorizes elimination). The covenant community collects the enforcement authority; individuals and outsiders bear the deprivation.
% ABSENT_VOICES: Non-covenant outsiders and mixed-boundary agents are structurally excluded from the reading's construction and interpretation. Outsiders are theorized but not heard; mixed-boundary agents face forced silence (expulsion silences their perspective). Sibling readings that interpret herem as non-durable or spiritualized are excluded from the covenant community's canonical process and treated as heretical within the tradition. These absent voices would contest the reading's core premise (that the mandate is timeless and binding).
% DISAPPEARANCE_RATIONALE: If the herem command and its enforcement vanished, covenant identity would reorganize: intermarriage would become possible, resource-sharing and kinship ties with outsiders would open, and the categorical boundary between inside and outside would erode. The reading's entire purpose is to prevent this rearrangement, so its disappearance would be exactly the rearrangement it was built to forestall.
% FOUNDING_PROBLEM: Ancient Israel's survival as a distinct people in a polytheistic environment where cultural fusion through intermarriage, idol worship, and kinship networks threatened to dissolve tribal identity into the surrounding imperial structures. The herem command encoded divine guarantee of separateness as a means of survival.
% FOUNDING_PROBLEM_CORROBORATION: This reading's proponents attest the founding problem is live: assimilation remains the existential threat to any minority religious or ethnic community. Sibling readings and external historians attest the founding problem was specific to ancient settlement conditions and is no longer operative (the community's survival no longer depends on violent boundary maintenance). No corroboration from outside the reading's own tradition—the reading is maintained primarily within communities that already affirm herem's binding force.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the reading's operation systematically transfers autonomy decisions (marriage, kinship, resource-sharing) from individuals to the collective. The transfer is justified by appeal to divine mandate, which frames the loss of autonomy as obedience rather than deprivation. Suppression is high (0.88) because the reading requires active enforcement: mixed-boundary agents must be detected and sanctioned; rival interpretations must be excluded from the covenant community's decision-making; the logic of contamination must be internalized (identity-locked agents learn to fear mixture rather than being externally coerced into fear—the suppression becomes self-maintaining). Theater is moderate and rising (0.28→0.47) because the reading increasingly performs boundary-maintenance rather than solving the coordination problem it originally addressed. In ancient contexts, herem functioned as military doctrine and settlement policy; in contemporary institutional contexts, the same reading performs separateness (discussion of the command, legal prohibition on intermarriage, ritual cleansing of boundary violations) more than it coordinates survival. The rise in theater_ratio reflects the constraint's drift from functional necessity to institutional maintenance. The shared time grid shows extractiveness stabilizing (reaching saturation in projected endpoints) while theater rises—the reading's performance cost increases as the coordination problem fades further into the past. Suppression continues rising as enforcement infrastructure must intensify to maintain boundaries under pressure from rival readings and secular legal frameworks that forbid discrimination.
 *
 * PERSPECTIVAL GAP:
 *   The covenant_community_elect seat and the non_covenant_outsiders seat compute fundamentally differently. From the elite's position, herem is genuine coordination: the alternative (assimilation) is dissolution of group identity—the command solves the coordination problem of boundary maintenance against fusion. The agent seat experiences the structure as voluntary cooperation in a shared identity project. From the outsider's position, the same structure operates as a Snare: the covenant community has unilateral power to exclude (they cannot convert without identity death); there is no genuine negotiation about terms of membership or coexistence; their elimination is authorized. The mixed-boundary agent sits between: they began inside the covenant community (so they share the identity-lock) but chose the outside (through marriage or kinship), and now face enforcement that severs what they chose. The engine will compute different type assignments for these seats from the same structural data: a beneficiary with identity-locked exit computing rope-type protection of group autonomy; a victim with trapped exit computing snare-type extraction of assimilation choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality assignments follow from the beneficiary/victim structure. The covenant_community_elect occupies d ≈ 0.1–0.2 (strong beneficiary: the reading vindicates their identity, the enforcement collects to their institutional authority, their exit is identity-locked so they cannot even conceptually exit). Non-covenant outsiders occupy d ≈ 0.9–1.0 (full target: they bear the cost of exclusion, their exit is structurally trapped—they can convert but only by abandoning prior identity and kinship, which is not a real alternative). Mixed-boundary agents occupy d ≈ 0.75–0.85 (high target: they face forced severance of chosen relationships, identity-locked exit prevents them from simply leaving the community, the suppression mechanism works by internalizing fear of contamination). The interpretive_authority_lineage occupies d ≈ 0.0–0.1 (strong beneficiary: their institutional survival depends on the reading's maintenance, they derive authority from its legitimacy, they have arbitrage-grade exit—if the reading fails they can adopt a sibling reading). No directionality override is needed: the derivation chain (beneficiary/victim + exit + power) produces the correct d values. Suppression is not scaled by directionality—it is measured as the raw structural force required to maintain boundaries against the reading's natural decay (rival readings, secular legal pressure, individual preference for mixed kinship).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading exhibits mandatrophy—the founding problem (assimilation threat in ancient settlement context) is substantially dead, but the mandate persists. The founding_problem_status is coded as 'contested' rather than 'dead' because the reading's own proponents attest the problem is live (assimilation remains an existential threat). But external observers and sibling readings attest the problem was historically bounded: modern minority religious communities maintain identity through institutions, education, and legal protection, not through herem's boundary violence. The constraint persists not because the problem it was built for requires it, but because institutional authorities benefit from its maintenance and because identity-locked community members have fused their self-concept with the boundary-keeping logic. The rising theater_ratio (0.28→0.47) confirms the mandatrophy: an increasing share of the herem reading's institutional operation is performative (discussing the command, debating its application, performing boundary-maintenance rituals) rather than functionally necessary. A genuine rope solving a live coordination problem would show theater_ratio near 0.0; a constraint whose functional problem is dead but whose institutional beneficiaries persist it shows rising theater as the performance cost increases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_cultural_mandate,
    'Is the herem command encoding a timeless natural/divine law of identity preservation (mountains-adjacent), or is it a culturally constructed mandate that happened to be sanctioned as divine?',
    'Empirical test: compare the binding force and function of separation mandates across religious traditions—if herem''s logic is universal, independent traditions facing similar assimilation pressure should discover the same mandate; if culturally constructed, the mandate will be idiosyncratic to Israelite context. Hermeneutical test: examine whether the reading''s own tradition recognizes supersession, reinterpretation, or abandonment in its own historical development—if the mandate is natural law, no supersession is possible; if constructed, supersession will appear.',
    'If natural: the reading is a Mountain; if constructed: the reading is a Snare masquerading as a Mountain through divine-command framing. High impact on FSM (false-summit mountain) diagnosis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_mandate, conceptual, 'Whether herem_encodes_timeless_categorical_mandate is a natural law or constructed doctrine.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.88) primarily structural (legal enforcement, institutional sanction) or internalized (community members have fused identity with boundary-keeping so they self-police)?',
    'Post-exit trajectory analysis: if covenant members who leave the community and are no longer subject to institutional sanction continue to avoid intermarriage and maintain separation from outsiders, the suppression is internalized; if exit from institutional enforcement results in rapid boundary-crossing, the suppression is primarily structural.',
    'If primarily internalized, the effective suppression is higher than the structural measure—targets carry the suppression with them post-exit, and exit does not free them from the constraint. If primarily structural, exit remedies the suppression. The internalized case is the pathologically extractive one: even removal of the coercive mechanism leaves the victim bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression mechanism is structural or internalized identity-fusion.').

omega_variable(
    coordination_extraction_separability,
    'Is the identity-coordination function (maintaining group coherence through boundary definition) structurally separable from the extraction function (denying individuals autonomy over marriage and kinship choices)?',
    'Counterfactual: could the covenant community preserve its identity and boundary integrity through positive identification (shared practice, doctrine, inheritance) without negative enforcement (prohibition on outsider relationship)? Natural experiment: communities that abandon herem enforcement but maintain ritual, education, and doctrinal transmission—do they experience accelerated assimilation or do they preserve identity?',
    'If separable: the extraction is decoupled from the coordination function and should be eliminated. If inseparable: the extraction is the price of the coordination. Separability would reclassify the constraint from tangled_rope to snare (pure extraction with coordination as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether identity-coordination and autonomy-extraction are structurally coupled or separable.').

omega_variable(
    divine_mandate_kernel_authenticity,
    'Is the herem command as presented in Deuteronomy the authentic expression of a timeless divine mandate, or is the command itself a product of Deuteronomic editorial redaction and theological agenda?',
    'Textual-historical analysis: examine the command''s literary history, redactional layers, and theological function within Deuteronomy''s overall literary project. If the command shows signs of late addition, theological shaping, or ideological polemic (contra other views), its authenticity as divine mandate is compromised.',
    'If the command is a construct of Deuteronomic editors advancing a particular theological position, then the durable_separation_reading''s claim to encode divine mandate is historically false. This is the kernel-level authenticity question: is the kernel itself what the reading claims it is?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_kernel_authenticity, empirical, 'Whether the herem command is authentic divine mandate or Deuteronomic theological construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t10, herem_command_dt7__durable_separation_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(here_tr_t10, observed).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(here_tr_t20, observed).
narrative_ontology:measurement(here_tr_t30, herem_command_dt7__durable_separation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(here_tr_t30, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(here_tr_t40, projected).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement_basis(here_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.76).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t10, herem_command_dt7__durable_separation_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement_basis(here_be_t10, observed).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(here_be_t20, observed).
narrative_ontology:measurement(here_be_t30, herem_command_dt7__durable_separation_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(here_be_t30, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(here_be_t40, projected).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(here_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.84).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t10, herem_command_dt7__durable_separation_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement_basis(here_su_t10, observed).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(here_su_t20, observed).
narrative_ontology:measurement(here_su_t30, herem_command_dt7__durable_separation_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement_basis(here_su_t30, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement_basis(here_su_t40, projected).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement_basis(here_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of the same scriptural text. The durable_separation_reading treats the command as eternally binding; the contextual_supersession_reading treats it as historically bounded; the allegorical_displacement_reading treats it as spiritualized interior struggle. The three readings yield different beneficiary/victim structures (who benefits from interpreting the command as timeless vs. bounded vs. allegorical), different extraction rates (the durable reading extracts high autonomy; the contextual reading dissolves the extraction via supersession; the allegorical reading reframes victims as interior faculties, not outsiders), and different institutional persistence mechanisms. They are linked via network.affects_constraints so cross-reading contamination analysis can track how the plausibility of one reading propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
