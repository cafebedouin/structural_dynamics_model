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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Durable Separation Reading of Herem Command (Deut 7)
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   The durable_separation_reading of Deuteronomy 7's herem command
 *   interprets the ban on intermarriage and requirement to destroy Canaanite
 *   nations as a timeless divine mandate for covenant identity preservation
 *   through categorical separation. This reading treats the text not as
 *   historically bounded (contextual_supersession) or spiritually allegorized
 *   (allegorical_displacement) but as establishing a perpetual structural
 *   principle: covenant faithfulness requires bounded membership and the
 *   treatment of designated outsiders as existential contamination threats.
 *   The constraint extracts heavily from intermarriage autonomy and the life
 *   prospects of designated outsider groups, while legitimating violence
 *   through divine command obedience. The coordination function — identity
 *   preservation — is real but structurally fused with expansive extraction.
 *
 * KEY AGENTS:
 *   - covenant_community_members: Primary beneficiaries (identity preserved through separation) but also constrained (marriage autonomy denied, children's autonomy pre-structured) — institutional/biographical/constrained
 *   - religious_leadership_authorities: Agenda setters (interpret and enforce boundary) — institutional/generational/arbitrage
 *   - designated_outsider_groups: Primary victims (existential threat designation, violence legitimation) — powerless/immediate/trapped
 *   - intermarried_families: Victims (coerced dissolution or exclusion) — moderate/biographical/constrained
 *   - assimilated_individuals: Victims (forced choice between identity and belonging) — moderate/biographical/identity_locked
 *   - covenant_children_denied_autonomy: Victims (marriage pool pre-restricted, identity assigned) — powerless/biographical/identity_locked
 *   - boundary_maintenance_institutions: Beneficiaries (institutional purpose and authority derived from enforcement) — organized/generational/mobile
 *   - hermeneutical_observers: Analytical seat — analytical/civilizational/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Durable Separation Reading of Herem Command (Deut 7)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'ec27fa93-dce8-400f-aa46-be3e26e28502').
narrative_ontology:cs_kernel_codification('ec27fa93-dce8-400f-aa46-be3e26e28502', fixed_text).
narrative_ontology:cs_authority_grounding('ec27fa93-dce8-400f-aa46-be3e26e28502', lineage).
narrative_ontology:cs_interpretation_layer_present('ec27fa93-dce8-400f-aa46-be3e26e28502').
narrative_ontology:cs_reading_relation('ec27fa93-dce8-400f-aa46-be3e26e28502', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('ec27fa93-dce8-400f-aa46-be3e26e28502', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('ec27fa93-dce8-400f-aa46-be3e26e28502', foundational, herem_perpetual_divine_mandate).
narrative_ontology:cs_axiom_status(herem_perpetual_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('ec27fa93-dce8-400f-aa46-be3e26e28502', herem_perpetual_divine_mandate, deontological).
narrative_ontology:cs_axiom('ec27fa93-dce8-400f-aa46-be3e26e28502', foundational, categorical_separation_as_covenantal_faithfulness).
narrative_ontology:cs_axiom_status(categorical_separation_as_covenantal_faithfulness, holdable).
narrative_ontology:cs_axiom_grounding('ec27fa93-dce8-400f-aa46-be3e26e28502', categorical_separation_as_covenantal_faithfulness, deontological).
narrative_ontology:cs_axiom('ec27fa93-dce8-400f-aa46-be3e26e28502', secondary, outsider_as_contamination_threat).
narrative_ontology:cs_axiom_status(outsider_as_contamination_threat, holdable).
narrative_ontology:cs_axiom_grounding('ec27fa93-dce8-400f-aa46-be3e26e28502', outsider_as_contamination_threat, deontological).
narrative_ontology:cs_reference_frame('ec27fa93-dce8-400f-aa46-be3e26e28502', sinai_covenantal_closure).
narrative_ontology:cs_drift_state('ec27fa93-dce8-400f-aa46-be3e26e28502', contemporary_application, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec27fa93-dce8-400f-aa46-be3e26e28502', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_leadership_authorities).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, boundary_maintenance_institutions).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsider_groups).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarried_families).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, assimilated_individuals).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_children_denied_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_community_members).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_identity_preservation_through_separation).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_boundary_as_covenantal_faithfulness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive identity coherence and communal belonging through the separation boundary. Simultaneously lose marriage autonomy — the marriage pool is pre-restricted to covenant members only. Children's marital futures are structured before they can choose. Exit from the community means losing the identity the constraint preserves. The constraint both gives and takes from the same agents.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_members, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_members, payer).

% Interpret and enforce the herem boundary. Determine who counts as 'Canaanite' or 'outsider' in each generation. Gain institutional authority, communal trust, and resource control from being the boundary's administrators. Can move between communities, reinterpret texts, or shift emphasis without personal cost — their exit options are arbitrage-grade. The constraint's persistence serves their institutional interest.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_leadership_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Designated as existential contamination threats by the constraint's logic. Face exclusion, dispossession, or violence legitimated as divine obedience. Have no voice in the designation, no appeal within the covenant framework, and no exit that removes the threat — the constraint follows them as a structural classification. The constraint extracts their security, autonomy, and sometimes their lives.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsider_groups, payer,
    powerless, immediate, trapped, local).

% Formed across the boundary the constraint forbids. Face communal pressure to dissolve the marriage, convert the outsider spouse, or accept exclusion from covenant community. The constraint extracts their family integrity and forces a choice between kinship and covenant belonging. Exit options are constrained: leaving the community severs kinship networks; staying requires betraying the marriage.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarried_families, payer,
    moderate, biographical, constrained, national).

% Outsiders who have culturally assimilated but remain structurally designated as 'other' by the herem logic. The constraint denies that assimilation can change status — identity is ascribed by the boundary, not achieved by behavior. Face permanent suspicion and exclusion from full covenant participation. Identity-locked because the constraint's logic fuses their personhood with the 'outsider' category; no performance of faithfulness can overcome the categorical separation.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, assimilated_individuals, payer,
    moderate, biographical, identity_locked, national).

% Born into the covenant community with marriage autonomy pre-foreclosed. The constraint structures their future spouse pool before they develop agency. Taught that exogamy is covenantal betrayal. Identity-locked because their self-concept is constituted through the community the constraint preserves; leaving means losing the only identity framework they have. The constraint extracts their future autonomy in the name of preserving the identity that constitutes them.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_children_denied_autonomy, payer,
    powerless, biographical, identity_locked, national).

% Schools, courts, publications, and communal organizations whose purpose and funding derive from maintaining the herem boundary. They administer the separation, police the margins, and produce the hermeneutics that justify it. Benefit materially and institutionally from the constraint's enforcement. Mobile — they can pivot to adjacent boundary-maintenance work if this specific constraint weakens.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, boundary_maintenance_institutions, beneficiary,
    organized, generational, mobile, global).

% Scholars, ethicists, and comparative religion analysts who study the herem command and its readings from outside the covenant commitment. They see the full structural pattern — the kernel contest, the reading-specific victim sets, the extraction-coordination fusion — but do not bear the constraint's costs or collect its benefits. Their exit is analytical: they can change frameworks without existential cost.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, hermeneutical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, religious_leadership_authorities).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves covenant community identity across generations by establishing a categorical boundary that defines membership and excludes contamination. Solves the coordination problem of 'who is us' in environments where assimilation threatens collective coherence.
% TRANSFER_FUNCTION: Moves marriage autonomy, life prospects, and physical security from designated outsiders, intermarried families, assimilated individuals, and covenant children to religious leadership authorities and boundary maintenance institutions, mediated through the covenant community's collective identity.
% ABSENT_VOICES: Designated outsider groups (Canaanites, Amalekites, and their historical successors) are structurally excluded — they are the objects of the constraint, not participants in its interpretation. Intermarried families and assimilated individuals are present but silenced by the constraint's logic (their dissent proves their contamination). Covenant children cannot yet speak. No external universalist ethic is admitted as a conversation partner — the divine mandate frame forecloses it.
% DISAPPEARANCE_RATIONALE: If the durable_separation_reading vanished overnight, covenant communities would lose their primary textual warrant for categorical separation and violence legitimation. Intermarriage would become a matter of conscience rather than covenantal betrayal. Designated outsider groups would lose their structural classification as existential threats. Religious leadership would lose a core institutional authority source. Boundary maintenance institutions would lose their founding mandate. The world would rearrange — but whether toward porous universalism or a new boundary logic is contested.
% FOUNDING_PROBLEM: Ancient Israel's identity preservation in a polytheistic Near Eastern environment where assimilation into Canaanite religion and culture threatened covenantal distinctiveness. The herem command provided a divine mandate for radical separation as the only viable survival strategy.
% FOUNDING_PROBLEM_CORROBORATION: The durable_separation_reading's beneficiaries (religious leadership, boundary institutions) attest the problem is live — assimilation threats persist in new forms. Contextual_supersession_reading proponents (prophetic universalism interpreters, Christian covenant theologians, historical-critical scholars) attest the problem is dead — the historical conditions are gone and the mandate is morally superseded. Allegorical_displacement_reading proponents attest the problem was never about ethnic separation but internal spiritual warfare. No neutral arbiter exists; the corroboration split mirrors the kernel contest itself.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint structurally denies marriage autonomy to all covenant members and designates all non-covenant outsiders as legitimate targets of exclusion or violence — a transfer of autonomy and life-prospects from victims to boundary-maintenance institutions. Suppression (0.78) is high because the constraint's persistence depends on active enforcement of boundaries (communal discipline, textual authority, social pressure) and the suppression of internal dissent and external alternatives. Theater ratio (0.28) is moderate: the identity-preservation function is genuine (communities do maintain coherence through boundaries) but a growing share of enforcement activity serves boundary maintenance for its own sake rather than any measurable identity threat. Accessibility collapse (0.72) is high because once the divine mandate frame is accepted, alternatives (porous boundaries, universalist ethics) appear as covenantal betrayal. Resistance (0.45) is moderate: internal dissent exists but is structurally suppressed as faithlessness; external critique is dismissed as irrelevant to covenant obligation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (religious leadership) experiences this as genuine coordination — they administer a structure that preserves the community they serve. The payer/victim seats (designated outsiders, intermarried families, covenant children) experience the same structure as enforced extraction with no exit. The beneficiary seat (covenant members) is split: they receive identity-coherence benefits but pay autonomy costs. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership authorities are structural beneficiaries (d ≈ 0.15): they control interpretation, gain institutional authority from enforcement, and face arbitrage-grade exit (can move between communities or reinterpret). Covenant community members are dual-positioned: beneficiaries of identity preservation (d ≈ 0.3) but payers of autonomy costs (d ≈ 0.65 for marriage decisions) — net directionality moderate. Designated outsider groups are full targets (d ≈ 0.95): trapped, powerless, existentially threatened. Intermarried families and assimilated individuals are high-target (d ≈ 0.8): constrained exit, identity-locked by community definition. Covenant children are identity-locked targets (d ≈ 0.85): autonomy pre-structured before agency forms. Boundary maintenance institutions are beneficiaries (d ≈ 0.2): organized, mobile, institutional purpose derives from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (identity preservation in polytheistic environment) is contested as live vs. dead. The constraint persists with high extraction and suppression despite the founding conditions (Canaanite nations, ancient Near Eastern polytheism) being historically gone. This is mandatrophy: the arrangement's mandate (identity preservation) has been superseded by its own enforcement machinery (boundary maintenance as institutional self-preservation). The durable_separation_reading prevents recognition of mandatrophy by treating the founding problem as perpetually live through the 'timeless divine mandate' frame. The engine's computed type divergence from claimed_type will reveal this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Is the durable_separation_reading one instantiation of a contested kernel (herem_command_dt7) with sibling readings (contextual_supersession_reading, allegorical_displacement_reading), and what does this reading structurally commit to that the siblings do not?',
    'Comparative reading analysis: this reading asserts timeless divine mandate for categorical separation; contextual_supersession asserts historical boundedness and moral supersession; allegorical_displacement asserts internal spiritual warfare. The structural delta is this reading''s expansive victim set (all non-covenant outsiders as contamination threat) and violence legitimation through divine command obedience.',
    'Confirms this constraint is one reading among multiple, not the kernel itself. Routes committer structure through omega apparatus rather than standard fields. Prevents averaging ε across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committee frame: this constraint is one reading of a contested kernel').

omega_variable(
    divine_command_extraction_boundary,
    'Is the high extractiveness on intermarriage autonomy and categorical separation a genuine coordination function (identity preservation) or extraction riding on divine authority cover?',
    'Counterfactual test: if the identity preservation function could be achieved without categorical separation and violence against outsiders, the extraction is separable from coordination. Historical comparison with communities maintaining identity through porous boundaries.',
    'If coordination and extraction are inseparable, this reading''s claimed tangled_rope structure holds. If separable, the reading masks a snare behind divine mandate. Engine classification will reflect the structural data authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_command_extraction_boundary, conceptual, 'Whether divine command cover obscures extractive structure').

omega_variable(
    outsider_designation_mechanism,
    'Who designates ''outsiders'' and by what authority? Is the outsider category fixed by the text or dynamically expanded by the interpreting community?',
    'Trace historical application: which groups were designated as ''Canaanites/Amalekites'' in different periods? Does the reading contain internal criteria for outsider designation or defer to living authority?',
    'If outsider designation is dynamically expanded by leadership, the victim set is open-ended and extractive surface grows. If fixed by text, the extraction surface is bounded. Affects suppression and accessibility_collapse scores.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(outsider_designation_mechanism, empirical, 'Mechanism and scope of outsider designation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(here_tr_t50, herem_command_dt7__durable_separation_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(here_tr_t150, herem_command_dt7__durable_separation_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(here_tr_t200, herem_command_dt7__durable_separation_reading, theater_ratio, 200, 0.25).
narrative_ontology:measurement(here_tr_t250, herem_command_dt7__durable_separation_reading, theater_ratio, 250, 0.28).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(here_be_t50, herem_command_dt7__durable_separation_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(here_be_t150, herem_command_dt7__durable_separation_reading, base_extractiveness, 150, 0.73).
narrative_ontology:measurement(here_be_t200, herem_command_dt7__durable_separation_reading, base_extractiveness, 200, 0.78).
narrative_ontology:measurement(here_be_t250, herem_command_dt7__durable_separation_reading, base_extractiveness, 250, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(here_su_t50, herem_command_dt7__durable_separation_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.6).
narrative_ontology:measurement(here_su_t150, herem_command_dt7__durable_separation_reading, suppression_requirement, 150, 0.66).
narrative_ontology:measurement(here_su_t200, herem_command_dt7__durable_separation_reading, suppression_requirement, 200, 0.72).
narrative_ontology:measurement(here_su_t250, herem_command_dt7__durable_separation_reading, suppression_requirement, 250, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.08).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of herem_command_dt7 kernel. This reading (durable_separation) asserts timeless divine mandate with high extractiveness on intermarriage autonomy and expansive victim set. contextual_supersession_reading asserts historical boundedness and moral supersession (lower extractiveness, bounded victim set). allegorical_displacement_reading asserts internal spiritual warfare (minimal extractiveness, metaphorical victim set). The three readings share the kernel text but instantiate structurally distinct constraints with different ε, beneficiaries, victims, and types. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, institutional, 0.15).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, moderate, 0.65).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
