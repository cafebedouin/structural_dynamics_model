% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Authentic Human Community Through Patient Participatory Labor (Jerusalem Reading)
 *   domain: political_theology/social_ethics/anthropology
 *
 * SUMMARY:
 *   The Jerusalem reading of the human transcendence pathway frames authentic
 *   community reconstruction as patient, participatory labor under divine
 *   blessing, where plurality (diversity of backgrounds, skills, theologies,
 *   recovery paces) becomes a resource for communion rather than a problem
 *   demanding uniformity. This reading instantiates one side of a contested
 *   kernel: against the Babel reading (unified systems as sufficiency) and
 *   the technocratic reading (transcendence as optimization), the Jerusalem
 *   reading holds that transcendence is received as grace through
 *   vulnerability, solidarity, and inclusive deliberation. The constraint
 *   operates primarily through formation and persuasion—modeling sacrifice of
 *   efficiency for solidarity, welcoming dissent, and justifying decisions by
 *   appeal to covenant rather than coercion. Extraction is low because the
 *   arrangement redistributes power toward marginalized populations rather
 *   than concentrating it; suppression is minimal because dissent and doubt
 *   are incorporated rather than suppressed.
 *
 * KEY AGENTS:
 *   - returning_exiles: the dispossessed populations whose restoration is the constraint's primary function; they hold powerless position but are moved to the center by the constraint's logic
 *   - local_prophetic_leadership: moderate-power actors who model the sacrifice of efficiency and set the pace of inclusive deliberation; they bear the coordination burden
 *   - technical_specialists: organized actors who sacrifice speed and control to participate in commons-building; they shift from hierarchy to solidarity
 *   - excluded_competitors: institutional actors outside the covenant community who mount resistance through alternative claims and material pressure
 *   - skeptical_insiders: moderate-power doubt-bearers whose dissent is heard and engaged rather than suppressed
 *   - external_theological_observers: analytical seat examining whether the low-extraction metrics reflect genuine solidarity or masked coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.28).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Authentic Human Community Through Patient Participatory Labor (Jerusalem Reading)").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "political_theology/social_ethics/anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '3a824634-6585-489a-a1ec-b51319a4b357').
narrative_ontology:cs_kernel_codification('3a824634-6585-489a-a1ec-b51319a4b357', distributed).
narrative_ontology:cs_authority_grounding('3a824634-6585-489a-a1ec-b51319a4b357', lineage).
narrative_ontology:cs_interpretation_layer_present('3a824634-6585-489a-a1ec-b51319a4b357').
narrative_ontology:cs_reading_relation('3a824634-6585-489a-a1ec-b51319a4b357', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a824634-6585-489a-a1ec-b51319a4b357', human_transcendence_pathway__technocratic_incarnational_reading, influences).
narrative_ontology:cs_axiom('3a824634-6585-489a-a1ec-b51319a4b357', foundational, transcendence_as_gift_through_vulnerability).
narrative_ontology:cs_axiom_status(transcendence_as_gift_through_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('3a824634-6585-489a-a1ec-b51319a4b357', transcendence_as_gift_through_vulnerability, deontological).
narrative_ontology:cs_axiom('3a824634-6585-489a-a1ec-b51319a4b357', foundational, plurality_as_communion_resource).
narrative_ontology:cs_axiom_status(plurality_as_communion_resource, holdable).
narrative_ontology:cs_axiom_grounding('3a824634-6585-489a-a1ec-b51319a4b357', plurality_as_communion_resource, conventional).
narrative_ontology:cs_axiom('3a824634-6585-489a-a1ec-b51319a4b357', secondary, participatory_labor_authenticity).
narrative_ontology:cs_axiom_status(participatory_labor_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('3a824634-6585-489a-a1ec-b51319a4b357', participatory_labor_authenticity, instrumental).
narrative_ontology:cs_reference_frame('3a824634-6585-489a-a1ec-b51319a4b357', covenant_community_restoration).
narrative_ontology:cs_drift_state('3a824634-6585-489a-a1ec-b51319a4b357', contemporary_post_displacement_contexts, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a824634-6585-489a-a1ec-b51319a4b357', '2026-06-12T14:33:22Z').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_communities).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, regenerated_social_fabric).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, technical_specialists).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, technical_specialists).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, skeptical_insiders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispossessed populations returning to rebuilding community after displacement. They gain restored identity, restored property claims, and reconstituted social bonds through participatory labor in reconstruction. Their labor is the primary material — they literally rebuild alongside others. The constraint's success means their marginal position becomes central to the reconstituted commons.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    powerless, generational, trapped, local).

% Exemplar figures (Nehemiah, Ezra in the textual kernel) articulate the vision of participatory rebuilding under divine blessing and model sacrifice of efficiency for solidarity. They set the pace (slow, inclusive), make room for dissent and alternative approaches, and justify the arrangement by appeal to covenant renewal rather than coercion. They bear the burden of holding the community together through material sacrifice.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, local_prophetic_leadership, agenda_setter,
    moderate, biographical, constrained, local).

% Masons, carpenters, administrators, scribes whose technical skill could optimize the rebuilding (build faster, more efficiently, centralize decisions). They sacrifice speed and control — work at the pace of inclusive deliberation, train others rather than hoard expertise, justify decisions to the whole community rather than imposing them. They gain solidarity and genuine social reintegration instead of professional hierarchy.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technical_specialists, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, technical_specialists, beneficiary).

% Neighboring powers and rival authorities (Samaria in the textual kernel) who would benefit from Jerusalem remaining fragmented or subordinate. They are not directly part of the community's deliberative process and mount resistance through interference, economic pressure, and alternative claims to legitimacy. Their exclusion is structural — they are not parties to the covenant.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, excluded_competitors, excluded,
    powerful, generational, arbitrage, regional).

% Community members who doubt the divine blessing framing or fear the participatory process will collapse into chaos. They sacrifice the comfort of either pure efficiency (faster building) or pure tradition (unchanged roles). Their doubts are heard, engaged with, and their resistance becomes part of the deliberative fabric rather than suppressed. The constraint acknowledges dissent rather than erasing it.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, skeptical_insiders, payer,
    moderate, biographical, constrained, local).

% Analytical seat examining whether the Jerusalem reading's claims about divine blessing, participatory authenticity, and plurality-preserving communion are descriptively accurate or ideological cover. They see the full structure and measure whether the low extraction metrics honestly reflect the constraint's operation or whether they mask coercion.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, external_theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).
narrative_ontology:fixing_cost_class(human_transcendence_pathway__jerusalem_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rebuilds fractured human community after displacement by integrating dispersed populations through shared labor under a common vision (covenant renewal). Solves the problem of how to reconstitute social bonds, property claims, and shared identity without replicating the hierarchies that created the original dispossession. Coordinates diversity of skills, backgrounds, and theological positions into functioning commons.
% TRANSFER_FUNCTION: Moves labor contribution, technical skill, and deferred individual efficiency gains (the sacrifice of technical speed) INTO the collective rebuilding process. The return flow is restored social position, renewed identity claims, and access to the reconstituted commons. The constraint transfers power from external authorities and technical specialists to the participatory community as a whole.
% ABSENT_VOICES: Neighboring powers excluded by the covenant boundary; secular authorities who would solve the problem through pure technical optimization; skeptical insiders whose doubts might counsel retreat or collaboration with external powers. The Jerusalem reading voices the excluded voices of the marginalized — but their exclusion from decision-making is precisely the dispersal that is being repaired, so their inclusion IS the constraint's function.
% DISAPPEARANCE_RATIONALE: If the constraint—the participatory labor framework under divine blessing, the sacrifice of efficiency for solidarity—vanished, reconstruction would proceed via either technocratic optimization (faster, more unequal, externally dependent) or external imposition (recolonization by neighboring powers). The community as an autonomous, self-governing, plural entity would not exist; returned exiles would remain marginal laborers in someone else's project rather than agents of their own restoration.
% FOUNDING_PROBLEM: How to reconstitute an authentic human community after collective trauma and dispersal, in a way that repairs the relational bonds and identity fractures that dispossession created, without replicating the power structures that enabled dispossession in the first place.
% FOUNDING_PROBLEM_CORROBORATION: The prophetic tradition (Nehemiah, Ezra, Isaiah 61) testifies to the founding problem and its stakes. Contemporary communities rebuilding after displacement (post-conflict reconciliation, marginalized-community self-determination, decolonial movements) attest to the same problem. External observers in theology, political philosophy, and development ethics corroborate that the problem—how to rebuild authentically after displacement—remains live in contemporary contexts.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 baseline, slight decline over interval) because the constraint explicitly redistributes power and material access toward marginalized populations (returning exiles) rather than concentrating benefit. The measured trend shows slight decline then stabilization, reflecting the constraint's maturation: initial formation overhead decreases as practices become habituated, then stabilizes as the community faces ongoing external pressure (excluded competitors) and internal skepticism. Suppression is low (0.15) because the constraint's persistence does not depend on coercion—it depends on formation, modeling, and persuasion. Dissent is not suppressed; it is engaged and incorporated into deliberative process. Theater is very low (0.08) because the participatory function is genuine: the labor IS the rebuilding, the deliberation IS the governance, the sacrifice IS the solidarity. There is minimal performative overhead masking a different real function. The measurement series uses one shared time grid across all three metrics so the constraint's stability picture is clear: this is a low-extraction, low-suppression, low-theater arrangement that maintains remarkably stable operation over time precisely because it does not depend on coercion or performance.
 *
 * PERSPECTIVAL GAP:
 *   The Jerusalem reading SHOULD compute identically across seats because it explicitly rejects private seats: the constraint's logic is that all participating agents share one common good (community restoration, divine blessing) and that diversity is preserved as a resource FOR that common good, not as a zone of competing interests. This is the reading's deepest claim—if seats compute differently, the reading has failed its own standard of authentic communion. However, the excluded competitors and skeptical insiders will experience the constraint differently: competitors experience exclusion and resistance, skeptics experience uncertainty and discomfort. The gap is not that the constraint operates differently for different people—it is that it operates LESS FULLY for those outside the covenant frame. The engine should detect this not as seat divergence but as a sharp inclusion/exclusion boundary: inside the reading's frame, seats align; outside it, the constraint appears as imposition.
 *
 * DIRECTIONALITY LOGIC:
 *   Returning exiles and marginalized communities occupy the structural beneficiary position (d near 0.0): the constraint restores their agency, property claims, and social position. They are not trapped by the constraint; they are freed by it from marginality into participation. Technical specialists occupy a near-symmetric position (d near 0.5): they sacrifice individual professional optimization but gain social reintegration and genuine community—costs and benefits roughly balance. Skeptical insiders occupy a slight-target position (d near 0.55): they bear the discomfort of uncertainty and participatory process friction, but they retain voice and are not coerced. Excluded competitors occupy the full-target position (d near 1.0): they are structurally excluded from participation and their interests (maintaining Jerusalem's fragmentation or subordination) are actively negated by the constraint. The low overall extraction reflects the fact that the primary directionality flow is toward beneficiaries (returned exiles) rather than away from targets—this is redistributive rather than concentrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to rebuild authentically after displacement) remains live, corroborated by contemporary post-conflict and decolonial movements. The disappearance verdict (world_rearranges) reflects that the constraint's operation is genuinely constitutive: without it, reconstruction would proceed by technocratic or imperial means. This is not a piton—the function has not atrophied. Theater is very low because participatory labor IS the rebuilding; there is no shadow function masking the stated one. Mandatrophy does not apply: the founding problem is live and the constraint still serves it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_blessing_mechanism,
    'Is ''divine blessing'' the operative coordination mechanism, or is it a narrative frame for genuinely effective human solidarity practices?',
    'Ethnographic observation of community practices: do they persist when participants interpret ''divine blessing'' secularly (as motivation/morale/shared meaning-making), or do they depend on supernatural belief? Do secular communities using identical participatory practices without theological framing achieve comparable outcomes?',
    'If supernatural, the constraint is reading-specific and may not generalize to secular contexts; if divine blessing is narrative wrapper for solidarity practices, the constraint generalizes but the reading''s theological claim is unfalsifiable-by-function. The measured low extraction holds either way, but the mechanism differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_blessing_mechanism, empirical, 'Whether the constraint''s coordination mechanism is theological or social-practice-based').

omega_variable(
    efficiency_sacrifice_sustainability,
    'Can the constraint sustain the sacrifice of technical efficiency indefinitely, or does material scarcity eventually force optimization that collapses participatory process?',
    'Long-term observation (generational scale): does participatory pace persist across resource constraints, or do crises revert to centralized command? Do communities using participatory rebuilding eventually reach a threshold where efficiency demands override inclusion?',
    'If efficiency reasserts inevitably, the constraint is temporary (scaffold) rather than stable rope—its theater_ratio would rise as performative inclusivity masks real top-down operation. If efficiency and participation remain decoupled, the rope classification holds and the reading''s claim to authentic transcendence is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficiency_sacrifice_sustainability, empirical, 'Whether the efficiency-solidarity trade-off holds under resource stress').

omega_variable(
    plurality_preservation_vs_cohesion,
    'Does preserving plurality as a resource actually prevent the crystallization of new hierarchies, or does participatory process generate new forms of marginalization (of doubters, minorities, slowest members)?',
    'Participatory audit: track which voices shape decisions over time, whether skeptical insiders'' dissent shifts decisions, whether new minorities emerge as structurally excluded. Compare decision velocity and outcomes under high vs. low internal plurality.',
    'If plurality crystallizes new hierarchies, the reading''s core claim (plurality AS resource, not problem to manage) is contradicted—the constraint becomes theater masking hierarchy reassertion. If genuine plurality-preservation holds, the claim stands and the low-extraction metrics reflect actual inversion of power flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plurality_preservation_vs_cohesion, empirical, 'Whether participatory process genuinely preserves plurality or reproduces new hierarchies').

omega_variable(
    kernel_reading_foreclosure,
    'Do the premises of the Jerusalem reading logically foreclose the Babel reading (unified systems suffice), or do they coexist as live positions held by different communities?',
    'Conceptual analysis: if humans genuinely transcend through vulnerable participation in grace, does that claim contradict the claim that unified systems achieve human flourishing? Or can both be held—communities of grace accepting some, technocratic communities building others? The foreclosure is conceptual, not empirical.',
    'If foreclosed: the readings are mutually exclusive and cannot coexist in one framework; if coexistent, both remain live options and the kernel dispute is genuine. This determines the reading_relations value (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the Jerusalem and Babel readings are logically incompatible or can coexist').

omega_variable(
    committer_frame_adequacy,
    'Does the Jerusalem reading''s status as ONE instantiation of a contested kernel mean its measured low extraction is an artifact of how the reading FRAMES the constraint, rather than a feature of the constraint''s actual operation?',
    'Comparison with sibling readings: author constraint_human_transcendence_pathway__babel_reading and constraint_human_transcendence_pathway__technocratic_incarnational_reading, measuring the same community processes under different reading frames. If extraction differs dramatically across readings of the same facts, the divergence indicates reading-frame dependency; if it holds, the measurements are robust to framing.',
    'If reading-dependent: this constraint''s low extraction may reflect the Jerusalem reading''s deliberate inversion of power narrative rather than objective operation; if robust, the low extraction reflects genuine solidarity practices observable under any frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_adequacy, conceptual, 'Whether the constraint''s measured metrics are robust to reading-frame changes or frame-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__jerusalem_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(huma_tr_t50, human_transcendence_pathway__jerusalem_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement(huma_be_t50, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(huma_su_t30, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(huma_su_t50, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.1).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_incarnational_reading).

% DUAL FORMULATION NOTE:
% The human_transcendence_pathway kernel decomposes into three structurally distinct constraints, each instantiating a different reading's response to the question: how do humans transcend their limits? The Jerusalem reading (this story) frames transcendence as gift received through vulnerable participation in covenant community. The Babel reading frames transcendence as achievable through unified technological/linguistic systems without transcendent authority. The technocratic reading frames transcendence as achievable through technological optimization and posthuman enhancement. These are not alternative measurements of one constraint—they are different constraints with different epsilon values, different victim/beneficiary structures, and different persistence mechanisms. The Jerusalem reading influences both siblings by offering an alternative grounding for human dignity and flourishing that undermines the premises of the other readings; it coexists with them (all remain live positions) but creates structural pressure on the technocratic premise (if grace is gift, then optimization-as-transcendence is incoherent) and the Babel premise (if community is constitutive of human transcendence, then unified systems may achieve efficiency but cannot achieve authentic human flourishing). Link all three stories via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
