% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Statehood Criteria (Democratic + Rights-Based Legitimacy)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The hybrid reading instantiates statehood as requiring BOTH objective
 *   criteria (Montevideo: territory, population, government, relations
 *   capacity) AND normative legitimacy (democratic governance, human rights
 *   compliance, non-aggression). This is ONE READING of the contested
 *   Montevideo kernel; the sibling declaratory_reading treats the four
 *   objective criteria as sufficient and statehood as a legal fact upon their
 *   satisfaction; the sibling constitutive_reading treats recognition by the
 *   existing state community as the constitutive gate. The hybrid reading
 *   occupies a middle position: it accepts the declaratory's objective
 *   foundation but adds a normative layer that liberal democratic states
 *   enforce through recognition practice. This creates an asymmetry:
 *   secessionist movements that meet objective criteria are blocked if they
 *   violate the normative gate; established states that violate the normative
 *   gate retain sovereignty through institutional recognition. The expected
 *   structural delta names the cost: non-liberal secessionists become victims
 *   (denial of recognition despite meeting objective criteria), liberal
 *   democratic states gain justification for denial, and humanitarian
 *   intervention gains legal cover. The constraint is CLAIMED as tangled_rope
 *   (coordination of legitimacy + asymmetric enforcement) while the authored
 *   metrics describe substantial extraction (0.68) with moderately rising
 *   theater (0.41) — the hybrid rule's justification as neutral law
 *   increasingly masks its selective enforcement by liberal coalitions.
 *
 * KEY AGENTS:
 *   - liberal_democratic_established_states: agenda-setter (institutional power, define and enforce normative gate)
 *   - non_liberal_secessionist_movements: payer (moderate power, trapped exit — blocked by normative requirement)
 *   - human_rights_advocates: beneficiary (organized power, mobile exit — invested in normative framing)
 *   - existing_parent_states: agenda-setter + beneficiary (institutional power, constrained exit — benefit from ambiguous rule)
 *   - stateless_populations: payer + excluded (powerless, identity-locked — doubly foreclosed from statehood)
 *   - humanitarian_interveners: beneficiary (powerful, arbitrage exit — gain legal cover for intervention)
 *   - international_legal_community: observer (institutional, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Statehood Criteria (Democratic + Rights-Based Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '5e1d34a4-7dc1-42c5-abb8-26935cec0bbb').
narrative_ontology:cs_kernel_codification('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', fixed_text).
narrative_ontology:cs_authority_grounding('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', extraction).
narrative_ontology:cs_interpretation_layer_present('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb').
narrative_ontology:cs_reading_relation('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', foundational, statehood_requires_normative_legitimacy).
narrative_ontology:cs_axiom_status(statehood_requires_normative_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', statehood_requires_normative_legitimacy, deontological).
narrative_ontology:cs_axiom('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', foundational, liberal_democratic_governance_validates_sovereignty).
narrative_ontology:cs_axiom_status(liberal_democratic_governance_validates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', liberal_democratic_governance_validates_sovereignty, conventional).
narrative_ontology:cs_reference_frame('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', objective_criteria_plus_liberal_normative_filter).
narrative_ontology:cs_drift_state('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', contemporary_selective_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5e1d34a4-7dc1-42c5-abb8-26935cec0bbb', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_established_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, human_rights_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_independence_claimants).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, stateless_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, existing_parent_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, humanitarian_interveners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, EU member states, Canada, Australia, and allied democracies set the recognition standard through diplomatic practice and legal precedent. They author and enforce the normative layer — the requirement that statehood claimants demonstrate democratic governance and human rights compliance — layered atop the Montevideo objective criteria. They justify this as protecting international order and preventing rogue regimes from achieving sovereignty. They control the practical gate: denied recognition means exclusion from UN membership, international finance, and diplomatic standing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_established_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Independence movements (Transnistria, Northern Cyprus, Donetsk/Luhansk, various authoritarian-controlled territories) meet the four Montevideo objective criteria but are denied recognition because they lack democratic governance structures or have poor human rights records. They face a binding constraint: meet the objective criteria, lose the normative gate; meet the normative gate, dissolve into the parent state's democratic structure. Their exit is foreclosed by the very requirement meant to validate them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    moderate, biographical, trapped, regional).

% International human rights organizations, civil society networks, and normative lawyers argue that linking statehood to human rights compliance incentivizes governance reforms and prevents authoritarian consolidation through sovereignty capture. They benefit from the constraint's framing: it positions human rights as a condition of international legitimacy, strengthening their advocacy claim. They have exit options (can shift focus to other advocacy domains) but are invested in this normative architecture.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, human_rights_advocates, beneficiary,
    organized, biographical, mobile, global).

% States whose territory is claimed by secessionists (Russia, Serbia, Syria, Azerbaijan, Ukraine) benefit from the hybrid rule's ambiguity: when the secessionist movement is authoritarian or rights-violating, denial of recognition supports territorial integrity; when the parent state itself violates human rights, the same standard threatens to delegitimize their own claim to the territory. They enforce the constraint through non-recognition and military/coercive means.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, existing_parent_states, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, existing_parent_states, beneficiary).

% Populations without any state protection (Rohingya, Kurds, Palestinians, Sahrawi) are locked into legal non-existence. They cannot meet the Montevideo criteria (lack territorial control and effective government) and simultaneously face denial when movements claiming to represent them show authoritarian tendencies or poor human rights practices. The constraint creates a bind: they need statehood but are structurally foreclosed from achieving it under either gate.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, stateless_populations, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, stateless_populations, excluded).

% International courts, legal scholars, and treaty interpretation bodies (ICJ, Institute of International Law, UN General Assembly legal committees) observe and parse the hybrid rule. They produce verdicts on recognition cases and calibrate how the normative layer is applied. Their analytical seat enables them to expose the rule's internal contradictions and pressure points.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Western military and humanitarian intervention frameworks gain legal cover from the hybrid rule: denying statehood recognition to rights-violating regimes and their secessionist allies can justify external intervention framed as humanitarian protection or regime change. Interventionist states benefit from normative authority the hybrid standard provides, even as they apply it selectively.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, humanitarian_interveners, beneficiary,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_established_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common international legitimacy test for statehood that rests on both capacity (Montevideo's objective criteria: territory, population, government, capacity for relations) and values (democratic governance, human rights compliance, non-aggression). The coordination function is to prevent both the proliferation of failed or predatory micro-states AND the absorption of breakaway regions whose populations lack genuine self-determination into authoritarian or non-rights-compliant frameworks.
% TRANSFER_FUNCTION: Transfers the sovereign capacity to be recognized as a state — the ability to enter treaties, secure membership in international bodies, access international finance and diplomatic standing — from secessionist movements that meet objective but not normative criteria, to liberal democratic states and their favored movements. The extraction moves recognition authority and legitimacy from those denied it (non-liberal movements) to those who control the gate (liberal established states).
% ABSENT_VOICES: Populations under authoritarian independence movements (who might prefer statehood under any governance) are structurally excluded from the legitimacy conversation. Realist states skeptical of human rights as international law (Russia, China, and others) contest the normative layer but are excluded from setting it. Leftist or socialist independence movements that reject liberal democratic framing are pre-classified as illegitimate without voice in the framework design.
% DISAPPEARANCE_RATIONALE: If the hybrid statehood criteria vanished and only the four Montevideo objective criteria remained, recognition practice would shift dramatically: territories meeting objective criteria but led by authoritarian governments would achieve recognized statehood within years (Transnistria, Northern Cyprus, Donetsk, Luhansk, and others would gain seats in international bodies). International law would operate on pure capacity, not values. The normative gate exists; its removal would reorganize the map of recognized sovereignty.
% FOUNDING_PROBLEM: The original Montevideo Convention (1933) provided only objective criteria for statehood, creating a gap: capacity alone does not guarantee legitimacy. The founding problem is that a fascist dictatorship could meet the four criteria and achieve recognized sovereignty, threatening international order. The hybrid reading adds that statehood requires normative legitimacy — democratic governance, human rights respect, non-aggression — to filter out predatory or illiberal regimes.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democracies and human rights organizations attest the founding problem remains live: they point to authoritarian independence movements and failed states as evidence that objective criteria alone are insufficient. Realist states and non-liberal secessionists attest the founding problem is the reverse: the real risk is liberal democracies using the normative gate to deny statehood to movements they dislike regardless of governance. International legal scholars outside the liberal consensus (e.g., from Global South jurisdictions, critical international law traditions) corroborate the realist reading, pointing to selective enforcement and post-hoc legitimacy narratives.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the hybrid gate transfers recognition authority and sovereign capacity to liberal democratic gatekeepers. Non-liberal movements meet objective criteria but are denied recognition; liberal states retain recognition despite normative violations (see Saudi Arabia, Israel on human rights; Russia/China on non-aggression). Suppression is high (0.72) because the denial of statehood recognition is enforced through military coercion (troops on borders, sanctions regimes, diplomatic isolation) and structural foreclosure — the normative gate cannot be 'exited' because meeting it requires dissolution into the parent state. Theater ratio (0.41) is moderate-rising: the justification as neutral law ('human rights are universal') increasingly masks selective application (enforcement against non-liberal movements, non-enforcement against liberal allies). The measurement series trace increasing professionalization of the theater — the rule is dressed up with more sophisticated human rights assessments and legal reasoning over the 40-year interval, but applied with stable selectivity.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal democratic agenda-setter seat, the constraint appears as coordinated legitimacy maintenance — filtering dangerous regimes and protecting international order through normative standards. From the non-liberal secessionist payer seat, the same constraint appears as structural foreclosure — objective capacity is necessary but no longer sufficient because the normative gate is controlled by rivals and set to exclude them. From the stateless population excluded seat, the constraint appears as doubly extractive — both gates (objective and normative) are independently impossible to satisfy (they lack territory and government, AND any movement claiming to represent them is likely non-liberal). The engine should compute these divergences from the structural data: beneficiary seats derive low directionality (they set and enforce), payer seats derive high directionality (they are blocked), excluded seats derive highest directionality (both gates are closed). This is precisely the seat divergence the hybrid reading instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic established states hold the agenda-setter role and control the normative gate's definition and enforcement (d near 0.0 — full beneficiaries, they set the rules). Non-liberal secessionist movements hold the payer role: they meet the objective criteria but are denied recognition because they fail the normative gate (d near 1.0 — full targets, extraction is denial of statehood). Stateless populations are in an excluded payer position: they cannot meet the objective criteria (lack territory/government) and would fail the normative gate even if they could (any independence movement is typically characterized as authoritarian or rights-violating) (d = 1.0 — maximally trapped). Human rights advocates benefit from the constraint's framing without bearing its costs (d near 0.2 — low extraction, they are designed beneficiaries). Humanitarian interveners benefit from the normative cover provided by the hybrid rule (d near 0.1 — they gain legal authority, minimal cost). Parent states have dual positioning: they benefit when the rule preserves territorial integrity against non-liberal secessionists (d near 0.2) but are vulnerable if the normative gate ever turns against them (d could spike to 0.5+, creating contention). The measurement series show steady extraction accumulation (base_extractiveness rising 0.52→0.68) with theater growing faster than function change (theater_ratio rising 0.25→0.41), indicating increasing professionalization of the narrative cover.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing predatory regimes from achieving sovereignty via objective criteria alone) was live and coherent in 1945–1990. By 2010, the problem's status became contested: liberal democracies and human rights advocates claim it remains live (new authoritarian independence movements emerge), while realist states and critical international law scholars claim it is dead (the real problem is selective enforcement and denial of statehood to non-liberal movements regardless of rights violations). The mandatrophy indicator is present: the constraint persists (high theater_ratio, active enforcement, significant stakeholder investment in the rules) while its justification has degraded (the normative gate is applied selectively enough that human rights advocates now dispute whether it actually prevents authoritarian consolidation — it seems to consolidate liberal coalitions instead). The hybrid reading does NOT resolve mandatrophy; it instantiates the contested status in structural form. A truly resolved mandatrophy reading would declare either that the normative gate no longer filters authoritarians (should be repealed) or that it does (evidence required). This reading stays in the contested space: high extraction, rising theater, but no consensus on whether the gate is earning its legitimacy claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_gate_selective_enforcement,
    'Is the normative legitimacy gate applied consistently across independence movements, or does liberal democratic state interest determine its enforcement?',
    'Comparative analysis of recognition decisions: audit cases where non-liberal movements met human rights standards (East Timor, Kosovo early independence movements) versus cases where liberal-aligned entities violated human rights standards but retained recognition (review Turkish Cyprus, Israeli settlements, Saudi Arabia diplomatic standing). If enforcement tracks liberal coalition interest rather than normative consistency, selective application is confirmed.',
    'If enforcement is selective, the constraint reclassifies from tangled_rope (genuine coordination + asymmetric extraction) to snare (pure extraction with normative cover). The beneficiary set narrows from ''liberal democracies + human rights advocates'' to ''liberal democratic coalitions.'' The extraction becomes coercive denial of statehood to non-aligned movements regardless of governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normative_gate_selective_enforcement, empirical, 'Whether the normative legitimacy gate is applied consistently or serves liberal coalition interests.').

omega_variable(
    coordinate_measurement_vs_normativity_capture,
    'Can the four Montevideo objective criteria be measured independently of the normative gate, or has the normative gate become conceptually inseparable from what counts as ''effective government''?',
    'Legal scholarship analysis: compare how ''effective government'' was assessed in the declaratory era (1945–1989, pre-normative-gate emphasis) versus post-1990 (normative criteria increasingly folded into effectiveness measures). If normative criteria have colonized the objective measurement itself (e.g., ''effective government'' now includes ''governs in accordance with rule of law''), the readings are no longer independent empirical alternatives.',
    'If the normative gate has colonized the objective criteria, the declaratory_reading becomes empirically indistinguishable from the hybrid reading in practice. The contest between readings shifts from empirical (do objective criteria suffice?) to purely narrative (what counts as ''effective?''). This reduces the kernel''s structural independence and suggests the contest is really about authority (who gets to define effectiveness), not substance (what criteria matter).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_measurement_vs_normativity_capture, conceptual, 'Whether the objective/normative distinction remains empirically separable or has been conceptually collapsed.').

omega_variable(
    humanitarian_intervention_moral_hazard,
    'Does the hybrid reading''s normative gate create a moral hazard for humanitarian intervention and regime change, where the denial of statehood recognition provides legal cover for external military action against non-liberal independence movements?',
    'Analysis of military interventions post-1990: track cases where denial of recognition to independence movements preceded or accompanied external military action (Kosovo, East Timor, Libya, Syria). Distinguish cases where intervention occurred despite recognition (Iraq 2003) versus where non-recognition enabled or justified intervention.',
    'If moral hazard is confirmed, the constraint''s extraction mechanism expands beyond recognition denial to include enabling military coercion and regime change. Humanitarian interveners (powerful institutional agents) gain extraction benefit beyond normative authority — they gain justification for force. The constraint moves toward snare classification for this stakeholder seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_moral_hazard, empirical, 'Whether the normative gate enables humanitarian intervention and regime change as secondary extraction mechanisms.').

omega_variable(
    kernel_reading_independence,
    'Are the three readings (declaratory, constitutive, hybrid) genuinely independent logical alternatives, or do they form a single continuum where ''normative legitimacy'' is simply how constitutive recognition is retrospectively justified?',
    'Formal logical analysis of reading premises: test whether denying any one reading''s core premise logically entails accepting another (e.g., does denying constitutive discretion logically entail accepting declaratory sufficiency, or can the hybrid reading reject both?). If all three readings can be held simultaneously by the same authority, they are not genuinely competing — they are different framings of discretionary recognition.',
    'If the readings are not logically independent, the kernel is under-determined: the contest is not about what the law is (an empirical question between three readings) but about what narrative justifies discretionary recognition (a political question). This would reframe the constraint from a legal boundary (which criteria apply?) to a power structure (who decides?).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_independence, conceptual, 'Whether the three readings represent genuinely independent logical alternatives or a single continuum of post-hoc justification.').

omega_variable(
    stateless_population_structural_exclusion,
    'Are stateless populations (Rohingya, Kurds, Palestinians, Sahrawi) structurally foreclosed from statehood under the hybrid reading, or do alternative pathways (regional autonomy, federalism, nation-building) provide exits?',
    'Case analysis: assess whether populations denied statehood under the hybrid reading have achieved meaningful self-determination through alternative arrangements (Dayton Bosnia, Lebanese confessionalism, autonomous regions). If autonomy or federal inclusion satisfies the founding problem (preventing authoritarian consolidation), then statehood denial is not pure extraction but redirection toward alternative coordination.',
    'If pathways exist, the extraction is less total but more complex: the constraint redirects rather than forecloses self-determination. If pathways do not exist (stateless populations remain in legal limbo under host states), the extraction is maximally total — they are permanently foreclosed from both gates (objective: lack government; normative: any independence movement is characterized as illiberal). The victim set''s structural position becomes clearer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stateless_population_structural_exclusion, empirical, 'Whether stateless populations have alternative self-determination pathways or face structural total foreclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mont_tr_t5, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(mont_tr_t10, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(mont_tr_t15, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(mont_tr_t25, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(mont_be_t5, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(mont_be_t10, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(mont_be_t15, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(mont_be_t25, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mont_su_t5, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(mont_su_t10, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(mont_su_t15, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(mont_su_t25, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, state_sovereignty_recognition_regime).

% DUAL FORMULATION NOTE:
% The montevideo_statehood_criteria kernel has three constraint stories corresponding to three readings: declaratory (objective criteria suffice, statehood is legal fact), constitutive (recognition by state community is constitutive), and hybrid (objective criteria plus normative legitimacy required). The three readings form a constraint family linked by network.affects_constraints. The declaratory and constitutive readings foreclose each other (one rules out the other in a single framework); the hybrid reading coexists with both while creating downstream pressure on each (acceptance of hybrid legitimacy undermines pure declaratory sufficiency AND pure constitutive discretion). The family's internal structure models how a single kernel (the text of the Montevideo Convention) instantiates multiple competing constraint topologies depending on how the normative layer is framed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, institutional, 0.05).
constraint_indexing:directionality_override(montevideo_statehood_criteria__hybrid_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
