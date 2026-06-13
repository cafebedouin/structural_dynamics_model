% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_governance_legitimacy__magisterial_subsidiarity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: AI Governance Legitimacy via Magisterial Subsidiarity Reading
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   The Roman Catholic Magisterium — claiming apostolic succession and 2,000
 *   years of moral teaching authority — articulates AI governance legitimacy
 *   through Catholic Social Doctrine principles: common good (technology must
 *   serve all, not concentrate power), subsidiarity (decisions should be made
 *   at the lowest capable level, not captured by distant elites), solidarity
 *   (the most vulnerable deserve priority protection), and universal
 *   destination of goods (technology cannot serve extraction and
 *   dispossession). This reading demands that AI systems be subordinated to
 *   human dignity through transparent accountability, participatory
 *   governance including affected communities, and explicit constraint of
 *   military and finance applications. The constraint entangles genuine
 *   coordination problems (ensuring technology serves human flourishing, not
 *   merely profit) with asymmetric extraction (imposing magisterial
 *   interpretive authority on secular technology establishments that claim
 *   efficiency, not dignity, as the primary legitimacy criterion). The
 *   measurement series track the constraint's evolution from peripheral
 *   theological claim (1960: Catholic Social Doctrine emerging from Vatican
 *   II) through institutional strength (1981: John Paul II's personalist
 *   philosophy; 2000: globalization critiques) to current articulation
 *   (2015-2026: Francis's environmental and technology ethics). Theater ratio
 *   rises modestly as enforcement mechanisms become more elaborate while core
 *   subordination of technology to dignity remains contested.
 *
 * KEY AGENTS:
 *   - magisterium: authoritative interpreter of legitimacy; sets the normative frame; claims apostolic authority grounded in tradition
 *   - workers_displaced_by_automation: beneficiary; face technological unemployment; lack voice in technology governance but benefit from claim that dignity supersedes efficiency
 *   - global_south_communities: beneficiary; experience algorithmic colonialism; benefit from universal destination principle but constrained exit
 *   - marginalized_populations: beneficiary + payer; benefit from solidarity principle but bear costs of enforcement mechanisms; identity-locked exposure to surveillance AI
 *   - private_technology_monopolies: payer; face transparency and accountability demands that reduce autonomy and competitive speed
 *   - military_industrial_technology_complex: payer; face restrictions on autonomous weapons and surveillance; constrained by state security doctrine but wield institutional resistance
 *   - extractive_finance_platforms: payer; face subordination of algorithmic optimization to human flourishing; can arbitrage to unregulated zones
 *   - ecclesial_networks_and_civil_society: agenda_setter + observer; operationalizes magisterial teaching; can mobilize moral suasion; has mobile exit
 *   - democratic_pluralist_constituencies: excluded; would argue governance legitimacy must emerge from inclusive deliberation, not magisterial monopoly; structurally subordinated
 *   - secular_technocratic_establishment: observer; represents efficiency-first governance; can dismiss constraint as sectarian
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.48).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.52).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "AI Governance Legitimacy via Magisterial Subsidiarity Reading").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'd4f1dc5b-4843-49a4-8bce-b98d6eb31bf5').
narrative_ontology:cs_kernel_codification('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', formalized).
narrative_ontology:cs_authority_grounding('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', lineage).
narrative_ontology:cs_interpretation_layer_present('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5').
narrative_ontology:cs_reading_relation('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_reading_relation('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_axiom('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', foundational, magisterial_authority_ai_governance).
narrative_ontology:cs_axiom_status(magisterial_authority_ai_governance, holdable).
narrative_ontology:cs_axiom_grounding('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', magisterial_authority_ai_governance, deontological).
narrative_ontology:cs_axiom('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', foundational, human_dignity_subordinates_efficiency).
narrative_ontology:cs_axiom_status(human_dignity_subordinates_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', human_dignity_subordinates_efficiency, deontological).
narrative_ontology:cs_axiom('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', secondary, technology_serves_common_good_not_extraction).
narrative_ontology:cs_axiom_status(technology_serves_common_good_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', technology_serves_common_good_not_extraction, instrumental).
narrative_ontology:cs_reference_frame('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', catholic_social_doctrine_teaching_authority).
narrative_ontology:cs_drift_state('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', post_industrial_digital_capitalism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4f1dc5b-4843-49a4-8bce-b98d6eb31bf5', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_displaced_by_automation).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_protecting_children_from_harm).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations_in_high_risk_zones).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_technology_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_technology_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_platforms).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, unregulated_ai_research_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_protecting_children).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations_in_high_risk_zones).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_primacy_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, solidarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Catholic Social Doctrine and issues encyclicals, papal statements, and Vatican teaching documents on AI governance. Claims doctrinal authority grounded in apostolic succession and the Church's 2,000-year tradition of moral teaching. Sets the legitimacy standard against which technology governance is measured within the tradition. Does not itself regulate AI systems but establishes the normative framework and calls for enforcement by civil authorities.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Face technological unemployment and wage pressure as AI systems displace labor. The magisterial reading demands that technology be subordinated to human dignity and that economic systems protect workers' right to meaningful work and just wages. They benefit from the constraint's assertion that automation cannot be justified purely on efficiency grounds without attending to human flourishing. However, they have no exit from labor markets and face suppression of their voice in technology governance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, workers_displaced_by_automation, beneficiary,
    powerless, biographical, trapped, global).

% Experience AI-driven resource extraction, algorithmic colonialism, and data sovereignty violations. The magisterial reading's principle of universal destination of goods and solidarity demands that technology not deepen structural inequality or serve extractive Northern capital. They benefit from the constraint's assertion that technological systems must be evaluated for their impact on the most vulnerable. Exit is constrained by global economic integration and lack of alternative technology platforms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_communities, beneficiary,
    moderate, generational, constrained, global).

% Seek to protect children from algorithmic manipulation, exploitative content, and psychologically addictive platform design. The magisterial reading demands that technology companies respect the dignity of the human person and the integrity of family formation. Families benefit from the constraint's assertion that profit maximization cannot override developmental wellbeing. Exit is constrained by the ubiquity of digital systems and economic necessity.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_protecting_children, beneficiary,
    moderate, biographical, constrained, global).

% Become targets of surveillance capitalism, algorithmic discrimination, and military-grade AI systems. The magisterial reading's solidarity principle demands that the weakest be protected first. They benefit from the constraint's assertion that technology governance must prioritize harm to the vulnerable. However, they often bear the costs of enforcement (exclusion from platforms, data collection to demonstrate compliance) and cannot escape identity-based targeting. Identity-locked: their status as vulnerable populations makes them permanently exposed to these systems.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations_in_high_risk_zones, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_populations_in_high_risk_zones, payer).

% Face demands to subordinate profit maximization to human dignity criteria, open source code and algorithms for scrutiny, implement participatory governance in content moderation, and internalize costs of harm. The constraint imposes transparency and accountability burdens that reduce operational autonomy and competitive speed. They can exit specific jurisdictions but not the constraint itself once it becomes binding international norm. They argue the constraint is illegitimate imposition of external values on market mechanisms.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_technology_monopolies, payer,
    powerful, biographical, arbitrage, global).

% Develops autonomous weapons, surveillance systems, and AI for strategic dominance. The magisterial reading demands that technology not serve death-dealing but must uphold human dignity even of adversaries. The constraint imposes restrictions on military applications, transparency requirements that undermine operational security, and participatory governance models that conflict with command-and-control. Exit is constrained by state power and security doctrine but they wield institutional power to resist the constraint.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_technology_complex, payer,
    institutional, generational, constrained, global).

% Use algorithmic systems to maximize predatory lending, high-frequency extraction, and debt-trap design targeting vulnerable populations. The magisterial reading's universal destination of goods principle demands that finance serve the common good, not extraction. The constraint requires them to subordinate algorithmic optimization to human flourishing. They can arbitrage to unregulated zones but face reputational pressure and investor restriction as the norm spreads.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_platforms, payer,
    institutional, biographical, arbitrage, global).

% Pursue research frontiers (AGI, synthetic biology, brain-computer interfaces) guided primarily by feasibility and competitive positioning rather than ethical framework integration. The constraint demands that all research be embedded within accountability structures, participatory governance, and explicit connection to the common good. This slows pace, increases cost, and subordinates pure exploration to constraint satisfaction. Exit is constrained by funding sources that increasingly require ethical compliance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, unregulated_ai_research_establishments, payer,
    organized, biographical, constrained, global).

% Operationalizes magisterial teaching through advocacy, civil disobedience, international law advocacy, and building alternative technology ecosystems. Organizes workers, environmental movements, and indigenous communities around the constraint. Holds the Magisterium accountable to its own teaching. Can mobilize moral suasion and reputation costs. Has mobile exit: can shift focus to other domains or engage in critical theology that challenges the Magisterium's interpretation.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, ecclesial_networks_and_civil_society, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, ecclesial_networks_and_civil_society, observer).

% Represents governmental technology policy, STEM institutional authority, and global governance forums that operate on efficiency and innovation-first logics. Observes the constraint as a competing legitimacy claim. Has no direct interest but wields institutional power to resist integration of the magisterial framework. Analytical exit: can dismiss the constraint as sectarian or aspirational without binding force.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, secular_technocratic_establishment, observer,
    institutional, biographical, analytical, global).

% Would argue that AI governance legitimacy must emerge from inclusive democratic deliberation across secular and religious voices, without granting interpretive monopoly to any single tradition. They are structurally excluded from this constraint's authority structure because the Magisterium claims authoritative teaching on what counts as legitimate governance, foreclosing input from alternative tradition-keepers (Islamic jurisprudence, Buddhist ethics, indigenous wisdom, secular humanist philosophy). Their voices matter in the deliberation but are subordinated to the magisterial frame.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, democratic_pluralist_constituencies, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium).
narrative_ontology:fixing_cost_class(ai_governance_legitimacy__magisterial_subsidiarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates technology development and deployment around a unified framework of human dignity, subsidiarity, and the common good. Solves the governance problem of how to ensure AI systems serve human flourishing rather than merely accumulating power and profit. Creates a coherent normative space where workers, families, and marginalized populations have standing to demand that technology be evaluated against human dignity criteria, not efficiency alone.
% TRANSFER_FUNCTION: Transfers authority to interpret the legitimacy of AI governance from technical experts and market actors to the Magisterium and ecclesial networks. Transfers obligation from technology companies and military establishments to subordinate profit/power maximization to human dignity. Transfers resources (time, compliance infrastructure, research focus) from technology acceleration to harm-prevention and participatory governance. Transfers voice from shareholders and engineers to affected communities and ecclesial networks.
% ABSENT_VOICES: Secular democratic pluralists would object that the Magisterium's claim to authoritative interpretation forecloses the inclusive deliberation necessary for legitimate governance. They argue that no single tradition should hold interpretive monopoly over legitimacy in diverse societies. Market libertarians would object that the constraint treats efficiency and innovation as subordinate goods rather than primary. Technocratic optimization proponents would object that it treats feasibility constraints as secondary to dignity claims. These voices are systematically excluded from the authority structure of the Magisterium, though some can participate as civil society actors.
% DISAPPEARANCE_RATIONALE: If the magisterial constraint vanished overnight, AI governance would revert primarily to technocratic optimization and market logics. Workers would lose the normative ground to claim technology must serve their flourishing. Global South communities would lose the frame of universal destination of goods. Marginalized populations would lose solidarity-based claims on platform design. Military and finance would operate AI without the requirement to subordinate systems to human dignity. Ecclesial networks would lose institutional backing for technology resistance. New governance legitimacy frameworks would emerge from technocratic, market, and democratic pluralist readings. The world would be reorganized around different authority claims.
% FOUNDING_PROBLEM: Technology development in the 21st century has become untethered from ethics and human dignity. Systems are designed for profit and power with no normative constraint to ensure they serve the common good or protect the vulnerable. Workers face displacement without protection. Global populations face algorithmic colonialism. Children face exploitation. The Magisterium undertook to articulate a comprehensive framework — grounded in 2,000 years of Catholic Social Doctrine — that subordinates technology to human dignity and demands political oversight.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by technology scholars, labor economists, Indigenous data sovereignty advocates, child development researchers, and human rights organizations — parties entirely outside the Magisterium. Their independent empirical research documents technological unemployment, algorithmic discrimination, platform addiction, and data extraction as live harms. The Magisterium's authority on WHETHER technology should be subordinated to dignity comes from its tradition; the evidence that subordination is needed comes from secular analysts. Civil society organizations, workers' movements, and international human rights bodies that are not ecclesial actors independently corroborate that technology governance detached from human dignity creates measurable harm.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_governance_legitimacy__magisterial_subsidiarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.48 at interval end, up from 0.12 in 1960 when Catholic Social Doctrine was peripheral to technology discourse. The constraint now imposes substantial obligation on technology establishments to subordinate profit/power to dignity criteria. Suppression measures 0.52, reflecting both the enforcement cost of sustaining magisterial authority claims in secular institutions and the real resistance from those bearing the obligation. Theater ratio (0.31) is modest relative to suppression: the constraint is not primarily theatrical, but a growing share of enforcement activity involves symbolic demonstration (papal letters, Vatican conferences, ecclesial statements) rather than binding institutional change. Accessibility_collapse (0.68) is moderately high: once the magisterial frame is understood, alternatives (pure efficiency, pure markets, pure democracy) appear inadequate from the magisterial perspective, though actors outside the tradition perceive real alternatives. Resistance (0.72) is high: significant constituencies (tech companies, military, libertarians) actively resist the constraint's authority claims. The claim/metric independence is deliberate: the constraint is CLAIMED as tangled_rope (genuinely coordinates dignity with efficiency; asymmetrically extracts magisterial authority on technology establishments) while the authored metrics describe the actual operation — the engine measures whether the empirical fit supports the claim or reveals it as snare (pure extraction of magisterial authority with no efficiency gain) or rope (genuine coordination without asymmetry).
 *
 * PERSPECTIVAL GAP:
 *   The Magisterium, ecclesial networks, and beneficiary populations (workers, Global South, families, marginalized) experience the constraint as necessary correction of technology's degradation of human dignity — a legitimate subordination of efficiency to human flourishing. Private technology monopolies and the technocratic establishment experience it as illegitimate imposition of external values on their operational logic — magisterial authority without democratic legitimation or market consent. Military and finance establishments experience it as constraint on strategic advantage and profit maximization — unacceptable cost of external accountability. Democratic pluralists experience it as sectarian monopoly on legitimacy that forecloses inclusive deliberation. The engine should compute these seats differently: magisterial authority has high d (beneficiary of the legitimacy-claim monopoly it wields) toward itself and low d toward beneficiary populations (it serves their interests); technology establishments have high d (targets of the subordination requirement); democratic pluralists have indeterminate d (neither collecting nor paying directly, excluded from authority structure). The perspectival divergence reflects the genuine structural asymmetry: magisterium claims authority, technology establishments resist it, beneficiaries lack voice but gain substantive protection.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (workers, Global South, families, marginalized populations) have low d because they benefit from the constraint's assertion that technology must serve their flourishing. Their exit options are constrained (trapped/identity_locked) so they cannot easily abandon the constraint even if it failed them; their power is low. The beneficiary role emerges from structural fact: the constraint protects their interests by subordinating technology to dignity criteria. Victims (private tech monopolies, military-industrial complex, extractive finance, unregulated research) have high d because they bear the obligation to subordinate their primary logic (profit, power, efficiency) to magisterial criteria. They have higher power and better exit options (arbitrage to unregulated zones, resist through institutional strength) so the effective extraction they experience is lower than the nominal suppression would suggest. The Magisterium itself occupies a peculiar position: it sets the constraint and benefits from authority recognition, so it has very low d toward itself. However, it also claims to serve the common good, not to extract rent, so the directionality is complicated by its own ideology — an omega variable addresses whether magisterial authority is genuinely universal or culturally contingent. Ecclesial networks and civil society that operationalize the constraint occupy mixed seats: they are partially beneficiaries (they benefit from the norm alignment) and partially payers (they bear organizational costs of enforcement and face resistance). Their mobile exit options keep d near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy misclassification by maintaining explicit connection to human dignity as its grounding purpose. The founding problem (technology detached from ethics and human dignity) remains live: technology companies continue optimizing for engagement and profit without subordination to dignity criteria; workers continue facing displacement without protection; algorithmic discrimination persists; extractive finance continues. The founding problem is not solved, which means the constraint has not become vestigial maintenance of a dead mandate. However, there is risk of theater accumulation: as the constraint becomes more elaborate (Vatican conferences, Catholic technology ethics institutes, media campaigns), the ratio of symbolic activity to binding institutional change could rise. The measurement series show theater_ratio increasing from 0.05 to 0.31 over 66 years while base_extractiveness rises only from 0.12 to 0.48 — a sign that enforcement machinery is becoming more visible while actual behavioral change in technology establishments remains contested. This is early-stage piton risk: if theater_ratio continues rising while suppression plateaus and extractiveness does not reach binding threshold, the constraint could decay into performance without function. The omega variables on enforcement mechanism sustainability directly address this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_legitimacy_grounding,
    'This constraint is one reading of a contested kernel: ai_governance_legitimacy. What grounds legitimate authority to interpret AI governance norms — tradition, democratic deliberation, market mechanisms, or technocratic expertise?',
    'Empirical observation: track which reading''s authority claims are actually binding in jurisdictions and institutional decisions. Track whether binding is enforced or voluntary. Track shifts in institutional alignment (do central banks, military establishments, tech companies, international bodies align with magisterial, democratic, libertarian, or technocratic frames?). Track whether enforcement mechanisms persist or degrade.',
    'If magisterial authority proves binding in significant jurisdictions, the constraint type holds at tangled_rope + enforced. If democratic pluralist frame wins, the constraint dissolves into competing traditions with no single binding frame. If technocratic wins, the constraint inverts to pure extraction (technology subordinated to none of these frames). If market libertarian wins, the constraint becomes snare (efficiency wins, workers/vulnerable bear costs). The reading is empirically contingent on authority persistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_legitimacy_grounding, empirical, 'Whether magisterial authority can sustain binding legitimacy claims on AI governance in secular pluralist societies.').

omega_variable(
    subsidiarity_efficiency_boundary,
    'Can technology systems be designed to satisfy both the magisterial subsidiarity/solidarity requirements AND the efficiency/innovation demands of global markets, or are these structurally incompatible optimization targets?',
    'Pilot programs that embed participatory governance, transparency, and human dignity criteria into technology development and measure outcomes (worker displacement, innovation rate, adoption by vulnerable populations, harm reduction). Compare against efficiency-first baselines. Test whether the constraint reduces innovation speed or redirects it toward different objectives.',
    'If compatible: the constraint is genuine coordination (efficiency + dignity both served). If incompatible: the constraint is pure extraction (efficiency sacrificed to magisterial authority, extracting cost from tech establishments and benefiting workers/vulnerable populations). This determines whether tangled_rope classification holds or reverts to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_efficiency_boundary, empirical, 'Whether magisterial principles and technology efficiency are compatible optimization targets or structurally opposed.').

omega_variable(
    magisterial_authority_cultural_contingency,
    'Does the Magisterium''s authority to ground AI governance legitimacy depend on Christian cultural dominance in Western institutions, and would its authority dissolve in genuinely pluralist governance contexts?',
    'Observe binding authority claims in non-Christian-majority jurisdictions and non-Western institutional contexts. Track whether the constraint''s enforcement mechanisms persist or degrade when magisterial institutional power weakens. Compare with other tradition-keepers (Islamic jurisprudence, Buddhist ethics, Indigenous governance) making parallel claims to AI governance authority.',
    'If authority is contingent on Western institutional dominance: the constraint is a cultural-particular imposition, not universal legitimacy grounding — this would reframe it as snare (imposing magisterial values on non-Christian populations). If authority proves universally binding regardless of context: the constraint''s legitimacy claim is robust. If multiple traditions achieve binding authority simultaneously: the constraint dissolves into competing legitimacy frames and the kernel itself is genuinely contested (no single reading dominant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterial_authority_cultural_contingency, conceptual, 'Whether magisterial authority is culturally contingent or genuinely universal in AI governance legitimacy.').

omega_variable(
    enforcement_mechanism_sustainability,
    'What enforcement mechanisms can sustain magisterial subsidiary/solidarity constraints on technology without either dissolving into voluntary aspiration or hardening into authoritarian control?',
    'Track how enforceability is maintained: moral suasion from civil society (sustainable?), investor divestment (effective?), state regulation aligned with magisterial teaching (durable?), technology worker unionization around dignity principles (scalable?), international law (binding?). Measure suppression requirement over time to detect whether enforcement costs rise toward prohibition or decay toward theater.',
    'If enforcement mechanisms prove fragile: the constraint decays into piton (theater_ratio rises, extraction persists but loses functional grounding). If enforcement hardens: the constraint may invert toward coercive imposition by state power claiming magisterial legitimacy (snare from the perspective of dissenting populations). If distributed enforcement (civil society, workers, investors) proves sustainable: the constraint stabilizes as tangled_rope with shared administration rather than top-down suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sustainability, empirical, 'Whether magisterial constraints can be enforced without either dissolving or authoritarian hardening.').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint instantiates the magisterial_subsidiarity_reading of the ai_governance_legitimacy kernel. Is the Magisterium''s authority claim to interpret legitimate AI governance (a) a genuine universal truth grounded in natural law and divine revelation, (b) one culturally-specific tradition among others in pluralist contexts, or (c) a claim that forecloses other readings within a single framework?',
    'Test whether the reading''s authority claims are foreclosing (this reading''s core premise logically rules out alternatives) or coexisting (alternatives remain live in other communities even if not in magisterial tradition). Examine whether the reading invokes universal legitimacy or tradition-specific authority. Track institutional alignment: does the reading achieve binding power or remain aspirational?',
    'If foreclosing: the magisterial reading claims unique legitimacy and siblings must be wrong — this is the strongest claim, most vulnerable to falsification if magisterial authority weakens. If coexisting: multiple readings remain live simultaneously in different communities — this is the most stable state empirically but the least aligned with magisterial exclusivity claims. If binding but not foreclosing: the reading holds authority in its domain but acknowledges other readings are legitimate in other domains — this is a hybrid position. The reading''s actual structural relationship to siblings determines whether the kernel itself is genuinely contested or one reading dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Whether the magisterial reading forecloses, coexists with, or influences sibling readings of the AI governance legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 1960, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t1960, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement_basis(ai_g_tr_t1960, observed).
narrative_ontology:measurement(ai_g_tr_t1981, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 1981, 0.08).
narrative_ontology:measurement_basis(ai_g_tr_t1981, observed).
narrative_ontology:measurement(ai_g_tr_t2000, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement_basis(ai_g_tr_t2000, observed).
narrative_ontology:measurement(ai_g_tr_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement_basis(ai_g_tr_t2015, observed).
narrative_ontology:measurement(ai_g_tr_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(ai_g_tr_t2020, observed).
narrative_ontology:measurement(ai_g_tr_t2026, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 2026, 0.31).
narrative_ontology:measurement_basis(ai_g_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t1960, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement_basis(ai_g_be_t1960, observed).
narrative_ontology:measurement(ai_g_be_t1981, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 1981, 0.18).
narrative_ontology:measurement_basis(ai_g_be_t1981, observed).
narrative_ontology:measurement(ai_g_be_t2000, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement_basis(ai_g_be_t2000, observed).
narrative_ontology:measurement(ai_g_be_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement_basis(ai_g_be_t2015, observed).
narrative_ontology:measurement(ai_g_be_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement_basis(ai_g_be_t2020, observed).
narrative_ontology:measurement(ai_g_be_t2026, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 2026, 0.48).
narrative_ontology:measurement_basis(ai_g_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t1960, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1960, 0.15).
narrative_ontology:measurement_basis(ai_g_su_t1960, observed).
narrative_ontology:measurement(ai_g_su_t1981, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 1981, 0.25).
narrative_ontology:measurement_basis(ai_g_su_t1981, observed).
narrative_ontology:measurement(ai_g_su_t2000, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement_basis(ai_g_su_t2000, observed).
narrative_ontology:measurement(ai_g_su_t2015, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(ai_g_su_t2015, observed).
narrative_ontology:measurement(ai_g_su_t2020, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement_basis(ai_g_su_t2020, observed).
narrative_ontology:measurement(ai_g_su_t2026, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 2026, 0.52).
narrative_ontology:measurement_basis(ai_g_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.18).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).

% DUAL FORMULATION NOTE:
% The ai_governance_legitimacy kernel decomposes into four structurally distinct constraint stories, one per reading. Each reading instantiates a different ε (probability of generating binding governance norms), different beneficiary/victim structures, and different classification. The magisterial_subsidiarity_reading (this story) claims high ε (0.48) and tangled_rope type; the democratic_pluralist_reading claims lower ε (multiple authorities, less binding) and rope type; the market_libertarian_reading claims negative ε (extraction of legitimacy claims without governance effect) and snare type; the technocratic_optimization_reading claims high ε but different beneficiaries/victims (benefits efficiency, targets ethical constraint). Each story is a complete ε-invariant constraint with its own beneficiary/victim structure and classification. The network link records that these four stories represent competing interpretations of a single kernel and that resolution of the kernel contest determines which reading(s) achieve binding authority. No single reading is 'correct' — the corpus measures which reading's authority claims prove empirically binding in institutional decisions, and the classification of each reading depends on that empirical outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
