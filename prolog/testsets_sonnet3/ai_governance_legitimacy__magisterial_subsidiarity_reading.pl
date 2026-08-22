% ============================================================================
% CONSTRAINT STORY: ai_governance_legitimacy__magisterial_subsidiarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ai_governance_legitimacy__magisterial_subsidiarity_reading
 *   human_readable: Magisterial Subsidiarity Reading of AI Governance Legitimacy
 *   domain: theological_ethics/technology_governance/political_theology
 *
 * SUMMARY:
 *   This constraint models the specific claim that AI governance legitimacy
 *   derives from conformity to Catholic Social Doctrine as authoritatively
 *   interpreted by the Magisterium — one reading among several competing
 *   accounts of where AI governance legitimacy comes from. The reading
 *   entangles a genuine coordination function (a shared normative vocabulary
 *   for resisting pure efficiency/profit logics in AI deployment) with an
 *   asymmetric extraction dynamic (moral and reputational costs imposed on
 *   tech monopolies, militaries, and finance, in the name of workers, the
 *   Global South, families, and the marginalized, enforced through moral
 *   suasion rather than binding law). The doctrine explicitly rejects both
 *   the technocratic-optimization and market-libertarian framings as
 *   insufficient on their own, while treating the democratic-pluralist
 *   framing as at most one voice among several rather than a rival source of
 *   ultimate legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5).
domain_priors:suppression_score(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.38).
domain_priors:theater_ratio(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ai_governance_legitimacy__magisterial_subsidiarity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_governance_legitimacy__magisterial_subsidiarity_reading, tangled_rope).
narrative_ontology:human_readable(ai_governance_legitimacy__magisterial_subsidiarity_reading, "Magisterial Subsidiarity Reading of AI Governance Legitimacy").
narrative_ontology:topic_domain(ai_governance_legitimacy__magisterial_subsidiarity_reading, "theological_ethics/technology_governance/political_theology").

domain_priors:requires_active_enforcement(ai_governance_legitimacy__magisterial_subsidiarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'de6b1e9d-0ea8-4021-a559-9e50de706f45').
narrative_ontology:cs_kernel_codification('de6b1e9d-0ea8-4021-a559-9e50de706f45', formalized).
narrative_ontology:cs_authority_grounding('de6b1e9d-0ea8-4021-a559-9e50de706f45', lineage).
narrative_ontology:cs_interpretation_layer_present('de6b1e9d-0ea8-4021-a559-9e50de706f45').
narrative_ontology:cs_reading_relation('de6b1e9d-0ea8-4021-a559-9e50de706f45', ai_governance_legitimacy__technocratic_optimization_reading, influences).
narrative_ontology:cs_reading_relation('de6b1e9d-0ea8-4021-a559-9e50de706f45', ai_governance_legitimacy__democratic_pluralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('de6b1e9d-0ea8-4021-a559-9e50de706f45', ai_governance_legitimacy__market_libertarian_reading, forecloses).
narrative_ontology:cs_axiom('de6b1e9d-0ea8-4021-a559-9e50de706f45', foundational, technology_subordinate_to_human_dignity).
narrative_ontology:cs_axiom_status(technology_subordinate_to_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('de6b1e9d-0ea8-4021-a559-9e50de706f45', technology_subordinate_to_human_dignity, deontological).
narrative_ontology:cs_axiom('de6b1e9d-0ea8-4021-a559-9e50de706f45', foundational, magisterium_holds_authoritative_interpretive_competence).
narrative_ontology:cs_axiom_status(magisterium_holds_authoritative_interpretive_competence, holdable).
narrative_ontology:cs_axiom_grounding('de6b1e9d-0ea8-4021-a559-9e50de706f45', magisterium_holds_authoritative_interpretive_competence, conventional).
narrative_ontology:cs_axiom('de6b1e9d-0ea8-4021-a559-9e50de706f45', secondary, universal_destination_of_goods_overrides_absolute_property_claims).
narrative_ontology:cs_axiom_status(universal_destination_of_goods_overrides_absolute_property_claims, holdable).
narrative_ontology:cs_axiom_grounding('de6b1e9d-0ea8-4021-a559-9e50de706f45', universal_destination_of_goods_overrides_absolute_property_claims, deontological).
narrative_ontology:cs_reference_frame('de6b1e9d-0ea8-4021-a559-9e50de706f45', conciliar_social_doctrine_synthesis).
narrative_ontology:cs_drift_state('de6b1e9d-0ea8-4021-a559-9e50de706f45', contemporary_ai_deployment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de6b1e9d-0ea8-4021-a559-9e50de706f45', '').
narrative_ontology:cs_kernel_id(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_and_gig_workers).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities).
narrative_ontology:constraint_beneficiary(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex).
narrative_ontology:constraint_victim(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, common_good_priority_doctrine).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_governance_legitimacy__magisterial_subsidiarity_reading, universal_destination_of_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues encyclicals and doctrinal statements authoritatively interpreting Catholic Social Doctrine as applied to AI. Convenes conferences, mobilizes bishops' conferences and Catholic civil-society networks, and applies moral suasion to states and firms. Has no coercive enforcement power of its own but claims interpretive authority over what counts as legitimate technology governance for the faithful and, aspirationally, for global policy discourse.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, magisterium, agenda_setter,
    institutional, civilizational, analytical, global).

% Face algorithmic management, automation displacement, and platform gig precarity. The doctrine's insistence on human dignity, labor rights, and subsidiarity is invoked on their behalf in advocacy campaigns and papal statements, though they have no direct channel to enforce the doctrine against their employers.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, industrial_and_gig_workers, beneficiary,
    powerless, biographical, trapped, national).

% Bear the downstream costs of AI-driven resource extraction, data colonialism, and automation of low-wage labor without proportionate access to AI's benefits. The universal destination of goods principle is cited to argue their claim on AI-generated value, but implementation depends entirely on voluntary compliance by governments and firms they cannot compel.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, global_south_populations, beneficiary,
    powerless, generational, trapped, global).

% Targeted by the subsidiarity principle, which holds decisions should be made at the most local competent level rather than by centralized tech platforms or distant state bureaucracies. Some capacity to organize through parish networks and local civil society, but limited leverage against platform-scale AI deployment decisions made elsewhere.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, families_and_local_communities, beneficiary,
    moderate, generational, constrained, local).

% Named explicitly as those AI systems most often exclude or harm through biased classification, algorithmic triage, and automated benefits determination. The doctrine's protection-of-the-vulnerable clause is invoked in their defense, but they depend on ecclesial and NGO advocacy rather than any direct standing to compel compliance.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, marginalized_and_disabled_populations, beneficiary,
    powerless, biographical, trapped, national).

% Own and deploy the dominant AI infrastructure and treat efficiency, growth, and shareholder return as primary metrics. The doctrine's demand for participatory governance and subordination of technology to human dignity is read internally as reputational and regulatory pressure rather than binding law. They can relocate operations, fund countervailing narratives, or absorb the reputational cost while continuing prior practice.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, private_tech_monopolies, payer,
    institutional, biographical, mobile, global).

% Develops autonomous weapons and surveillance AI justified by national security logics that the doctrine explicitly subordinates to common-good and human-dignity constraints. State backing gives it strong insulation from ecclesial pressure; exit from the doctrine's moral claims is easy, exit from state contracts is harder.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, military_industrial_complex, payer,
    institutional, generational, constrained, global).

% Finances AI infrastructure and data-extraction business models on short investment horizons incompatible with the doctrine's universal-destination-of-goods principle. Can shift capital across jurisdictions faster than any doctrinal or regulatory response can be organized, making the constraint's bite on this sector largely reputational.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, extractive_finance_sector, payer,
    institutional, biographical, arbitrage, global).

% Sit between the Magisterium's moral authority and the technical/economic authority of firms and militaries. Largely absent from the doctrine's own deliberative process (it is issued top-down, not negotiated with states) yet expected to be the actual enforcement lever through law and regulation. Their non-Catholic or secular constitutional commitments make direct doctrinal adoption politically fraught.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, excluded,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_governance_legitimacy__magisterial_subsidiarity_reading, national_governments, observer).

% Would be governed by AI systems shaped in part by this doctrine's advocacy without having participated in generating its principles or having any doctrinal standing to contest the Magisterium's interpretive authority. Their objection — that a single religious tradition should not set terms for pluralistic technology governance — is voiced mainly through the rival democratic-pluralist reading, not within this constraint.
narrative_ontology:constraint_stakeholder(ai_governance_legitimacy__magisterial_subsidiarity_reading, non_catholic_and_secular_publics, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, long-tested normative vocabulary (common good, subsidiarity, solidarity, universal destination of goods) that civil society, labor movements, and reform-minded policymakers can invoke to coordinate resistance to purely efficiency- or profit-maximizing AI deployment, without needing to build a governance framework from first principles.
% TRANSFER_FUNCTION: Attempts to move normative legitimacy, reputational capital, and eventually regulatory and investment constraint away from actors organizing AI around efficiency, growth, or military logics, toward actors organizing it around labor dignity, local subsidiarity, and universal access to AI's benefits — enforced almost entirely through moral suasion rather than binding transfer of money or power.
% ABSENT_VOICES: National governments and non-Catholic/secular publics who would be asked to adopt or defer to this doctrinal framework had no seat in producing it; their objection is that a single confessional tradition's interpretive authority should not set terms for pluralistic, multi-faith, and secular technology governance — this objection is structurally the democratic_pluralist_reading, external to this constraint.
% DISAPPEARANCE_RATIONALE: The Magisterium's advocates argue that without this doctrinal counterweight, AI governance discourse would drift further toward pure efficiency and market logics with no principled anchor for the vulnerable — the world would rearrange toward technocratic and market defaults. Critics of the reading argue the doctrine currently exerts negligible binding force on actual deployment decisions, so its disappearance would leave enforcement outcomes largely unchanged while removing one voice from an already crowded advocacy field. Both positions are held by identifiable parties; the story does not adjudicate between them.
% FOUNDING_PROBLEM: Rapid, capital-intensive AI deployment by unaccountable private and state actors was outpacing existing labor protections, local democratic control, and equitable distribution of technological benefits, with no widely legible moral vocabulary demanding subordination of efficiency to human dignity.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists and UN rapporteurs on extreme poverty and technology (outside the Church's own institutional structure) corroborate that algorithmic labor displacement and data-extraction harms to Global South populations remain unresolved and largely unaddressed by existing regulatory frameworks — supporting that the founding problem is live. No corroboration exists, however, for whether THIS doctrinal framework specifically (as opposed to secular labor law or multilateral regulation) is an effective remedy; that is contested even among sympathetic outside observers.
narrative_ontology:disappearance_verdict(ai_governance_legitimacy__magisterial_subsidiarity_reading, contested).
narrative_ontology:founding_problem_status(ai_governance_legitimacy__magisterial_subsidiarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_governance_legitimacy__magisterial_subsidiarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.5, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness sits at the high end of the expected 0.45-0.55 band (0.50) because the doctrine's demands — participatory governance, protection of the vulnerable, subordination of technology to dignity — impose real (if currently soft) reputational and eventually regulatory costs on institutional actors whose business models depend on the opposite priorities. Suppression is moderate (0.38) because the Magisterium's only enforcement tools are moral suasion, ecclesial witness, and civil-society mobilization — it cannot compel compliance the way a state or market mechanism can. Resistance is high (0.72) because the doctrine is actively contested by all three rival readings and by the institutional actors it targets. Theater ratio (0.42) reflects that a substantial share of institutional engagement with the doctrine (corporate ESG statements citing Laudato Si'-style language, government photo-ops with Vatican delegations) is performative acknowledgment without operational change, while a genuine minority of engagement (some Catholic-affiliated pension funds' divestment policies, some labor organizing that explicitly invokes subsidiarity) reflects real behavioral commitment.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's seat, this is a coordination structure restoring proper ends to a technology whose logic has drifted from human dignity — a rope, from the inside. From the tech-monopoly or finance seat, the same structure reads as unaccountable reputational pressure from an actor with no democratic mandate or technical competence in AI governance, closer to symbolic extraction than binding coordination. From the powerless beneficiary seats, the doctrine is aspirational solidarity without enforcement teeth — real in intention, thin in delivered protection. The engine computes each seat's type from the structural power/exit data; the divergence between the Magisterium's self-understanding and the tech sector's experience of the same doctrine is exactly what the tangled_rope classification is built to hold without collapsing into either pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Workers, Global South populations, families, and marginalized/disabled populations are declared beneficiaries because the doctrine's principles are explicitly constructed and invoked on their behalf — they receive advocacy, moral standing, and a vocabulary for claims-making, even though they have little direct power to enforce those claims themselves (hence powerless/trapped exit options despite beneficiary role: benefit without leverage). Private tech monopolies, the military-industrial complex, and extractive finance are victims of the doctrine's normative pressure specifically when they are non-compliant with its principles — their institutional power and mobile/arbitrage exit options mean the effective extraction they experience is heavily damped relative to the powerless beneficiaries' nominal gain, which is itself the central tension the tangled_rope classification is meant to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unaccountable AI deployment outpacing labor protection, local democratic control, and equitable benefit distribution — remains live by outside corroboration (labor economists, UN rapporteurs), which supports treating this as an active tangled_rope rather than a piton coasting on inertia. However the doctrine's effective enforcement capacity has not scaled with its rhetorical elaboration (suppression_requirement rises only modestly, 0.25 to 0.38, while theater_ratio rises faster, 0.30 to 0.42) — a pattern worth monitoring: if theater continues outpacing enforcement, later measurement windows may show drift toward piton (function persisting mainly as institutional performance) even though the underlying problem stays live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_monopoly_vs_shared_moral_vocabulary,
    'Does this reading''s claim to authoritative interpretation via the Magisterium constitute a genuine monopoly on AI governance legitimacy, or is it better understood as one authoritative voice contributing a moral vocabulary that other frameworks (secular, democratic, technical) can also draw on without accepting its interpretive supremacy?',
    'Track whether secular and multi-faith AI governance bodies (UN, OECD, national regulators) cite Catholic Social Doctrine as binding precedent versus as one persuasive source among many in policy documents over the coming decade.',
    'If treated as binding precedent, this reading''s claim to interpretive authority is being structurally vindicated beyond its own confessional community, strengthening the tangled_rope''s coordination function. If treated as merely persuasive, the doctrine functions closer to advocacy within a pluralistic field, weakening the claim that Magisterial interpretation is the SOURCE of legitimacy rather than a contributor to a legitimacy process the democratic_pluralist_reading better describes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_monopoly_vs_shared_moral_vocabulary, conceptual, 'Whether magisterial interpretation functions as sole legitimacy source or as one voice in pluralistic legitimation.').

omega_variable(
    enforcement_capacity_ceiling,
    'Can moral suasion, ecclesial witness, and civil-society mobilization ever generate enforcement capacity proportionate to the doctrine''s ambitious claims on tech monopolies, militaries, and finance, or is there a structural ceiling that keeps this reading permanently in the tangled_rope''s soft-extraction regime without graduating to binding governance?',
    'Compare this doctrine''s actual behavioral effects on institutional AI deployment decisions (divestment, policy adoption, labor practice change) against comparable soft-law instruments (UN Guiding Principles on Business and Human Rights, OECD AI Principles) over a 10-20 year horizon.',
    'A demonstrated enforcement ceiling would support reclassifying long-run trajectory toward piton (theatrical maintenance of a coordination claim that cannot deliver enforcement); demonstrated escalating enforcement capacity would support the tangled_rope''s coordination function strengthening relative to its extraction cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_ceiling, empirical, 'Whether soft enforcement mechanisms can scale to match the doctrine''s substantive claims.').

omega_variable(
    confessional_particularity_vs_universal_claim,
    'Is it coherent for a claim grounded in one religious tradition''s authoritative interpretation to assert legitimacy conditions for AI governance that would bind non-adherents, or does the universalist language (common good, human dignity) smuggle in confessional particulars that a genuinely pluralistic governance framework could not accept without translation into non-confessional terms?',
    'Examine whether the doctrine''s specific claims (subsidiarity, universal destination of goods) can be restated in secular human-rights or capabilities-approach language without loss of normative force, as tested by their reception in explicitly secular international governance venues.',
    'If fully translatable without loss, the reading''s practical force converges with a secular framework and the confessional grounding becomes largely rhetorical scaffolding; if not translatable, the reading''s distinct force depends on accepting Magisterial authority specifically, sharpening the tension with excluded non-Catholic and secular publics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_particularity_vs_universal_claim, conceptual, 'Whether the doctrine''s universalist claims survive translation out of their confessional grounding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_g_tr_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ai_g_tr_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(ai_g_tr_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ai_g_tr_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(ai_g_tr_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(ai_g_tr_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_g_tr_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_g_be_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ai_g_be_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(ai_g_be_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(ai_g_be_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(ai_g_be_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(ai_g_be_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(ai_g_be_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, base_extractiveness, 24, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ai_g_su_t0, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ai_g_su_t4, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(ai_g_su_t8, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(ai_g_su_t12, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(ai_g_su_t16, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(ai_g_su_t20, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(ai_g_su_t24, ai_governance_legitimacy__magisterial_subsidiarity_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_governance_legitimacy__magisterial_subsidiarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_governance_legitimacy__magisterial_subsidiarity_reading, 0.12).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__technocratic_optimization_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__democratic_pluralist_reading).
narrative_ontology:affects_constraint(ai_governance_legitimacy__magisterial_subsidiarity_reading, ai_governance_legitimacy__market_libertarian_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the single natural-language kernel 'AI governance legitimacy' (kernel_id: ai_governance_legitimacy), per the ε-invariance principle: the label 'AI governance legitimacy' conflates structurally distinct claims about where that legitimacy comes from (magisterial doctrinal authority, democratic deliberation, technocratic welfare-maximization, market voluntarism). Each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and claimed type, linked here via affects_constraints. This reading (magisterial_subsidiarity_reading) authors the highest ε of the four (0.45-0.55 band) because it is the only reading that both names concrete institutional victims of non-compliance (tech monopolies, military-industrial complex, extractive finance) AND claims binding interpretive authority over the others' terms, producing genuine cross-reading friction rather than mere disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
