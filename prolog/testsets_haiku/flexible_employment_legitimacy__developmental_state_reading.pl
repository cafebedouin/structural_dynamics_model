% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as Managed Transition Toward Formalization (Developmental State Reading)
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   This story instantiates the developmental-state reading of flexible
 *   employment legitimacy. The constraint is framed as a temporary scaffold:
 *   the state tolerates flexible (non-formal) employment during a bounded
 *   transition period (2027 endpoint) while building administrative and
 *   institutional capacity to absorb workers into formal employment.
 *   Contingent workers are told (and the state authority asserts) that
 *   flexibility is transitional, not permanent — that wage growth, benefits,
 *   and employment security are the endpoint, not the constraint itself. The
 *   reading's core claim: extractiveness during the interim is the justified
 *   cost of state capacity-building, not the purpose of the arrangement.
 *   Under the market-efficiency sibling reading, flexibility is legitimate
 *   permanent clearing mechanism. Under the precarity-extraction sibling,
 *   flexibility is structural precarity enabling platform rent-taking, and
 *   the 'transition' narrative is cover. This story does NOT defend any
 *   reading; it instantiates this one as an ε-invariant constraint with
 *   metrics reflecting what the reading takes the constraint to be: a
 *   scaffold (sunset via 2027 target) with moderate interim extraction
 *   (workers bear cost; state/platforms/formal employers collect benefit),
 *   declining extractiveness as formalization proceeds, and resistance from
 *   labor movements and precarity advocates excluded from the transition
 *   plan's design.
 *
 * KEY AGENTS:
 *   - developmental_state_authority: Sets the 12-point plan and 2027 formalization target; administers regulatory tolerance and capacity-building; institutional power, analytical exit.
 *   - contingent_workers: Bear immediate extraction (no benefits, income volatility); told the flexibility is temporary; trapped exit, powerless position.
 *   - platform_operators: Access flexible labor without formalization cost during interim; powerful, arbitrage-capable exit (can move operations).
 *   - formal_sector_employers: Benefit from interim competitive-fairness gain; protected from flexible-wage competition during transition; powerful but constrained exit (formalization failure harms their position too).
 *   - informal_sector_participants: Remain outside the flexible track; told formalization will include them; trapped, powerless.
 *   - labor_union_movement: Excluded from interim transition design; perceive long-term alignment (all workers formalized) but object to interim precarity and loss of organizing power.
 *   - international_development_bodies: Observer seat; validate the developmental-state narrative as best practice; assume 2027 endpoint is genuine.
 *   - contingent_worker_advocates: Excluded by institutional framing; argue minimum standards should apply immediately; push back on 'temporary' extraction narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.58).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.42).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as Managed Transition Toward Formalization (Developmental State Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '1b36616b-5ce7-4400-a231-21f5bde54efd').
narrative_ontology:cs_kernel_codification('1b36616b-5ce7-4400-a231-21f5bde54efd', formalized).
narrative_ontology:cs_authority_grounding('1b36616b-5ce7-4400-a231-21f5bde54efd', extraction).
narrative_ontology:cs_interpretation_layer_present('1b36616b-5ce7-4400-a231-21f5bde54efd').
narrative_ontology:cs_reading_relation('1b36616b-5ce7-4400-a231-21f5bde54efd', flexible_employment_legitimacy__market_efficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('1b36616b-5ce7-4400-a231-21f5bde54efd', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('1b36616b-5ce7-4400-a231-21f5bde54efd', foundational, flexibility_is_transitional_form).
narrative_ontology:cs_axiom_status(flexibility_is_transitional_form, holdable).
narrative_ontology:cs_axiom_grounding('1b36616b-5ce7-4400-a231-21f5bde54efd', flexibility_is_transitional_form, empirically_contingent).
narrative_ontology:cs_axiom('1b36616b-5ce7-4400-a231-21f5bde54efd', foundational, state_capacity_is_binding_constraint).
narrative_ontology:cs_axiom_status(state_capacity_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('1b36616b-5ce7-4400-a231-21f5bde54efd', state_capacity_is_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('1b36616b-5ce7-4400-a231-21f5bde54efd', secondary, formal_employment_social_contract_is_stable_endpoint).
narrative_ontology:cs_axiom_status(formal_employment_social_contract_is_stable_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('1b36616b-5ce7-4400-a231-21f5bde54efd', formal_employment_social_contract_is_stable_endpoint, deontological).
narrative_ontology:cs_reference_frame('1b36616b-5ce7-4400-a231-21f5bde54efd', formal_employment_social_contract).
narrative_ontology:cs_drift_state('1b36616b-5ce7-4400-a231-21f5bde54efd', contemporary_interim_flexibility, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b36616b-5ce7-4400-a231-21f5bde54efd', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, state_administrative_capacity).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formal_employment_growth).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, contingent_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, contingent_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, informal_sector_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers labor formalization policy via 12-point standardization plan targeting 2027 completion. Justifies flexible employment as temporary regulatory tolerance while state capacity builds (social insurance administration, tax infrastructure, labor inspection, enforcement machinery). Sets transition timeline, minimum standards floor, wage indexing mechanism. Could revise or abandon the plan; currently maintains it as the authoritative formalization roadmap.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, developmental_state_authority, agenda_setter,
    institutional, generational, analytical, national).

% Work under flexible terms (no minimum hours, benefits contingent on utilization, termination at-will). Bear the cost of formalization delay: income volatility, no paid leave, no unemployment insurance, no pension, no collective-bargaining protection. Simultaneously told by the state's own narrative that the flexibility is temporary and transitional — that formal employment is the endpoint. Their exit options: formal sector jobs (if they materialize by 2027 as promised), informal sector (always available but presented as worse), or unemployment (not viable). The trap: formal sector cannot absorb them immediately (state asserts capacity constraint); informal sector offers no transition pathway; unemployment is not a stable exit.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, contingent_workers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, contingent_workers, beneficiary).

% Access flexible labor pools without bearing formalization costs during the transition period. Operate under regulatory tolerance justified by the state's 'temporary' framing. Can absorb costs once formalization proceeds (by raising prices, contracting supply, or offshoring); until then, manage under existing informal-sector labor market dynamics. Maintain arbitrage by offshoring operations if domestic formalization threatens cost structure before the state's 2027 deadline.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_operators, beneficiary,
    powerful, biographical, arbitrage, global).

% Benefit from a managed transition that preserves their formalized labor cost advantage over flexible competitors during the interim. Standardization plan creates competitive fairness once the transition completes. Their constraint exit is limited: if they refuse to participate in formalization, they lose the competitive-fairness guarantee; if they defect by hiring flexible labor themselves, they undermine their own long-term stabilization and face regulatory pressure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers, beneficiary,
    powerful, generational, constrained, national).

% Street vendors, casual laborers, subsistence enterprises, home-based workers. Flexible employment ostensibly draws from informal sector by formalizing its participants — they are the state's target population for transition. In practice, they remain outside the flexible-employment track (platforms recruit from unemployed and school-leavers preferentially), face new barriers as flexible workers absorb available low-skill positions, and bear the state's administrative attention (licensing, permits, data collection) without receiving its protections.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, informal_sector_participants, payer,
    powerless, immediate, trapped, local).

% Historically organized around formal-sector protections and collective bargaining. Flexible employment undercuts their membership base and negotiating leverage during the interim. The state's formalization narrative aligns with union goals (all workers eventually in formal sector) but the transition period is one unions do not control — workers are excluded from collective organization during the 'temporary' phase. Their objection: the 'temporary' becomes permanent through institutional inertia; the developmental-state framing preempts pressure for immediate formal conversion; unions have no seat at the policy table despite representing their core constituency.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_union_movement, excluded,
    organized, generational, constrained, national).

% View the developmental-state reading as a 'best practice' for emerging economies formalizing informality. Provide technical assistance for the 12-point plan, track metrics on formal-employment growth and tax-base expansion. Their analyst role operates from the assumption that the state's capacity-building narrative is genuine and that formalization is the stable endpoint — they do not independently verify that the interim extraction is temporary rather than permanent or institutional inertia.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, international_development_bodies, observer,
    institutional, generational, analytical, global).

% Argue that flexibility without protections is precarity, not transition — that the state should impose minimum standards immediately rather than tolerating interim extraction. Excluded from formal policy forums whose legitimacy rests on the developmental-state narrative (they are perceived as opposing 'necessary' transition). Their objection: the 2027 target is not binding; the plan lacks enforcement mechanisms; the narrative justifies indefinite delay; interim extraction is deliberate policy choice, not capacity constraint.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, contingent_worker_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, developmental_state_authority).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bridges informality and formality by licensing temporary flexible employment, during which the state builds tax administration, social insurance, labor inspection, and employer compliance infrastructure. Workers and platforms transition together: workers from unregistered informal sector to registered flexible employment to formal sector employment. The state absorbs the cost of capacity-building, not workers.
% TRANSFER_FUNCTION: Moves regulatory tolerance from the state to workers and platforms during the interim: they bear the cost of no minimum income guarantee, no employer-provided benefits, no collective-bargaining protection. The transfer from contingent workers flows to: (1) platform operators (labor cost reduction, estimated at 20-35% below formal-sector equivalents), (2) formal-sector employers (interim competitive-fairness gain, estimated at 10-15% cost advantage during transition), (3) state tax administration (data infrastructure and reporting systems built using flexible-employment data pipeline).
% ABSENT_VOICES: Labor unions are excluded from transition-period policy design; they perceive long-term alignment (formalization benefits their members) but cannot organize workers during the interim and oppose the extractive interim terms. Informal-sector participants not already in flexible employment are outside the conversation; they are told formalization will include them, but the flexible-employment track absorbs the most job-ready informal workers (school-leavers, recently displaced formal workers), leaving subsistence participants behind. Precarity researchers and contingent-worker advocates pushing for immediate minimum standards are excluded by institutional framing that treats 'temporary extraction' as a non-negotiable capacity-building cost.
% DISAPPEARANCE_RATIONALE: Developmental-state reading: if the constraint disappeared (formalization mandate imposed immediately with no interim flexibility), the state's capacity-building timeline would compress to political impossibility — formalization would fail, enforcement would be uneven, and the informal sector would absorb workers again, recreating the original coordination problem. Market-efficiency reading: if it disappeared, workers and platforms would renegotiate voluntarily on terms reflecting actual scarcity, labor productivity would improve from better matching, and efficiency gains would offset transition costs. Precarity-extraction reading: if it disappeared, workers would organize into collective structures and platform costs would rise sharply; disappearance would rearrange power dramatically in workers' favor.
% FOUNDING_PROBLEM: Emerging economies face the challenge of formalizing informality without destroying the absorptive capacity of the informal sector (which employs workers the formal sector cannot yet hire at scale). The founding problem is stated as the state's capacity constraint: the formal-sector institutions (tax administration, social insurance, labor inspection, collective-bargaining frameworks) cannot absorb informal workers immediately, and building those institutions takes time. Flexible employment is presented as a managed-transition device: temporary regulatory tolerance allowing the state to build infrastructure while workers graduate from informality without creating unemployment or forcing them back to subsistence.
% FOUNDING_PROBLEM_CORROBORATION: The developmental state authority attests the founding problem is live and capacity-building is necessary. International development bodies and labor economists studying formalizing economies (World Bank, ILO studies on informal-to-formal transitions) corroborate the capacity constraint narrative at a general level. Contingent worker advocates and precarity researchers attest the founding-problem framing is ex-post rationalization — that the state tolerates flexibility primarily for revenue (tax data, registration fees) and platforms' cost advantage, and that interim formalization is technically feasible at higher cost to employers/platforms. No neutral party has conducted an independent measurement of state capacity to formalize more rapidly under higher-cost scenarios; the capacity narrative is not empirically corroborated by comparison to counterfactual.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) not because extraction is low, but because the reading locates extraction as a transitional cost with a declared endpoint (2027 formalization). Workers bear significant immediate cost (no benefits, precarity); the developmental-state reading treats this as justified by state capacity requirements, not as permanent rent extraction. Theater (0.31) reflects the performative elements: the 12-point plan shows commitment to transition; government capacity reports track progress; formalization targets are published. But a portion of this performance is the maintenance of the 'temporary' narrative itself — theater is the gap between the plan's stated timeline and actual capacity-building pace. Suppression (0.42) is moderate because the constraint does not rely primarily on coercion: workers comply because the alternative is informal unemployment; platforms comply because regulatory tolerance is profitable; the state's authority legitimates the arrangement through the transition narrative, not through force. Accessibility collapse (0.62) reflects that alternatives are partly foreclosed: workers cannot easily exit to formal employment (capacity constraint is real or asserted as real); they can exit to informal sector or unemployment, but those are presented as worse. Resistance (0.71) is substantial because labor unions, worker advocates, and precarity researchers actively contest the narrative — they do not accept that extraction is temporary or justified. The measurement series spans 30 time units with the peak extractiveness around t=20 (midway through the projected 2027 transition window), declining after as formalization is assumed to accelerate. The projection is baked in: the reading assumes the 2027 endpoint is reached; actual measurements after t=10 are authored as projections (basis=projected) to reflect that the full transition is a conditional scenario under the reading's own assumptions.
 *
 * PERSPECTIVAL GAP:
 *   The developmental-state authority and contingent workers would compute dramatically different seat classifications. From the authority seat, the constraint is rope (genuine coordination: capacity-building benefits workers and state infrastructure simultaneously; temporary extraction is proportionate cost). From the contingent-worker seat, it appears as snare: extractive mechanism with suppressed alternatives ('flexibility is temporary' is a cover story; formal jobs won't materialize; informal sector is worse; the state's timeline is not binding). From the platform operator seat, the arrangement looks like rope: regulatory tolerance is the beneficiary seat's gain. From the formal employer seat, it looks like rope too: interim competitive protection. The engine computes these per-seat and will show divergence; that divergence is exactly the measurement this reading is designed to capture — the structural asymmetry in how the constraint appears depending on who bears the extraction and who is promised transition.
 *
 * DIRECTIONALITY LOGIC:
 *   Contingent workers: victims (base_properties.victims). They are told by the authority that flexibility is temporary and they will transition to formality, but they bear the cost during the interim (no benefits, precarity, income volatility). Their exit is trapped (formal jobs not available yet; informal sector available but worse; unemployment not viable). Directionality toward full target (d near 1.0): they are clearly structured as extractive targets during the interim, though the reading asserts the interim is bounded. Platform operators: beneficiaries (base_properties.beneficiaries). They collect labor cost advantage during the interim from regulatory tolerance. Exit is arbitrage (can move operations if formalization raises costs). Directionality toward beneficiary (d near 0.0): they benefit and can exit. Formal-sector employers: beneficiaries. They get interim competitive-fairness guarantee. Exit is constrained (if they refuse formalization, they lose the guarantee; if formalization fails, they lose the stability they need). Directionality middle-ground (d ~0.4): they benefit from the interim but are locked into the formalization endpoint by their own interest. State authority: agenda-setter (neither target nor beneficiary in the strict sense, but administers the constraint). Directionality analytical (d not computed for agenda-setters; they are the framing seat). The reading's claim that this is scaffolding (temporary) depends on the directionality of contingent workers being temporary — i.e., that they will move toward d~0.5 (symmetric benefit-cost) or d~0.0 (beneficiary) as formalization proceeds. If formalization stalls and d stays near 1.0, the constraint reclassifies from scaffold to snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The developmental-state reading avoids mandatrophy if and only if the 2027 formalization endpoint is met. The founding problem is 'state capacity to absorb informal workers into formal sector without destroying informality beforehand.' If the state reaches the 2027 target, the founding problem is solved and the constraint's mandate is fulfilled (transition complete). If 2027 passes without formalization, the mandate has outlived the founding-problem claim: the state would have collected years of interim extraction (worker precarity, platform cost-cutting, employer protections) under a stated-temporary justification that has now become permanent. That condition triggers mandatrophy_resolved: the constraint no longer has a mandate to extract during transition if the transition is not happening. The reading's own internal logic predicts mandatrophy if 2027 slips. This is structurally built into the scaffold type: a constraint with an honest sunset clause is betting that the sunset will be reached. If it is not reached, the constraint flips classification on the basis of that failure alone (not on changing metrics, but on the mandate becoming indefensible). The measurement series projects extractiveness declining after t=25 (leading to the 2027 endpoint) and dropping sharply at t=30 (formalization complete). A story where extractiveness stays flat or rises past t=25 would be forecasting mandate failure and thus mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capacity_authenticity,
    'Is the state''s capacity-building claim genuine? Does the state actually lack the administrative infrastructure to formalize immediately, or does it tolerate flexibility for revenue/data advantage?',
    'Comparative analysis: measure (a) time and cost to build capacity by state-reported timeline vs. (b) time and cost to build capacity by international development bodies'' independent assessment; conduct counterfactual: what would happen if the state imposed formalization immediately? If capacity-building is the binding constraint, formalization would fail or regress catastrophically. If capacity is not binding, formalization would proceed with higher employer/platform costs.',
    'Genuine capacity constraint: the reading''s mandate is real and the extraction is justified transitional cost. Not genuine: the reading is cover for permanent extraction, and the constraint should reclassify to snare or tangled_rope with no sunset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_authenticity, empirical, 'Whether state capacity is the binding constraint for formalization.').

omega_variable(
    id_2027_target_binding,
    'Is the 2027 formalization target a genuine commitment with enforcement mechanisms, or a target-in-principle that will slip indefinitely?',
    'Observational: at t=2027, measure whether formal-employment ratio reaches the published target. If yes, sunset clause was genuine. If no, track state policy response: do they republish a new target (mandate-failure + goalpost-moving = precarity-extraction reading confirmed) or do they acknowledge capacity limits and extend timeline with revised milestones?',
    'Binding target: scaffold classification is warranted; extractiveness should decline sharply after 2027 as formalization proceeds. Non-binding target: the constraint becomes piton (performing transition while extracting indefinitely), or reclassifies to snare if the state actively extends extraction by deferring the target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_2027_target_binding, empirical, 'Whether the 2027 endpoint is credible or will slip.').

omega_variable(
    interim_extraction_inevitability,
    'Is interim extraction during the transition phase a necessary cost of capacity-building, or is it avoidable through higher employer/platform investment?',
    'Design exercise: specify a regulatory regime that builds state capacity while imposing minimum benefits/protections on contingent workers immediately. If such a regime is technically feasible but not adopted, the choice to extract during interim is political/preference-driven, not capability-driven.',
    'Unavoidable extraction: the interim cost is justified by real constraints; scaffold framing is sound. Avoidable extraction: the state has chosen to impose cost on workers to subsidize other actors'' formalization; the reading is cover for a deliberate transfer decision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interim_extraction_inevitability, conceptual, 'Whether interim worker precarity is necessary or chosen.').

omega_variable(
    formalization_endpoint_irreversibility,
    'If formalization is achieved in 2027, is it stable, or will flexible employment re-emerge as platforms and employers pressure for deregulation?',
    'Post-2027 observation: track whether formal-employment ratio stays above the published target or begins slipping back toward flexibility. If stable, the constraint''s endpoint is real. If slips, the transition was temporary but not the flexibility — the constraint cycles, and the reading''s stability assumption is violated.',
    'Stable formalization: the reading''s endpoint is defensible; the constraint can resolve (mandate fulfilled). Cycling: the constraint never genuinely sunsets; flexible employment re-emerges; the scaffold framing becomes inaccurate and the constraint should be reclassified as piton or tangled_rope (repeating cycle).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(formalization_endpoint_irreversibility, empirical, 'Whether formalization, once achieved, remains stable.').

omega_variable(
    reading_framing_under_determination,
    'The developmental-state reading places state capacity-building as the legitimating frame for interim extraction. But what if the frame is market-efficiency (flexibility is permanent and beneficial) or precarity-extraction (flexibility is permanent and extractive)? Does evidence exist that could arbitrate between the three readings?',
    'Structured observation of the 2027 endpoint: (1) If formalization occurs and sticks, market-efficiency reading is falsified (flexibility was not permanent), developmental-state reading is confirmed (capacity-building narrative was validated), precarity-extraction reading is partly disproven (extraction ended, though interim extraction did occur). (2) If formalization does not occur, developmental-state reading is falsified (capacity-building narrative was not validated), market-efficiency reading is confirmed (flexibility persists), precarity-extraction reading is confirmed (extraction continued). (3) If formalization occurs but immediately degrades, all three readings are partly true (flexible employment was temporarily transitioned to formal, then re-emerged; extraction occurred during interim; but capacity-building did happen).',
    'The three readings are structurally distinguishable by the 2027 outcome. This omega documents that the reading-contest is empirically resolvable in principle but only observable after the constraint''s stated timeline reaches. Until then, the three readings coexist as live institutional positions held by different parties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_under_determination, empirical, 'Whether the three readings of flexible-employment legitimacy can be empirically arbitrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(flex_tr_t5, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(flex_tr_t10, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(flex_tr_t25, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(flex_tr_t30, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(flex_be_t5, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(flex_be_t10, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(flex_be_t25, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(flex_be_t30, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(flex_su_t5, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(flex_su_t10, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(flex_su_t25, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(flex_su_t30, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 30, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__developmental_state_reading, 0.18).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% flexible_employment_legitimacy is a contested kernel decomposed into three constraint stories: (1) developmental_state_reading (THIS STORY): flexible employment is temporary scaffold requiring state management toward formalization by 2027; moderate interim extraction justified by capacity-building. (2) market_efficiency_reading: flexible employment is legitimate permanent market-clearing mechanism; low extraction (market prices flexibility). (3) precarity_extraction_reading: flexible employment is structural precarity enabling platform extraction; high extraction permanently masked by 'transition' narrative. The three readings share the same kernel (what is flexible employment for, and does it have an endpoint?) but instantiate different constraints with different ε, different beneficiary/victim structures, different resistances. All three stories should be generated separately and linked via network.affects_constraints. The developmental-state reading (this story) influences the market-efficiency and precarity readings because its 2027 endpoint claim directly shapes whether the other readings' premises remain viable: if the endpoint is reached, market-efficiency reading's permanence claim is falsified and precarity-extraction reading's indefinite-extraction claim is challenged. Each sibling reading contains an omega documenting the alternative framing and what would confirm/disconfirm it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, powerless, 0.88).
constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
