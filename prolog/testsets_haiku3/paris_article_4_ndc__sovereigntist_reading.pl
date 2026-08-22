% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Article 4 NDCs: Sovereigntist Reading
 *   domain: international/environmental/political_economy
 *
 * SUMMARY:
 *   The sovereigntist reading of Paris Article 4 NDCs frames voluntary,
 *   nationally-determined climate pledges as a decentralized coordination
 *   mechanism that preserves state sovereignty and permits fossil-dependent
 *   economies to maintain development pathways. Under this reading, NDCs are
 *   non-binding commitments that states can revise unilaterally, with no
 *   supranational enforcement capacity. The reading vindicated the
 *   Westphalian doctrine of permanent state sovereignty over natural
 *   resources and energy policy. This stands in direct structural opposition
 *   to the supranational reading (which frames NDCs as binding commitments on
 *   a ratcheting trajectory toward net-zero) and tension with the equity
 *   reading (which frames NDCs through Common But Differentiated
 *   Responsibilities and structural state distinctions). The sovereigntist
 *   reading is the ONLY one that permits low extractiveness: because states
 *   retain full exit and revision authority, the constraint operates as
 *   voluntary coordination rather than coercive commitment. The measurement
 *   series show rising theater ratio (pledges increasingly decoupled from
 *   actual emissions reductions) and rising suppression requirement
 *   (defending state discretion against mounting international pressure for
 *   binding targets).
 *
 * KEY AGENTS:
 *   - Fossil-dependent economies: beneficiaries of unconstrained energy sovereignty; retain authority to maintain or expand production
 *   - Developing nations with energy access mandates: beneficiaries; preserve policy space to prioritize electrification and development over rapid decarbonization
 *   - Global climate stabilization advocates: payers; rely on voluntary coordination instead of binding obligation or escalating pressure
 *   - European Union bloc: agenda-setters internally (adopted binding net-zero) but cannot externalize expectations onto others under sovereigntist reading
 *   - Petro-state governments: beneficiaries; protect fiscal dependency on fossil fuel revenue from supranational override
 *   - Island states and climate-vulnerable populations: excluded from enforcement mechanisms; survival not coupled to NDC revision authority
 *   - UNFCCC secretariat: constrained to informational role; no authority to review, challenge, or escalate pledges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.28).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Article 4 NDCs: Sovereigntist Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international/environmental/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '76f0f2c6-5a47-4805-868c-ebf8266c9c6a').
narrative_ontology:cs_kernel_codification('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', formalized).
narrative_ontology:cs_authority_grounding('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', distributed).
narrative_ontology:cs_reading_relation('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', foundational, national_sovereignty_over_energy_policy_is_inviolable).
narrative_ontology:cs_axiom_status(national_sovereignty_over_energy_policy_is_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', national_sovereignty_over_energy_policy_is_inviolable, deontological).
narrative_ontology:cs_axiom('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', foundational, voluntary_commitment_preserves_participation_and_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_commitment_preserves_participation_and_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', voluntary_commitment_preserves_participation_and_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', westphalian_sovereign_state_system).
narrative_ontology:cs_drift_state('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', contemporary_climate_emergency_phase, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('76f0f2c6-5a47-4805-868c-ebf8266c9c6a', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, developing_nations_with_energy_access_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, carbon_intensive_industry).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, petro_state_governments).
narrative_ontology:constraint_victim(paris_article_4_ndc__sovereigntist_reading, global_climate_stabilization_advocates).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, westphalian_state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, principle_of_permanent_sovereignty_over_natural_resources).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States whose domestic energy infrastructure, fiscal revenue, and development pathways are locked into coal, oil, and gas production. The sovereigntist reading allows them to define their own transition timelines without external pressure, preserving the option to expand production for domestic development and export revenue. They retain the authority to revise NDCs unilaterally and to interpret CBDR provisions as exempting them from rapid decarbonization.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies, beneficiary,
    moderate, generational, constrained, national).

% Nations facing domestic pressure to expand energy access (electricity, heating, transport) to populations without reliable supply. Under the sovereigntist reading, they retain authority to choose their energy mix and timeline, including fossil fuels, without binding international constraints on their development pathway. The reading protects their policy space from supranational mandate.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, developing_nations_with_energy_access_mandates, beneficiary,
    moderate, generational, constrained, national).

% Climate scientists, environmental NGOs, and justice-oriented climate advocates who interpret NDCs as a mechanism to drive rapid, binding emissions reduction at global scale. Under the sovereigntist reading, the NDC system is stripped of its binding character and enforcement capacity, leaving advocates reliant on moral suasion and voluntary coordination rather than contractual obligation or escalating pressure. They bear the cost of constrained policy leverage; their identity as climate stabilizers is fused to the success of global coordination.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, global_climate_stabilization_advocates, payer,
    organized, civilizational, identity_locked, global).

% The EU and allied developed economies have adopted the supranational reading internally (binding net-zero targets, supranational oversight, ratcheting mechanism). Under the sovereigntist reading, their capacity to externalize expectations onto other states is blocked — the same sovereignty doctrine they invoke domestically cannot be unilaterally overridden internationally. They set the negotiating agenda by modeling binding targets, but cannot enforce them on others.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, european_union_bloc, agenda_setter,
    institutional, generational, arbitrage, regional).

% Cities and provinces often lead climate action within their jurisdictions, but the NDC system frames commitments at the national level. Under the sovereigntist reading, which emphasizes national sovereignty, subnational actors have no formal voice in NDC setting or revision; their climate commitments are structurally subordinated to national-level state discretion.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, subnational_and_municipal_governments, excluded,
    powerful, biographical, constrained, local).

% The UNFCCC and Paris Agreement secretariat maintain the NDC registry, monitor submissions, and produce synthesis reports. Under the sovereigntist reading, the secretariat's role is strictly informational — it records and compiles, but has no authority to review, challenge, or escalate national pledges. Its institutional power is constrained by the reading's core insistence on voluntary, self-determined commitment.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, international_climate_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Fossil fuel producers, heavy industrial manufacturers, and energy-intensive sectors benefit from the sovereigntist reading's lack of binding enforcement. States retain discretion to relax pledges or shift timelines, reducing policy certainty and allowing industry lobbying to operate within state decision-making without facing supranational override. Industry can arbitrage across jurisdictions where sovereigntist deference is highest.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, carbon_intensive_industry, beneficiary,
    powerful, biographical, mobile, global).

% Nations facing existential threat from sea-level rise and extreme weather have no formal mechanism within the sovereigntist reading to escalate pressure on high-emission states. Their survival is decoupled from the NDC process by design: the reading privileges each state's voluntary choice over any claim to equitable climate outcomes. They are excluded from enforcement mechanisms because the reading rejects the legitimacy of external pressure as such.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, island_states_and_climate_vulnerable_populations, excluded,
    powerless, immediate, trapped, global).

% Governments whose fiscal revenue, state capacity, and geopolitical leverage depend on oil and gas exports. The sovereigntist reading protects their authority to maintain or expand production regardless of global climate targets. They can pledge modest efficiency gains or renewable deployment while leaving production expansion untouched, and face no supranational mechanism to challenge the mismatch between pledge and outcome.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, petro_state_governments, beneficiary,
    powerful, biographical, constrained, national).

% Researchers tracking atmospheric carbon, ice-sheet dynamics, and climate tipping points serve as the analytical seat. Under the sovereigntist reading, their data on emissions trajectories and climate outcomes inform synthesis reports but carry no binding implications for NDC revision or escalation. The reading decouples evidence from policy obligation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, frontier_climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a decentralized, national-level framework for voluntary climate pledges that allows states to coordinate emissions reductions without surrendering sovereign authority over energy policy, domestic development pathways, or revision rights. Each state sets its own ambition level; transparency through registry and peer review is the coordination mechanism.
% TRANSFER_FUNCTION: Moves the locus of climate commitment-setting from supranational bodies to individual nation-states. States retain the authority to define their own decarbonization timelines, exclude sectors from obligations, and revise pledges upward or downward. The transfer flows in the negative direction: power is returned FROM the supranational system TO states, not extracted BY it.
% ABSENT_VOICES: Island states and climate-vulnerable populations have no formal escalation mechanism or enforcement lever within the sovereigntist reading; they can request, advocate, and testify, but cannot override state sovereignty. Subnational governments are excluded from the formal NDC process entirely. Carbon-intensive workers and communities dependent on fossil fuel industries are similarly excluded from the formal NDC decision-making process.
% DISAPPEARANCE_RATIONALE: Sovereigntist advocates argue that if the sovereigntist reading vanished and were replaced by supranational enforcement, states would resist through non-compliance, treaty withdrawal, or demand renegotiation—the NDC system would collapse. Supranational advocates argue that without the sovereigntist reading, states would already be bound by ratcheting mechanisms and face escalating pressure, so the system's shape would shift fundamentally. Vulnerable populations argue the distinction is immaterial if outcomes remain unchanged (emissions still rising, climate impacts still worsening).
% FOUNDING_PROBLEM: Post-Kyoto deadlock: the Kyoto Protocol's binding targets created compliance crises and withdrawal (US exit 2001), fragmenting climate governance. The founding problem was how to engage all major emitters in climate pledging WITHOUT recreating the Kyoto bind that deterred participation. The NDC frame solved participation by making targets voluntary and nationally-determined.
% FOUNDING_PROBLEM_CORROBORATION: The UNFCCC secretariat attests the founding problem was real and the sovereigntist framing solved participation (195 Paris signatories vs. 192 Kyoto signings, higher coverage in developing world). Climate negotiators confirm participation was the binding constraint. However, climate scientists and vulnerable-nation representatives attest the founding problem—Kyoto's weak enforcement—was solved BY REMOVING ENFORCEMENT ENTIRELY, not by designing better enforcement, and that the tradeoff (universal participation + no compliance pressure) has produced NDCs as performance theater: pledges have consistently failed to deliver emissions reductions at the necessary scale, making the founding problem's 'solution' questionable under outcome-centered readings.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the sovereigntist reading explicitly preserves state exit: any state can unilaterally revise its NDC upward or downward, withdraw from the agreement entirely (with two years' notice per Article 28), or reinterpret its obligations through self-determined carbon accounting rules. No supranational authority can override a state's energy choices. This is the defining characteristic of the reading—it WIN-PRESERVES sovereignty over extraction. Suppression is minimal (0.12) because the constraint depends on voluntary internalization and peer pressure, not coercive institutional machinery. Theater is rising (0.08→0.18) because the mechanism increasingly functions as symbolic commitment-signaling: NDCs are revised upward rhetorically while actual energy policy expands fossil fuel capacity, and the UNFCCC process accommodates this mismatch without formal challenge. Accessibility_collapse is low (0.22) because alternatives to Paris remain available: states can withdraw (two-year exit window), redefine their pledges (no audit mechanism), or simply fail to implement (no enforcement). Resistance is high (0.71) because: (1) climate advocates push for binding targets and accountability mechanisms; (2) vulnerable nations demand ratcheting pressure on high-emitting states; (3) scientists document the failure of voluntary pledges to produce necessary emissions reductions. The sovereigntist reading actively resists and defends against these pressures by reasserting state sovereignty as the paramount value.
 *
 * PERSPECTIVAL GAP:
 *   From the fossil-dependent economy seat: the sovereigntist reading is liberation—it protects the state from external override and permits energy policy to be determined by domestic constituencies. Extractiveness is near zero because the state is the decision-maker. From the climate advocate seat: the same reading is constraint—it strips their only available lever (supranational binding pressure) and leaves them reliant on moral suasion, which has failed to drive adequate emissions reductions. Extractiveness is substantial because their goal (climate stabilization) is made structurally unattainable. The EU bloc occupies an intermediate position: it benefits from its own binding-target system but cannot impose it externally. The engine should compute different types per seat: states might compute 'rope' or 'mountain' (their own sovereign authority), while climate advocates compute 'snare' (constrained to advocacy without policy power). This perspectival gap is the core of the kernel contest—it is not resolvable by choosing the 'right' reading, but by recognizing that different structural relationships produce different classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure here is institution-level, not individual. Fossil-dependent economies benefit from state-centered framing (beneficiary role, d near 0.2) because it blocks supranational constraints on their production decisions. Climate advocates are the targets (d near 0.8): they are committed to climate stabilization (identity-locked, civilizational horizon) and have no exit from the goal, while the sovereigntist reading explicitly strips them of binding policy levers. The EU bloc sits near symmetric (d ≈ 0.4) in a specific way: they benefit internally from their own binding targets (coordination benefit, low d for their own policies) but cannot externalize enforcement onto others (high d relative to their preferred supranational system). Vulnerable nations are excluded entirely—their directionality is not computed because they have no formal seat in the NDC process under this reading. The reading's core move is to block d-derivation at the supranational level and relocate it entirely to the state level, where state sovereignty axiomatically implies d ≈ 0 for the state itself (it cannot be targeted by a constraint it authors unilaterally).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading is NOT a case of mandatrophy (abandoned mandate persisting via inertia). The founding problem was real (Kyoto deadlock; need for universal participation) and remains structurally operative. The sovereigntist framing SOLVES that problem by making targets voluntary, which is why NDCs command near-universal participation (195 signatories in Paris 2015, maintained through 2026). The mandatrophy question would arise ONLY if the founding problem had become obsolete—i.e., if the participation constraint was no longer binding. That is not true: any attempt to impose binding supranational targets would immediately trigger withdrawal threats from fossil-dependent economies, recreating the Kyoto deadlock. The constraint persists because it solves a live coordination problem. However, there is a SECOND mandatrophy question operating at the outcome level: the founding problem was framed as 'how to get participation,' but climate advocates frame the real problem as 'how to achieve necessary emissions reductions.' Under that reframing, the sovereigntist NDC system is theater—it solves the participation mandate while failing the climate mandate, and the persistence of the participation-focused framing masks failure on the actual outcome. This is a case of GOAL DISPLACEMENT (the mechanism optimizes for participation while neglecting emissions outcomes) rather than classical mandatrophy, but it carries the same signature: a constraint whose original function has become secondary to the preservation of institutional form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    participation_vs_emissions_trade_off,
    'Does the sovereigntist reading''s success in achieving universal participation (solution to founding problem) outweigh its failure to produce emissions reductions at necessary scale (solution to climate problem)?',
    'Outcome comparison: measure emissions reductions under sovereigntist NDC system vs. counterfactual of binding supranational targets (estimated via modeling or proxy from pre-Paris baselines). If sovereigntist participation yields lower global emissions reductions than binding targets would, the trade-off was destructive; if participation unlocked sufficient co-benefits and voluntary compliance to match binding outcomes, the trade-off was productive.',
    'If the trade-off was destructive, the sovereigntist reading is a case of goal displacement—it optimizes for institutional form (universal participation) at the cost of actual climate stabilization. This would shift classification toward ''snare'' (the constraint extracts climate stabilization as the price of maintaining state sovereignty). If productive, the reading remains ''rope'' (genuine coordination with acceptable trade-offs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(participation_vs_emissions_trade_off, empirical, 'Whether participation was a successful strategy for emissions reduction or a substitution for it.').

omega_variable(
    exit_option_credibility,
    'Is the sovereigntist reading''s exit option (state unilateral withdrawal) genuinely costless, or is it suppressed by reputational, economic, or diplomatic penalties?',
    'Track the cost profile for states considering NDC withdrawal: what economic sanctions, diplomatic isolation, or reputational damage would follow? Interview state negotiators on the perceived cost of exit. Observe actual withdrawal behavior and its consequences.',
    'If exit is reputationally or diplomatically costly but not formally prohibited, the sovereigntist reading contains internalized suppression—states are identity-locked to the system by expectation of judgment, not by contractual obligation. This would increase measured suppression and lower the accessibility_collapse score (the reading''s defining claim that alternatives remain available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_credibility, empirical, 'Whether state exit from NDC commitments is structurally available or informally suppressed.').

omega_variable(
    sovereignty_doctrine_vs_planetary_boundary,
    'Is Westphalian state sovereignty an irreducible principle that cannot be overridden by planetary climate emergency, or is it a contingent institutional framework negotiable under existential pressure?',
    'Conceptual: assess whether the foundational axiom ''states have permanent sovereignty over natural resources'' is held as non-negotiable across all parties or only in parties that benefit from unconstrained fossil use. Track whether parties acknowledge a supremacy question: when state sovereignty and climate survival conflict, which takes precedence? If yes, the axiom is negotiable; if no, it is held as foundational.',
    'This is the core kernel contest. If sovereignty is overridable, the supranational reading becomes viable and the sovereigntist reading is contingent on fossil-fuel interests'' power to defend it. If sovereignty is non-negotiable, the sovereigntist reading is the only one that respects foundational commitments and the supranational reading is illegitimate. The impact on classification is structural: it determines whether the constraint is ''rope'' (genuinely preserving something valuable) or ''snare'' (using sovereignty doctrine as cover for extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_doctrine_vs_planetary_boundary, conceptual, 'Whether state sovereignty or planetary climate limits is the foundational constraint.').

omega_variable(
    enforcement_absence_as_feature_or_bug,
    'Is the absence of enforcement mechanisms in the sovereigntist reading intentional design (preserving state authority and voluntary participation) or a failure to build institutions that could enforce binding targets without triggering withdrawal?',
    'Historical analysis: did negotiators explicitly reject enforcement mechanisms to preserve participation, or were enforcement mechanisms simply not negotiated because the political will did not exist? What mechanisms were proposed and rejected? Track the negotiation record and statement-of-intent data.',
    'If intentional design, enforcement absence is feature and the reading is ''rope'' (coordination by voluntary agreement). If institutional failure, enforcement absence is bug and the reading is ''piton'' or ''snare'' (the appearance of commitment without substance, maintained by institutional theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_absence_as_feature_or_bug, empirical, 'Whether the lack of enforcement is a principled choice or an institutional capacity gap.').

omega_variable(
    reading_foreclosure_possibility,
    'Can the sovereigntist reading coexist indefinitely with the supranational reading, or does accumulated climate damage eventually force a choice between sovereignty and survival?',
    'Monitor climate tipping-point proximity and state responses: as climate impacts worsen, do states voluntarily strengthen NDC commitments (coexistence), or do vulnerable states demand supranational override of sovereignty (foreclosure)? Track the political trajectory of binding-target proposals at COP summits; rising support for binding targets despite developing-nation resistance would signal movement toward foreclosure.',
    'If coexistence holds even as impacts worsen, the readings are genuinely non-foreclosing and the constraint family remains open. If accumulated stress forces a choice, one reading forecloses the other and the kernel is resolved by necessity rather than negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, empirical, 'Whether the sovereigntist and supranational readings can coexist or whether climate pressure forces a choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 2015, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2017, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2017, 0.11).
narrative_ontology:measurement_basis(pari_tr_t2017, observed).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2019, 0.14).
narrative_ontology:measurement_basis(pari_tr_t2019, observed).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2021, 0.16).
narrative_ontology:measurement_basis(pari_tr_t2021, observed).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2023, 0.17).
narrative_ontology:measurement_basis(pari_tr_t2023, observed).
narrative_ontology:measurement(pari_tr_t2026, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(pari_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2017, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2017, 0.18).
narrative_ontology:measurement_basis(pari_be_t2017, observed).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2019, 0.22).
narrative_ontology:measurement_basis(pari_be_t2019, observed).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2021, 0.26).
narrative_ontology:measurement_basis(pari_be_t2021, observed).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2023, 0.27).
narrative_ontology:measurement_basis(pari_be_t2023, observed).
narrative_ontology:measurement(pari_be_t2026, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(pari_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2015, 0.05).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2017, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2017, 0.06).
narrative_ontology:measurement_basis(pari_su_t2017, observed).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2019, 0.08).
narrative_ontology:measurement_basis(pari_su_t2019, observed).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2021, 0.1).
narrative_ontology:measurement_basis(pari_su_t2021, observed).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2023, 0.11).
narrative_ontology:measurement_basis(pari_su_t2023, observed).
narrative_ontology:measurement(pari_su_t2026, paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(pari_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.06).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% NDCs are a contested kernel producing three structurally distinct constraints. The sovereigntist reading (this story) frames NDCs as voluntary, nationally-determined, and sovereignty-preserving—low extractiveness, states retain exit and revision authority. The supranational reading frames NDCs as binding commitments on a ratcheting trajectory—high extractiveness, supranational oversight and escalation mechanisms. The equity reading frames NDCs through Common But Differentiated Responsibilities—moderate extractiveness with asymmetric burden distribution. All three readings instantiate different ε values from the same treaty text. They are not observables; they are distinct constraints. Links: sovereigntist reading influences the other two by preserving state sovereignty as a negotiating constraint (all readings must accommodate exit threats from fossil-dependent states); supranational reading forecloses the sovereigntist reading's claim that voluntary pledges suffice for climate stabilization; equity reading coexists with sovereigntist reading but contests its definition of 'fair distribution' of revision authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
