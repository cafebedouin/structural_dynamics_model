% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market as Natural Default (Hybrid Amnesia Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   The market-as-natural-default constraint operates in two stages. Stage 1
 *   (1930s-1970s): genuine forgetting — the historical experience of
 *   successful non-market coordination (wartime planning, Keynesian demand
 *   management, developmental states) lapses from collective memory as the
 *   generation that lived it retires. Stage 2 (1980s-present): defensive
 *   rationalization — beneficiaries of the now-dominant market order
 *   (financial sector, multinational corporations, neoclassical economics
 *   establishment) weaponize the pre-existing amnesia, actively suppressing
 *   alternatives and constructing elaborate justifications (efficient markets
 *   hypothesis, rational expectations, dynamic stochastic general equilibrium
 *   models) that present market outcomes as natural laws. Extractiveness
 *   rises from 0.20 to 0.45 as the constraint shifts from a lapsed
 *   coordination device to an active extraction mechanism.
 *
 * KEY AGENTS:
 *   - market_incumbents: Primary agenda setter (institutional/arbitrage) — sets policy framework, captures rents
 *   - neoclassical_economics_establishment: Primary agenda setter/beneficiary (institutional/identity_locked) — defines epistemic boundaries, career depends on paradigm
 *   - financial_sector: Primary beneficiary (powerful/mobile) — extracts rents from financialization enabled by market naturalness
 *   - heterodox_economists: Primary payer (moderate/constrained) — marginalized, denied resources, exit blocked by professional gatekeeping
 *   - policy_alternatives_advocates: Primary payer (moderate/constrained) — excluded from policy imagination, cognitive cost of making alternatives thinkable
 *   - workers_and_communities: Primary payer (powerless/trapped) — bear material costs of austerity, privatization, commodification
 *   - rival_paradigms: Excluded (moderate/trapped) — structural exclusion from discourse and institutions
 *   - critical_social_scientists: Observer (analytical/analytical) — documents genealogy, no direct leverage on constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.45).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.7).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market as Natural Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, 'a2243293-43b0-40fe-a11b-ac2c1b7d8910').
narrative_ontology:cs_kernel_codification('a2243293-43b0-40fe-a11b-ac2c1b7d8910', distributed).
narrative_ontology:cs_authority_grounding('a2243293-43b0-40fe-a11b-ac2c1b7d8910', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a2243293-43b0-40fe-a11b-ac2c1b7d8910', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('a2243293-43b0-40fe-a11b-ac2c1b7d8910', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('a2243293-43b0-40fe-a11b-ac2c1b7d8910', foundational, market_naturalness_is_constructed_then_weaponized).
narrative_ontology:cs_axiom_status(market_naturalness_is_constructed_then_weaponized, holdable).
narrative_ontology:cs_axiom_grounding('a2243293-43b0-40fe-a11b-ac2c1b7d8910', market_naturalness_is_constructed_then_weaponized, empirically_contingent).
narrative_ontology:cs_axiom('a2243293-43b0-40fe-a11b-ac2c1b7d8910', secondary, amnesia_enables_extraction).
narrative_ontology:cs_axiom_status(amnesia_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a2243293-43b0-40fe-a11b-ac2c1b7d8910', amnesia_enables_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('a2243293-43b0-40fe-a11b-ac2c1b7d8910', embedded_liberalism_consensus).
narrative_ontology:cs_drift_state('a2243293-43b0-40fe-a11b-ac2c1b7d8910', neoliberal_hegemony, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a2243293-43b0-40fe-a11b-ac2c1b7d8910', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_incumbents).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_establishment).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_sector).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, policy_alternatives_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, workers_and_communities_subject_to_market_fundamentalism).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, natural_rate_of_unemployment).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, rational_expectations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large corporations and industry groups that benefit from market-fundamentalist policy frameworks. They fund think tanks, lobby for deregulation, and shape the Overton window. Their exit options are strong: they can relocate capital, influence multiple jurisdictions, and capture regulatory bodies.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Mainstream economics departments, top journals, central banks, and international financial institutions. Their professional identity, career advancement, and intellectual authority are fused with the market-as-natural paradigm. Exit means abandoning the entire epistemic framework that grants them status.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_establishment, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, neoclassical_economics_establishment, beneficiary).

% Banks, asset managers, and fintech firms that extract rents from the privileging of market-based allocation. They benefit from the constraint's naturalization of financialization. Their exit is mobile: they can adapt to alternative paradigms if profitable, but currently they capture immense value from the status quo.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_sector, beneficiary,
    powerful, biographical, mobile, global).

% Scholars in Marxist, post-Keynesian, institutional, ecological, and feminist economics. They bear the cost of marginalization: denied tenure, excluded from policy circles, grants, and top journals. Their exit is constrained: they can persist in peripheral institutions but cannot access the levers of power.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, heterodox_economists, payer,
    moderate, biographical, constrained, global).

% Civil society organizations, progressive policymakers, and activists pushing for industrial policy, universal basic services, or ecological planning. They pay in political capital and cognitive effort to make alternatives thinkable. Exit is constrained by the constraint's colonization of the policy imagination.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, policy_alternatives_advocates, payer,
    moderate, biographical, constrained, global).

% Populations subjected to austerity, privatization, labor market flexibilization, and commodification of essential goods. They bear the material costs of market naturalization: precarity, inequality, ecological degradation. Exit is trapped: they lack the capital or mobility to escape the constraint's material effects.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, workers_and_communities_subject_to_market_fundamentalism, payer,
    powerless, immediate, trapped, global).

% Ecological economics, feminist economics, degrowth, and other frameworks that challenge the market-as-natural ontology. They are structurally excluded from mainstream discourse, funding, and policy uptake. Their exit is trapped: the constraint's dominance makes it nearly impossible to build institutional footholds.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, rival_paradigms, excluded,
    moderate, biographical, trapped, global).

% Historians, sociologists, and political economists who study the construction of market naturalness (e.g., Mirowski, Fourcade, Polanyi scholars). They analyze the constraint from outside, documenting its genealogy and effects. Their analytical exit is unconstrained, but their influence on the constraint's operation is indirect.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, critical_social_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The market-as-natural-default belief coordinates economic expectations across a complex, globalized economy by providing a shared, seemingly apolitical framework for price formation, resource allocation, and policy evaluation. It reduces transaction costs of coordination by presenting market outcomes as inevitable rather than chosen.
% TRANSFER_FUNCTION: The arrangement transfers legitimacy, policy space, and material resources from alternative economic arrangements (planning, cooperatives, public provision, commons-based governance) to market-centric ones. Gains accrue to market incumbents, the economics establishment, and the financial sector; costs are borne by heterodox economists, policy alternatives advocates, and vulnerable populations.
% ABSENT_VOICES: Historical alternatives — wartime planning boards, social democratic corporatism, Latin American structuralism, Eastern bloc planning, postcolonial development models — and their living advocates were excluded from the discourse after the 1970s. The lapsed closure of the 1930s-1970s erased their institutional memory; the subsequent defensive rationalization cemented their exclusion.
% DISAPPEARANCE_RATIONALE: If the market-as-natural-default constraint vanished overnight, the policy imagination would immediately expand: industrial policy, public banking, universal basic services, ecological planning, and commons governance would become thinkable and actionable. Power would shift from financial markets and incumbent corporations to democratic institutions and alternative economic actors. The world would rearrange profoundly.
% FOUNDING_PROBLEM: The need for a stable, scalable coordination mechanism for complex industrial economies after the crises of the early 20th century — the Great Depression, two world wars, and the collapse of the gold standard. The market-as-natural narrative emerged as a cognitive and institutional solution to the chaos of uncoordinated national economies.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Karl Polanyi, Philip Mirowski, Quinn Slobodian, Johanna Bockman) document the deliberate construction of market naturalness by the Mont Pelerin Society and allied networks. The establishment (mainstream textbooks, central bank communications) claims the market's naturalness is a positive scientific discovery, not a political construction.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the constraint's function as a rent-protection mechanism: the market-as-natural narrative shields incumbent profits from democratic contestation. Suppression (0.7) is high because maintaining the naturalization requires active marginalization of heterodox economics, policing of journal boundaries, and disciplinary capture of central banks. Theater ratio (0.5) captures the half-genuine, half-performative nature: markets do coordinate some activity, but the *naturalness* claim is theatrical cover for power. Accessibility collapse (0.8) is high because the constraint operates at the level of the thinkable — alternatives are not just discouraged but cognitively inaccessible to most policymakers. Resistance (0.4) is moderate: heterodox economics persists but remains marginal; social movements contest specific policies but rarely the meta-constraint.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (market incumbents, economics establishment) experience the constraint as a coordination success: they built a paradigm that orders the world. The payer seats (heterodox economists, advocates, workers) experience it as an enforced cognitive prison: the paradigm makes their preferred worlds unthinkable. The engine computes this divergence from the structural data — the same constraint appears as rope from the agenda-setter seat and snare from the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Market incumbents and the economics establishment are structural beneficiaries (d near 0.0): they collect rents, status, and authority from the constraint. The financial sector is a beneficiary with mobile exit (d ~0.15). Heterodox economists, policy advocates, and workers are targets (d near 1.0): they bear the costs of marginalization, exclusion, and material immiseration. Rival paradigms are excluded rather than coordinated — their exclusion is the enforcement mechanism. Critical social scientists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint began as a genuine coordination solution (post-war embedded liberalism) but the coordination function atrophied as the narrative detached from the reality of managed markets. The mandate (stable coordination) was captured by beneficiaries who now use the constraint purely for extraction. The classification as tangled_rope (not snare) acknowledges the residual coordination function — markets do coordinate — while the high extractiveness and suppression reveal the capture. This prevents mislabeling a captured coordination mechanism as pure extraction, which would miss the genuine believers and the coordination residue that gives the constraint its durability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the hybrid_amnesia_reading''s two-stage genealogy (lapsed closure → defensive rationalization) structurally differentiate it from the lapsed_alternative_reading and beneficiary_maintained_reading of the same kernel?',
    'Comparative historical analysis of the 1930s-1970s period: if alternatives were actively suppressed (not just forgotten), the lapsed_alternative_reading is falsified; if beneficiaries did not consciously weaponize amnesia until the 1980s, the beneficiary_maintained_reading is falsified.',
    'If the two-stage structure is validated, this reading occupies a distinct structural position: it predicts a specific temporal trajectory of extractiveness (rising) and a specific suppression mechanism (amnesia as enabler). This affects classification (tangled_rope vs snare) and network influence on sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural differentiation of this kernel reading from its siblings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of heterodox economics structural (gatekeeping, funding denial) or internalized (self-censorship, identity fusion with the paradigm)?',
    'Post-exit trajectory study: track heterodox economists who leave academia — if suppression persists (they cannot publish, get cited, or influence policy), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit, making the constraint more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the marginalization of heterodox economics.').

omega_variable(
    coordination_extraction_boundary,
    'Is the market''s coordination function genuine and separable from its extraction function, or is the coordination story entirely cover for extraction?',
    'Counterfactual simulation: if market mechanisms were retained but the naturalness narrative removed (e.g., markets as democratic tools), would coordination persist? Historical cases (Allende''s Cybersyn, Yugoslav self-management) offer partial evidence.',
    'If separable, the constraint is tangled_rope (coordination + extraction). If inseparable, it is snare (pure extraction with coordination as cover). This reading claims tangled_rope; resolution would confirm or reclassify.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the market''s coordination function is genuine or a cover story for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mark_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(mark_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(mark_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.5).
narrative_ontology:measurement(mark_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.5).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(mark_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(mark_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.35).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(mark_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.43).
narrative_ontology:measurement(mark_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mark_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(mark_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(mark_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(mark_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.68).
narrative_ontology:measurement(mark_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, information_standard).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.02).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, neoliberal_policy_framework).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, financial_deregulation_regime).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, austerity_as_necessity_narrative).

% DUAL FORMULATION NOTE:
% The market_as_natural_default kernel decomposes into three readings with distinct ε trajectories: lapsed_alternative_reading (ε flat low, ~0.15), beneficiary_maintained_reading (ε high flat, ~0.55), hybrid_amnesia_reading (ε rising, 0.20→0.45). This reading's rising extractiveness creates downstream pressure on the lapsed_alternative_reading (influences) and coexists with the beneficiary_maintained_reading in contemporary discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__hybrid_amnesia_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
