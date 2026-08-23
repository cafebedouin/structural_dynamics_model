% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response (Feasibility-Bounded Present Protection)
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the
 *   climate-harm-prevention kernel as a single epsilon-invariant constraint:
 *   the standing normative-institutional arrangement under which legitimate
 *   climate response is defined as near-term resilience building, rapid
 *   mitigation is classified as politically and economically infeasible, and
 *   a higher warming trajectory is accepted as the price. The arrangement's
 *   operation is concrete — adaptation finance windows, national adaptation
 *   plans, resilience infrastructure pipelines, and the feasibility discourse
 *   that bounds what climate policy may attempt. Epsilon's referent is this
 *   standing adaptation-priority arrangement as this reading assesses it,
 *   never any alternative arrangement critics would install. The interval
 *   maps T=0 to 2005, when adaptation-first became an organized, fundable
 *   position in the post-Kyoto period, through T=20 to 2025. The claimed type
 *   and the metrics are authored independently: tangled_rope is my structural
 *   belief — the arrangement solves a real present-protection coordination
 *   problem while transferring large residual costs to parties with no seat —
 *   while the metric values describe how the arrangement actually operates;
 *   the engine computes per-seat classifications, and any divergence between
 *   claim and computation is the datum the corpus exists to take.
 *
 * KEY AGENTS:
 *   - adaptation_budget_authorities: agenda-setting seat (institutional/arbitrage) — allocates adaptation finance and maintains the feasibility framing
 *   - present_vulnerable_populations: primary beneficiary seat (powerless/trapped) — receives resilience spending; bears residual harm adaptation does not prevent
 *   - fossil_dependent_industries: secondary beneficiary (powerful/constrained) — shielded from near-term transition costs by the infeasibility premise
 *   - adaptation_finance_institutions: beneficiary/conduit (institutional/constrained) — receive mandates and budget flows, administer project pipelines
 *   - future_generations: payer seat (powerless/trapped) — bear residual warming damages with no political seat
 *   - low_adaptation_capacity_regions: payer seat (moderate/trapped) — bear the largest residual damages with the least fiscal capacity
 *   - mitigation_policy_advocates: excluded seat (organized/constrained) — would contest the prioritization; position pre-classified as infeasible
 *   - ipcc_assessment_bodies: analytical observer (institutional/analytical) — documents both locked-in warming and mitigation feasibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.62).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.55).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Climate Response (Feasibility-Bounded Present Protection)").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'e6e1dad4-3f66-48a3-9bf1-715628d0f1aa').
narrative_ontology:cs_kernel_codification('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', fixed_text).
narrative_ontology:cs_authority_grounding('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', practice).
narrative_ontology:cs_interpretation_layer_present('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa').
narrative_ontology:cs_reading_relation('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', foundational, mitigation_primary_infeasible).
narrative_ontology:cs_axiom_status(mitigation_primary_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', mitigation_primary_infeasible, empirically_contingent).
narrative_ontology:cs_axiom('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', foundational, present_protection_precedence).
narrative_ontology:cs_axiom_status(present_protection_precedence, holdable).
narrative_ontology:cs_axiom_grounding('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', present_protection_precedence, deontological).
narrative_ontology:cs_reference_frame('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', feasibility_bounded_present_protection).
narrative_ontology:cs_drift_state('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', post_renewables_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e6e1dad4-3f66-48a3-9bf1-715628d0f1aa', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_dependent_industries).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments, finance ministries, and development-bank boards that decide how climate finance and political capital are split between resilience programs and emissions reduction. They publish adaptation plans, fund sea defenses, early-warning systems, and resilient agriculture, and defend the allocation by citing political and economic limits on rapid mitigation. Their planning horizons track electoral and budget cycles; when politics shift, the allocation framing can be revised without institutional rupture.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_budget_authorities, agenda_setter,
    institutional, biographical, arbitrage, global).

% Communities already experiencing climate impacts — coastal settlements, drought-exposed farmers, heat-exposed urban residents. They receive adaptation investment that reduces but does not eliminate their exposure, and they also live inside the higher warming trajectory the prioritization accepts. Leaving exposed places is possible only through costly migration that most cannot finance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, payer).

% Producers and heavy users of fossil energy whose assets and revenue models depend on continued high emissions. The premise that rapid mitigation is politically and economically infeasible means no near-term transition costs are imposed on them; capital plans proceed on the accepted trajectory. Their exit from this position is limited by asset lifetimes and stranded-investment risk rather than by any barrier they cannot price.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_dependent_industries, beneficiary,
    powerful, biographical, constrained, global).

% Multilateral funds and development banks operating adaptation windows. They receive mandates and budget allocations, administer project pipelines, and report on resilience outcomes. Their institutional growth tracks the size of the adaptation channel; they do not set the political premise but they administer everything that flows from it, and their project pipelines depend on the channel remaining open.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions, agenda_setter).

% People who will live with the climate the present trajectory produces. They bear the residual warming damages that adaptation does not prevent — committed sea-level rise, ecosystem losses, intensified extremes — and they hold no seat in any budget process that allocates the present. Their interests appear only through proxy advocates, litigation brought on their behalf, and ombudsperson proposals.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, global).

% Low-income and geographically exposed regions — small island states, parts of Sub-Saharan Africa and South Asia — that face the largest residual damages with the least fiscal capacity to build defenses. They participate in climate negotiations through coalitions but hold little agenda power over how finance is split, and adaptation finance reaches them late and partially while the accepted trajectory compounds their exposure.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    moderate, biographical, trapped, regional).

% Campaigners, scientists, and policy coalitions arguing for emissions reduction as the primary response. They contest the infeasibility premise with cost and deployment data, but the prioritization's framing classifies their position as politically unrealistic, which keeps it at the margins of legitimate climate response and out of core budget allocations.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_policy_advocates, excluded,
    organized, generational, constrained, global).

% The international scientific assessment structure. It documents both facts the arrangement depends on — warming already locked in, impacts arriving now — and facts that pressure its premise — falling mitigation costs, remaining carbon budgets. It takes no allocation position; its reports are inputs every seat cites selectively.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, ipcc_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, fossil_dependent_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce climate finance and political capital toward resilience infrastructure — early-warning systems, coastal defenses, resilient agriculture, heat response — for populations already facing warming that no emissions path now removes, solving a present-protection problem that exists on every trajectory.
% TRANSFER_FUNCTION: Moves adaptation finance and political attention toward present-vulnerable populations and resilience projects; correspondingly shifts the costs of unmitigated warming (residual damages, infrastructure lock-in) onto future generations and low-adaptation-capacity regions, and relieves present high-emitting actors of near-term transition costs.
% ABSENT_VOICES: Future generations have no seat and appear only through proxy advocates and rights litigation brought on their behalf. Low-adaptation-capacity regions are present in negotiation processes but hold little agenda power over allocation rules. Mitigation-first advocates are in the room but their position is pre-classified as infeasible, which functions as exclusion from the budget-relevant conversation.
% DISAPPEARANCE_RATIONALE: If the prioritization norm vanished overnight, adaptation budgets and resilience institutions would lose their legitimating frame and the vacated agenda space would be contested between the rival readings of the same commitment; present vulnerable populations would face a near-term protection gap before any alternative allocation settled; high-emitting actors would lose the infeasibility shield and face immediate transition-cost demands. Organized parties depend on the arrangement, so the world rearranges.
% FOUNDING_PROBLEM: Early climate politics faced a repeated failure of the primary response: coordinated global mitigation collapsed at Kyoto and Copenhagen while warming impacts were already arriving. The adaptation-priority reading was built to solve the problem of responding legitimately to climate harm when the primary response is politically blocked — protect who can be protected now, within what politics permits.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is split and partly outside the beneficiary set. IPCC assessment working groups corroborate the live half — warming already committed, impacts arriving — while the same assessments, together with IEA and IRENA deployment and cost data, challenge the infeasibility half by documenting rapid mitigation that is technically proven and increasingly cheap. Youth climate litigation and small-island-state submissions attest the harm framing from outside the adaptation-funding coalition. No external source attests the founding problem exactly as the arrangement's holders state it; the political block is observable only from inside the political process, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: part of the arrangement's operation is genuine necessity — some warming is locked in and present protection is non-optional — so not all of its cost structure is extraction; the extractive component is the increment of accepted warming beyond what mitigation-feasible pathways would produce, transferred onto parties with no seat, plus the near-term transition costs the infeasibility premise relieves present high emitters of. Suppression 0.55 is authored as a raw structural property (it is not scaled by power or scope; the engine owns any scaling): it operates through agenda control, funding-allocation rules, and feasibility framing rather than coercion, with an internalized component — policymakers' realism about what is possible is itself partly a product of the framing. Theater 0.28: adaptation delivery is largely functional (defenses, early warning, resilient agriculture are real), with a growing performative share in resilience pledges and feasibility summitry. Accessibility_collapse 0.45: alternatives remain articulated and live — they are marginalized, not collapsed. Resistance 0.6: sustained pushback from climate-justice movements, youth litigation, exposed-state coalitions, and parts of the scientific community. The measurement series run on one shared six-point grid (T=0,4,8,12,16,20) with all three tracked metrics authored at every point; the trajectories are monotonic rather than cyclical — extraction and enforcement intensity rise together as the accepted trajectory diverges further from safe pathways and the infeasibility premise hardens into institutional infrastructure (adaptation plans, finance windows, planning bureaucracies that now have careers and budgets attached to the premise).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement is pragmatic governance: doing what is possible for people who need protection now, under real political constraints. From the future-generations and low-adaptation-capacity seats the same structure is an imposed transfer: their damages are the budget balance that funds present protection, and neither seat can decline or exit. From the present-vulnerable seat it is partial rescue inside continued exposure — the arrangement's declared beneficiaries also bear residual harm, which is why the seat carries a payer secondary role. Fossil-dependent industries experience it as regulatory relief. The engine computes these per-seat classifications from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. future_generations (payer, powerless, trapped, generational horizon) sits nearest the full-target end: the arrangement's largest costs land there and the seat has zero exit and zero agenda power. low_adaptation_capacity_regions (payer, moderate, trapped) sits near the target end with slightly damped extraction — they receive some adaptation finance and hold organized voice through negotiation coalitions. present_vulnerable_populations are genuinely dual-positioned: adaptation spending subsidizes them (damping d toward the beneficiary end) while the accepted trajectory taxes them (pushing toward the target end); the derivation should place them near the middle, which is why they carry both roles rather than a directionality override. fossil_dependent_industries (beneficiary, powerful, constrained) sit near the beneficiary end — the infeasibility premise relieves them of transition costs — with constrained exit reflecting stranded-asset exposure rather than extraction borne. adaptation_finance_institutions benefit through mandates and budgets but act as conduits rather than capturers of the underlying transfer. No directionality overrides are authored: the structural derivation from roles, power, and exit captures these relationships, and the one dual-positioned seat is handled through its secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. Reading the arrangement as a snare would erase the genuine coordination function — present vulnerable populations face warming that no emissions path now removes, and resilience spending solves a real problem they would otherwise face unassisted. Reading it as a rope would erase the asymmetric extraction — the accepted trajectory is not forced by physics alone but by a maintained political premise, and its residual costs land on parties who never agreed and cannot object. The R5 interview records the founding problem as contested rather than dead, so no dead-mandate mismatch flag fires; the live risk is drift, not atrophy. If the infeasibility premise hardens from description into self-fulfilling settlement while cost data undermines it, the coordination share shrinks and the arrangement slides toward snare — the rising base_extractiveness series is the early signature of exactly that drift, and the fixing_cost authoring (prohibitive) records why no single agenda-setter bears the cost of arresting it: the present concentrated costs of overturning the allocation exceed the present-diffuse benefits any one fixer captures, which is the same collective-action structure that produced the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_infeasibility_status,
    'Is mitigation''s political and economic infeasibility a fixed structural constraint, or a contingent settlement that is actively maintained by the coalition the arrangement shelters?',
    'Comparative natural experiments: jurisdictions and periods where mitigation was pursued aggressively under similar political conditions (cost-decline-driven deployment, subnational and coalition-of-the-willing pathways). If mitigation scales where political coalitions differ, infeasibility is contingent rather than structural.',
    'If contingent, the reading''s foundational axiom is empirically failing and the constraint drifts toward snare — infeasibility as cover for shielded extraction; if structural, the reading is tragic necessity and the residual-cost transfer is unavoidable, supporting the rope-like coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_infeasibility_status, empirical, 'Whether the infeasibility premise is structural fact or maintained political settlement').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the climate_harm_prevention kernel — how does the beneficiary/victim structure change under the sibling readings, and is the disagreement located only in the feasibility premise and the time-horizon weighting of harm?',
    'Comparative authoring of the sibling readings as separate constraint stories over the same referent; the structural delta to test is whether future generations become the primary protected class (mitigation_priority) or present high-consumption populations become targets and the accepted-trajectory transfer is repudiated (degrowth_reading).',
    'Under either sibling, this story''s victim and beneficiary sets invert or re-weight and its epsilon is re-authored for the sibling''s own lights; the current value of 0.62 is reading-indexed to the adaptation-priority arrangement and does not transfer to the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-level contest: three readings of climate harm prevention with different victim sets and time-horizon weights').

omega_variable(
    intergenerational_weighting,
    'What ethical weight should the welfare of future generations carry relative to present persons, and is the discount the arrangement implicitly applies defensible?',
    'Not resolvable by data alone — it is a normative parameter. Partial evidence comes from democratic deliberation, constitutional and human-rights courts handling intergenerational-justice claims, and revealed policy preferences; none is decisive.',
    'A near-zero discount renders the accepted-trajectory transfer a massive extraction and pushes the classification toward snare; a heavy discount renders it tragic necessity and supports the coordination reading. The classification is sensitive to this parameter in a way no empirical measurement settles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_weighting, preference, 'Normative weighting that determines whether residual future costs count as extraction or necessity').

omega_variable(
    adaptation_sufficiency_limit,
    'Does adaptation actually protect present vulnerable populations at the accepted warming level, or does protection fail beyond a thermal threshold, making the arrangement''s benefit promise structurally hollow?',
    'Adaptation-gap assessments (UNEP Adaptation Gap Reports and successors), observed impact losses versus adaptation investment at rising temperatures, and hard-limit studies on heat, water, and sea-level exposure.',
    'If adaptation fails catastrophically above roughly two degrees, the arrangement''s declared beneficiaries are also among its casualties and the coordination function collapses toward pure extraction wearing a protection cover story; if adaptation scales, the beneficiary declaration holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_sufficiency_limit, empirical, 'Whether the adaptation benefit promise survives the warming trajectory the arrangement accepts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t4, climate_harm_prevention__adaptation_priority, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(clim_tr_t4, observed).
narrative_ontology:measurement(clim_tr_t8, climate_harm_prevention__adaptation_priority, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(clim_tr_t8, observed).
narrative_ontology:measurement(clim_tr_t12, climate_harm_prevention__adaptation_priority, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(clim_tr_t12, observed).
narrative_ontology:measurement(clim_tr_t16, climate_harm_prevention__adaptation_priority, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(clim_tr_t16, observed).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(clim_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t4, climate_harm_prevention__adaptation_priority, base_extractiveness, 4, 0.49).
narrative_ontology:measurement_basis(clim_be_t4, observed).
narrative_ontology:measurement(clim_be_t8, climate_harm_prevention__adaptation_priority, base_extractiveness, 8, 0.53).
narrative_ontology:measurement_basis(clim_be_t8, observed).
narrative_ontology:measurement(clim_be_t12, climate_harm_prevention__adaptation_priority, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(clim_be_t12, observed).
narrative_ontology:measurement(clim_be_t16, climate_harm_prevention__adaptation_priority, base_extractiveness, 16, 0.6).
narrative_ontology:measurement_basis(clim_be_t16, observed).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(clim_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t4, climate_harm_prevention__adaptation_priority, suppression_requirement, 4, 0.44).
narrative_ontology:measurement_basis(clim_su_t4, observed).
narrative_ontology:measurement(clim_su_t8, climate_harm_prevention__adaptation_priority, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(clim_su_t8, observed).
narrative_ontology:measurement(clim_su_t12, climate_harm_prevention__adaptation_priority, suppression_requirement, 12, 0.51).
narrative_ontology:measurement_basis(clim_su_t12, observed).
narrative_ontology:measurement(clim_su_t16, climate_harm_prevention__adaptation_priority, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(clim_su_t16, observed).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.55).
narrative_ontology:measurement_basis(clim_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'legitimate climate response' covers three structurally distinct claims with different epsilon values and victim sets; per the epsilon-invariance principle they are authored as separate stories in one kernel family rather than one story with a feasibility parameter. This (adaptation-priority) story authors epsilon for the standing adaptation-priority arrangement as this reading assesses it. The structural coupling runs through the carbon budget: this reading's accepted trajectory depletes the mitigation headroom the mitigation_priority reading depends on, and the degrowth_reading rejects the growth premise the other two share. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
