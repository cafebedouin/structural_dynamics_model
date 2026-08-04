% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Endogenous Practice Standardization via Voluntary Adoption
 *   domain: political_history/institutional_change/modernization
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous-displacement reading of the
 *   legitimacy-of-practice-standardization kernel. The reading holds that
 *   practice change is legitimate and self-sustaining when driven by
 *   voluntary adoption rooted in perceived utility or cultural
 *   prestige—calendar reforms spread because merchants find standardized
 *   timing useful; dress norms shift because adopters perceive status or
 *   efficiency gains; administrative procedures change because communities
 *   find new forms functional. The reading distinguishes itself from the
 *   exogenous-override reading (which grounds legitimacy in state decree) and
 *   the dual-practice-equilibrium reading (which partitions domains between
 *   state and tradition). In this reading, early adopters and
 *   utility-perceiving communities are beneficiaries; traditional authority
 *   holders experience delegitimization and loss of gatekeeping role; lagging
 *   populations face rising coordination pressure but not direct coercion.
 *   The constraint's type is rope under this reading: genuine coordination
 *   function (standardization reduces transaction costs), minimal suppression
 *   (adoption is voluntary), low theater (utility perception is the driving
 *   mechanism, not performed compliance). Extractiveness rises modestly as
 *   adoption spreads because traditional authorities become structurally
 *   marginalized, but this reading treats that as a consequence of voluntary
 *   utility-preference shifts, not as systematic extraction. The measured
 *   trajectory matches endogenous diffusion: slow early adoption,
 *   acceleration in the middle period as network effects kick in and utility
 *   becomes obvious, plateauing at saturation with holdout populations and
 *   ritual domains retaining old practices where utility perception remains
 *   weak or identity-fused.
 *
 * KEY AGENTS:
 *   - Adopting communities: voluntary shifters perceiving utility or prestige
 *   - Commercial interests: profit from standardization but do not coerce it
 *   - Traditional authority holders: bear delegitimization as utility-perception erodes their gatekeeping role
 *   - Early adopter networks: drive visible diffusion and model adoption for others
 *   - Lagging populations: face rising coordination pressure but remain unmoved by utility perception
 *   - State authority: facilitates or accelerates but does not drive legitimacy in this reading
 *   - Analytical observer: tracks adoption curves to distinguish endogenous from exogenous mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.22).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Endogenous Practice Standardization via Voluntary Adoption").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/institutional_change/modernization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '219a151f-b934-4a76-9796-ce1b8684e3e9').
narrative_ontology:cs_kernel_codification('219a151f-b934-4a76-9796-ce1b8684e3e9', distributed).
narrative_ontology:cs_authority_grounding('219a151f-b934-4a76-9796-ce1b8684e3e9', diffuse_epistemic).
narrative_ontology:cs_reading_relation('219a151f-b934-4a76-9796-ce1b8684e3e9', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('219a151f-b934-4a76-9796-ce1b8684e3e9', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('219a151f-b934-4a76-9796-ce1b8684e3e9', foundational, voluntary_adoption_grounds_legitimacy).
narrative_ontology:cs_axiom_status(voluntary_adoption_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('219a151f-b934-4a76-9796-ce1b8684e3e9', voluntary_adoption_grounds_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('219a151f-b934-4a76-9796-ce1b8684e3e9', foundational, utility_perception_drives_adoption).
narrative_ontology:cs_axiom_status(utility_perception_drives_adoption, holdable).
narrative_ontology:cs_axiom_grounding('219a151f-b934-4a76-9796-ce1b8684e3e9', utility_perception_drives_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('219a151f-b934-4a76-9796-ce1b8684e3e9', cultural_evolution_via_preference_aggregation).
narrative_ontology:cs_drift_state('219a151f-b934-4a76-9796-ce1b8684e3e9', contemporary_globalization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('219a151f-b934-4a76-9796-ce1b8684e3e9', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_interests_in_new_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_networks).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authority_holders).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, lagging_populations).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_drives_legitimacy).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, voluntary_adoption_principle).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, utility_perception_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that voluntarily shift to new practices (calendar reform, dress norms, administrative procedures) because perceived utility or cultural prestige makes adoption attractive. They experience reduced transaction costs, coordination gains with external actors, or status signaling benefits. Their exit option is always open: they retain the choice to revert if utility perception shifts. Adoption is gradual, often uneven across demographic groups (early adopters, laggards, holdouts in ritual contexts).
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_communities, beneficiary,
    moderate, generational, mobile, regional).

% Merchants, manufacturers, and technical professionals whose business models depend on standardized practices spreading. They profit from calendar alignment enabling trade, from dress-good production and marketing, from administrative efficiencies enabling commerce. Their interests align with adoption but they do not coerce it—they benefit from the pull of perceived utility that drives voluntary shifts.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, commercial_interests_in_new_practice, beneficiary,
    powerful, biographical, arbitrage, global).

% Religious specialists, ritual masters, and keepers of inherited practice who experience voluntary adoption as delegitimizing their guardianship. They bear a diffuse cost: their authority to govern practice through transmission erodes as communities choose new norms over inherited ones. Exit is identity-fused (their role is constituted through practice mastery); they cannot leave without ceasing to exist as practitioners. They often advocate for dual-practice equilibrium (public conformity, private tradition), which this reading treats as transitional friction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authority_holders, payer,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_authority_holders, observer).

% Cosmopolitan traders, educated elites, and professional communities who adopt new practices first, gaining status and efficiency advantages. They model adoption for others and create positive-feedback loops: as adoption spreads, the utility of adopting rises (network effects). Their adoption decisions are driven by calculated utility, not coercion.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, early_adopter_networks, beneficiary,
    moderate, biographical, mobile, regional).

% Rural, poor, or ritually committed populations who adopt slowly because new practices carry transaction costs (new tools, skill retraining, disruption of inherited routines) that outweigh perceived utility for them. They are not coerced into adoption; rather, they remain in the old practice space until either the cost-benefit calculus shifts (new practice becomes cheaper/easier) or adoption pressure from surrounding communities makes persistence impractical. Their constraint is structural scarcity, not extraction.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, lagging_populations, payer,
    powerless, generational, constrained, local).

% Government actors who may facilitate or accelerate adoption through standardization decrees, tax incentives, or administrative unification—but in this reading, legitimacy derives from the voluntary utility calculus, not from state command. State action is secondary; it amplifies endogenous momentum rather than displacing it. (This reading differs from the exogenous_override reading, which would place state decree as the legitimacy ground.)
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authority, observer,
    institutional, generational, analytical, national).

% Historians and social scientists documenting adoption curves, mapping diffusion patterns, and testing whether adoption fits the S-curve of voluntary utility-driven innovation or the flatter trajectory of coerced/decreed change. They seek to distinguish this reading's structural signature (gradual, uneven, reversible where utility perception fails) from exogenous-override dynamics (uniform, rapid, persistent even when utility is disputed).
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes practices (calendar, dress, administrative procedure) across regions and communities, reducing transaction costs for trade, communication, and inter-group coordination. A shared calendar enables commerce scheduling; standardized dress reduces friction in professional interaction; uniform administrative procedure enables scale. The coordination function emerges from voluntary adoption driven by perceived utility gains, not from imposed mandate.
% TRANSFER_FUNCTION: Transfers authority to govern practice from traditional authorities (ritual specialists, community elders) to a diffuse ensemble of utility-perceiving adopters and commercial interests. The transfer is not extraction per se—traditional authorities lose status and teaching authority, but they do not transfer resources to a concentrated payer seat. The cost they bear is delegitimization and erosion of their gatekeeping role.
% ABSENT_VOICES: Non-adopting holdouts, particularly in ritual and identity-constituting domains, are marginalized. Conservative populations whose transaction costs are high relative to perceived utility are not consulted on adoption timelines; as adoption accelerates around them, their cost rises (isolation, incompatibility with surrounding norms). The reading acknowledges this friction but treats it as a transitional phenomenon of utility-perception heterogeneity, not systematic extraction—unlike the exogenous reading, which would hear holdout voices as evidence that coercion is necessary.
% DISAPPEARANCE_RATIONALE: If voluntary-adoption legitimacy were abandoned and reverted to exogenous-decree or dual-practice equilibrium models, the trajectory of practice change would shift: adoption would either require explicit state enforcement (making it non-voluntary) or would crystallize into permanent parallel systems. The reading's claim is that adoption curves driven by voluntary utility perception are self-sustaining; their disappearance would require active suppression of the utility perception itself (propaganda campaigns denying utility, or institutional barriers to adoption), which would then shift the observable signature.
% FOUNDING_PROBLEM: How does practice legitimately change across communities with different risk-tolerance, transaction-cost structures, and attachment to inherited norms? The endogenous reading answers: through communities adopting practices they perceive as useful, creating diffusion networks and positive feedback without requiring top-down mandate. The founding problem is the multi-level coordination problem of getting diverse communities to converge on a new norm; the endogenous mechanism solves it by letting utility perception do the work.
% FOUNDING_PROBLEM_CORROBORATION: Historians studying Gregorian calendar adoption (16th–18th century), metric adoption (19th–20th century), and dress-code shifts document regional variation and elite-to-mass diffusion patterns consistent with utility-driven adoption. However, state authorities and economic historians document state decrees accelerating adoption in parallel—the exogenous reading claims decrees are the primary lever. The corroboration is mixed: adoption curves from multiple domains (calendar, metric, administrative procedure) show both endogenous momentum and exogenous acceleration, but historians disagree on the relative weight. No external actor entirely outside both readings adjudicates the founding problem; the disagreement is between competing readings held by different scholarly traditions.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.38 at interval end, moderate-low, reflecting the mismatch between the reading's claim (legitimate voluntary adoption) and the structural reality of delegitimization costs borne by traditional authorities. The extraction is not to a concentrated beneficiary—it is diffuse erosion of gatekeeping authority. Suppression is 0.22, low, because adoption is genuinely voluntary; resistance is 0.52, moderate, because holdout populations and identity-locked traditional authorities resist the implicit pressure to abandon inherited practices. Theater is 0.15, low, because the constraint's operation is driven by genuine utility perception, not by performative compliance—early adopters are not play-acting utility; they are actually gaining efficiency or status. The measurement trajectory shows extractiveness rising steeply to t=50 as adoption accelerates and traditional authority erosion becomes visible, then declining slightly to t=100 as adoption stabilizes and holdout populations ossify. This pattern is consistent with endogenous diffusion: peak extraction pressure occurs during the transition phase (t=25-75) when voluntary adoption is displacing traditional practices fastest; once adoption plateaus and a stable equilibrium emerges with some populations adhering to old practices in ritual contexts, the extraction pressure eases slightly (the constraint stops actively displacing because displacement is nearly complete). Suppression requirement follows a similar pattern: rises during the transition phase as holdout populations experience rising coordination pressure, plateaus as the new equilibrium stabilizes. Theater ratio remains consistently low, supporting the claim that the mechanism is genuinely utility-driven rather than enforced through performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The traditional authority seat and the early-adopter seat should compute different constraint types from the same structural data. An early adopter perceives genuine coordination value and experiences the constraint as enabling cooperation (rope-like); a traditional authority perceives their own delegitimization and the constraint as destabilizing their role (snare-like or piton-like—the authority structure is maintained theatrically but its function has atrophied). The engine computes this divergence from directionality: beneficiaries compute toward rope, targets compute toward snare. The reading's narrative claim is that endogenous adoption resolves this divergence in favor of beneficiaries—that utility perception is the ultimate arbiter and traditional authority loss is justified as the cost of coordination gain. But the structural data is symmetric: both seats experience real effects; the reading chooses which effects to weigh as legitimate.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting communities and commercial interests sit near the beneficiary end of the directionality scale (d ~ 0.2-0.35): they perceive utility or profit from adoption, their exit option is mobile (they chose to adopt; they could revert if utility shifted), and they accumulate coordination gains. Traditional authority holders sit near the target end (d ~ 0.75-0.85): their identity is fused to practice mastery, they cannot exit without ceasing to exist as practitioners, and they experience delegitimization as a structural consequence—not through overt coercion, but through the erosion of the social demand for their authority. Lagging populations sit mid-range (d ~ 0.55-0.65): they experience rising coordination pressure as adoption spreads around them (constrained exit option), but they are not systematically extracted from—their constraint is structural scarcity and high transaction costs, not predatory capture. The state authority is observational (d ~ 0.5): it neither benefits decisively nor bears costs; it can facilitate or impede but does not determine legitimacy under this reading. The asymmetry in directionality is the key to understanding this constraint as a rope: the coordination function is real, and beneficiaries genuinely gain from it; but traditional authorities lose authority and identity-integrity, which the endogenous reading treats as the inevitable cost of cultural evolution, not as systematic extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT manifest mandatrophy under this reading. The mandate (voluntary adoption driven by utility perception) remains live; the founding problem (multi-level coordination of practice change) is still unsolved without it; the mechanism (utility perception and adoption diffusion) is still the primary driver. However, there is latent tension: if the reading were adopted as doctrine and formal state policy, and the state began enforcing 'voluntary' adoption via administrative pressure or subtle coercion, the constraint would shift toward theater (apparent voluntariness masking enforcement). The measurement of theater_ratio remaining low is the diagnostic test for mandatrophy absence: if performance rose sharply while utility-perception signals remained flat, that would indicate mandatrophy (the mandate to be voluntary is failing, and performance is compensating). This reading's survival depends on adoption curves remaining consistent with utility-driven diffusion; if adoption accelerates beyond what utility perception can explain, the reading's empirical ground erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    utility_perception_objectivity,
    'Is perceived utility an objective measure of practice efficiency, or a socially constructed preference that reflects elite interests masked as utility?',
    'Comparative analysis of adoption patterns across socioeconomic strata: if adoption is genuinely utility-driven, lagging populations should adopt once transaction costs fall or utility becomes salient; if adoption is ideologically driven, lagging populations should adopt on a different timeline than utility-based models predict, correlating instead with elite pressure or cultural prestige seeking.',
    'If perceived utility is socially constructed, the endogenous reading becomes a cover story for elite-driven displacement of traditional practice; if utility is objective, the reading''s legitimacy claim holds. This determines whether extraction exists (cover story) or whether cost to traditional authorities is justified as coordination gain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(utility_perception_objectivity, conceptual, 'Whether utility perception is objective or socially constructed preference.').

omega_variable(
    traditional_authority_extraction_vs_adaptation,
    'Do traditional authorities bear delegitimization as an extraction loss, or do they adapt by repositioning as custodians of heritage (shifting from active practice governance to curated tradition)?',
    'Post-adoption institutional trajectories: do traditional authorities disappear, become marginalized (extraction), or successfully reposition as cultural guardians in a new domain (adaptation, no extraction)? Examine whether ritual specialists, for instance, transform from daily practice masters to ceremonial curators after adoption.',
    'If adaptation is common, the cost to traditional authorities is repositioning friction, not systematic extraction. If marginalization is common, extraction is significant and the reading''s claim of non-extraction is falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditional_authority_extraction_vs_adaptation, empirical, 'Whether traditional authority loss is transitioned to new roles or constitutes systematic extraction.').

omega_variable(
    endogenous_vs_exogenous_causal_priority,
    'In cases where both voluntary adoption and state facilitation occur in parallel, which is the primary driver? Does state facilitation accelerate adoption that is already endogenously spreading, or does state decree initiate adoption that would not occur endogenously?',
    'Counterfactual analysis: cases where state facilitation is absent (legal prohibition, lack of state capacity, neutral state stance) should show slower adoption if exogenous action is primary, or unchanged adoption trajectory if endogenous mechanisms dominate. Temporal sequencing analysis: does adoption accelerate after state decree, or does state decree formalize adoption already underway?',
    'If exogenous action is primary, the endogenous reading is a post-hoc rationalization of state-driven change. If endogenous mechanisms are primary and state action is secondary acceleration, the reading''s legitimacy claim stands. This omega determines whether the reading forecloses or coexists-with the exogenous reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_causal_priority, empirical, 'Whether endogenous or exogenous mechanisms are the causal primary driver of adoption.').

omega_variable(
    reading_frame_under_determination,
    'This reading treats adoption curves and utility perception as the observable ground for legitimacy. But an alternative framing could treat the same curves as evidence of elite propaganda success, and utility perception as a manipulated preference. What signals could distinguish genuine utility-perception from manufactured preference?',
    'Longitudinal preference mapping: track whether adopting populations report utility gains before, during, or after adoption. Measure consistency of utility claims across populations; if utility perception is genuine, it should be stable and cross-culturally recognizable (efficiency gains are similar everywhere); if utility is manufactured, it should vary with elite messaging. Cross-validate with behavioral data: do adopters use new practices in contexts where they could revert? Do they report friction costs and satisfaction tradeoffs, or only praise?',
    'If genuine utility is revealed, the endogenous reading is supported and adoption is non-coercive. If utility perception is ex-post rationalization, the reading is a frame for legitimizing elite-driven displacement, and extraction exists where the reading claims coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_under_determination, conceptual, 'Whether utility perception is the authentic driver or a reframing of elite-driven change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(legi_tr_t0, projected).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(legi_tr_t10, observed).
narrative_ontology:measurement(legi_tr_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement_basis(legi_tr_t25, observed).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 50, 0.13).
narrative_ontology:measurement_basis(legi_tr_t50, observed).
narrative_ontology:measurement(legi_tr_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 75, 0.15).
narrative_ontology:measurement_basis(legi_tr_t75, observed).
narrative_ontology:measurement(legi_tr_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement_basis(legi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(legi_be_t0, projected).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(legi_be_t10, observed).
narrative_ontology:measurement(legi_be_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(legi_be_t25, observed).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(legi_be_t50, observed).
narrative_ontology:measurement(legi_be_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 75, 0.42).
narrative_ontology:measurement_basis(legi_be_t75, observed).
narrative_ontology:measurement(legi_be_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(legi_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(legi_su_t0, projected).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(legi_su_t10, observed).
narrative_ontology:measurement(legi_su_t25, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(legi_su_t25, observed).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 50, 0.21).
narrative_ontology:measurement_basis(legi_su_t50, observed).
narrative_ontology:measurement(legi_su_t75, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 75, 0.23).
narrative_ontology:measurement_basis(legi_su_t75, observed).
narrative_ontology:measurement(legi_su_t100, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement_basis(legi_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, resource_allocation).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimacy_of_practice_standardization kernel. The kernel is the contested ground on which three readings coexist: endogenous_displacement (this story), exogenous_override (state decree drives legitimacy), and dual_practice_equilibrium (domain partition resolves the contest). Each reading produces different structural predictions for adoption patterns, enforcement machinery, and holdout dynamics. The kernel itself is not a constraint; the three readings are three distinct constraints that share a common normative kernel but diverge in their ε values (extraction), suppression (enforcement), and claimed_type. This story claims the endogenous reading produces a rope (genuine coordination, voluntary, low suppression); siblings claim snare or tangled_rope (extraction, enforcement, or domain fragmentation). Stories are linked to enable analysis of how different readings of the same kernel show different empirical signatures and compute different per-seat types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__endogenous_displacement_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
