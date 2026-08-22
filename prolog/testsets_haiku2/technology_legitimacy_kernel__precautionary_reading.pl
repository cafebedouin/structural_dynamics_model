% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Reversibility Standard for Climate Mitigation Technology Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The precautionary reading of the technology-legitimacy kernel grounds
 *   legitimacy in reversibility: a climate mitigation technology is
 *   legitimate if and only if its worst-case failure modes and legacy costs
 *   can be bounded and reversed within a generation (~30 years). This reading
 *   directly benefits renewable energy operators (whose decommissioning
 *   footprint and land remediation align with the criterion) while excluding
 *   nuclear (whose waste legacy and accident risk extend centuries), coal and
 *   gas baseload (whose climate damage and stranded-asset costs are
 *   irreversible at generational timescale), and fast-deplorable but
 *   legacy-heavy technologies. The reading instantiates as a tangled_rope:
 *   genuine coordination function (protecting future generations from
 *   irreversible technological lock-in), paired with asymmetric extraction
 *   (nuclear advocates, baseload defenders, and velocity-optimizers bear the
 *   cost of precautionary gating while renewables collect the legitimacy
 *   benefit). The claim/metric independence is deliberate: this constraint is
 *   CLAIMED as tangled_rope on its structural reading (coordination +
 *   enforcement + asymmetric extraction), and the authored metrics
 *   (extractiveness 0.68, suppression 0.52, theater 0.28) describe its actual
 *   operation in policy debates and technology approval. The sibling readings
 *   (reliability_primacy, velocity_primacy) would authorize different
 *   beneficiary/victim sets and different enforcement machinery, yielding
 *   different ε and different types.
 *
 * KEY AGENTS:
 *   - renewable_energy_operators: institutional power, beneficiary, arbitrage exit — collect legitimacy and capital from precautionary gating
 *   - nuclear_technology_advocates: organized power, payer, constrained exit — bear exclusion cost and must defend via alternative criteria
 *   - dispatchable_baseload_providers: powerful, payer, constrained exit — excluded unless they can prove reversibility within a generation
 *   - future_generations: powerless, beneficiary, trapped — benefit in principle but cannot participate in enforcement
 *   - energy_policy_enforcers: institutional power, agenda_setter, mobile exit — set and maintain the precautionary standard
 *   - climate_scientists_and_ethicists: analytical power, beneficiary, analytical exit — their authority is vindicated by precautionary framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.52).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Reversibility Standard for Climate Mitigation Technology Legitimacy").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '72b1fbe3-eb97-4edc-8b52-cd67fe4352ad').
narrative_ontology:cs_kernel_codification('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', fixed_text).
narrative_ontology:cs_authority_grounding('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', extraction).
narrative_ontology:cs_interpretation_layer_present('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad').
narrative_ontology:cs_reading_relation('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', foundational, irreversible_technology_exclusion_obligation).
narrative_ontology:cs_axiom_status(irreversible_technology_exclusion_obligation, holdable).
narrative_ontology:cs_axiom_grounding('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', irreversible_technology_exclusion_obligation, deontological).
narrative_ontology:cs_axiom('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', secondary, generational_stewardship_primacy).
narrative_ontology:cs_axiom_status(generational_stewardship_primacy, holdable).
narrative_ontology:cs_axiom_grounding('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', generational_stewardship_primacy, deontological).
narrative_ontology:cs_reference_frame('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', intergenerational_reversibility_standard).
narrative_ontology:cs_drift_state('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', contemporary_renewable_scaling_success, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('72b1fbe3-eb97-4edc-8b52-cd67fe4352ad', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, present_generation_energy_consumers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, dispatchable_baseload_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, climate_scientists_and_ethicists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar and wind companies collect deployment permits, investment capital, and policy preferences because their technology's reversibility (land can be remediated, equipment recycled, lifecycle bounded to decades) aligns with precautionary legitimacy. They benefit from the criterion excluding competitors and can exit profitably by selling assets if the policy environment shifts.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Nuclear operators, fuel suppliers, waste-handling firms, and industry consortia bear the cost of precautionary exclusion. Their technology is legitimate under reliability and velocity criteria but illegitimate under precaution. They must invest heavily in counter-advocacy, waste-isolation engineering, and accident-prevention to re-enter legitimacy debates. Exiting means abandoning nuclear altogether; staying means fighting the standard.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_technology_advocates, payer,
    organized, generational, constrained, global).

% Natural gas utilities, coal plants with carbon capture, and any firm dependent on stable baseload generation must prove their technology is reversible within a generation to claim precautionary legitimacy. Gas can transition to renewables with modest stranded assets; coal is nearly impossible to reverse. They face forced obsolescence (coal) or capital-intensive transition costs (gas).
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, dispatchable_baseload_providers, payer,
    powerful, biographical, constrained, global).

% Future populations benefit from present deployment of reversible technologies (they inherit manageable legacy costs) but cannot enforce the constraint or negotiate its terms. They depend on present-generation advocates and institutional rules to represent their interests. The constraint binds them to inherit whatever irreversible commitments present generation makes despite precautionary intent.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, future_generations, observer).

% Current energy consumers benefit from precautionary gating ensuring no catastrophic waste or accident legacy is imposed on their children. They may face higher energy costs in the short term if renewable deployment is slower than dispatchable alternatives. They participate through voting and regulatory comment but have diffuse influence.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, present_generation_energy_consumers, beneficiary,
    organized, biographical, constrained, global).

% Scientific and ethical authority bodies (IPCC, National Academies, universities) benefit from precautionary governance because it vindicated their intergenerational-justice framework and made reversibility assessment their core analytical function. They do not collect rents but their epistemic authority is elevated by precautionary gatekeeping.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_scientists_and_ethicists, beneficiary,
    analytical, civilizational, analytical, global).

% Governments, regulatory bodies (EPA, IEA, national energy commissions), and international bodies (UNFCCC) set and enforce the precautionary criterion through permitting, financing, and technology assessment. They face political pressure from excluded advocates and must continuously defend the criterion. They can modify the standard if political costs become too high (mobile exit), but they bear administrative costs of maintaining it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, energy_policy_enforcers, agenda_setter,
    institutional, generational, mobile, national).

% Energy analysts and climate-urgency advocates who prioritize speed-to-scale are structurally excluded from the precautionary conversation. They would argue that decarbonization targets (2030/2050) are incompatible with waiting for reversibility assessment, and that fast-deploying nuclear or coal-with-CCS should outrank precautionary concerns. Their objections are treated as competing readings, not as voices inside precautionary legitimacy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, velocity_advocates, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__precautionary_reading, renewable_energy_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__precautionary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates climate mitigation technology choices toward options that do not impose irreversible harm on future generations. Solves the intergenerational coordination problem by requiring that present-generation energy deployment not lock future populations into catastrophic risks, perpetual stewardship, or technological path dependency they cannot exit.
% TRANSFER_FUNCTION: Transfers epistemic authority and policy legitimacy from technology-speed optimizers and reliability-maximizers to precaution-advocates. Transfers investment capital and deployment permission from nuclear, coal-baseload, and fast-nonreversible technologies to renewables and reversible hybrid systems. Transfers stewardship burden away from future generations (by requiring present-generation technology developers to contain failure modes within a generation) and toward present-generation energy system designers.
% ABSENT_VOICES: Nuclear technologists and their supply chains are structurally excluded: they would argue that engineered geological isolation meets reversibility standards and that precautionary rejection ignores demonstrated safety. Velocity-priority advocates (energy analysts, climate-urgency advocates) are excluded: they would argue that deployment speed outweighs reversibility concerns when carbon budgets are exhausted. Baseload-dependent utilities are excluded: they would argue that stable dispatchability should dominate reversibility. All three groups are outside the precautionary framework by the criterion itself, not by oversight — precaution does not admit their voices as equals because admitting them would require accepting alternative readings of legitimacy.
% DISAPPEARANCE_RATIONALE: If the precautionary legitimacy standard vanished, energy deployment policy would immediately reorganize around competing criteria: reliability-primacy would readmit nuclear and penalize variable renewables, velocity-primacy would favor whatever decarbonizes fastest (coal-with-CCS if deployable, nuclear if scalable), and intergenerational precaution would cease to constrain technology choice. Trillions in committed investment (renewable subsidies, nuclear moratoria, gas-transition plans) currently flow through precautionary gating; removing it would reverse technology deployment trajectories within years. The energy system would rearrange to prioritize whichever of reliability/speed/reversibility dominates the new legitimacy criterion.
% FOUNDING_PROBLEM: Climate mitigation requires urgent energy system transformation, but transformation choices made now lock in decades of consequences that future generations cannot change—nuclear waste repositories, accident-risk zones, technological dependencies, stranded assets. The founding problem: pursue rapid decarbonization without saddling future generations with irreversible technological commitments or catastrophic legacy costs they did not consent to and cannot exit.
% FOUNDING_PROBLEM_CORROBORATION: The precautionary reading is endorsed by the IPCC (AR6 synthesis on intergenerational justice and technological risk), climate ethics scholars (Broome on intergenerational fairness, Gardiner and Shue on climate justice), and environmental justice communities (Indigenous water protectors, communities facing permanent waste-site impacts). External to the beneficiary set: reliability-primacy advocates (nuclear industry, grid-stability engineers) and velocity-priority advocates (climate scientists favoring rapid decarbonization, energy analysts) all acknowledge the founding problem exists; they dispute which criterion should dominate when they conflict. The founding problem is live and widely acknowledged; the contest is over solutions.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderately high because the precautionary criterion concentrates deployment permission and investment capital toward renewables while excluding proven low-carbon alternatives (nuclear) that would compete if reliability-primacy or velocity-primacy dominated. The measurement series shows extractiveness rising from 0.54 to 0.68 over the first 20 years (observed) as the constraint's enforcement machinery strengthens (IEA net-zero scenarios, EU taxonomy, national net-zero legislation) and then stabilizing (projected) at 0.68 as the policy environment matures and nuclear advocates adjust to the new equilibrium. Suppression is moderate (0.52) and rising (0.38 to 0.52 over 20 years) because the constraint's persistence depends on actively enforcing the precautionary criterion against competing readings — regulatory bodies must continuously defend why reversibility matters more than reliability or speed, and this requires blocking alternatives even when they might solve urgent grid problems. Theater is modest (0.28) and climbing slowly because while reversibility is a real criterion, much of the enforcement activity is devoted to managing political pressure from excluded technology advocates, not to refining the reversibility assessment itself. All metrics are authored on one shared time grid: every metric has a value at every time point (0, 5, 10, 15, 20, 25, 30, 35, 40), preventing the misaligned-grid drift-dating problem. The observable/projected basis distinction tracks when historical measurement ends and policy projection begins (around year 25).
 *
 * PERSPECTIVAL GAP:
 *   The precautionary reading and the reliability-primacy reading would produce opposite victim sets: precaution makes nuclear a victim (excluded), while reliability-primacy would make variable renewables a victim (penalized for grid instability). From the enforcer's seat, the constraint appears as a necessary protection against intergenerational harm; from the nuclear advocate's seat, it appears as arbitrary exclusion of the best available low-carbon option. The divergence is maximal and structurally irreducible without resolving the kernel contest itself — it is not a matter of incomplete information (both sides understand the technical facts), but of different normative premises about what 'legitimate' means.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality ranges from 0.1 (renewable operators as full beneficiaries, low extraction from their seat) to 0.9 (nuclear advocates as trapped targets). Renewable operators benefit directly: their technology's reversibility becomes a competitive advantage in a precautionary regime. Nuclear advocates are the primary targets: they face identity-locked constraints (nuclear physics cannot be reversible at the waste timescale, so exit means leaving their discipline), constrained market access (precautionary gating closes financing and permitting), and organized political suppression (their technical arguments are overridden by the reading's normative premise). Baseload providers (gas, coal utilities) have slightly higher exit options (they can transition to hybrid-renewable + storage), so their directionality is 0.75 rather than 0.9. Energy-policy enforcers sit near symmetric (d = 0.45–0.55): they benefit from having a clear legitimacy criterion (administrative coherence, international alignment), but they also bear costs (defending the criterion against pressure, managing stranded assets in excluded sectors, absorbing the risk that precautionary gating slows decarbonization below climate targets). The engine derives directionality automatically from beneficiary/victim declarations and exit_options; no overrides are needed because the structural data is already precise.
 *
 * MANDATROPHY ANALYSIS:
 *   The precautionary reading does not show classic mandatrophy (a constraint that solved a real problem but persists after the problem disappears). Rather, it shows what might be called 'criterion legitimacy drift': the founding problem (intergenerational stewardship in decarbonization choices) is still live, but the criterion used to solve it (bounded reversibility within a generation) is increasingly contested as decarbonization accelerates. Around year 25–30 (projected), if rapid renewable deployment and battery-storage scaling make the electricity system adequately stable without baseload, the political rationale for suppressing nuclear under precautionary grounds will shift — nuclear may re-enter legitimacy debates not because the founding problem disappears, but because the reversibility criterion is reframed (permanent geological storage becomes 'reversible enough' under a revised timescale). The theater_ratio rise (0.12 to 0.28 over 20 years) suggests the constraint is developing theatrical components: increasing amounts of enforcement effort go into defending precautionary purity against technology-driven reality (battery costs fell faster than anyone predicted, solar scaling beat timelines, wind integration improved) rather than into genuine reversibility assessment. This is a sign the constraint might be approaching a precarious state where the criterion survives more on institutional inertia and beneficiary political power than on its original justification. An omega variable captures this uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_criterion_definability,
    'What constitutes ''reversible within a generation'' for complex energy infrastructure? Is geological nuclear waste storage that meets engineered containment standards for 10,000 years irreversible, or is it reversible if future generations can choose to relocate waste if isolation fails?',
    'Geological and engineering science: if waste-isolation engineering demonstrates engineered retrievability (waste can be exhumed without catastrophic hazard), then reversibility may be redefined to include engineered stewardship rather than natural decay. If retrieval remains permanently hazardous, the criterion holds.',
    'A redefinition would open nuclear to precautionary legitimacy under a refined reading, collapsing the victim set and lowering extractiveness. The boundary between reversible-stewardship and irreversible-legacy is the crux of the entire reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_criterion_definability, conceptual, 'Whether engineered reversibility satisfies the normative intent of precautionary reversibility.').

omega_variable(
    criterion_legitimacy_drift_risk,
    'Will the precautionary reversibility criterion remain the enforceable standard as renewable deployment success (batteries, grid stability, land-use solutions) makes the founding problem of intergenerational lock-in less salient?',
    'Policy analysis: if precautionary gating becomes politically ornamental (honored in rhetoric but overridden in practice when speed or reliability pressure rises), theater_ratio will spike above 0.5 and the constraint will drift toward piton classification. Monitor legislative backsliding and enforcement erosion.',
    'If the criterion erodes from policy enforcement even as the reading survives in rhetoric, the constraint shifts from tangled_rope (active enforcement) to piton (theater-maintained inertia). The extraction persists but the coordination function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_legitimacy_drift_risk, empirical, 'Whether the precautionary criterion maintains genuine enforcement or becomes institutionally ornamental.').

omega_variable(
    competing_reading_foreclosure,
    'Do the precautionary and reliability-primacy readings genuinely foreclose each other, or do they coexist as competing values held by different institutional actors?',
    'Institutional analysis: if a single energy system can admit both precautionary constraints on legacy-prone technologies AND reliability constraints on variable resources (via hybrid-renewable plus storage plus limited nuclear), then readings coexist (neither forecloses the other). If the readings require exclusive technology bets, they foreclose.',
    'Determines the reading_relations value: forecloses (rare) vs. coexists_with (more common in pluralistic energy systems). The impact on this constraint: if coexistence is possible, suppression (0.52) can decline as conflict diminishes; if foreclosure is structural, suppression must remain high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether precautionary and reliability-primacy readings can coexist in one energy system or must exclude each other.').

omega_variable(
    future_generation_representation_legitimacy,
    'Are present-generation precautionary advocates (climate scientists, ethicists, policy advocates) structurally authorized to bind future generations to irreversibility criteria, or does the constraint overreach by imposing intergenerational paternalism?',
    'Normative political philosophy (Broome, Gardiner, intergenerational justice): does the precautionary criterion respect future-generation agency, or does it impose present values on choices future generations should make autonomously? If future generations would reject the precautionary criterion, is it imposing one form of lock-in to prevent another?',
    'If the constraint is seen as paternalistic overreach, its legitimacy erodes regardless of its effect on technology deployment. The constraint might persist (due to institutional inertia) but lose normative authority, shifting toward piton or theater-heavy tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_representation_legitimacy, preference, 'Whether precautionary intergenerational constraints respect or violate future-generation autonomy.').

omega_variable(
    extraction_beneficiary_alignment,
    'Do renewable energy operators genuinely benefit from precautionary legitimacy because the criterion is good policy, or do they benefit because they captured the policy process to disadvantage competitors (regulatory capture with precautionary framing)?',
    'Historical analysis: if the precautionary criterion emerged from climate ethics and intergenerational-justice scholarship (external to beneficiaries), and renewable operators later aligned themselves with it, the extraction may be legitimate. If the criterion emerged from renewable-industry advocacy, the extraction is likely captured.',
    'If captured, the constraint becomes closer to snare (using a legitimate-sounding criterion to exclude competitors) rather than tangled_rope (genuine coordination with asymmetric extraction). False-summit-mountain dynamics may apply if precautionary legitimacy is manufactured cover for monopoly-seeking behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_alignment, empirical, 'Whether the precautionary criterion is exogenously derived or endogenously captured by renewable beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(tech_tr_t20, observed).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(tech_tr_t25, projected).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(tech_tr_t30, projected).
narrative_ontology:measurement(tech_tr_t35, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 35, 0.29).
narrative_ontology:measurement_basis(tech_tr_t35, projected).
narrative_ontology:measurement(tech_tr_t40, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(tech_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(tech_be_t20, observed).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tech_be_t25, projected).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(tech_be_t30, projected).
narrative_ontology:measurement(tech_be_t35, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement_basis(tech_be_t35, projected).
narrative_ontology:measurement(tech_be_t40, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(tech_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(tech_su_t20, observed).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(tech_su_t25, projected).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(tech_su_t30, projected).
narrative_ontology:measurement(tech_su_t35, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(tech_su_t35, projected).
narrative_ontology:measurement(tech_su_t40, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(tech_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel has three structurally distinct constraint stories, one per reading. Each reading is a separate constraint with its own ε, beneficiary/victim set, enforcement costs, and computed type. This constraint (precautionary_reading) benefits renewables and excludes nuclear; the reliability_primacy_reading benefits nuclear and penalizes variable renewables; the velocity_primacy_reading benefits whatever technology decarbonizes fastest. The readings form a constraint family linked by network.affects_constraints. The shared kernel is the legitimacy standard itself; the readings instantiate it differently. Decomposition follows the ε-invariance principle: changing which criterion (precaution vs. reliability vs. velocity) governs legitimacy yields different ε values and different beneficiary structures, so they are separate constraints, not different measurements of the same constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
