% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth Sufficiency Reading of Climate Mitigation Legitimacy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The degrowth sufficiency reading of climate mitigation legitimacy asserts
 *   that carbon reductions to climate-safe levels require absolute demand
 *   reduction, making large-scale generation expansion (nuclear,
 *   utility-scale renewables) unnecessary and illegitimate. This reading
 *   constrains both nuclear and renewable energy industries as
 *   growth-dependent; it privileges conservation and behavioral change; it
 *   faces resistance from development pathways, energy-intensive industries,
 *   and energy-access advocates. The constraint operates as a tangled rope:
 *   it coordinates a coherent mitigation narrative (demand reduction as the
 *   primary pathway) while extracting costs from technology industries and
 *   developing nations. The claim/metric gap is deliberate: the reading is
 *   framed as coordination toward true decarbonization; the metrics describe
 *   substantial extraction (0.68 at interval end) and active suppression
 *   (0.72) of alternative pathways.
 *
 * KEY AGENTS:
 *   - degrowth_advocates (organized; sets the normative frame; d≈0.0 beneficiary)
 *   - environmental_conservation_constituencies (organized; benefits from land-use constraint; d≈0.15)
 *   - nuclear_industry (institutional; capital deployment foreclosed; d≈1.0 target)
 *   - renewable_technology_manufacturers (institutional; scale-up delegitimized; d≈0.95 target)
 *   - energy_intensive_industries (powerful; demand reduction mandatory; d≈0.85 target)
 *   - global_development_countries (moderate; infrastructure expansion denied; d≈0.80 target)
 *   - working_class_populations (moderate; excluded; energy-access threatened)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.72).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth Sufficiency Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'f4c719c7-3f7b-42f0-8feb-4f8588525b71').
narrative_ontology:cs_kernel_codification('f4c719c7-3f7b-42f0-8feb-4f8588525b71', distributed).
narrative_ontology:cs_authority_grounding('f4c719c7-3f7b-42f0-8feb-4f8588525b71', distributed).
narrative_ontology:cs_reading_relation('f4c719c7-3f7b-42f0-8feb-4f8588525b71', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('f4c719c7-3f7b-42f0-8feb-4f8588525b71', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f4c719c7-3f7b-42f0-8feb-4f8588525b71', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('f4c719c7-3f7b-42f0-8feb-4f8588525b71', foundational, absolute_decoupling_impossible).
narrative_ontology:cs_axiom_status(absolute_decoupling_impossible, holdable).
narrative_ontology:cs_axiom_grounding('f4c719c7-3f7b-42f0-8feb-4f8588525b71', absolute_decoupling_impossible, empirically_contingent).
narrative_ontology:cs_axiom('f4c719c7-3f7b-42f0-8feb-4f8588525b71', foundational, energy_growth_perpetuates_extraction).
narrative_ontology:cs_axiom_status(energy_growth_perpetuates_extraction, holdable).
narrative_ontology:cs_axiom_grounding('f4c719c7-3f7b-42f0-8feb-4f8588525b71', energy_growth_perpetuates_extraction, instrumental).
narrative_ontology:cs_reference_frame('f4c719c7-3f7b-42f0-8feb-4f8588525b71', growth_dependent_energy_system).
narrative_ontology:cs_drift_state('f4c719c7-3f7b-42f0-8feb-4f8588525b71', climate_emergency_acknowledgment, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f4c719c7-3f7b-42f0-8feb-4f8588525b71', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, environmental_conservation_constituencies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_technology_manufacturers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_development_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue that carbon emissions can only be reduced to safe levels through absolute demand reduction (degrowth), making large new generation capacity unnecessary regardless of technology. They position demand-side transformation as the legitimate mitigation pathway and frame technology-expansion strategies as false solutions that perpetuate growth-dependent extraction. They set the normative frame for what counts as 'real' decarbonization and what gets dismissed as 'greenwashing.'
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Benefit from the constraint because it privileges land and ecosystem conservation over infrastructure expansion. Under a degrowth reading, large-scale solar/wind farms, transmission corridors, and nuclear plants represent unacceptable land-use claims; minimal new capacity means minimal new extraction pressure. They are beneficiaries of the constraint's de-prioritization of technology-scale-up.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, environmental_conservation_constituencies, beneficiary,
    organized, generational, mobile, global).

% Bears the constraint as loss of justification for new capacity deployment. Under degrowth framing, nuclear expansion is illegitimate regardless of its low-carbon credentials because it assumes energy growth. Existing plants may operate, but new projects face systematic delegitimization. Capital deployment for new builds is foreclosed by the normative frame that rejects supply-side solutions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Face the same constraint as nuclear: large-scale deployment is delegitimized under the degrowth reading even though renewables are carbon-free. Massive solar/wind rollouts require supply-side expansion, which the constraint frames as false solution. Manufacturing and deployment investment is redirected away from technology scale-up and toward demand-reduction retrofitting and behavioral change.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_technology_manufacturers, payer,
    institutional, generational, constrained, global).

% Steel, cement, chemicals, data centers, and industrial processes bear the constraint through mandatory demand reduction requirements. Under degrowth framing, their energy consumption is treated as discretionary and must shrink; electrification and efficiency gains alone are framed as inadequate. Exit options are constrained: relocating moves the problem rather than solving it, and abandoning the business means accepting comparative economic disadvantage.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, trapped, global).

% Face constraint through the delegitimization of energy infrastructure expansion that historically accompanied development. Degrowth framing denies legitimacy to the supply-side expansion developing nations have relied on for poverty reduction. Their option to 'grow first, decarbonize later' is foreclosed; they are required to pursue simultaneous development and demand-reduction, an asymmetric burden.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_development_countries, payer,
    moderate, biographical, constrained, global).

% Occupy an observer seat: degrowth framing eliminates their preferred alternative (maintain fossil dominance) while also undercutting energy technology alternatives that would replace them. They have no positive role but watch the constraint foreclose multiple displacement pathways.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_incumbents, observer,
    institutional, biographical, constrained, global).

% Navigate competing legitimacy claims about decarbonization pathways. Degrowth reading constrains their option set by delegitimizing technology-scale-up approaches; they must choose between adopting degrowth framing or defending technology-scale-up as legitimate, each choice carries political consequence.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policymakers, observer,
    institutional, generational, analytical, national).

% Are systematically excluded from the degrowth debate despite bearing its distributional consequences. Demand reduction policies directly reduce income-dependent energy access; they would argue for supply-side decarbonization that preserves living standards. Their exclusion is structural: degrowth advocacy attracts affluent constituencies and marginalizes working-class voices advocating for energy access.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, working_class_populations, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a coherent climate mitigation narrative: instead of assuming growth can persist with low-carbon energy substitution, degrowth frames mitigation as requiring absolute material throughput reduction, eliminating the demand for large-scale generation expansion (nuclear, utility-scale solar/wind) by reducing demand itself.
% TRANSFER_FUNCTION: Transfers legitimacy and capital allocation from supply-side technology deployment to demand-side reduction. Moves research funding, policy priority, and infrastructure investment away from nuclear plants and renewable capacity toward building efficiency, consumption reduction, and behavioral change. Moves the burden of emissions reduction from energy infrastructure onto energy consumers and energy-intensive industries.
% ABSENT_VOICES: Working-class populations dependent on affordable energy access; global development nations pursuing industrialization; engineers and energy workers whose livelihoods depend on infrastructure buildout; consumers in energy-poverty regions whose demand reduction would mean material deprivation. These constituencies would dispute the degrowth framing but are structurally excluded from the climate policy conversation that the constraint governs.
% DISAPPEARANCE_RATIONALE: If the degrowth framing disappeared, climate policy would revert to technology-portfolio approaches where nuclear and renewables compete on efficiency/cost grounds; development pathways would normalize energy growth; capital would flow back to supply-side solutions. If degrowth advocacy were completely removed, the political economy of climate mitigation would reorganize around energy abundance as legitimate. The parties dispute whether this reorganization represents reality discovery or ideological capture.
% FOUNDING_PROBLEM: Carbon emissions are decoupled from growth only in decarbonized energy systems; absent such systems, absolute demand reduction is the only proven pathway to carbon reductions matching climate targets. Degrowth advocates argue that efficiency gains and technology shifts have repeatedly failed to deliver carbon reductions at required scale because they assume growth can persist.
% FOUNDING_PROBLEM_CORROBORATION: Degrowth advocates cite historical decoupling failure rates and thermodynamic bounds on efficiency gains. Technology advocates and development economists cite counterexamples of emissions reductions achieved through efficiency and substitution without demand reduction (e.g., UK carbon reductions 1990–2020 with maintained GDP; Costa Rica renewable electricity with growth). The founding problem's status is the live dispute itself: is decoupling failure fundamental or historical-contingent?
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint transfers capital allocation away from proven supply-side decarbonization technologies toward demand-side transformation whose deployment path is uncertain. Suppression is higher (0.72) because maintaining the degrowth framing requires actively delegitimizing technological alternatives and foreclosing development pathways; alternative legitimacy claims must be continuously suppressed. Theater ratio climbs from 0.22 to 0.41 over the interval as the constraint shifts from a primarily normative position (early advocacy era) to increasingly performative (policy adoption, green rituals, carbon accounting that presumes degrowth without materializing demand reduction). The measurements are authored on one shared time grid (all metrics at all time points). The rising theater ratio is diagnostic: as degrowth enters policy frameworks without corresponding material demand reduction, the proportion of activity that is ceremonial increases.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (degrowth advocates) experiences this as legitimate coordination and mandatory transition; payers and excluded populations experience it as coercive imposition of a contested ideology. The constraint's persistence depends on continuously defending the degrowth frame against empirical challenges (decoupling achievements, renewable scaling curves, energy-access requirements). This defense work is partly functional (elaborating the frame) and partly performative (ritual reaffirmation without material shift). The theatrical component rises over time as degrowth enters mainstream policy adoption: carbon budgets, consumption tracking, and green narrative become tools of governance without necessarily producing the demanded demand reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit-option asymmetry. Degrowth advocates hold (organized, mobile, generational): they set policy, can shift focus if degrowth fails, and see their position legitimized. Conservation constituencies hold (organized, mobile, generational): they benefit from the constraint without running it. Energy industries hold (institutional, constrained, generational): they cannot exit (energy is essential infrastructure) and their capital options are foreclosed by the normative frame. Developing nations hold (moderate, constrained, biographical): they cannot exit development, face tightening climate constraints, and are denied the historical pathway. The directionality gradient is steep: from full beneficiaries (d=0.0–0.15) to full targets (d=0.8–1.0).
 *
 * MANDATROPHY ANALYSIS:
 *   The degrowth reading avoids the false-mountain trap because it explicitly declares victims and active enforcement; it declares itself tangled-rope (coordination function + asymmetric extraction). The founding problem is contested: degrowth advocates argue decoupling has failed; technology advocates argue it is early and scaling. The constraint's mandate is NOT degraded—it is live and contested. The theater ratio's rise is NOT inertial decay (piton signal) but ideological maturation: as degrowth enters policy, the work becomes increasingly about narrative maintenance and less about material intervention, which increases theater. This is NOT mandatrophy but a shift in the constraint's operational mode from advocacy to governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_status,
    'Has relative decoupling (emissions declining while GDP grows) achieved in some economies proven to be durable and scalable to global scale, or is it a temporary artifact of carbon leakage and accounting illusions?',
    'Long-term emissions tracking in decoupling economies (30+ years), lifecycle analysis of goods consumed (not just produced), and energy system simulation at global scale to test whether decoupling persists at 100% decarbonization target.',
    'If decoupling is robust and scalable, degrowth is not mandatory for climate safety, and the constraint''s premise is empirically undermined; alternative readings (portfolio_pragmatism, renewable_primacy) become more legitimate. If decoupling is ephemeral or an accounting artifact, degrowth framing is vindicated and the constraint gains structural support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_empirical_status, empirical, 'Whether decoupling of growth from carbon is achievable at required scale or is an illusion.').

omega_variable(
    demand_reduction_realism,
    'Can the level of demand reduction degrowth advocates require (50–80% material throughput reduction in wealthy economies) be achieved through policy and behavioral change without coercive imposition or catastrophic economic disruption?',
    'Pilot policies in wealthy regions (carbon rationing, circular economy mandates); behavioral economics of consumption under scarcity; historical cases of rapid demand reduction (wartime rationing, oil shocks, economic collapse) and their distributional consequences.',
    'If achievable at scale with acceptable distribution, degrowth becomes a defensible mitigation pathway and the constraint''s extraction of sacrificial burden is a necessary feature. If achievable only through coercion or disruption, the constraint''s foundation shifts from coordination to imposition and its type reclassifies toward pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demand_reduction_realism, empirical, 'Whether the demand reduction required by degrowth can be achieved without coercive mechanisms.').

omega_variable(
    technology_determinism_vs_social_choice,
    'Is the growth/energy/carbon nexus a physical inevitability or a socially contingent relationship? Do energy systems necessarily drive growth, or does growth create demand for energy?',
    'Economic history of growth and energy decoupling; energy systems anthropology (cultures with low energy throughput and material satisfaction); future scenario modeling with different social organizations.',
    'If the nexus is contingent, degrowth is one legitimate pathway among others, and the constraint''s privileging of demand-reduction over technology is ideological choice, not natural necessity. If the nexus is necessary, degrowth becomes THE legitimate pathway and alternative readings are false solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_determinism_vs_social_choice, conceptual, 'Whether growth-energy-carbon coupling is a physical law or a social construct that could be otherwise.').

omega_variable(
    distribution_and_justice_framing,
    'Does the degrowth reading incorporate or exclude the distributional consequences of demand reduction for energy-poor and developing populations? Is degrowth compatible with energy justice?',
    'Degrowth policy analysis for energy-access provisions; comparative case studies of demand reduction in wealthy vs. developing regions; social justice framings of energy transitions; empirical tracking of energy poverty under degrowth policies.',
    'If degrowth can be implemented with energy-justice provisions, the constraint''s victim set shrinks and its legitimacy increases. If degrowth necessitates energy-access sacrifice in developing regions, the constraint reveals hidden victims and reclassifies toward snare; excluded voices gain force as evidence of unjust distribution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distribution_and_justice_framing, preference, 'Whether degrowth can be just or necessarily externalizes costs onto the globally poor and energy-deprived.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, projected).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, projected).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(clim_be_t15, projected).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(clim_be_t20, projected).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(clim_su_t15, projected).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(clim_su_t20, projected).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(clim_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.22).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The climate_mitigation_legitimacy kernel contains four constraint stories, each instantiating a different reading of the legitimate decarbonization pathway. Degrowth_sufficiency_reading forecloses and coexists_with sibling readings depending on their core premises. This story is positioned as one reading among contested alternatives; its claim of necessity is precisely what siblings dispute. All four stories share the same kernel (climate mitigation legitimacy) but diverge in their ε values, victim sets, and authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
