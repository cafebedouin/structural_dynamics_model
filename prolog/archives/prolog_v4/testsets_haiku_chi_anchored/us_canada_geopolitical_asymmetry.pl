% ============================================================================
% CONSTRAINT STORY: us_canada_geopolitical_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_canada_geopolitical_asymmetry, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_canada_geopolitical_asymmetry
 *   human_readable: The Geopolitical Constraint of US Proximity on Canadian Sovereignty
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The geopolitical asymmetry between the United States and Canada
 *   represents a structural constraint on Canadian sovereignty that operates
 *   through institutional embedding rather than explicit coercion. Geographic
 *   proximity (the 49th parallel border), military asymmetry (US military
 *   spending ≈15x Canada's), and economic interdependence (70% of Canadian
 *   exports to US) create a framework in which Canadian foreign policy
 *   choices face asymmetric costs if misaligned with US strategic interests.
 *   This constraint exhibits the full range of classification types depending
 *   on the observer's structural position: Canadian policymakers experience
 *   it as a snare (constrained exit with significant extraction costs),
 *   domestic coalitions see mixed coordination and extraction (tangled rope),
 *   US institutions experience it as coordination (rope), international
 *   observers recognize it as a modern extraction mechanism (snare), and
 *   geographic determinists risk naturalizing it as immutable law (false
 *   summit). The constraint has intensified over the measurement interval:
 *   extractiveness rose from 0.35 (post-WWII NATO coordination emphasis) to
 *   0.52 (post-Cold War pivot toward trade/tech dominance), while theater
 *   ratio increased from 0.38 (explicit bilateral negotiation) to 0.55
 *   (performative partnership framing masking asymmetric institutional veto).
 *
 * KEY AGENTS:
 *   - Canadian Federal Government: Primary victim (moderate/constrained) — must align foreign policy with US preferences or accept economic and security penalties
 *   - Canadian Strategic Community (defense, intelligence, foreign affairs): Primary victim (organized/constrained) — institutional position requires US coordination; independent positioning attracts regulatory and career costs
 *   - Canadian Domestic Coalition (energy, tech, finance sectors): Secondary victim/partial beneficiary (moderate/mobile) — benefits from integrated supply chains and trade frameworks; constrained by regulatory alignment requirements and US policy shifts
 *   - United States Strategic Establishment (Pentagon, State Department, Congress): Primary beneficiary (institutional/arbitrage) — receives coordinated continental defense, resource access, and aligned geopolitical positioning with minimal institutional friction
 *   - North American Integration Institutions (NORAD, USMCA, Five Eyes): Institutional actor (organized/constrained) — embed coordination mechanisms while simultaneously enforcing alignment requirements; experience constraint as functional necessity rather than extraction
 *   - Washington Policy Consensus: Institutional actor (powerful/arbitrage) — maintains Canada as default regional subordinate through institutional inertia; experiences constraint as natural order requiring no active enforcement
 *   - International Coalition of US-Adjacent Powers (Mexico, Australia, Poland): Comparative observer (organized/constrained) — reveals that US extraction patterns are generalized to all neighbors, suggesting systematic rather than bilateral arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, 0.52).
domain_priors:suppression_score(us_canada_geopolitical_asymmetry, 0.68).
domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_canada_geopolitical_asymmetry, tangled_rope).
narrative_ontology:human_readable(us_canada_geopolitical_asymmetry, "The Geopolitical Constraint of US Proximity on Canadian Sovereignty").
narrative_ontology:topic_domain(us_canada_geopolitical_asymmetry, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_canada_geopolitical_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, united_states_strategic_interests).
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, north_american_defense_integration).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_independent_foreign_policy).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN STRATEGIC INDEPENDENCE (SNARE) — Canada's foreign policy autonomy is structurally constrained by geographic proximity, economic interdependence, and military asymmetry. While Canada retains nominal sovereignty, strategic decisions in defense, trade, and geopolitical alignment face asymmetric costs if misaligned with US preferences. Exit options are constrained: reorienting toward other allies (EU, Indo-Pacific) incurs economic and security penalties through reduced US coordination. d≈0.78, f(d)≈1.12, σ=1.1 → χ≈0.64.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: CANADIAN DOMESTIC COALITION (TANGLED ROPE) — Canadian firms, workers, and institutions benefit from integrated North American supply chains, defense industrial partnerships, and trade frameworks (USMCA), while also bearing extraction costs through policy alignment requirements and loss of independent regulatory space. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC ESTABLISHMENT (ROPE) — The US benefits from NATO/NORAD coordination, continental resource access, defense burden-sharing, and aligned geopolitical positioning. The constraint functions as a coordination mechanism (unified North American defense posture) that enables US strategic simplicity. d≈0.10, f(d)≈0.08, σ=1.1 → χ≈0.05. Low effective extraction because the US experiences the constraint primarily as coordination.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: NORAD/NATO/USMCA INSTITUTIONAL FRAMEWORK (TANGLED ROPE) — These institutions solve genuine coordination problems (continental air defense, coordinated supply chains, joint security standards) while embedding asymmetric extraction: Canadian participation requires alignment with US strategic preferences, limiting independent action on issues like China policy, Iran sanctions, or Middle East positioning. The institutions also provide Canada with amplified voice in continental security decisions. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.37.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WASHINGTON FOREIGN POLICY ESTABLISHMENT (PITON) — US grand strategy treats Canada as a natural regional subordinate, not through explicit dominance but through institutional inertia. The constraint persists because alternative geopolitical architectures (Canada pivoting to EU or Indo-Pacific partnerships) would require sustained US accommodation and institutional redesign. The US establishment experiences Canadian alignment as the default setting rather than as an active extraction mechanism. theater_ratio=0.55 reflects that much US-Canada coordination is presented as mutual partnership (performative) while the underlying asymmetry in veto power remains unacknowledged. χ≈-0.12 for US — net beneficiary.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL GEOPOLITICAL OBSERVER (SNARE) — From a civilizational perspective, the US-Canada asymmetry reveals itself as a modern extraction constraint masquerading as partnership. The constraint operates through structural position (geography, military asymmetry, economic interdependence) rather than explicit coercion, making it appear natural or inevitable. However, the metrics (suppression=0.68, extractiveness=0.52) show that Canada faces genuine alternatives being suppressed through institutional design rather than immutable law. d≈0.88, f(d)≈1.28, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOGRAPHIC DETERMINISM (MOUNTAIN) — From a purely structural geography perspective, proximity to a 10x larger military and economic power is an immutable fact. Geographic constraints might appear as natural law (d≈1.0, f(d)≈1.42). However, the base extractiveness (0.52) exceeds the mountain threshold (≤0.25), and suppression (0.68) exceeds mountain requirements (≤0.05). This is a false summit — geographic proximity is a constant, but the extraction mechanism is institutional (NORAD, USMCA, intelligence sharing). Alternative architectures (looser alliance, trilateral Mexico coordination, EU-aligned policies) are suppressed by policy design, not by geographic law.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_canada_geopolitical_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, TR),
    TR >= 0.70.

:- end_tests(us_canada_geopolitical_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Canada faces measurable extraction through asymmetric institutional veto: US can unilaterally reshape trade frameworks (tariff threats, USMCA renegotiation), defense postures (NORAD decisions), intelligence sharing (Five Eyes), and geopolitical alignment (China policy sanctions). The extraction is not maximal (≠0.70+) because Canada retains nominal sovereignty and can impose costs on the US (resource cutoffs, NATO treaty termination, Arctic positioning). The 0.52 reflects that extraction mechanisms are institutional rather than direct coercion, leaving margins of maneuver. Suppression (0.68): High. Institutional suppression of Canadian alternatives is substantial: (1) Economic: USMCA locks Canada into North American supply chains, making diversification costly; US market dominance (70% of exports) creates dependency. (2) Military: NORAD integration embeds Canadian defense into US command structure; alternative defense postures would require NATO recalibration and lost continental protection. (3) Intelligence: Five Eyes integration creates organizational lock-in; departure would isolate Canadian intelligence agencies. (4) Geopolitical: US implicit threat of reduced coordination if Canada pursues independent positions on China, Middle East, or climate policy. Theater ratio (0.55): Moderate. Much US-Canada coordination is framed as mutual partnership (bilateral meetings, joint statements, defense burden-sharing rhetoric) while underlying asymmetry in veto power remains unacknowledged. The theater has increased over the interval as the post-Cold War shift from explicit alliance negotiation to implicit alignment expectations made the relationship appear more consensual and less extractive (false consensus). However, theater is not high (≥0.70) because genuine coordination functions persist—NORAD really does provide mutual continental defense, USMCA really does enable trade, Five Eyes really does share intelligence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Canadian policymakers and US institutions is stark and reveals the constraint's true structure. Canadian officials (moderate/constrained) experience a snare: they retain nominal control over foreign policy but face substantial costs for independent action. The US establishment (institutional/arbitrage) experiences a rope: the same institutional framework enables US strategic simplification and resource access through natural partnership. The gap emerges because the institutions (NORAD, USMCA, Five Eyes) solve genuine coordination problems (continental defense, trade efficiency, intelligence sharing) while simultaneously embedding asymmetric veto: US preferences are treated as defaults; Canadian alternatives face institutional friction. The global observer sees through this theater: the constraint is an extraction mechanism dressed as partnership. The geographic determinist risks a false summit by naturalizing proximity as law; the actual mechanism is institutional design that could be reformed (looser alliance, pluralistic frameworks, independent regulatory space).
 *
 * DIRECTIONALITY LOGIC:
 *   Canadian government/strategic community: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction cost for independent action; constrained exit (cannot quickly reorient). Canadian domestic coalition: Partial beneficiary + partial victim + mobile → d≈0.55, f(d)≈0.75. Mixed experience: benefits from trade/supply chains but constrained by regulatory alignment and US policy shifts. US establishment: Beneficiary + arbitrage → d≈0.10, f(d)≈0.08. Low effective extraction; experiences coordination as default. NORAD/USMCA institutions: Coordination function + enforcement mechanism + organized power → d≈0.50, f(d)≈0.65. Both enable and constrain; institutional actors experience dual function. Global observer: Sees extraction mechanism → d≈0.88, f(d)≈1.28. High chi reflects that asymmetry is structural and observable from external position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits a genuine mandatrophy at the institutional level: NORAD, USMCA, and Five Eyes simultaneously solve coordination problems and enforce extraction. NORAD genuinely improves continental defense (coordination). Five Eyes genuinely improves intelligence (coordination). USMCA genuinely enables trade (coordination). Simultaneously, these same institutions function as mechanisms for asymmetric US veto: US can reshape USMCA unilaterally (extraction); US intelligence dominance within Five Eyes influences Canadian policy (extraction); NORAD command structure privileges US strategic preferences (extraction). The mandatrophy resolves not by denying one function, but by recognizing that the extraction is ENABLED BY and HIDDEN WITHIN the coordination mechanisms. The classification choices reflect this: Canadian perspective = snare (extraction dominates from victim position); institutional perspective = tangled rope (both functions are real); US perspective = rope (coordination is what they experience). The false summit (mountain) must be explicitly rejected: geographic proximity is immutable, but the institutional structures that translate proximity into extraction are not. Alternative architectures (looser alliance, trilateral frameworks, independent regulatory space) are suppressed by beneficiary preference, not by geographic law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canadian_coalition_tipping_point,
    'At what level of geopolitical divergence do Canadian coalitions (tech sector, energy, finance) begin to organize for strategic independence, making exit from US-aligned position feasible?',
    'Monitoring Canadian corporate lobbying positions on China policy, trade with non-US allies, regulatory autonomy; tracking political support for independent foreign policy; measuring corporate investment in non-US partnerships',
    'If tipping point < 5 years: Canada could rapidly shift to constrained exit (still victim but with coalition power). If > 15 years: Canada remains trapped in snare from domestic perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canadian_coalition_tipping_point, empirical, 'Coalition strength threshold for Canadian strategic independence').

omega_variable(
    us_hegemony_structural_necessity,
    'Does the US actually require Canadian alignment as structurally necessary for continental defense, or is the constraint primarily maintained through institutional convenience and legacy commitment?',
    'NORAD technical analysis of actual geographic/surveillance requirements; comparison of defense postures with scenarios of independent Canadian neutrality; USMCA trade simulation with independent Canadian tariff policies',
    'If structurally necessary: constraint is partially mountain-like (cannot be easily reformed). If institutional convenience: constraint is pure extraction maintained by beneficiary preference — reframing as snare or tangled rope from Canadian perspective becomes analytically correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_hegemony_structural_necessity, empirical, 'Whether US requires Canadian alignment or prefers it').

omega_variable(
    alternative_alliance_credibility,
    'Could Canada credibly align with EU-centered security architecture or Indo-Pacific partnerships, or would such alignment require sustained US accommodation that would trigger punitive responses?',
    'Analysis of historical cases where mid-power allies shifted alignments (Australia''s China pivot, Poland''s EU integration); assessment of trade/investment costs for Canadian diversification; tracking US signaling on acceptable Canadian partnerships',
    'If credible: Canada has true mobile exit option, reframing constraint from snare/tangled rope to rope or even mountain with lower chi. If not credible: US implicit threat of economic penalty keeps exit analytically unavailable, confirming snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_alliance_credibility, conceptual, 'Credibility of Canadian alternative alliance options').

omega_variable(
    mandatrophy_institutional_vs_geographic,
    'Is the constraint fundamentally a coordination problem (institutions enabling mutual benefit, suggesting rope or tangled rope) or an extraction problem (institutions enforcing asymmetric alignment, suggesting snare)?',
    'Counterfactual analysis: what would Canada choose if geographic proximity were held constant but institutional frameworks were reset? Measurement of coordination gains vs extraction losses from Canadian perspective over 50-year horizon.',
    'If coordination-primary: beneficiaries and victims framework should emphasize how institutions solve collective problems. If extraction-primary: beneficiaries and victims framework should emphasize how institutions suppress Canadian alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_institutional_vs_geographic, conceptual, 'Mandatrophy resolution: coordination vs extraction primacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_canada_geopolitical_asymmetry, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscna_tr_t0, us_canada_geopolitical_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(uscna_tr_t30, us_canada_geopolitical_asymmetry, theater_ratio, 30, 0.48).
narrative_ontology:measurement(uscna_tr_t60, us_canada_geopolitical_asymmetry, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(uscna_be_t0, us_canada_geopolitical_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uscna_be_t30, us_canada_geopolitical_asymmetry, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(uscna_be_t60, us_canada_geopolitical_asymmetry, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_canada_geopolitical_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, us_china_strategic_competition).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, canadian_energy_sovereignty).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, arctic_geopolitical_competition).
narrative_ontology:affects_constraint(us_canada_geopolitical_asymmetry, indo_pacific_regional_order).

% DUAL FORMULATION NOTE:
% This constraint is upstream to multiple downstream constraints (China strategy, Arctic positioning, Indo-Pacific alignment) by setting the institutional framework within which Canadian foreign policy operates. US-China competition constrains Canadian tech sector and geopolitical positioning; Canadian energy sovereignty is structurally shaped by USMCA frameworks; Arctic geopolitics are conditioned by NORAD integration; Indo-Pacific alignment reflects US strategic preferences transmitted through intelligence and alliance structures. All downstream constraints inherit the asymmetric institutional environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_canada_geopolitical_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
