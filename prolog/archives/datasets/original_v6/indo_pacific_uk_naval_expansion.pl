% ============================================================================
% CONSTRAINT STORY: indo_pacific_uk_naval_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_pacific_uk_naval_expansion, []).

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
 *   constraint_id: indo_pacific_uk_naval_expansion
 *   human_readable: Indo-Pacific UK Naval Expansion as Geopolitical Coordination and Extraction
 *   domain: geopolitical/military/regional_stability
 *
 * SUMMARY:
 *   The UK's post-Brexit strategic repositioning includes sustained naval
 *   expansion in the Indo-Pacific, framed as 'global Britain' and rules-based
 *   order enforcement. This constraint exhibits the characteristic Tangled
 *   Rope structure: genuine coordination benefits coexist with asymmetric
 *   extraction. The UK's presence provides freedom-of-navigation guarantees
 *   and deterrence against unilateral hegemonic claims, solving a real
 *   coordination problem for ASEAN traders and smaller regional states.
 *   Simultaneously, the expansion suppresses regional autonomy, forces
 *   compliance choices, raises military spending pressures, and shifts risk
 *   of conflict onto those least able to bear it. The theater ratio (0.58)
 *   reflects that much of the UK's geopolitical messaging is performative —
 *   maintaining a 'great power' identity with limited material resources —
 *   while the core military-technical function (deterrence and rules
 *   enforcement) is moderate but real. The extractiveness trajectory
 *   (0.38→0.52) shows accumulation as deployment frequency increases and
 *   regional actors internalize constraints on their freedom of action. The
 *   constraint is organized between institutional actors (UK, US, China,
 *   ASEAN states) with fundamentally different exit options and strategic
 *   interests, making it a textbook Tangled Rope.
 *
 * KEY AGENTS:
 *   - United Kingdom Strategic Interests: Institutional beneficiary (institutional/arbitrage) — regains perceived great-power relevance post-Brexit; extracts geopolitical influence at relatively low resource cost relative to messaging value
 *   - United States Strategic Partnership: Institutional beneficiary (institutional/arbitrage) — primary driver of containment strategy; captures majority coordination benefits through alliance leadership
 *   - ASEAN States and Regional Traders: Moderate victims (moderate/constrained) — benefit from freedom of navigation guarantees but constrained by rising military presence and forced alignment choices
 *   - Small Maritime Nations: Primary victims (powerless/trapped) — caught between expanding powers with no credible exit; experience maximum suppression through military asymmetry
 *   - China Strategic Interests: Organized victim (organized/constrained) — faces containment while organizing counter-coordination; suppressed expansion room but maintains strategic options
 *   - International Rules-Based Order Advocates: Organized beneficiaries (organized/constrained) — see enforcement mechanism with sunset logic; assume norms will mature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing great power competition as immutable structural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_pacific_uk_naval_expansion, 0.52).
domain_priors:suppression_score(indo_pacific_uk_naval_expansion, 0.65).
domain_priors:theater_ratio(indo_pacific_uk_naval_expansion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_pacific_uk_naval_expansion, extractiveness, 0.52).
narrative_ontology:constraint_metric(indo_pacific_uk_naval_expansion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indo_pacific_uk_naval_expansion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_pacific_uk_naval_expansion, tangled_rope).
narrative_ontology:human_readable(indo_pacific_uk_naval_expansion, "Indo-Pacific UK Naval Expansion as Geopolitical Coordination and Extraction").
narrative_ontology:topic_domain(indo_pacific_uk_naval_expansion, "geopolitical/military/regional_stability").

domain_priors:requires_active_enforcement(indo_pacific_uk_naval_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_pacific_uk_naval_expansion, united_kingdom_security_interests).
narrative_ontology:constraint_beneficiary(indo_pacific_uk_naval_expansion, us_aligned_coalition_partners).
narrative_ontology:constraint_beneficiary(indo_pacific_uk_naval_expansion, regional_maritime_commerce).
narrative_ontology:constraint_victim(indo_pacific_uk_naval_expansion, regional_stability_equilibrium).
narrative_ontology:constraint_victim(indo_pacific_uk_naval_expansion, chinese_regional_autonomy).
narrative_ontology:constraint_victim(indo_pacific_uk_naval_expansion, local_maritime_access_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL REGIONAL MARITIME NATIONS (SNARE) — Caught between expanding naval powers with no credible exit option. Forced to navigate between UK-US presence and Chinese regional claims. Limited agency; suppression through military asymmetry and geopolitical coercion. Maximum experienced extraction via constraint on sovereignty and freedom of action.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN STATES AND REGIONAL TRADERS (TANGLED ROPE) — Benefit from freedom of navigation guarantees and deterrence against unilateral hegemonic claims (UK-US security provision). Also constrained by rising military presence, increased risk of incident escalation, and pressure to align with one coalition or another. Genuine coordination function alongside asymmetric extraction of compliance.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNITED STATES STRATEGIC PARTNERSHIP (ROPE) — Primary beneficiary with arbitrage exit option. Views UK naval expansion as coordination mechanism for shared containment and rule-of-law enforcement. Low suppression of US interests; the constraint enables US regional objectives. Net benefit flows toward US-UK coalition from implementation.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE STRATEGIC INTERESTS (TANGLED ROPE) — Faces both coordination pressures (freedom of navigation norms, international law compliance) and extraction via military encirclement and containment strategy. Organized actor with strategic options but constrained by escalation risks and legitimacy costs. Asymmetric suppression of expansion room; also provides counter-coordination platform for alternative regional arrangements.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL RULES-BASED ORDER ADVOCATES (SCAFFOLD) — See UK naval expansion as temporary enforcement mechanism for open seas norms with sunset logic: as international institutions mature and Chinese compliance with law-of-the-sea conventions increases, the need for military presence to guarantee rules-based access declines. Theater of force projected to decline as norms strengthen. Sunset clause implicit in institutional norm maturation.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: POST-IMPERIAL BRITISH STRATEGIC IDENTITY (PITON) — UK naval expansion performs geopolitical relevance despite reduced material capacity. Theater of 'global Britain' maintains institutional commitments without proportional resource allocation. The constraint is largely performative — maintaining an appearance of strategic weight in a region 7,000 miles away with a handful of ships. Inertial maintenance of great-power identity through symbolic military presence. Theater ratio high; actual military-technical function moderate.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - GREAT POWER COMPETITION (MOUNTAIN) — From civilizational view, the constraint reflects immutable structural properties of great power dynamics: rising powers must either challenge or accommodate existing order; established powers must either adapt or contest; regional actors cannot escape the coordination problem imposed by hegemonic competition. This perspective risks naturalizing what is contingent strategic choice as inevitable structural law.
constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_pacific_uk_naval_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_pacific_uk_naval_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_pacific_uk_naval_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_pacific_uk_naval_expansion, TR),
    TR >= 0.70.

:- end_tests(indo_pacific_uk_naval_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts compliance and autonomy costs from regional actors disproportionately to benefits received. The UK-US presence enforces norms that benefit global maritime commerce but imposes local security costs and limits regional strategic autonomy. The extraction has grown as deployment frequency increased from 2021-2024 (trajectory 0.38→0.52). Suppression (0.65): High. Military asymmetry is the primary suppression mechanism: small nations cannot credibly resist or exit; ASEAN faces diplomatic pressure to align; China faces regional encirclement risk. Formal sovereignty remains intact but practical strategic freedom is constrained. Theater ratio (0.58): Moderate-high. UK messaging emphasizes 'global Britain' and geopolitical weight disproportionately to actual military resources deployed. The constraint is reinforced by performative signaling — speeches, high-level visits, media coverage — that creates impression of sustained commitment despite rotating deployments. Theater has increased (0.48→0.58) as political importance of the narrative has grown relative to constant material presence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are substantial and reveal the extraction mechanism. UK-US beneficiaries see coordination (Rope) — solving a real problem of rule enforcement. ASEAN sees tangled coordination (genuine benefits + asymmetric costs). Small nations see pure extraction (Snare) — no coordination benefits, only suppression. China sees mixed strategic constraint (Tangled Rope) — faces both norms enforced against it and opportunities to organize alternatives. The open-systems view (Scaffold) sees temporary enforcement with sunset as institutions mature. The British institutional identity (Piton) sees itself as performing great-power relevance through theater. The civilizational analytical view (Mountain) risks naturalizing the strategic choice as inevitable competition. The widest gap is between the powerless/trapped perspective (Snare, maximum chi) and the institutional/arbitrage perspective (Rope, negative chi) — same constraint, radically different experienced extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural relationships and exit capacity. The UK and US (institutional/arbitrage) have low d values (0.05-0.20) — they are beneficiaries with options; the constraint enables their preferred outcomes. ASEAN states (moderate/constrained) have moderate d values (0.55-0.65) — they face real costs but retain some agency through diplomatic positioning. Small nations (powerless/trapped) have maximum d values (0.90-0.95) — they bear full costs with no exit options. China (organized/constrained) has high d values (0.70-0.80) — organized enough to mount counter-strategies but constrained by encirclement logic and escalation risks. The analytical observer at civilizational scope uses canonical d≈0.73 for the mountain perspective, which produces the false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the only classification consistent with all structural properties. Pure Rope classification would require suppression ≤ 0.35 (actual 0.65) and no meaningful victims — violated. Pure Snare would require extractiveness ≥ 0.66 (actual 0.52) and no genuine coordination function — false, ASEAN traders do benefit from open sea guarantees. The Tangled Rope classification (0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, suppression ≥ 0.40, real beneficiaries, real victims, active enforcement required) maps precisely: beneficiaries (UK, US, ASEAN commerce), victims (regional autonomy, China, small nations), genuine coordination function (freedom of navigation enforcement), asymmetric extraction (differential costs borne by powerless), active enforcement (naval deployments), suppression ≥ 0.40 (military coercion). The false summit (mountain) at the analytical/civilizational level is flagged as naturalization risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rules_based_order_sincerity,
    'Is the UK-US framework genuinely committed to rules-based order enforcement, or does it selectively enforce when aligned with coalition interests?',
    'Historical analysis of enforcement consistency: how often do UK-US forces intervene in violations of rules-based order when perpetrator is coalition-aligned vs adversarial? Comparative analysis of selective rule invocation.',
    'If genuinely enforced: classification shifts toward Rope (pure coordination). If selectively enforced: classification confirms Snare for victims (extraction disguised as rules enforcement) and Tangled Rope for beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rules_based_order_sincerity, empirical, 'Whether rules-based order enforcement is applied consistently or selectively').

omega_variable(
    chinese_regional_challenge_capability,
    'Does China possess credible military capability to militarily contest UK-US presence, or is the constraint primarily psychological/diplomatic?',
    'Military capability assessment: force structure analysis, naval capacity projections, sustained operations capacity in South China Sea. Correlation with observed Chinese responses to UK deployments.',
    'If credible: suppression is justified by material threat and classification remains robust. If not credible: high suppression despite low threat indicates extraction mechanism (Snare intensifies for victims), and UK expansion becomes pure theater (Piton strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_regional_challenge_capability, empirical, 'Whether China possesses credible military challenge capability').

omega_variable(
    regional_stability_net_effect,
    'Does UK-US naval expansion increase or decrease overall regional stability and maritime security?',
    'Quantitative analysis of incident rates, escalation frequency, maritime commerce disruption, and regional tension indices before/after deployment. Survey data on perceived stability from ASEAN states.',
    'If increases stability: Rope and Scaffold perspectives vindicated; coordination function confirmed. If decreases: Snare perspective confirmed; extraction disguised as stabilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_stability_net_effect, empirical, 'Net effect of UK-US presence on regional stability').

omega_variable(
    uk_capability_sustainability,
    'Can the UK sustain the material, financial, and political costs of permanent Indo-Pacific naval presence, or is it inherently temporary theater?',
    'Budget allocation trends, naval construction timelines, strategic doctrine persistence across government changes, historical precedent for British presence in region (Suez withdrawal, East of Suez retreat).',
    'If sustainable: Scaffold sunset is real and negotiable. If unsustainable: Piton classification confirmed; presence is performative inertia that will collapse when political will declines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uk_capability_sustainability, empirical, 'Whether UK can sustain long-term Indo-Pacific presence').

omega_variable(
    aukus_alliance_coherence,
    'Does AUKUS (Australia-UK-US) represent genuine trilateral coalition or performative alliance with divergent strategic interests?',
    'Analysis of strategic doctrine alignment, resource commitment consistency, decision-making transparency, coordination effectiveness in joint operations. Comparison of stated objectives to actual capability deployment.',
    'If coherent: Rope classification strengthened; coordination function is real. If performative: Piton classification for UK role; theater without substance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aukus_alliance_coherence, empirical, 'Coherence and functionality of AUKUS alliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_pacific_uk_naval_expansion, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indopac_tr_t0, indo_pacific_uk_naval_expansion, theater_ratio, 0, 0.48).
narrative_ontology:measurement(indopac_tr_t3, indo_pacific_uk_naval_expansion, theater_ratio, 3, 0.54).
narrative_ontology:measurement(indopac_tr_t6, indo_pacific_uk_naval_expansion, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(indopac_be_t0, indo_pacific_uk_naval_expansion, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indopac_be_t3, indo_pacific_uk_naval_expansion, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(indopac_be_t6, indo_pacific_uk_naval_expansion, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_pacific_uk_naval_expansion, resource_allocation).
narrative_ontology:affects_constraint(indo_pacific_uk_naval_expansion, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(indo_pacific_uk_naval_expansion, chinese_regional_hegemony_containment).
narrative_ontology:affects_constraint(indo_pacific_uk_naval_expansion, asean_strategic_autonomy).
narrative_ontology:affects_constraint(indo_pacific_uk_naval_expansion, australia_security_alignment).

% DUAL FORMULATION NOTE:
% UK-US naval expansion is downstream of broader US Indo-Pacific Strategy (containment). Each affected constraint has its own extractiveness: South China Sea freedom-of-navigation has different ε than UK expansion (ε=0.38 initial for SCS FON, primarily coordination), but they reinforce each other strategically. The network links represent structural dependency and coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_pacific_uk_naval_expansion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
