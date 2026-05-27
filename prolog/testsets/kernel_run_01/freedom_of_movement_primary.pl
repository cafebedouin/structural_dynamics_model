% ============================================================================
% CONSTRAINT STORY: freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_of_movement_primary, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: freedom_of_movement_primary
 *   human_readable: Freedom of Movement as a Fundamental Right Presumptively Extending Across Borders
 *   domain: international_law/political_philosophy/migration_studies
 *
 * SUMMARY:
 *   This constraint instantiates one reading of a fundamental contested
 *   kernel in international law: what is the normative status of state
 *   borders? This reading asserts that freedom of movement is a presumptive
 *   human right that states may regulate but not categorically exclude absent
 *   compelling justification. The contrasting reading (sovereignty_primary)
 *   asserts that state control of borders is a foundational attribute of
 *   sovereignty itself. Both readings ground themselves in the same formal
 *   commitment structures (international law, human rights conventions, the
 *   UN Charter) but interpret the kernel's legitimacy source differently.
 *   This constraint models the first reading: freedom to move as the primary
 *   normative claim, with state regulation as the legitimate exception. The
 *   structural effect is immediate: excluded migrants become victims of
 *   illegitimate coercion; displaced domestic workers experience extraction
 *   but are not reframed as victims (because no corresponding right to
 *   exclude exists in this reading); Global labor mobility and destination
 *   labor markets are identified as beneficiaries. The constraint exhibits
 *   Tangled Rope classification at its analytical core: genuine coordination
 *   function exists (visa regimes do match labor supply and demand; border
 *   administration does serve legitimate public health and security
 *   interests) alongside embedded extraction (visa-tied labor, selective
 *   exclusion, suppression of unskilled migration). Extractiveness has
 *   accumulated over the 80-year interval (0.42 → 0.61) as states have
 *   developed sophisticated border enforcement infrastructure while claiming
 *   to honor the presumption. Theater has declined (0.55 → 0.38) as human
 *   rights institutions have made the legitimacy claim more explicit and
 *   institutionalized: the presumption is no longer purely performative but
 *   embedded in binding frameworks (ECHR, regional mobility agreements, IOM
 *   conventions). This trajectory marks the constraint moving from Piton
 *   (performative naturalization) toward Scaffold (institutionalized
 *   presumption with visible sunset as states internalize the norm).
 *
 * KEY AGENTS:
 *   - Excluded migrant populations: Primary victim (powerless/trapped) — structural beneficiaries of the presumption but systematically denied its benefits by state enforcement; experience maximum suppression
 *   - Displaced domestic workers: Secondary victim (powerless/trapped) — experience extraction when migration depresses local wages or displaces employment; lack alternatives and exit options
 *   - Global labor mobility ecosystem: Primary beneficiary (institutional/arbitrage) — consists of employers seeking talent, labor markets seeking to fill skill gaps, and migrant workers who succeed in crossing borders; benefits from presumption enabling cross-border labor matching
 *   - Destination labor markets: Beneficiary (institutional/arbitrage) — receiving countries benefit from migrant labor without requiring wage adjustments or domestic labor market restructuring
 *   - Nation-state enforcement authorities: Mixed (institutional/constrained) — experience both genuine coordination function (border administration legitimately serves security, public health) and extraction pressure (required to enforce selective exclusion and visa dependency); constrained by international human rights law
 *   - International human rights coalition: Organized enforcer (organized/constrained) — UN bodies, IOM, human rights advocacy networks institutionalizing the presumption into binding frameworks; constrained by state sovereignty resistance but seeing a visible exit path as regional mobility agreements expand
 *   - Global South states: Institutional victims (institutional/identity_locked) — presented with the presumption as international law norm but structurally locked into emigration dependency (remittance-dependent economies, IMF labor mobility requirements); cannot exit without bearing massive economic and international legitimacy costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_of_movement_primary, 0.58).
domain_priors:suppression_score(freedom_of_movement_primary, 0.72).
domain_priors:theater_ratio(freedom_of_movement_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_of_movement_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(freedom_of_movement_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(freedom_of_movement_primary, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_of_movement_primary, tangled_rope).
narrative_ontology:human_readable(freedom_of_movement_primary, "Freedom of Movement as a Fundamental Right Presumptively Extending Across Borders").
narrative_ontology:topic_domain(freedom_of_movement_primary, "international_law/political_philosophy/migration_studies").

domain_priors:requires_active_enforcement(freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(freedom_of_movement_primary, 'dbad4f59-8e3c-42b0-93e8-f91b58e408c2').
narrative_ontology:cs_created_at('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', '').
narrative_ontology:cs_kernel_codification('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', fixed_text).
narrative_ontology:cs_authority_grounding('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', lineage).
narrative_ontology:cs_interpretation_layer_present('dbad4f59-8e3c-42b0-93e8-f91b58e408c2').
narrative_ontology:cs_kernel_id(freedom_of_movement_primary, border_normative_status).
narrative_ontology:cs_reading_relation('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', managed_migration_hybrid, coexists_with).
narrative_ontology:cs_axiom('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', foundational, freedom_of_movement_presumptive).
narrative_ontology:cs_axiom_status(freedom_of_movement_presumptive, holdable).
narrative_ontology:cs_axiom('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', foundational, state_exclusion_requires_compelling_justification).
narrative_ontology:cs_axiom_status(state_exclusion_requires_compelling_justification, holdable).
narrative_ontology:cs_reference_frame('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', universal_human_right_to_move).
narrative_ontology:cs_drift_state('dbad4f59-8e3c-42b0-93e8-f91b58e408c2', contemporary_enforcement_sophistication, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, global_labor_mobility).
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, destination_labor_markets).
narrative_ontology:constraint_beneficiary(freedom_of_movement_primary, migrant_populations).
narrative_ontology:constraint_victim(freedom_of_movement_primary, excluded_migrant_populations).
narrative_ontology:constraint_victim(freedom_of_movement_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(freedom_of_movement_primary, state_sovereign_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Structurally trapped by state border enforcement. The presumption of freedom to move is negated by state police power and visa regimes. No exit option; bears full suppression cost. Maximum experienced extraction from structural barriers to movement.
constraint_indexing:constraint_classification(freedom_of_movement_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED DOMESTIC WORKER (SNARE) — Experiences the constraint as extraction when migrant labor depresses local wages or displaces employment. Trapped by geographic immobility and lack of capital for retraining. The freedom of movement presumption privileges migrant mobility over domestic worker protection. No escape; bears cost of labor market disruption.
constraint_indexing:constraint_classification(freedom_of_movement_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-SKILL MIGRANT (TANGLED ROPE) — Experiences mixed coordination and extraction. Border regimes coordinate labor market access through skill-based visa categories (coordination function exists: employers identify needed talent, workers find opportunities). But extraction is embedded: visa tie-to-employer creates dependency; temporary visa regimes extract labor below market cost; skilled mobility is selectively permitted while unskilled is systematically excluded. Mobile exit options exist but constrained by visa dependency.
constraint_indexing:constraint_classification(freedom_of_movement_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: DESTINATION LABOR MARKET (ROPE) — Experiences the constraint as pure coordination. Receiving countries benefit from migrant labor filling skill gaps and labor shortages; the presumption of freedom to move enables labor market matching without requiring domestic wage adjustments. Net beneficiary. Arbitrage options exist: countries can set selective criteria and still benefit from mobility presumption.
constraint_indexing:constraint_classification(freedom_of_movement_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized agents (UN human rights bodies, IOM, refugee advocacy networks) see freedom of movement as a temporary coordination problem with a visible sunset: regional mobility agreements (Schengen, ECOWAS, Mercosur) and international human rights protocols are gradually institutionalizing the presumption into binding frameworks. Theater is low — human rights norms operate through soft law and advocacy, not performative ritual. Exit path visible: institutionalization of the presumption reduces state discretion. Theater low, extractiveness moderate because the coalition has agency.
constraint_indexing:constraint_classification(freedom_of_movement_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATION-STATE ENFORCEMENT AUTHORITY (TANGLED ROPE) — Experiences genuine coordination function (border regimes do organize legitimate state security interests, public health screening, labor market matching) alongside extraction (visa regimes extract processing fees, create labor dependency, enforce citizenship hierarchy). Active enforcement required. The state must coordinate between competing demands: honoring the presumption of freedom to move while managing domestic labor market disruption, public services provision, and security. Constrained exit — international human rights law creates legal and reputational pressure; cannot exit the presumption without bearing legitimacy costs.
constraint_indexing:constraint_classification(freedom_of_movement_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY NATURALIZATION (MOUNTAIN) — From civilizational scope, sovereign state control of borders appears as an immutable feature of the international system: state territorial integrity and control of population movement are foundational to state existence itself. This perspective naturalizes state border enforcement as a law of political organization. However, the structural data (explicit beneficiaries and victims, active enforcement requirement, significant suppression) indicate this is a false summit: the 'immutability' of state borders is a legitimating frame that naturalizes what is actually a contingent institutional arrangement sustained by military enforcement and international recognition.
constraint_indexing:constraint_classification(freedom_of_movement_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_of_movement_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_of_movement_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_of_movement_primary, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits mixed coordination and extraction. Base coordination function: visa systems do match labor supply and demand; border administration serves legitimate security and public health interests; these are non-zero coordination benefits worth capturing. Embedded extraction: visa-tied labor arrangements extract worker surplus through employer dependency; selective exclusion of unskilled migrants creates artificial scarcity premium for skilled workers; suppression of mobility rights through enforcement infrastructure. The accumulated extractiveness trajectory (0.42 → 0.61) reflects deepening institutionalization of selective enforcement rather than rising extraction per se — states have built increasingly sophisticated mechanisms for claiming to honor the presumption while systematically excluding most migrants. Suppression (0.72): High. Significant barriers to movement include: military border enforcement; visa regimes with selective criteria; deportation machinery; lack of legal status in destination countries creating de facto bondage conditions; geographic immobility of excluded populations. Suppression varies by position — high for excluded migrants and displaced workers (0.85+), moderate for visa-dependent skilled migrants (0.55), low for institutional beneficiaries (0.15). Theater ratio (0.38): Moderate-low. The constraint increasingly moves from performative (theater-high in 1945 when the presumption was merely stated in the UN Charter without enforcement) toward functional (theater-low in 2025 as regional mobility agreements, human rights court decisions, and IOM protocols create binding institutional frameworks). The declining theater trajectory marks the constraint's maturation from Piton toward Scaffold — the presumption is becoming institutionalized with genuine exit mechanisms (regional mobility) visible on the horizon.
 *
 * PERSPECTIVAL GAP:
 *   The excluded migrant sees a Snare: systematic exclusion masked by a presumption that claims to protect them. The displaced domestic worker sees a Snare: their labor market interests are sacrificed on the altar of global mobility. The skilled migrant sees Tangled Rope: mobility presumption enables their career but ties them to employers through visa dependency. The destination labor market sees Rope: pure coordination of labor supply and demand without domestic cost. The state sees Tangled Rope: genuine security/health coordination alongside extraction pressure from international human rights law. The human rights coalition sees Scaffold: a temporary coordination problem being solved by institutionalizing the presumption. The analytical observer risks seeing Mountain: state sovereignty as an immutable law of international politics. The perspectival gap widest between powerless victims (Snare) and institutional beneficiaries (Rope) — a gap of three classification types reflecting the structural inversion of who experiences extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation from beneficiary/victim declarations and exit options: Excluded migrants (powerless + trapped) → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Displaced domestic workers (powerless + trapped) → d ≈ 0.92 → f(d) ≈ 1.35 (near-maximum extraction). High-skill migrants (powerful + mobile) → d ≈ 0.55 → f(d) ≈ 0.75 (moderate-to-moderate extraction despite being beneficiaries, because mobility options create exit leverage). Destination labor markets (institutional + arbitrage) → d ≈ 0.05 → f(d) ≈ -0.12 (negative extraction = net benefit). Nation-states (institutional + constrained) → d ≈ 0.48 → f(d) ≈ 0.60 (moderate extraction despite institutional power, because constrained by international human rights pressure). Human rights coalition (organized + constrained) → d ≈ 0.35 → f(d) ≈ 0.35 (low-moderate extraction; organized agent with visible exit path). Global South states (institutional + identity_locked) → d ≈ 0.70 → f(d) ≈ 1.15 (high extraction; despite institutional formal power, locked into remittance dependency and international institutional pressure makes exit unthinkable). The perspectival gap arises because beneficiary status (destination labor markets) produces negative d → negative χ, while victim status (excluded migrants) produces high d → high χ. The same constraint produces opposite directionality signs depending on observer position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through the kernel frame. The question 'is freedom of movement a human right or is state sovereignty the fundamental principle?' cannot be answered by choosing one type. Instead, the constraint simultaneously IS a human right reading (which makes it Snare/Tangled Rope from victim perspectives, Rope from beneficiary perspectives) AND an institutional reading (which makes it Tangled Rope from state enforcement perspective). The mandatrophy dissolves once we recognize this is one reading of a contested kernel, not a universally applicable classification. From within this reading's normative frame, the classification structure is stable and internally consistent. From within the competing sovereignty_primary reading, the entire classification inverts (what this reading calls victims become non-victims; what this reading calls beneficiaries become extractors). The engine's role is not to adjudicate between readings but to model each internally and enable cross-reading comparison.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_freedom_vs_sovereignty,
    'Is this constraint fundamentally a freedom claim (human right to move) or a sovereignty claim (state right to exclude)? Can both be ''fundamental'' without one foreclosing the other?',
    'Identification of which reading''s core premise each legal tradition actually grounds itself in; analysis of whether legal systems attempt to hold both simultaneously (coexist) or structure one as subordinate to the other (forecloses)',
    'If truly coexist: constraint is Tangled Rope from all institutional perspectives (both principles operative). If one forecloses: constraint is Snare/Mountain from the foreclosed perspective''s standpoint. The entire classification structure depends on whether this is a genuine hybrid or a masked hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_freedom_vs_sovereignty, conceptual, 'Whether freedom and sovereignty are coexisting principles or one forecloses the other').

omega_variable(
    displaced_worker_extraction_quantification,
    'How much of observed labor market disruption in receiving countries is actual displacement of domestic workers vs. complementary hiring (migrants fill gaps that native workers don''t)? Is extraction measurable or mostly distributional conflict?',
    'Meta-analysis of labor market studies (native/migrant wage correlation, employment level effects, sectoral displacement patterns); comparison of wage suppression magnitude across skill levels and industries',
    'If substantial displacement: suppression (0.72) is justified and snare perspective is empirically grounded. If minimal/complementary: suppression overestimated and some perspectives should reclassify to rope; extractiveness drops to ~0.35.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_worker_extraction_quantification, empirical, 'Magnitude of actual worker displacement caused by migration').

omega_variable(
    visa_dependency_extraction_vs_coordination,
    'Does the coordination function of skill-based visa matching justify the extraction embedded in visa-tie-to-employer arrangements? At what point does coordination overhead become pure extraction?',
    'Comparison of wage premiums in visa-tied vs independent labor positions; analysis of employer switching costs and worker bargaining power; quantification of visa processing costs borne by workers',
    'If coordination costs are proportional: visa regimes are legitimate tangled_rope (extraction is payment for matching service). If extraction exceeds coordination cost: reclassify to snare; beneficiaries and victims lists change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(visa_dependency_extraction_vs_coordination, empirical, 'Whether visa-tied labor arrangements extract beyond coordination costs').

omega_variable(
    southern_hemisphere_state_exit_capacity,
    'Do weaker states in the Global South have genuine choice in accepting or rejecting the ''freedom of movement'' presumption, or is it imposed through institutional power asymmetry (IMF labor mobility requirements, remittance dependency on emigration)?',
    'Historical analysis of how freedom of movement presumption entered international law (Northern states as formulators); examination of which states benefit vs bear costs; analysis of institutional pressure on emigration-dependent economies',
    'If imposed: constraint is snare from Global South state perspective (trapped by international institutional pressure); beneficiaries list should include ''Northern receiving states'' and ''international labor market arbitrage''; extractiveness rises to ~0.68 from Global South state view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(southern_hemisphere_state_exit_capacity, empirical, 'Whether Global South states have genuine choice in accepting freedom of movement presumption').

omega_variable(
    reading_kernel_contest,
    'This constraint is one reading of the contested kernel: border_normative_status. Which fundamental principle grounds legitimacy — the human right to move (this reading) or state sovereignty to exclude (sibling reading: sovereignty_primary)? Can an institutional framework hold both?',
    'Comparative jurisprudence across legal traditions (European rights courts vs national supremacy courts); identification of which principle takes priority when they conflict in actual cases; determination of whether any framework simultaneously treats both as supreme without hierarchical ordering',
    'If this reading''s premise (freedom to move is foundational) actually coexists with the sovereignty reading''s premise (state exclusion power is foundational): both constraints are tangled_rope from their respective institutional positions and the kernel permits stable coexistence. If one reading logically forecloses the other: constraint type changes to snare or mountain depending on which is dominant in actual state practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether this reading (freedom-primary) and sovereignty-primary reading can coexist as equal principles or if one forecloses the other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_of_movement_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1945, freedom_of_movement_primary, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_1970, freedom_of_movement_primary, theater_ratio, 25, 0.48).
narrative_ontology:measurement(theater_1995, freedom_of_movement_primary, theater_ratio, 50, 0.4).
narrative_ontology:measurement(theater_2020, freedom_of_movement_primary, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(extract_1945, freedom_of_movement_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(extract_1970, freedom_of_movement_primary, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(extract_1995, freedom_of_movement_primary, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(extract_2020, freedom_of_movement_primary, base_extractiveness, 75, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_of_movement_primary, resource_allocation).
narrative_ontology:affects_constraint(freedom_of_movement_primary, sovereignty_primary).
narrative_ontology:affects_constraint(freedom_of_movement_primary, managed_migration_hybrid).
narrative_ontology:affects_constraint(freedom_of_movement_primary, visa_tied_labor_extraction).
narrative_ontology:affects_constraint(freedom_of_movement_primary, remittance_dependency_trap).
narrative_ontology:affects_constraint(freedom_of_movement_primary, skilled_migration_arbitrage).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the border_normative_status kernel. The sibling reading sovereignty_primary is a structurally distinct constraint with different ε, different beneficiary/victim declarations, and different classification profiles — not a different measurement of the same constraint. Both readings coexist in international law and are held by different state and institutional actors simultaneously. The constraint family includes three stories: this one (freedom_of_movement_primary, ε=0.58), the sovereignty reading (sovereignty_primary, ε≈0.45-0.55, likely Piton from human rights perspective), and the hybrid reading (managed_migration_hybrid, ε≈0.50, likely Tangled Rope from all perspectives). Network edges link them as coexisting instantiations of the same kernel with different normative starting points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(freedom_of_movement_primary, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
