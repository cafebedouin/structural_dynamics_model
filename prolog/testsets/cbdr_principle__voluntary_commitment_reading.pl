% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__voluntary_commitment_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cbdr_principle__voluntary_commitment_reading
 *   human_readable: CBDR Voluntary Commitment Reading: Technology Transfer as Developed Nation Obligation
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The CBDR (Common But Differentiated Responsibility) principle is a
 *   contested kernel in international climate law. This constraint
 *   instantiates ONE reading: the voluntary commitment reading, which
 *   interprets CBDR as permitting developed nations to exit binding emissions
 *   reductions via national determination while framing technology transfer
 *   as a primary (but voluntary) obligation rather than mandatory
 *   compensation. Under this reading, developed nations retain sovereignty to
 *   set their own contribution levels; developing nations face mandatory
 *   climate damages with no guaranteed technology transfer or adaptation
 *   finance. The voluntary commitment reading has dominated institutional
 *   practice since Rio 1992, creating a structural tension: the framework
 *   claims to coordinate global climate action while permitting the largest
 *   historical emitters to opt out of binding reductions. The constraint
 *   exhibits tangled rope properties at the core (genuine coordination
 *   benefit for setting national targets alongside asymmetric extraction via
 *   lack of enforcement) with dramatic perspectival variation: developed
 *   nations experience rope (coordination without binding cost), vulnerable
 *   populations experience snare (mandatory damage, no exit), developing
 *   nation states experience tangled rope (mixed agency and constraint), and
 *   the UNFCCC itself exhibits piton characteristics (performative ritual
 *   divorced from functional verification). Theater ratio has risen over the
 *   30-year interval as COP meetings have become increasingly elaborate while
 *   actual emissions reductions have decoupled from national commitments.
 *   Extractiveness has accumulated as the consequences of voluntary
 *   frameworks have become clearer: developing nations have built
 *   infrastructure based on promised technology transfer that never
 *   materialized at scale, while developed nations have maintained industrial
 *   competitive advantage through selective application of emission
 *   standards.
 *
 * KEY AGENTS:
 *   - Developed Nations' Sovereigns (institutional/arbitrage): Primary beneficiary — retain sovereignty via national determination, avoid binding emissions reductions, maintain industrial advantage through technology control
 *   - Developed Nations' Industrial Incumbents (institutional/arbitrage): Secondary beneficiary — proprietary technology remains controlled, carbon-intensive exports to developing nations face no reciprocal emission reductions, green technology markets can be gate-kept via IP law
 *   - Climate-Vulnerable Populations in LDCs (powerless/trapped): Primary victim — mandatory climate damages (flooding, drought, food insecurity) with zero guaranteed compensation, zero technology access, zero exit option
 *   - Developing Nations as States (moderate/constrained): Secondary victim/mixed agent — set own national targets (agency) but face mandatory climate impacts without guaranteed support; technology transfer is rhetorical obligation without enforcement
 *   - Progressive Developed Nations (organized/constrained): Tertiary victim — constrained to fund adaptation and climate finance despite voluntary framework, isolate themselves if they move beyond developed-nation consensus toward binding commitments
 *   - UNFCCC as Institution (institutional/arbitrage): Institutional actor maintaining performative ritual — coordinates global climate governance symbolically while functional verification capacity has atrophied; benefits from continued institutional relevance despite inability to enforce compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.58).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.62).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR Voluntary Commitment Reading: Technology Transfer as Developed Nation Obligation").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, '98d6d6b4-0641-4833-81cb-ca054cdbcb3a').
narrative_ontology:cs_kernel_codification('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', formalized).
narrative_ontology:cs_authority_grounding('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', lineage).
narrative_ontology:cs_interpretation_layer_present('98d6d6b4-0641-4833-81cb-ca054cdbcb3a').
narrative_ontology:cs_reading_relation('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', foundational, developed_nation_sovereignty_primacy).
narrative_ontology:cs_axiom_status(developed_nation_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', developed_nation_sovereignty_primacy, conventional).
narrative_ontology:cs_axiom('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', foundational, technology_transfer_as_differentiation_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_as_differentiation_obligation, holdable).
narrative_ontology:cs_axiom_grounding('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', technology_transfer_as_differentiation_obligation, instrumental).
narrative_ontology:cs_reference_frame('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', sovereign_national_determination_framework).
narrative_ontology:cs_drift_state('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', contemporary_post_paris, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('98d6d6b4-0641-4833-81cb-ca054cdbcb3a', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nations_industrial_incumbents).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nations_sovereigns).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, developing_nations).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VULNERABLE (SNARE) — Trapped without exit. Developing nations and LDCs bear adaptation costs (sea-level rise, agricultural collapse, extreme weather) while voluntary commitment framework imposes no binding emission reductions on developed nations and no guaranteed technology transfer. Highest experienced extraction: mandatory climate damage, zero guaranteed compensation, zero negotiating power. The voluntary reading structurally ensures developed nations can exit binding obligation through national determination.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION STATE (TANGLED ROPE) — Constrained by structural dependence on developed-nation technology, finance, and markets. The voluntary reading creates genuine coordination benefit (nations can set own emission pathways) but asymmetric extraction (technology transfer is 'primary obligation' rhetorically but lacks enforcement mechanism; adaptation finance is subordinate to mitigation; conditionality structures favor incumbent industrial exports from developed nations). Mixed experience: some agency in setting national targets, but no guarantee of technology access or adaptation support needed to meet those targets.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEVELOPED NATION SOVEREIGN / INDUSTRIAL INCUMBENT (ROPE) — Beneficiary position. The voluntary commitment reading permits developed nations to exit binding emission reductions via 'national determination' while framing technology transfer as a voluntary gift rather than a compensatory obligation. Industrial incumbents benefit from maintaining control over proprietary technology and carbon-intensive export markets. This perspective experiences the framework as coordination (harmonized climate governance, international consensus) with negligible extraction cost — the framework coordinates global climate action while protecting developed-nation industrial advantage.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE JUSTICE COALITION (SCAFFOLD) — Organized agents (indigenous groups, climate-vulnerable country alliances, loss-and-damage advocacy networks) see the voluntary reading as a temporary institutional failure with a sunset condition embedded in climate reality: extreme weather events will eventually force renegotiation toward binding historical responsibility. The scaffold has a quasi-built-in enforcement mechanism — climate disasters generate political pressure for stronger obligations. Extractiveness remains moderate because organized coalition has exit pathway: walk away from CBDR entirely, demand separate loss-and-damage mechanisms, build parallel funding and technology-sharing networks outside UN framework.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC INSTITUTIONAL ACTOR (PITON) — The voluntary reading represents the UNFCCC's degraded state: the institution maintains a coordination ritual (annual COP meetings, nationally determined contributions framework) whose functional verification capacity has atrophied. CBDR principles are invoked symbolically while actual emissions reductions are decoupled from enforcement. The institution exhibits high theater ratio (ritualized negotiations, pledges without binding consequences, voluntary commitments performatively demonstrating state responsibility) and low functional verification (no mechanism to audit compliance, no sanctions for non-compliance, no technology transfer verification). UNFCCC persists through inertia and the absence of an alternative global climate governance structure, not because the voluntary framework actually coordinates effective action.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEVELOPED NATION CLIMATE COALITION (TANGLED ROPE) — Some developed nations (EU members, island-state allies) face internal pressure to move beyond voluntary frameworks toward binding commitments. These states experience the voluntary reading as constraining their own climate ambitions (cannot commit to binding targets without isolating themselves from developed-nation consensus) while also extracting from them (must fund adaptation in developing nations without global framework requiring reciprocal emissions reductions from laggard developed nations). Moderate extraction with genuine coordination benefit — the coalition benefits from international legitimacy and coordination mechanisms even though the mechanisms are weak.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the voluntary commitment reading reflects a natural law of international relations: sovereign nations cannot be compelled to reduce emissions via binding treaty without forfeiting sovereignty. This perspective sees the voluntary framework as an immutable feature of how interstate cooperation works — states must retain exit options or they will not sign. However, structural analysis contradicts this: the framework benefits specific developed-nation actors and harms specific vulnerable populations, indicating the 'natural law of sovereignty' is actually a contingent institutional arrangement protecting developed-nation privilege. The analytical observer risks naturalizing what is a false summit — a constructed constraint disguised as inevitable.
constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__voluntary_commitment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cbdr_principle__voluntary_commitment_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, TR),
    TR >= 0.70.

:- end_tests(cbdr_principle__voluntary_commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The voluntary reading creates asymmetric extraction: developed nations capture benefit of climate-stable atmosphere without bearing binding reduction costs, while developing nations bear mandatory climate damages without guaranteed technology transfer or adaptation finance. However, extractiveness is not 0.72+ because genuine coordination benefit exists (national determination allows states to set own pathways rather than top-down quotas), and some technology transfer does occur (at rates far below commitment rhetoric, but not zero). The 0.58 value reflects that the extraction mechanism is mixed with coordination function. Suppression (0.62): Moderate-high. Developing nations face high barriers to exit: geographic climate vulnerability is non-negotiable, dependence on developed-nation technology and finance is structural, and alternative governance pathways (subnational networks, private carbon markets) are insufficient substitutes. Developed nations face lower suppression (can exit via national determination), but face political suppression from climate movements and climate-vulnerable nation coalitions. Theater ratio (0.65): Moderate-high. Annual COP meetings are highly performative — nations pledge commitments, make symbolic gestures, negotiate granular text while emissions trajectories remain decoupled from pledged reductions. The voluntary framework enables theater because national determination lacks public verification or enforcement. However, theater is not maximal (0.85+) because some coordination genuinely occurs (baseline of emissions data, peer pressure on targets, some technology transfer), and the ritual does create political costs for flagrant non-compliance. Rising theater ratio over the interval reflects increasing disconnect between COP rhetoric and actual emissions trajectories.
 *
 * PERSPECTIVAL GAP:
 *   The full perspectival gap demonstrates how CBDR volunteer reading produces opposite classifications for symmetrically positioned agents based on power and exit. Developed nations (institutional/arbitrage) classify the constraint as rope — genuine coordination without binding cost. Climate-vulnerable populations (powerless/trapped) classify it as snare — mandatory extraction with no exit. Developing nation states (moderate/constrained) classify as tangled_rope — mixed coordination and extraction. The UNFCCC (institutional/arbitrage) classifies as piton — performative coordination divorced from functional verification. The analytical observer at civilizational scale risks mountain classification — treating the voluntary framework as a law of international relations rather than a chosen institutional arrangement. The perspectival gaps reveal three structural insights: (1) the voluntary reading fundamentally benefits developed-nation actors and harms vulnerable populations, (2) the framework's coordination function is genuine but asymmetrically distributed (development-nation states benefit from setting their own targets; vulnerable populations have no targets to set), and (3) the ritualisation of the UNFCCC process (piton) enables the extraction to persist by creating appearance of progress while functional verification capacity atrophies. If the historical responsibility reading were instantiated instead, these gaps would invert: developed nations would experience snare (binding reductions), vulnerable populations might experience rope (guaranteed compensation), and the false summit would reverse to flag the historical reading's risk of naturalizing compensation obligations as inherent to sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from beneficiary/victim status and exit options. Developed nations as beneficiaries with arbitrage options (can exit via national determination, can enter alternative arrangements) derive low d (~0.15) → low f(d) → low experienced extraction chi. Climate-vulnerable populations as victims with trapped exit (mandatory climate damage, no negotiating power, no escape) derive high d (~0.95) → high f(d) (1.42) → high experienced extraction chi. Developing nation states as mixed agents with constrained exit (depend on developed-nation technology but can theoretically build alternatives, climate damage mandatory but can adapt) derive moderate-high d (~0.65) → moderate-high f(d) (1.00). This directionality structure explains why the voluntary reading is experienced as rope by beneficiaries (low chi), tangled_rope by moderately-positioned states (mixed chi), and snare by trapped populations (high chi). The perspectival gap is entirely explained by directionality: the same structural ε (0.58) combined with different (P,T,E,S) tuples and different d values produces the full range of experienced classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled_rope classification is accurate at the core while multiple perspectives experience either pure coordination (rope) or pure extraction (snare) depending on structural position. The voluntary reading's tangled rope nature is not a hedging between two extremes — it is a genuine mixed structure: CBDR as an institution DOES coordinate global climate goal-setting (genuine coordination function) while DOES create asymmetric extraction (developed nations escape binding reductions, developing nations bear mandatory damage). The false summit manifests in the analytical observer's natural law perspective: the framing that 'sovereign nations cannot accept binding obligations without exit rights' naturalizes what is actually a choice to privilege developed-nation sovereignty over vulnerable-population welfare. If the kernel were interpreted via historical responsibility reading instead, developed nations would NOT have exit rights (compensation obligations would be binding), and the coordination function would be rebalanced — beneficiaries and victims would reverse roles. The mandatrophy is not resolved by choosing one perspective as 'correct' — it is resolved by recognizing that the kernel reading itself (voluntary vs. historical responsibility) determines who is beneficiary and who is victim, and therefore determines whether the constraint exhibits rope, tangled rope, or snare from any given structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_transfer_enforceability,
    'Does ''technology transfer as primary obligation'' constitute a binding commitment or a voluntary aspiration?',
    'Comparative analysis of CBDR treaty language across negotiation rounds; empirical tracking of actual technology transfer versus committed amounts; dispute resolution cases in UNFCCC adjudication or WTO mechanisms',
    'If binding: voluntary reading reclassifies toward rope or scaffold (developed nations experience enforcement). If voluntary: tangled_rope confirmed — asymmetric extraction persists. If partially enforced through IP carve-outs: snare classification for LDCs deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_enforceability, empirical, 'Whether technology transfer obligations are actually enforceable').

omega_variable(
    climate_damage_attribution_causality,
    'Can specific climate damages in developing nations be causally attributed to specific emissions from developed nations in ways that would establish compensatory obligation under alternative readings?',
    'Climate attribution science (extreme event attribution, sea-level rise causation); legal discovery in loss-and-damage litigation; IPCC assessment reports on causal chains between historical emissions and contemporary impacts',
    'If strong attribution established: historical responsibility reading becomes empirically grounded, and voluntary reading loses legitimacy basis (the ''natural law'' of sovereignty becomes transparent as choice to privilege developed-nation exit). If weak attribution: voluntary reading''s position strengthened (causation ambiguous, so no automatic compensation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_damage_attribution_causality, empirical, 'Causal attribution of climate damages to developed-nation emissions').

omega_variable(
    alternative_governance_structures,
    'Are functioning non-voluntary climate governance alternatives (subnational networks, private carbon markets, regional binding treaties) creating parallel exit pathways that undermine or supplement CBDR voluntary framework?',
    'Empirical tracking of emissions reductions via alternative mechanisms; comparative analysis of developed nations'' actual emissions trajectories under voluntary CBDR versus binding subnational/private mechanisms; network analysis of climate governance fragmentation',
    'If alternatives achieving equivalent emissions reductions: voluntary reading becomes a shell framework (piton deepens). If alternatives failing: voluntary reading remains primary mechanism but extraction increases (developing nations have no substitutes). If alternatives competing effectively: scaffold classification confirmed (sunset pressure intensifies as alternative institutions mature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_structures, empirical, 'Effectiveness and proliferation of alternative non-voluntary climate governance structures').

omega_variable(
    committer_kernel_reading_contest,
    'Is the voluntary commitment reading a stable interpretation of CBDR''s foundational commitment to ''common but differentiated responsibility,'' or does the historical responsibility reading represent the true kernel claim?',
    'Analysis of UNFCCC treaty text genesis and negotiation records (Rio Summit 1992 preparatory documents); subsequent COP decisions and their interpretations of CBDR; statements from negotiating delegations (developed vs developing nations) about what CBDR was understood to commit to',
    'If voluntary reading is primary: developed-nation exit is legitimate, technology transfer is gift not compensation, and the snare classification for LDCs is structural feature not bug. If historical responsibility reading is primary: voluntary reading is illegitimate reinterpretation, developed nations are in victim set for binding obligations, and snare classification for developed nations follows. If neither is primary and kernel is genuinely contested: both readings coexist indefinitely, and the institutional conflict IS the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_contest, conceptual, 'Which reading (voluntary commitment vs. historical responsibility) represents the true kernel claim of CBDR').

omega_variable(
    developed_nation_sovereign_exit_legitimacy,
    'Can developed nations legitimately claim national determination permits zero binding emissions reduction, or does CBDR''s ''differentiated'' language impose a minimum obligation floor?',
    'Treaty interpretation under Vienna Convention on Law of Treaties (good faith interpretation, ordinary meaning, object and purpose); COP decisions establishing practice norms; climate litigation challenging national determination adequacy (e.g., German Constitutional Court cases, Netherlands v. State cases)',
    'If minimum floor exists: voluntary reading loses its core claim (developed nations cannot actually exit to zero), and rope classification emerges for developed nations (they have obligations after all). If no floor: voluntary reading confirmed, and snare classification for LDCs is unambiguous — no reciprocal obligation exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developed_nation_sovereign_exit_legitimacy, conceptual, 'Whether CBDR implies a minimum binding emissions reduction floor for developed nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_vol_theater_1992, cbdr_principle__voluntary_commitment_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cbdr_vol_theater_2007, cbdr_principle__voluntary_commitment_reading, theater_ratio, 15, 0.65).
narrative_ontology:measurement(cbdr_vol_theater_2022, cbdr_principle__voluntary_commitment_reading, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(cbdr_vol_extract_1992, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdr_vol_extract_2007, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(cbdr_vol_extract_2022, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_vol_suppress_1992, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cbdr_vol_suppress_2007, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(cbdr_vol_suppress_2022, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, loss_and_damage_finance_mechanism).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, green_technology_ip_gatekeeping).

% DUAL FORMULATION NOTE:
% The CBDR principle is a contested kernel with two structurally distinct readings instantiated as separate constraint stories. The voluntary commitment reading (this file) and the historical responsibility reading (sibling file) interpret the founding text (UNFCCC Article 3.1) differently, producing opposite beneficiary/victim assignments and opposite classifications from analytical observers. Both readings have been held institutionally stable since Rio 1992, though the voluntary reading dominates practical UNFCCC operations. The network edges show how the voluntary reading influences downstream constraints (loss-and-damage mechanisms must operate within voluntary framework; green technology remains under IP control because developed nations lack binding emissions obligations that would trigger IP carve-outs; historical responsibility debates recur annually at COP meetings, generating institutional pressure on the voluntary reading's legitimacy). Constraint families involving contested kernels require multiple files because ε values differ: the voluntary reading's tangled_rope structure (ε=0.58) reflects asymmetric extraction within coordination; the historical reading's snare structure (ε=0.75+, expected) would reflect pure extraction if developed nations lose sovereignty arguments. These are not the same constraint viewed from different angles — they are different constraints grounded in incompatible interpretations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
