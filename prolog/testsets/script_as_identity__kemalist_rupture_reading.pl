% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Latin Script Adoption as Kemalist Identity Rupture and State Modernization
 *   domain: political_authority/state_building/linguistic_identity
 *
 * SUMMARY:
 *   The Latin script adoption by the Turkish Republic under Atatürk's
 *   leadership (1928 official transition, though gradual implementation
 *   through 1930s-1940s) is a foundational modernization moment in
 *   20th-century state-building. The Kemalist rupture reading instantiates a
 *   specific interpretation: script change as deliberate, irreversible
 *   severing of Ottoman-Islamic institutional and cultural continuity,
 *   enabling the construction of a secular, modern, European-aligned Turkish
 *   national identity. This reading treats the script transition as a core
 *   instrument of state transformation — not merely a practical convenience
 *   but a constitutive act of nation-building that forecloses return to
 *   Ottoman governance structures. The structural analysis reveals this
 *   reading embodies a Tangled Rope constraint: genuine coordination
 *   functions (literacy simplification, administrative unification,
 *   international legitimacy alignment) are intertwined with asymmetric
 *   extraction (displacement of Ottoman-educated elites, suppression of
 *   Arabic-script communities, state monopoly over literacy apparatus). The
 *   constraint's theater rises over the interval (0.50 → 0.68) as the
 *   symbolic importance of the script change grows while actual institutional
 *   rupture proves incomplete. The suppression requirement rises (0.55 →
 *   0.72) as maintaining the script monopoly requires ongoing enforcement
 *   against alternative literacy practices and historical preservation
 *   efforts.
 *
 * KEY AGENTS:
 *   - Kemalist State Apparatus & Urban Secular Elites (institutional/arbitrage): Primary beneficiary — captures coordination gains and identity alignment. Script transition legitimizes their vision of modernization.
 *   - Ottoman-Educated Clergy, Qadis, and Scribal Class (powerless/trapped): Primary victim — decades of literacy investment become worthless; career pathways close; institutional knowledge inaccessible to successors. Bears full cost of transition.
 *   - Rural Communities & Traditional Knowledge Holders (moderate/constrained): Secondary victim — face generational discontinuity and dependence on state-provided new literacy. Some benefit from national integration but at cost of severing local knowledge transmission.
 *   - Islamic Institutional Continuity (institutional/constrained): Systemic victim — Quranic education in Arabic-script becomes marginalized; religious textual authority is diminished when sacred texts exist in script the broader population cannot read. Suppression is indirect but effective.
 *   - International Modernization Community (organized/constrained): Indirect beneficiary — Turkey's script adoption becomes model for post-colonial state legitimacy claims, strengthening European-aligned modernization ideology.
 *   - Analytical Observer (analytical/analytical): Positions the constraint as natural law of modernization, risking false summit naturalization of political choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.58).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.72).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Latin Script Adoption as Kemalist Identity Rupture and State Modernization").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political_authority/state_building/linguistic_identity").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'c2b19162-0d07-47ed-9d80-cd655f8e8320').
narrative_ontology:cs_kernel_codification('c2b19162-0d07-47ed-9d80-cd655f8e8320', fixed_text).
narrative_ontology:cs_authority_grounding('c2b19162-0d07-47ed-9d80-cd655f8e8320', extraction).
narrative_ontology:cs_interpretation_layer_present('c2b19162-0d07-47ed-9d80-cd655f8e8320').
narrative_ontology:cs_reading_relation('c2b19162-0d07-47ed-9d80-cd655f8e8320', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2b19162-0d07-47ed-9d80-cd655f8e8320', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('c2b19162-0d07-47ed-9d80-cd655f8e8320', foundational, script_encodes_civilizational_identity).
narrative_ontology:cs_axiom_status(script_encodes_civilizational_identity, holdable).
narrative_ontology:cs_axiom_grounding('c2b19162-0d07-47ed-9d80-cd655f8e8320', script_encodes_civilizational_identity, deontological).
narrative_ontology:cs_axiom('c2b19162-0d07-47ed-9d80-cd655f8e8320', foundational, rupture_with_ottoman_past_is_necessary_for_modernization).
narrative_ontology:cs_axiom_status(rupture_with_ottoman_past_is_necessary_for_modernization, holdable).
narrative_ontology:cs_axiom_grounding('c2b19162-0d07-47ed-9d80-cd655f8e8320', rupture_with_ottoman_past_is_necessary_for_modernization, instrumental).
narrative_ontology:cs_reference_frame('c2b19162-0d07-47ed-9d80-cd655f8e8320', ottoman_scriptural_legitimacy).
narrative_ontology:cs_drift_state('c2b19162-0d07-47ed-9d80-cd655f8e8320', kemalist_reform_moment, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('c2b19162-0d07-47ed-9d80-cd655f8e8320', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, urban_secular_elites).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_arabic_literate_class).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_communities).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, islamic_institutional_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN-EDUCATED CLERGY & SCRIBAL CLASS (SNARE) — Trapped by enforced script transition with no material alternatives. Decades of Arabic-script literacy become worthless overnight. Career mobility collapses; institutional knowledge becomes inaccessible to new generations. Zero exit options: the state monopolizes education and documentation. Maximum suppression — the constraint operates through state licensing of teachers, state control of textbooks, and legal prohibition of Ottoman-era documents in public administration. Pure extraction: the class bears all costs; no coordination benefit accrues to them.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL COMMUNITIES & TRADITIONAL KNOWLEDGE HOLDERS (TANGLED ROPE) — Constrained by dependence on urban-mediated literacy and state services. Village scribes, Quranic teachers, traditional craftspeople with guild knowledge encoded in Arabic script face career discontinuity and loss of status. Some benefit emerges from national integration (access to state services, schooling for children) but asymmetric extraction is clear: the cost of relearning is borne locally; the benefits of unified literacy accrue to the state apparatus. Moderate time horizon because generational transition allows slow adaptation, but within a biographical span the constraint is severe.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KEMALIST STATE APPARATUS & URBAN SECULAR ELITES (ROPE) — Net beneficiary. The script transition performs genuine coordination: unified Latin script enables national integration, simplifies administration, and reduces literacy barriers for the new generation. The elites experience the constraint primarily as coordination — the script change aligns with their modernist identity claims and reduces transaction costs for state bureaucracy. Arbitrage options abound: the elites can code-switch, access education in multiple scripts during transition, and leverage their institutional position. Effective extraction (chi) runs *toward* this group, not away.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL MODERNIZATION OBSERVERS & LEAGUE OF NATIONS (TANGLED ROPE) — Organized external actors observe the script transition as evidence of successful state modernization and European alignment. They benefit from the Turkey's repositioning as a modern nation-state (coordination function: legitimacy alignment). But they also extract: the script change becomes a model for colonial and post-colonial states to adopt Latin scripts, often imposed without local consensus. The organized international perspective sees both coordination (prestige, alignment with Western modernity norms) and extraction (imposition of a model that serves European cultural dominance). Constrained exit because Turkey's legitimacy depends partly on being recognized as modernized.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OTTOMAN INSTITUTIONAL CONTINUITY IN DEGRADED FORM (PITON) — From the civilizational perspective of institutional succession, the script change is performative theater masking actual continuity. Ottoman administrative structures, hierarchies, and power distributions persist largely unchanged; only the written surface has shifted. The theater ratio is high (0.68): the script transition is celebrated as radical rupture while underlying institutional arrangements remain intact. The piton classification reflects that the constraint's primary function has atrophied — it was meant to enact rupture, but institutional inertia has preserved much of what it claimed to sever. The constraint persists through theatrical emphasis on the symbol rather than through actual functional transformation.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical position, script change appears as an immutable consequence of modernization itself: all modernizing states adopt alphabetic scripts compatible with printing and standardized education. The constraint appears to be a natural law of state development, not a political choice. However, the structural data contradicts this mountain classification: identifiable beneficiaries exist (kemalist elites), clear victims exist (ottoman-educated class), and the constraint requires active enforcement (state monopoly on education, prohibition of Ottoman documents). The engine will flag this as a false summit — the 'natural law of modernization' frames mask what is actually a political extraction mechanism and identity rupture policy.
constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(script_as_identity__kemalist_rupture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, TR),
    TR >= 0.70.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine coordination function (literacy simplification, administrative unification) embedded in asymmetric extraction (class displacement, institutional suppression). The reading is not pure extraction — the script transition does solve real coordination problems — but it solves them by concentrating benefits on elites and costs on displaced Ottoman-educated populations. The baseline value (0.35) reflects that the constraint begins as a practical modernization measure with coordination features; it rises to 0.58 as the symbolic and enforcement dimensions intensify and institutional continuity is revealed as masking rather than rupture. Suppression (0.72): High. The constraint operates through state monopoly on education, prohibition of Ottoman-era documents in public administration, systematic replacement of Ottoman-educated personnel, and legal status of only Latin-script documents as valid. Suppression is not violent (no physical coercion) but is structural and total — no legitimate alternative exists for those whose livelihoods depend on literacy. Theater Ratio (0.68): Moderate-high. The script transition is celebrated as radical rupture, but underlying Ottoman administrative hierarchies, power distributions, and institutional logic persist substantially unchanged. The theater ratio rises over time as the constraint becomes increasingly symbolic: maintaining script monopoly becomes a performative marker of Kemalist identity rather than a functional necessity as bilingual literacy naturally emerges. Claimed Type (Tangled Rope): The constraint requires active enforcement (state apparatus must continuously maintain script monopoly), exhibits dual beneficiaries (state and elites) and dual victims (scribal class and rural communities), and combines genuine coordination (administrative unification, literacy access) with asymmetric extraction (class displacement, institutional suppression).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits the full range of Deferential Realism perspectives from a single structural event. The Kemalist beneficiaries see Rope (coordination enabling national unification). The scribal class sees Snare (pure extraction with no coordination benefit). Rural communities see Tangled Rope (mixed burden and benefit across generations). International observers see Tangled Rope (prestige coordination with ideological extraction). The Ottoman institutional order sees Piton (performative rupture masking continuity). The analytical observer risks Mountain (natural law of modernization). These gaps are not observer error — they reflect genuine structural differences in how agents experience the constraint. The script transition is simultaneously a coordination success (from the state's perspective), an extractive disaster (from the scribal class's perspective), and theatrical performance (from the institutional continuity perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from agent power, exit options, and beneficiary/victim status. The Ottoman-educated clergy occupy the highest-extraction position: they are victims (paying adaptation costs), trapped (no exit options in a state monopoly), and powerless (no institutional leverage). Their derived d ≈ 0.95 → f(d) ≈ 1.42, producing maximum experienced extractiveness. The Kemalist state apparatus occupies the lowest-extraction position: they are beneficiaries, have arbitrage options (can code-switch, access international legitimacy), and institutional power. Their derived d ≈ 0.05 → f(d) ≈ -0.12, producing negative effective extraction (they are subsidized by the constraint). Rural communities occupy middle ground: they are partially victimized (knowledge discontinuity) but also partially benefit (national integration, education access), constrained rather than trapped (generational transition possible), and moderate power. Their derived d ≈ 0.70 → f(d) ≈ 1.15, producing moderate-high experienced extraction. The analytical observer at civilizational scope occupies the naturalization position: derived d ≈ 0.72 → f(d) ≈ 1.15, but this position risks treating political choice as natural law — the omega variable flags this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by revealing that the Kemalist rupture reading is a genuine Tangled Rope — not a pure Snare falsely labeled as Rope, nor a Rope falsely elevated to Snare. The coordination function is real (literacy access, administrative unification, European alignment). The extraction is also real (class displacement, institutional suppression, state monopoly). The question is not 'which type is correct?' but 'how much of each?' The reading's legitimacy claim rests on the coordination function being genuinely transformative — that it enables modernization that could not occur under Ottoman script arrangements. The structural data confirms mixed-function: extractiveness of 0.58 reflects both coordination (preventing it from being Snare ≥ 0.66) and extraction (preventing it from being Rope ≤ 0.45). The mandatrophy is resolved in the perspectival gap: the same constraint that appears as Rope from the beneficiary's position appears as Snare from the victim's position. Both readings are structurally valid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_cost_distribution_ambiguity,
    'Were script-transition costs genuinely distributed across society, or concentrated on Ottoman-educated elites by design?',
    'Historical analysis of Kemalist policy documents, educational transition timelines, and literacy rate trajectories across urban/rural and class-stratified populations. Examination of whether transition subsidies or compensation mechanisms were offered to displaced scribal class.',
    'If costs were designed to concentrate on elites: constraint is a directed extraction mechanism (Snare from powerless perspective confirmed). If costs were broadly distributed: more agents experience it as Tangled Rope or Rope (coordination with uneven burden-sharing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_cost_distribution_ambiguity, empirical, 'Whether script-transition costs were intentionally concentrated on Ottoman elites').

omega_variable(
    continuity_versus_rupture_claim,
    'Is the Kemalist reading''s core claim — that script change enables rupture from Ottoman-Islamic past — empirically true, or does it mask institutional continuity?',
    'Structural comparison: Ottoman administrative hierarchy vs early Turkish Republic hierarchy; continuity of personnel in state apparatus; analysis of which Ottoman institutions were actually dismantled vs reformed; examination of whether script change produced measurable functional transformation or primarily symbolic repositioning.',
    'If substantial rupture occurred: Kemalist reading''s extraction claim is accurate but may overstate the degree of change (some coordination genuinely occurred). If institutional continuity dominates: constraint is primarily performative (Piton), and the Kemalist reading''s legitimacy claim is largely theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_versus_rupture_claim, empirical, 'Empirical degree of institutional rupture vs continuity following script adoption').

omega_variable(
    reading_foreclosure_condition,
    'Does the Kemalist rupture reading logically foreclose the Ottoman continuity reading within a single commitment framework, or do both remain live alternatives held by different parties?',
    'Examination of Turkish historiography and political discourse: can Ottoman continuity scholars and Kemalist rupture scholars both occupy the same epistemic and institutional space? Are they competing interpretations within one framework (coexistence) or mutually exclusive commitments (foreclosure)?',
    'If foreclosure: only one reading can be true within Turkish state identity claims; the other must be repudiated or hidden. If coexistence: both readings remain available as live positions, suggesting the kernel itself is fundamentally contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_condition, conceptual, 'Whether Kemalist and Ottoman continuity readings logically foreclose each other or coexist').

omega_variable(
    literacy_access_mechanism_ambiguity,
    'Did the Latin script transition increase or decrease literacy access for the broader Turkish population?',
    'Longitudinal literacy statistics: pre-transition (1920s) vs post-transition (1930s, 1940s) literacy rates by region, age cohort, and urban/rural split. Analysis of whether Latin script''s phonetic transparency reduced barrier-to-entry for new learners compared to Arabic script''s complexity.',
    'If literacy increased substantially: the constraint''s coordination function (simplifying literacy acquisition) was genuine, and Rope/Tangled Rope classification is accurate. If literacy increase was marginal or attributable to education expansion (not script change): the script transition was primarily symbolic, and Piton/Snare classification is more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(literacy_access_mechanism_ambiguity, empirical, 'Whether Latin script adoption increased net literacy access').

omega_variable(
    state_monopoly_intentionality,
    'Did the Kemalist state deliberately engineer script transition to establish monopoly control over the literacy apparatus, or was monopoly a side effect of standardization policy?',
    'Examination of Kemalist policy statements, educational reform legislation, and enforcement mechanisms. Analysis of whether transition policy was designed with explicit monopoly objectives or whether control emerged as a practical necessity of managing literacy transition.',
    'If intentional monopoly: state apparatus is primary beneficiary and designer of extraction (supports Snare for victims, Rope for state). If unintentional: constraint may be better classified as Tangled Rope with extraction as side effect rather than design feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_intentionality, empirical, 'Whether state monopoly over literacy was deliberate policy objective or practical side effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_kemalist_theater_t0, script_as_identity__kemalist_rupture_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(script_kemalist_theater_t5, script_as_identity__kemalist_rupture_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(script_kemalist_theater_t10, script_as_identity__kemalist_rupture_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(script_kemalist_extract_t0, script_as_identity__kemalist_rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(script_kemalist_extract_t5, script_as_identity__kemalist_rupture_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(script_kemalist_extract_t10, script_as_identity__kemalist_rupture_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(script_kemalist_suppress_t0, script_as_identity__kemalist_rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(script_kemalist_suppress_t5, script_as_identity__kemalist_rupture_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(script_kemalist_suppress_t10, script_as_identity__kemalist_rupture_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_institutional_authority_succession).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, quranic_literacy_suppression).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, european_alignment_legitimacy_claim).

% DUAL FORMULATION NOTE:
% The script-as-identity constraint decomposes into three structurally distinct constraints with different epsilon values: (1) script_as_administrative_tool (ε ≈ 0.25, Rope) — technical unification of state documentation, (2) script_as_identity_rupture (ε ≈ 0.58, Tangled Rope, THIS constraint) — the identity and cultural significance of the change, (3) script_as_islamic_suppression (ε ≈ 0.72, Snare) — the effect on Islamic institutional continuity and Quranic literacy. Each story has its own beneficiary/victim structure and measurements. They are linked via this network because the Kemalist state used identity-rupture framing to justify administrative unification, which enabled institutional suppression — the causal and legitimacy chains run through multiple constraints. The administrative constraint alone would be Rope; the identity constraint reframes it as rupture; the suppression constraint weaponizes it against Islamic institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
