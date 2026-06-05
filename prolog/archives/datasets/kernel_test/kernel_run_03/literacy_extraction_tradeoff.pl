% ============================================================================
% CONSTRAINT STORY: literacy_extraction_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_extraction_tradeoff, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: literacy_extraction_tradeoff
 *   human_readable: Literacy-Extraction Tradeoff: State-Imposed Script Reform and Cultural Access
 *   domain: political_economy/state_formation/linguistic_engineering
 *
 * SUMMARY:
 *   Turkey's 1928 alphabet reform (Law 1353) replaced Arabic script with
 *   Latin script via state imposition within three months, severing immediate
 *   access to the entire corpus of Ottoman literary, religious, and
 *   administrative texts. The constraint exhibits the full range of DR
 *   classifications depending on the observer's structural position and time
 *   horizon. For Ottoman literati (powerless/trapped), the reform is pure
 *   extraction — their accumulated human capital becomes instantly worthless,
 *   enforcement criminalizes their prior literacy, and no coordination
 *   benefit exists. For the state apparatus (institutional/arbitrage), it is
 *   pure coordination — script unification reduces administrative overhead
 *   and enables European integration. For merchants (moderate/constrained),
 *   it is tangled rope — genuine transition costs mixed with real
 *   coordination benefits for trade. For teacher-activists
 *   (organized/mobile), it is scaffold — temporary coercive enforcement
 *   supporting a sunset goal of mass literacy. For Ottoman institutional
 *   legacy (institutional/arbitrage at civilizational scale), it is piton —
 *   the script reform completes institutional extinction dressed in literacy
 *   progress rhetoric. For the analytical observer (analytical/analytical),
 *   the constraint risks appearing as a natural law — script reform as
 *   inevitable technical rationality — which the false summit detector
 *   identifies as naturalization of a political choice. The measurements show
 *   extractiveness declining over the first decade (as mass literacy
 *   stabilizes and coercive enforcement requirements drop) and theater ratio
 *   declining (as literacy genuinely expands rather than remaining
 *   performative state display). The constraint tests whether a commitment
 *   system kernel can be installed with zero prior occupancy of the new
 *   reading — whether state will alone can create new legitimacy claims
 *   without invoking pre-existing authority.
 *
 * KEY AGENTS:
 *   - Ottoman Literati Class: Primary victim (powerless/trapped) — scribes, scholars, intellectuals whose accumulated Arabic literacy becomes instantly worthless; face criminalized non-compliance and professional extinction
 *   - State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — achieves script uniformity, enables bureaucratic standardization, captures legitimacy claim of rational modernization
 *   - Adult Merchant Class: Secondary victim and partial beneficiary (moderate/constrained) — face significant retraining burden but benefit from Latin script's integration with European commerce
 *   - Teacher-Activist Coalition: Organized agent (organized/mobile) — pedagogical reformers who leverage script reform to achieve mass literacy goal with sunset horizon
 *   - Ottoman Institutional Legacy: Institutional victim (institutional/arbitrage at civilizational scale) — religious authority, administrative continuity, legitimacy grounding in Ottoman precedent all atrophy; script reform completes a severance begun with Caliphate abolition
 *   - Future Schooled Generations: Secondary beneficiary (moderate/mobile) — gain rapid functional literacy via Latin script education; lose direct access to Ottoman heritage unless dual-literacy training provided
 *   - Analytical Observer: Epistemically positioned agent (analytical/analytical) — risks naturalizing the reform as inevitable rationality rather than political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_extraction_tradeoff, 0.58).
domain_priors:suppression_score(literacy_extraction_tradeoff, 0.72).
domain_priors:theater_ratio(literacy_extraction_tradeoff, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_extraction_tradeoff, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_extraction_tradeoff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_extraction_tradeoff, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_extraction_tradeoff, tangled_rope).
narrative_ontology:human_readable(literacy_extraction_tradeoff, "Literacy-Extraction Tradeoff: State-Imposed Script Reform and Cultural Access").
narrative_ontology:topic_domain(literacy_extraction_tradeoff, "political_economy/state_formation/linguistic_engineering").

domain_priors:requires_active_enforcement(literacy_extraction_tradeoff).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_extraction_tradeoff, '81b928cf-dc5d-4c80-a6d6-12f2acc136be').
narrative_ontology:cs_created_at('81b928cf-dc5d-4c80-a6d6-12f2acc136be', '').
narrative_ontology:cs_kernel_codification('81b928cf-dc5d-4c80-a6d6-12f2acc136be', formalized).
narrative_ontology:cs_authority_grounding('81b928cf-dc5d-4c80-a6d6-12f2acc136be', extraction).
narrative_ontology:cs_interpretation_layer_present('81b928cf-dc5d-4c80-a6d6-12f2acc136be').
narrative_ontology:cs_reading_relation('81b928cf-dc5d-4c80-a6d6-12f2acc136be', ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('81b928cf-dc5d-4c80-a6d6-12f2acc136be', western_modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('81b928cf-dc5d-4c80-a6d6-12f2acc136be', islamic_script_preservation_reading, forecloses).
narrative_ontology:cs_axiom('81b928cf-dc5d-4c80-a6d6-12f2acc136be', foundational, latin_script_necessary_modernity).
narrative_ontology:cs_axiom_status(latin_script_necessary_modernity, holdable).
narrative_ontology:cs_axiom_grounding('81b928cf-dc5d-4c80-a6d6-12f2acc136be', latin_script_necessary_modernity, empirically_contingent).
narrative_ontology:cs_axiom('81b928cf-dc5d-4c80-a6d6-12f2acc136be', foundational, state_will_sufficient_legitimacy).
narrative_ontology:cs_axiom_status(state_will_sufficient_legitimacy, overridden).
narrative_ontology:cs_axiom_grounding('81b928cf-dc5d-4c80-a6d6-12f2acc136be', state_will_sufficient_legitimacy, deontological).
narrative_ontology:cs_axiom('81b928cf-dc5d-4c80-a6d6-12f2acc136be', secondary, administrative_efficiency_moral_imperative).
narrative_ontology:cs_axiom_status(administrative_efficiency_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('81b928cf-dc5d-4c80-a6d6-12f2acc136be', administrative_efficiency_moral_imperative, instrumental).
narrative_ontology:cs_reference_frame('81b928cf-dc5d-4c80-a6d6-12f2acc136be', ottoman_administrative_rationality).
narrative_ontology:cs_drift_state('81b928cf-dc5d-4c80-a6d6-12f2acc136be', post_reform_establishment, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_extraction_tradeoff, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(literacy_extraction_tradeoff, future_schooled_generations).
narrative_ontology:constraint_victim(literacy_extraction_tradeoff, existing_literate_classes).
narrative_ontology:constraint_victim(literacy_extraction_tradeoff, ottoman_cultural_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN LITERATI (SNARE) — Arabic script literacy is their primary source of cultural authority and professional employment (scribes, religious scholars, intellectuals). The script reform severs their knowledge base instantly. They cannot exit — their accumulated human capital becomes worthless overnight. Suppression is maximal: non-compliance with the script change is criminalized; possession of Ottoman texts is discouraged; retraining programs do not restore their prior social position. No coordination benefit exists for this group — the constraint extracts their accumulated literacy capital entirely.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADULT MERCHANT CLASS (TANGLED ROPE) — Face significant costs: existing business correspondence must be relearned; contracts and ledgers require script conversion or replacement; training time competes with business operations. But genuine coordination benefit exists: the Latin alphabet enables integration with European commerce, reduces transliteration errors in trade names, and harmonizes with emerging international business standards. Benefits are distributed but real — constrained mobility rather than entrapment. Some merchants possess sufficient resources to hire scribes for conversion; others bear full retraining burden.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE ADMINISTRATIVE APPARATUS (ROPE) — Experiences the script reform as coordination: unified writing system reduces administrative overhead, standardizes official documents, and enables systematic bureaucratic control. The apparatus benefits from script uniformity without bearing conversion costs (state resources absorb the burden). Low effective extraction from the state's perspective — the constraint solves legitimate coordination problems of statecraft. Arbitrage access: the state can maintain Arabic script usage internally during transition or leverage the reform as political capital with European powers.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TEACHER-ACTIVIST COALITION (SCAFFOLD) — Organized pedagogical movement (Atatürk's education reformers, literacy campaigns) sees the script reform as temporary structural support for a sunset goal: mass literacy. The old Arabic-script system blocked literacy expansion (required years of study, limited to elite). Latin script enables rapid mass teaching and achieves functional literacy within months. The coalition experiences suppression (coercive enforcement, rapid curriculum overhaul) but with clear horizon: once mass literacy is achieved and stabilized, the coercive enforcement phase should decline. Theater ratio is moderate — genuine literacy achievement, not purely performative state display.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: OTTOMAN INSTITUTIONAL LEGACY (PITON) — The Arabic script system was the primary institutional carrier of Ottoman identity, religious authority, and administrative continuity. The script reform dissolves this institutional substrate. What persists is theatrical: official rhetoric frames the reform as 'rational modernization,' but the real institutional function (Islamic theological interpretation, legitimacy grounding in Ottoman precedent) atrophies. The legacy institutions persist in degraded form — the Caliphate was abolished one year prior; religious courts are subordinated; Ottoman law codes are being replaced. Script reform is the final institutional extinction dressed in literacy progress rhetoric. Theater ratio reflects that the 'literacy' narrative masks institutional severance.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, script reform is a contingent technical solution to a universal coordination problem: human populations require shared written symbols. The Arabic script system was merely one solution; Latin script is another. Neither is inherently superior — the 'naturalness' of any script is a sociological fact, not a law of nature. Yet the state apparatus invokes naturalness ('Latin is more rational,' 'Arabic is backward') to justify coercive imposition. The engine's false summit detector will identify this perspective as naturalization of a political choice masquerading as inevitable technical rationality.
constraint_indexing:constraint_classification(literacy_extraction_tradeoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_extraction_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(literacy_extraction_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(literacy_extraction_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_extraction_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(literacy_extraction_tradeoff, TR),
    TR >= 0.70.

:- end_tests(literacy_extraction_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high at t=0, declining to 0.48 by t=10. The base value reflects that the constraint involves asymmetric costs and benefits — Ottoman literati bear full transition costs while state apparatus captures administrative benefits. The decline over time shows that extractiveness decreases as mass literacy stabilizes (bottleneck constraint on retraining capacity eases) and as enforcement burden drops (active suppression becomes less necessary once new script is dominant). However, extractiveness does not decline to pure coordination levels (≤0.35) because the permanent loss of easy access to Ottoman texts remains — the cultural extraction is irreversible even as functional extraction (literacy bottleneck) resolves. Suppression (0.72): High throughout. The constraint exhibits multiple suppression mechanisms: criminalization of Arabic script, removal of Ottoman texts from schools and libraries, professional penalties for non-compliance (scholars and scribes lose employment), curriculum disruption, and ideological framing that positions Arabic as 'backward.' Suppression is structural — alternatives are actively eliminated, not merely costly. Theater ratio (0.48): Moderate. The literacy gains are genuine (functional illiteracy does decline, new generations achieve rapid reading capability), so the constraint is not purely theatrical. But the reform is also politically displayed — invoked as proof of modernization and rational statecraft — so some ratio of the reform's legitimacy claim is performative rather than functional. Claimed type (Tangled Rope) reflects that genuine coordination (mass literacy, administrative unification, European integration) coexists with asymmetric extraction (Ottoman literati dispossession, cultural severance, enforcement mechanisms). The state must enforce active cooperation from the population; hence requires_active_enforcement is true.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The Ottoman literati see a Snare — pure extraction with no coordination benefit, maximum suppression, no exit. The state apparatus sees a Rope — pure coordination with genuine administrative efficiency gains, no extraction from their perspective, arbitrage options available. The gap reflects their opposite structural positions: one loses accumulated capital entirely; the other gains administrative power entirely. Merchants see a Tangled Rope (mixed costs/benefits); the teacher-activist coalition sees a Scaffold (temporary coercive support for a sunset goal of mass literacy); the Ottoman legacy sees a Piton (degraded institutional persistence through performative display); the analytical observer risks seeing a Mountain (natural law of script efficiency) — which the false summit detector flags as naturalization. The perspectival gap reveals that whether this constraint is a justified coordination mechanism or an illegitimate extraction depends entirely on whose structural position you occupy. No single perspective captures the full constraint; the presheaf over the observation sites IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies across perspectives based on each agent's structural relationship to the constraint. Ottoman literati are full victims (d ≈ 0.95) with trapped exit options — the constraint extracts their literacy capital entirely with no escape route. The state apparatus is a full beneficiary (d ≈ 0.05) with arbitrage options — it can maintain Arabic internally, selectively enforce compliance, or leverage the reform diplomatically. Merchants are mixed (d ≈ 0.55) with constrained exit — they face real retraining costs but also benefit from European business integration. The teacher-activist coalition is partially beneficiary (d ≈ 0.40) with mobile options — they can advocate for slower transition or parallel Arabic-literacy education. The Ottoman legacy is a victim (d ≈ 0.85) with institutional arbitrage (the state can choose to preserve texts, fund dual-script education, or allow erosion). At the analytical context, d ≈ 0.72 — the observer sees extractive structural dynamics but retains analytical distance (not fully targeted by the enforcement mechanisms). The sigmoid f(d) transforms these directionality values into experienced extractiveness (chi), modulated by scope (national = 1.0). Beneficiaries with low d experience negative chi (they perceive the constraint as beneficial coordination); victims with high d experience high chi (they perceive it as coercive extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED via perspectival pluralism. The constraint resolves the mandatrophy by showing that Tangled Rope is the correct classification at the analytical observer's position (institutional + moderate agents + constrained exit + national scope), while Snare, Rope, and Scaffold are legitimate from their respective positions. The false summit (Mountain) is correctly identified as naturalization — the state's claim that script reform is inevitable technical rationality is contradicted by the structural data showing clear beneficiaries (state apparatus, future generations) and clear victims (Ottoman literati, cultural continuity). The Piton classification of the Ottoman legacy is correct at the civilizational timescale — institutional functions have atrophied, but the shell persists through theater. The scaffold classification of the teacher-activist coalition is correct IF the enforcement suppression actually declines after mass literacy stabilizes — the measurements support this hypothesis (extractiveness declining from 0.72 to 0.48 over 10 years, theater ratio declining from 0.62 to 0.42). The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' — and the answer is that the constraint is a genuine Tangled Rope that APPEARS as different types depending on observer position, with the false summit and piton classifications marking the naturalizing and theatrical perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literacy_gains_attribution,
    'Did the literacy gains (functional illiteracy declining from ~80% to ~20% within a decade) result from the script reform itself or from simultaneous investment in state education infrastructure?',
    'Comparative analysis: literacy trajectories in Ottoman regions before/after reform vs. control regions with equivalent education investment but no script change; decompose literacy gains by age cohort (pre-reform adults vs. post-reform children)',
    'If reform was primary driver: constraint is genuinely coordination-heavy (Rope/Tangled Rope dominate). If infrastructure was primary: constraint is primarily extractive (Snare/Tangled Rope dominate) — the script reform was a vehicle for enforcing state control over education, not a technical necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_gains_attribution, empirical, 'Attribution of literacy gains to script reform vs. education infrastructure').

omega_variable(
    cultural_severing_reversibility,
    'Is the loss of access to Ottoman literary heritage (religious texts, classical literature, administrative records) permanent, or can dual-literacy education recover it?',
    'Historical study: did post-reform generations with dual-script education demonstrate meaningful engagement with Ottoman texts? Did archival recovery programs or religious education maintain textual transmission chains?',
    'If permanent: the extraction from cultural continuity is absolute and irreversible (supports Snare classification for intellectuals). If recoverable: the extraction is a transition cost with potential reversal (supports Tangled Rope with long-term coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_severing_reversibility, empirical, 'Reversibility of Ottoman literary heritage access loss').

omega_variable(
    kernel_installation_mechanism,
    'Can a commitment system kernel (the legitimacy claim grounding the script reform) be installed with zero prior occupancy of the new reading, or does it require pre-existing consensus?',
    'Doctrinal analysis: did the state invoke a pre-existing reading (e.g., Ottoman modernization doctrine) or did it declare a new reading ex nihilo? Examine speeches, decrees, and pedagogical materials for invocation of prior authority vs. assertion of pure state will.',
    'If zero-occupancy installation: the reform instantiates a new kernel reading without lineage grounding (high fragility, high theater). If pre-existing reading invoked: the reform reinterprets an existing Ottoman modernization doctrine (more stable, genuine authority grounding). Affects whether this is a false summit (natural law) or a Piton (degraded institution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_installation_mechanism, conceptual, 'Kernel installation mechanism: ex nihilo vs. pre-existing reading invocation').

omega_variable(
    enforcement_mechanism_sustainability,
    'Did the enforcement mechanisms (criminalization of Arabic script, removal of Arabic texts from schools, professional penalties for non-compliance) persist as permanent suppression or decline over time?',
    'Historical legal documentation: track enforcement intensity by decade; examine whether Arabic-script literacy was eventually permitted, tolerated, or actively encouraged; study whether suppression metrics declined as mass literacy stabilized.',
    'If suppression persisted: constraint is stable high-extraction (Snare). If suppression declined after mass literacy achieved: constraint is genuine scaffold (Tangled Rope transitioning to Rope). Directly tests scaffold hypothesis that coercion was temporary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_sustainability, empirical, 'Enforcement mechanism persistence or decline over time').

omega_variable(
    false_summit_naturalness_claim,
    'Does the analytical observer''s ''mountain'' perspective rest on a genuine natural law about script systems, or is it naturalizing a political choice?',
    'Comparative linguistics: are there inherent properties of Latin vs. Arabic script that make one ''more rational'' or ''more natural''? Or are efficiency and naturalness sociologically constructed attributes that the state imposed?',
    'If inherent properties exist: mountain is legitimate (script efficiency is a law of information theory). If constructed: mountain is a false summit (the state naturalized a political choice). Determines whether the engine''s FSM detector flags this perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalness_claim, empirical, 'Naturalness of script superiority claims').

omega_variable(
    beneficiary_identification_ambiguity,
    'Who are the primary beneficiaries of the script reform? The state administrative apparatus? Future schooled generations? European-oriented modernizers? Or is ''benefit'' entirely constructed retrospectively?',
    'Examine contemporary documents (1920s-1930s) to identify who advocated for the reform and what benefits they explicitly claimed. Compare to actual beneficiary distribution across Ottoman social groups. Document whether beneficiary claims match empirical outcomes.',
    'If clear beneficiary identification: constraint is Tangled Rope (genuine asymmetric structure). If beneficiary identification is retroactive: constraint may be pure Piton (theater masking institutional extinction). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Beneficiary group identification and benefit distribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_extraction_tradeoff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lit_tr_t0, literacy_extraction_tradeoff, theater_ratio, 0, 0.62).
narrative_ontology:measurement(lit_tr_t2, literacy_extraction_tradeoff, theater_ratio, 2, 0.55).
narrative_ontology:measurement(lit_tr_t5, literacy_extraction_tradeoff, theater_ratio, 5, 0.48).
narrative_ontology:measurement(lit_tr_t10, literacy_extraction_tradeoff, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(lit_be_t0, literacy_extraction_tradeoff, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(lit_be_t2, literacy_extraction_tradeoff, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(lit_be_t5, literacy_extraction_tradeoff, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(lit_be_t10, literacy_extraction_tradeoff, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_extraction_tradeoff, information_standard).
narrative_ontology:affects_constraint(literacy_extraction_tradeoff, ottoman_institutional_dissolution).
narrative_ontology:affects_constraint(literacy_extraction_tradeoff, post_colonial_identity_engineering).
narrative_ontology:affects_constraint(literacy_extraction_tradeoff, script_literacy_coordination).

% DUAL FORMULATION NOTE:
% The alphabet reform constraint family should decompose into: (1) literacy_coordination — the genuine coordination problem of achieving mass literacy (epsilon ≈ 0.30, Rope); (2) cultural_heritage_severance — the permanent loss of access to Ottoman texts (epsilon ≈ 0.75, Snare); (3) literacy_extraction_tradeoff (this file) — the combined constraint showing how genuine coordination is inseparable from coercive extraction. The constraint family represents the ε-invariance principle applied to state-imposed reforms: measure coordination separately from extraction, and they decompose into distinct constraints with different ε values, different beneficiary/victim structures, and different terminal classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
