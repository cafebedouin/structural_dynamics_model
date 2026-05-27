% ============================================================================
% CONSTRAINT STORY: absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_absolutist_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: absolutist_reading
 *   human_readable: Absolutist Speech Protection (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of speech protection — articulated in Brandenburg
 *   v. Ohio (1969) and embedded in contemporary U.S. constitutional doctrine
 *   — treats speech as near-absolutely protected except for direct incitement
 *   to imminent lawless action. This constraint exhibits a false-summit
 *   dynamic: from the perspective of marginalized communities subject to
 *   coordinated hate speech, the doctrine functions as a snare (suppressed
 *   targets bear aggregate harm with zero exit capacity). From the
 *   perspective of civil liberties organizations defending the principle, it
 *   functions as rope (pure coordination of democratic self-defense). From
 *   the analytical observer's vantage, it functions as mountain (treated as a
 *   natural law of liberal democracy). The structural data — identifiable
 *   beneficiaries (institutional speech actors, media corporations, civil
 *   liberties infrastructure), identifiable victims (marginalized
 *   communities, targeted groups), and rising theater ratio over 55 years —
 *   reveals the mountain classification as a false summit. The absolutist
 *   reading is ONE interpretation of the contested kernel
 *   'speech_protection_boundary.' Sibling readings (harm_limited_reading,
 *   balancing_reading) hold different foundational axioms about whether
 *   aggregate harm to minoritized communities is a relevant classification
 *   variable. The absolutist reading forecloses the harm-limited reading by
 *   treating demonstrable aggregate harm as irrelevant to the scope of
 *   protection. This is not a factual disagreement; it is a fundamental
 *   disagreement about which facts matter for constitutional legitimacy.
 *
 * KEY AGENTS:
 *   - Civil Liberties Organizations (ACLU, similar): Primary beneficiary (institutional/arbitrage) — defend principle, build litigation dockets, secure funding and institutional prestige
 *   - Marginalized Communities (racial minorities, religious minorities, LGBTQ+ communities): Primary victim (powerless/trapped) — bear aggregate harm from protected hate speech, incitement short of imminence, coordinated harassment; cannot exit speech environment
 *   - Media Corporations: Secondary beneficiary (institutional/arbitrage) — benefit from absolute protection when reporting on state/corporate misconduct; also use protection to protect from editorial accountability
 *   - Investigative Journalists: Secondary victim (moderate/constrained) — benefit from protection for accountability work, face coordinated harassment under same protection
 *   - Democratic Self-Correction Movements: Organized agents (organized/constrained) — building counter-speech, platform accountability, social pressure infrastructure as workarounds to legal immunity
 *   - Judicial Review System: Institutional administrator (institutional/arbitrage) — maintains Brandenburg doctrine as constitutional law through precedent and institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional reading as inherent structure of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(absolutist_reading, 0.68).
domain_priors:suppression_score(absolutist_reading, 0.72).
domain_priors:theater_ratio(absolutist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(absolutist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(absolutist_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(absolutist_reading, snare).
narrative_ontology:human_readable(absolutist_reading, "Absolutist Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(absolutist_reading, 'ef6129f2-6caf-4d54-a09e-9f1682901be5').
narrative_ontology:cs_created_at('ef6129f2-6caf-4d54-a09e-9f1682901be5', '').
narrative_ontology:cs_kernel_codification('ef6129f2-6caf-4d54-a09e-9f1682901be5', formalized).
narrative_ontology:cs_authority_grounding('ef6129f2-6caf-4d54-a09e-9f1682901be5', lineage).
narrative_ontology:cs_kernel_id(absolutist_reading, speech_protection_boundary).
narrative_ontology:cs_reading_relation('ef6129f2-6caf-4d54-a09e-9f1682901be5', harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('ef6129f2-6caf-4d54-a09e-9f1682901be5', balancing_reading, coexists_with).
narrative_ontology:cs_axiom('ef6129f2-6caf-4d54-a09e-9f1682901be5', foundational, speech_protection_categorically_prior_to_harm_regulation).
narrative_ontology:cs_axiom_status(speech_protection_categorically_prior_to_harm_regulation, holdable).
narrative_ontology:cs_axiom_grounding('ef6129f2-6caf-4d54-a09e-9f1682901be5', speech_protection_categorically_prior_to_harm_regulation, deontological).
narrative_ontology:cs_axiom('ef6129f2-6caf-4d54-a09e-9f1682901be5', secondary, brandenburg_imminence_threshold_immutable).
narrative_ontology:cs_axiom_status(brandenburg_imminence_threshold_immutable, overridden).
narrative_ontology:cs_axiom_grounding('ef6129f2-6caf-4d54-a09e-9f1682901be5', brandenburg_imminence_threshold_immutable, empirically_contingent).
narrative_ontology:cs_reference_frame('ef6129f2-6caf-4d54-a09e-9f1682901be5', first_amendment_as_written).
narrative_ontology:cs_drift_state('ef6129f2-6caf-4d54-a09e-9f1682901be5', contemporary_digital_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(absolutist_reading, institutional_speech_actors).
narrative_ontology:constraint_beneficiary(absolutist_reading, organized_political_minorities).
narrative_ontology:constraint_beneficiary(absolutist_reading, media_corporations).
narrative_ontology:constraint_victim(absolutist_reading, marginalized_communities).
narrative_ontology:constraint_victim(absolutist_reading, targeted_groups).
narrative_ontology:constraint_victim(absolutist_reading, collective_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITIES (SNARE) — Bear aggregate harm from protected hate speech, incitement short of imminent action, and coordinated harassment. Cannot exit the speech environment; suppressed by both the speech itself and the legal prohibition against responding through regulation. Biological/geographic constraints lock exposure. Maximum experienced extraction with zero exit capacity.
constraint_indexing:constraint_classification(absolutist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JOURNALISTS AND INVESTIGATIVE MEDIA (TANGLED ROPE) — Benefit from absolute speech protection when exposing state/corporate misconduct; also face coordinated harassment campaigns and targeted doxxing under the same protection. Mixed extraction: protection enables accountability work but exposes individual journalists to harms they cannot legally address. Constrained exit — can reduce coverage of controversial topics at professional cost.
constraint_indexing:constraint_classification(absolutist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL SPEECH ACTORS (ROPE) — ACLU, free-speech advocacy groups, civil liberties legal infrastructure benefit materially from the absolutist standard. Defend the principle, build litigation dockets, secure funding, establish institutional prestige. Experience the constraint as pure coordination: the principle aligns their material interests perfectly with their stated values. Net beneficiary with arbitrage exit capacity (can reposition if political climate shifts).
constraint_indexing:constraint_classification(absolutist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC SELF-CORRECTION MOVEMENTS (SCAFFOLD) — Organized grassroots and civil society responses to harmful speech (counter-speech, platform accountability, social pressure on speakers). See the absolutist constraint as a temporary coordination challenge with a sunset: they are building alternative verification pathways (social media platform policies, advertiser pressure, academic norms, counter-narrative infrastructure) that constrain harmful speech without legal suppression. Constrained by the legal immunity layer but building workarounds with sunset logic (10-15 year horizon for cultural norm shifts).
constraint_indexing:constraint_classification(absolutist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL REVIEW SYSTEM (PITON) — Brandenburg doctrine is administered as immutable constitutional law, but its functional role has atrophied. Judges apply the imminence test mechanically to cases that have already sorted through market-driven content moderation, platform algorithms, and social coordination. The legal doctrine persists through institutional inertia and lineage authority (Warren Court precedent) long after the real content-regulation work has migrated to private platforms and social pressure. Theater ratio high: doctrine is maintained as constitutional gospel despite degraded functional verification of its core premise (that legal permissiveness protects democratic speech).
constraint_indexing:constraint_classification(absolutist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, absolute speech protection is treated as a bedrock principle of liberal democracy itself — a natural law of the political order that cannot be compromised without dissolving democracy. This perspective sees the Brandenburg standard as emerging naturally from the structure of pluralism and deliberation. However, the structural data (beneficiaries concentrated in institutional speech actors, victims concentrated in powerless groups, theater ratio elevated at judicial level) reveals this as a false summit: the 'natural law of democracy' naturalizes a contingent institutional reading that serves identifiable interests.
constraint_indexing:constraint_classification(absolutist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(absolutist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(absolutist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(absolutist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(absolutist_reading, TR),
    TR >= 0.70.

:- end_tests(absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The absolutist reading concentrates protection benefits in institutional speech actors and media corporations while distributing harm costs to marginalized communities with minimal exit capacity. The measurement trajectory shows rising extractiveness over 55 years (0.48→0.72), reflecting technological acceleration of harm-causation (internet coordination, algorithmic amplification, instant targeting) that makes the Brandenburg imminence standard less functionally protective against modern threats. The doctrine's text remained constant while its operational context shifted, producing increasing extraction from powerless agents. Suppression (0.72): High. Marginalized communities are suppressed by three mechanisms: (1) direct harm from protected speech, (2) legal prohibition against responding through speech regulation, (3) social/institutional coordination that uses protected speech to suppress counter-speech (coordinated harassment, algorithmic amplification of hate speech). No legitimate exit mechanism exists for targeted groups. Theater ratio (0.55): Moderate. The judicial doctrine persists through lineage authority and constitutional legitimacy framing, but its functional role has migrated to private platforms and social pressure. Courts apply Brandenburg mechanically; the real content moderation occurs off-stage through advertiser pressure, platform policies, and social consequences. The doctrine is maintained as constitutional orthodoxy despite degraded verification of its core empirical premise (that legal permissiveness protects democratic speech).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single set of base properties. The institutional speech actors see rope — pure coordination of democratic principles that align perfectly with their material interests. The civil liberties organization sees rope—their entire professional identity and funding depend on defending the principle. The marginalized communities see snare — trapped in a speech environment with no exit and maximum experienced extraction. The journalists see tangled rope — mixed coordination (enabling accountability) and extraction (face coordinated harassment). The democratic self-correction movements see scaffold — building alternative pathways to constrain harmful speech through social/market mechanisms with a generational sunset horizon. The judicial system sees piton — administrative application of a precedent whose functional role has atrophied. The analytical observer risks seeing mountain — natural law of democracy — until the structural data (beneficiaries, victims, theater ratio drift) reveals this as false-summit naturalization. The perspectival gap reveals that the absolutist reading's claim to universality is itself a contingent institutional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to this specific constraint. Marginalized communities as victims with trapped exit face maximum d (≈0.95), producing high f(d) (≈1.42) and experienced χ near maximal. Institutional beneficiaries with arbitrage exit face low d (≈0.15), producing negative f(d) (≈-0.01) and experienced χ near zero or negative. Moderate agents (journalists) with constrained exit face mid-range d (≈0.65), producing moderate f(d) (≈1.00). The analytical observer's identity_locked perspective would register that the naturalization of the absolutist reading as 'inherent to democracy' is itself a captured position — the observer's analytical frame cannot see alternative readings that the framework would require examining. The engine's directionality derivation chain produces these d values automatically from beneficiary/victim declarations; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the absolutist reading's snare classification (for powerless agents) is not reconcilable with its natural-law framing (mountain). The mountain perspective attempts to naturalize what is structurally a contingent institutional reading. The false-summit signature fires when the engine detects (a) beneficiaries present (civil liberties organizations, media corporations, institutional speech actors), (b) mountain classification attempted, and (c) non-zero chi from beneficiary presence. The omega variables document the irreducible uncertainties: the aggregate harm threshold, the coordinated-versus-individual causation distinction, the imminence-threshold drift over technology change, and the comparative outcomes across speech regimes. The mandatrophy is resolved by showing that the absolutist reading is a live institutional position with real structural consequences, not a natural law. The sibling readings (harm_limited, balancing) are alternative institutional readings with different beneficiary/victim structures. No single reading is universally correct; the framework's job is to classify each reading's structural properties accurately and show where they diverge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_harm_causation_threshold,
    'At what aggregate harm level to targeted communities does the externality of absolutist protection become a structural extraction mechanism rather than an acceptable democratic cost?',
    'Longitudinal measurement of targeted group health outcomes, harassment severity, civic participation, and sense of safety correlated with speech protection jurisprudence changes; comparative analysis across democracies with different speech thresholds',
    'If threshold is crossed (measurable disparate impact on targeted groups): constraint reclassifies from snare→rope for marginalized perspective (acknowledging coordination function) toward snare→tangled_rope (harms become explicit extraction side effect). If no threshold is identifiable: absolutist reading maintains that distributional harm is not a relevant classification dimension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_harm_causation_threshold, empirical, 'Aggregate harm threshold for extracting reclassification').

omega_variable(
    coordinated_harassment_versus_individual_speech,
    'Is coordinated harassment campaigns using protected speech (doxxing, swarm attacks, threat networks) the same constraint as individual speech protection, or a structurally distinct extraction mechanism?',
    'Network analysis of speech coordination: identify whether aggregate harm comes from dispersed individual speakers or from organized coordination with shared targeting. Separate statistical/causal analysis for individual speaker contribution versus network effect.',
    'If coordinated: the constraint may decompose into two stories (individual speech protection=rope/scaffold, coordinated harassment=snare), requiring network.affects_constraints linking. If purely individual: snare classification stands, but the victims'' experienced extraction is aggregated rather than individualized (different causation model, same outcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_harassment_versus_individual_speech, empirical, 'Whether aggregate harm is from individual speakers or coordinated networks').

omega_variable(
    imminence_threshold_drift,
    'Has the Brandenburg imminence standard''s operational meaning shifted over time due to changes in communication technology and threat acceleration, such that the same doctrine applies to fundamentally different harm-causation structures?',
    'Historical analysis of Brandenburg case law: extraction of the operational imminence threshold across decades; correlation with communication technology and threat acceleration rates; measurement of mean time-to-harm for contemporaneous threats versus threats at the 1969 Brandenburg standard''s empirical context',
    'If significant drift: the doctrine''s functional role has changed while its text remained constant (classic piton signature). The ''imminent lawless action'' standard may now permit speech harms that occur over days/weeks rather than the immediate context Brandenburg addressed. Reclassifies piton diagnosis as confirmed rather than hypothetical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_threshold_drift, empirical, 'Temporal drift in Brandenburg imminence threshold across technology changes').

omega_variable(
    false_summit_committer_frame,
    'Is the absolutist reading''s claim that speech protection is a natural law of democracy, or a contingent institutional reading that benefits identifiable actors (civil liberties organizations, institutional speakers, media corporations)?',
    'Committer-frame analysis: This constraint is ONE READING of the contested kernel ''speech_protection_boundary.'' The sibling readings (harm_limited_reading, balancing_reading) hold different foundational axioms about whether aggregate harm to targeted communities is a relevant constraint-classification variable. The absolutist reading forecloses the harm_limited reading''s core premise: that demonstrable aggregate harm to minoritized communities constitutes sufficient grounds for narrowing the protected set. This is not a disagreement about facts; it is a disagreement about which facts matter for constitutional legitimacy.',
    'If this reading''s natural-law framing is itself a contingent institutional choice: classification domain shifts from ''inherent structure of democracy'' (mountain) to ''institutional interpretation benefiting speech actors'' (snare from powerless perspectives, rope from beneficiary perspectives). The false summit signature is triggered by identifiable beneficiaries + explicit naturalizing language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_committer_frame, conceptual, 'Whether absolutist framing is natural law or contingent reading').

omega_variable(
    alternative_balancing_regimes_comparative,
    'In democracies that employ balancing tests rather than absolute protection (Germany, Canada, South Africa), do marginalized communities experience measurably lower harm while maintaining functional democratic speech, or does the balancing regime produce different extraction structures?',
    'Comparative empirical study: measure targeted-group safety, civic participation, and harassment severity across speech regimes; measure democratic accountability, investigative journalism capacity, and government constraint across regimes; identify which regime''s victims experience lower aggregate extraction.',
    'If balancing regimes show lower harm to targets without degrading democratic function: absolutist reading is contingent choice, not natural necessity (snare classification confirmed). If balancing regimes show equivalent or worse outcomes for marginalized groups: absolutist reading''s harm-externality claim is overstated (snare→tangled_rope at powerless perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_balancing_regimes_comparative, empirical, 'Comparative outcomes across speech protection regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(absolutist_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(absol_theater_1969, absolutist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(absol_theater_1984, absolutist_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(absol_theater_2004, absolutist_reading, theater_ratio, 35, 0.5).
narrative_ontology:measurement(absol_theater_2024, absolutist_reading, theater_ratio, 55, 0.55).

% Extraction over time
narrative_ontology:measurement(absol_extract_1969, absolutist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(absol_extract_1984, absolutist_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(absol_extract_2004, absolutist_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(absol_extract_2024, absolutist_reading, base_extractiveness, 55, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(absolutist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(absolutist_reading, 0.12).
narrative_ontology:affects_constraint(absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(absolutist_reading, balancing_reading).
narrative_ontology:affects_constraint(absolutist_reading, platform_content_moderation).
narrative_ontology:affects_constraint(absolutist_reading, counter_speech_infrastructure).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three constraint stories: absolutist_reading (ε=0.68, snare from powerless perspective), harm_limited_reading (ε unknown, victim-centered), and balancing_reading (ε unknown, proportionalist). Each instantiates a different set of beneficiaries, victims, and operational mechanisms. All three are linked as sibling readings of the same contested kernel. The absolutist reading influences the downstream constraints: platform_content_moderation (private platforms operating in the legal immunity space created by absolutism) and counter_speech_infrastructure (organized responses to the externalities absolutism creates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
