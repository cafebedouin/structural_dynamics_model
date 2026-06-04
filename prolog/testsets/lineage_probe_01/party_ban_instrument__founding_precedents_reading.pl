% ============================================================================
% CONSTRAINT STORY: party_ban_instrument__founding_precedents_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_ban_founding_precedents, []).

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
 *   constraint_id: party_ban_instrument__founding_precedents_reading
 *   human_readable: Party Ban Instrument — Founding Precedents Reading (SRP/KPD Bans)
 *   domain: constitutional_law/political_exclusion
 *
 * SUMMARY:
 *   The Federal Republic of Germany's constitutional instrument for
 *   dissolving political parties emerged from the immediate post-WWII
 *   founding moment through two landmark Federal Constitutional Court
 *   (BVerfGE) decisions: the 1952 Socialist Reich Party (SRP) ban and the
 *   1956 Communist Party (KPD) ban. This constraint instantiates the
 *   FOUNDING_PRECEDENTS reading of the contested party_ban_instrument kernel.
 *   Under this reading, the simultaneity and symmetry of early bans —
 *   executing suppression against both extremist flanks (neo-Nazi right and
 *   communist left) — established the instrument's founding doctrine as a
 *   neutral, bounded defense of the constitutional order. The reading claims
 *   that the doctrine was proven on both flanks at once, establishing that
 *   the ban power is not the tool of a dominant faction but a structural
 *   feature of constitutional self-defense. The constraint exhibits
 *   tangled_rope characteristics: genuine coordination function
 *   (constitutional center defending the democratic framework against
 *   anti-system movements) combined with asymmetric extraction (flank
 *   movements have no exit, center retains political flexibility). The
 *   theater ratio (0.38) reflects that the early bans were executed on
 *   genuine organizational and ideological grounds — not as performative
 *   ritual but as substantial structural intervention. Extractiveness (0.48)
 *   is moderate because the doctrine was chosen deliberately by the
 *   constitutional center as preferable to alternative regime-stabilization
 *   mechanisms, yet the constraint does extract costs: banned movements lose
 *   organizational form and electoral access, and the precedent constrains
 *   the center's own political behavior through the latent threat of
 *   reciprocal application.
 *
 * KEY AGENTS:
 *   - Constitutional Center (Parliamentary Majority): Institutional actor (institutional/arbitrage) — primary beneficiary and primary enforcer. Establishes itself as guardian of constitutional order. Experience: Rope (pure coordination with no extraction cost to the center itself).
 *   - SRP and KPD Members: Powerless agents (powerless/trapped) — primary victims. Organizational dissolution, membership prosecution, property seizure, electoral participation foreclosed. Experience: Snare (pure extraction with no exit).
 *   - Anti-System Flanks (Successor Movements): Moderate agents (moderate/constrained) — secondary victims. Can reorganize but under threat of the same doctrine. Experience: Snare (significant extraction through organizational form suppression) or Constrained Tangled Rope (if successor movements adapt to legal constraints).
 *   - Federal Constitutional Court: Institutional actor (institutional/arbitrage) — key enforcer and beneficiary. Gains precedent-setting authority and role as neutral arbiter of constitutional boundaries. Experience: Rope (coordination mechanism strengthening institutional position).
 *   - Democratic Ecosystem (Long-term Perspective): Organized agents (organized/mobile) — sees the bans as temporary scaffolding necessary for founding stabilization. Experience: Scaffold (temporary measure with implicit sunset as democratic institutions mature).
 *   - Analytical Observer: Transcendent perspective (analytical/analytical) — risks naturalizing the contingent constitutional choice as a law of democratic order. Experience: Mountain (false summit — naturalizing a constitutional choice as a necessity).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_ban_instrument__founding_precedents_reading, 0.48).
domain_priors:suppression_score(party_ban_instrument__founding_precedents_reading, 0.65).
domain_priors:theater_ratio(party_ban_instrument__founding_precedents_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_ban_instrument__founding_precedents_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(party_ban_instrument__founding_precedents_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(party_ban_instrument__founding_precedents_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_ban_instrument__founding_precedents_reading, tangled_rope).
narrative_ontology:human_readable(party_ban_instrument__founding_precedents_reading, "Party Ban Instrument — Founding Precedents Reading (SRP/KPD Bans)").
narrative_ontology:topic_domain(party_ban_instrument__founding_precedents_reading, "constitutional_law/political_exclusion").

domain_priors:requires_active_enforcement(party_ban_instrument__founding_precedents_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_ban_instrument__founding_precedents_reading, '35372188-7357-469c-9b63-481ff8b21654').
narrative_ontology:cs_kernel_codification('35372188-7357-469c-9b63-481ff8b21654', formalized).
narrative_ontology:cs_authority_grounding('35372188-7357-469c-9b63-481ff8b21654', lineage).
narrative_ontology:cs_interpretation_layer_present('35372188-7357-469c-9b63-481ff8b21654').
narrative_ontology:cs_reading_relation('35372188-7357-469c-9b63-481ff8b21654', party_ban_instrument__chilling_critique_reading, coexists_with).
narrative_ontology:cs_reading_relation('35372188-7357-469c-9b63-481ff8b21654', party_ban_instrument__potentiality_threshold_reading, influences).
narrative_ontology:cs_axiom('35372188-7357-469c-9b63-481ff8b21654', foundational, founding_bans_established_symmetric_doctrine).
narrative_ontology:cs_axiom_status(founding_bans_established_symmetric_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('35372188-7357-469c-9b63-481ff8b21654', founding_bans_established_symmetric_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('35372188-7357-469c-9b63-481ff8b21654', foundational, ban_power_bounded_by_existential_threat).
narrative_ontology:cs_axiom_status(ban_power_bounded_by_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('35372188-7357-469c-9b63-481ff8b21654', ban_power_bounded_by_existential_threat, deontological).
narrative_ontology:cs_reference_frame('35372188-7357-469c-9b63-481ff8b21654', constitutional_republic_self_defense).
narrative_ontology:cs_drift_state('35372188-7357-469c-9b63-481ff8b21654', contemporary_post_cold_war, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('35372188-7357-469c-9b63-481ff8b21654', '').
narrative_ontology:cs_kernel_id(party_ban_instrument__founding_precedents_reading, party_ban_instrument).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_ban_instrument__founding_precedents_reading, constitutional_center).
narrative_ontology:constraint_victim(party_ban_instrument__founding_precedents_reading, anti_system_left_flank).
narrative_ontology:constraint_victim(party_ban_instrument__founding_precedents_reading, anti_system_right_flank).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BANNED PARTY MEMBER (SNARE) — Structurally trapped. Cannot participate in electoral politics, faces membership prosecution, loses organizational infrastructure overnight. The ban forecloses exit: political identity is dissolved, property seized, organizational communication criminalized. No alternative path to pursue political aims within the legal system. Maximum experienced extraction.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDEOLOGICAL FLANK / SUCCESSOR MOVEMENTS (SNARE) — Constrained but not trapped. Can reorganize under new names, operate in civil society, carry ideas forward through sympathetic figures. But the ban's sword hangs overhead: the same doctrine used to dissolve the KPD or SRP can be wielded against successor movements that manifest 'hostile intent' + organizational capacity. The constraint suppresses direct political participation and establishes precedent for expanded bans. Significant extraction through foreclosure of preferred organizational forms, though exit via ideological persistence remains.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (ROPE) — Benefits from the ban instrument as a coordination mechanism. The court gains institutional authority, precedent-setting power, and a clear doctrine for defending the constitutional order against anti-system movements. The ban provides the court with an arbitrage option: it can deploy the same instrument against threats from either flank, establishing itself as the neutral guardian of the constitutional order. The court experiences the constraint as pure coordination — a tool for systemic stabilization that poses no extraction cost to the institution.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL CENTER / PARLIAMENTARY MAJORITY (TANGLED ROPE) — Primary beneficiary and primary enforcer. The ban protects the constitutional order from both flanks simultaneously, establishing the center's legitimacy as the framework-keeper. But the constraint also binds the center: using the ban instrument constrains the center's own political flexibility. The threat of reciprocal bans (if political majority shifts) disciplines centrist parties not to overreach. The constraint coordinates defense of the constitutional order (genuine function) while extracting costs from the center through the discipline of mutual vulnerability. Moderate extractiveness because the center chose the constraint deliberately as preferable to undefended democracy.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC ECOSYSTEM / EVOLVING CONSENSUS (SCAFFOLD) — From a civilizational view, the ban instrument is a temporary stabilization mechanism with implicit sunset. The founding precedents (SRP/KPD bans) were justified by the acute post-WWII threat and Cold War emergency. As democratic institutions mature and anti-system movements lose organizational capacity or adopt less existential stances, the ban instrument's necessity declines. The ecosystem perspective sees the ban as a scaffolding: necessary to stabilize the constitutional order in its founding phase, but reducing in necessity as institutional practices deepen and extremist threats recede. Theater ratio is low (0.38) because the bans were executed on genuine organizational grounds, not performatively.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the ban instrument might appear as an immutable structural feature of constitutional order: any constitutional system must exclude movements that deny the constitutional order's foundational premises. This perspective risks naturalizing the contingent institutional choice (banning vs. monitoring, propaganda bans vs. party bans, or open confrontation) as a law of politics itself. The engine will detect this as a false summit — the structural data reveals the ban as a contingent choice, not a natural necessity.
constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_ban_instrument__founding_precedents_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_ban_instrument__founding_precedents_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(party_ban_instrument__founding_precedents_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(party_ban_instrument__founding_precedents_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The ban instrument extracts substantially from flank movements — organizational dissolution is a severe cost. But extractiveness is not maximal (≥0.66) because: (1) the bans were executed under genuine constitutional doctrine, not arbitrary state power; (2) the center itself bears costs through mutual vulnerability (the same doctrine could be deployed against a future center government); (3) successor movements can reorganize in attenuated forms. The value reflects that extraction exists but is bounded by constitutional principle rather than unconstrained state predation. Suppression (0.65): High. The bans suppress alternatives for flank movements: direct electoral participation is eliminated, organizational infrastructure is criminalized, membership carries legal penalties. But suppression is not total (≤1.0) because: (1) successor organizations can form under alternative names; (2) ideological work continues in civil society; (3) international cooperation and diaspora organizations persist. Theater ratio (0.38): Low. The founding bans were substantive institutional actions with demonstrable organizational effects, not performative ritual. The court issued detailed judgments grounding the bans in specific organizational and ideological criteria. The low theater reflects the reading's claim that the founding shape was set by genuine doctrinal clarity, not by theatrical performance. Theater has risen slightly over the interval (0.28 → 0.38) as the doctrine has been applied to movements with weaker existential threat claims, suggesting scope creep. Claimed type: Tangled Rope. The constraint coordinates constitutional defense (beneficiary: center; victims: flanks; genuine coordination function) while extracting from flank movements (suppression ≥0.40, extraction ≥0.30, active enforcement required). The center benefits from the coordination mechanism (establishing itself as guardian of constitutional order) while bearing costs through mutual vulnerability.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the asymmetric extraction underlying the constraint despite its formal symmetry. The center's perspective (rope) emphasizes coordination: the bans are a neutral institutional mechanism for constitutional self-defense. The flanks' perspective (snare/constrained tangled rope) emphasizes extraction: the bans foreclose political participation and establish precedent for disciplining any movement deemed sufficiently hostile. The analytical observer's mountain perspective risks naturalizing this choice, claiming it is inherent to any constitutional order. The scaffold perspective (ecosystem view) occupies the middle ground: the bans are necessary but temporary, providing stability during democratic founding but becoming dispensable as institutions mature. The court's perspective (rope) experiences the bans as a tool strengthening judicial authority. This perspectival geometry shows that the founding_precedents reading is not value-neutral: it emphasizes the symmetry and boundedness of the founding doctrine while the chilling_critique reading emphasizes the asymmetric expansion of the ban power, and the potentiality_threshold reading emphasizes the refinement of the doctrine over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship of each agent to the ban instrument. Beneficiary (constitutional center): d ≈ 0.15 (institutional power + arbitrage exit → low d → low/negative effective extraction experienced by the center). The center sees the constraint as coordination because it controls the instrument and can deploy it symmetrically across flanks, giving it low extraction cost and high institutional benefit. Victims (flank movements): d ≈ 0.85–0.95 (powerless/moderate power + trapped/constrained exit → high d → high effective extraction). Flank movements face maximum extraction: organizational dissolution, electoral exclusion, membership penalties. No arbitrage option exists for trapped movements; constrained movements can attempt successor organization but under permanent threat. Enforcers (Constitutional Court): d ≈ 0.10 (institutional power + arbitrage → very low d → negative effective extraction). The court benefits from the instrument as a tool for establishing its authority; it experiences no extraction cost. The perspectival gap between beneficiary/enforcer (rope/low chi) and victims (snare/high chi) is the diagnostic signature of extraction directed toward the flanks.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by anchoring on the FOUNDING_PRECEDENTS reading's core claim: that the simultaneous banning of both extremist flanks (SRP right, KPD left) established a clear, symmetric founding doctrine proving the instrument's constitutional neutrality. The mandatrophy question is 'Is the party ban instrument a neutral constitutional defense mechanism or an instrument of political extraction?' The founding_precedents_reading answers 'Neutral constitutional defense, proven by symmetric early application.' The chilling_critique_reading answers 'Extractive instrument of political discipline, disguised by symmetric founding but revealed by expanding application.' The potentiality_threshold_reading answers 'Unclear initially, refined by NPD judgment to require both hostile aims AND organizational capacity.' These are not empirically resolvable as one truth — they are THREE DIFFERENT READINGS OF THE SAME CONSTITUTIONAL TEXT AND PRECEDENTS. The founding_precedents reading is the reading that emphasizes symmetry, clarity, and boundedness. It is instantiated as a clean constraint with specific omegas that would distinguish it empirically from its siblings if those data were collected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symmetry_v_asymmetry_in_founding,
    'Was the simultaneous banning of SRP (right) and KPD (left) genuinely symmetrical enforcement, or did institutional power differences mean the bans extracted asymmetrically despite equal formal structure?',
    'Comparative analysis of enforcement intensity: arrest rates, property seizure, successor organization tolerance, surveillance intensity, and judicial resource allocation to each banned movement. Historical reconstruction of political power distribution at the moment of each ban.',
    'If symmetric: the founding precedent establishes neutral doctrine that binds all flank movements equally. If asymmetric: the founding precedent masks extraction of one flank more than the other, and the ''symmetric'' doctrine provides cover for asymmetric suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_v_asymmetry_in_founding, empirical, 'Whether SRP and KPD bans were enforced symmetrically or asymmetrically').

omega_variable(
    existential_v_ideological_threat,
    'Did the ban doctrine rest on demonstrated existential threat to the constitutional order (proven organizational capacity for violent seizure of power), or on ideological incompatibility (rejection of liberal democracy as a matter of principle)?',
    'Analysis of the court judgments: explicit doctrine in BVerfGE decisions on SRP and KPD bans regarding the threshold for unconstitutional aims. Comparison of stated grounds (organizational capacity + hostile intent vs. ideology alone) across both cases.',
    'If existential threat: the founding precedent is grounded in genuine structural danger and establishes a defensible doctrine. If ideological: the doctrine is a cover for political exclusion, and the ''founding shape'' is the instrument of ideological suppression, not constitutional protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(existential_v_ideological_threat, conceptual, 'Whether the ban doctrine rested on existential threat or ideological exclusion').

omega_variable(
    founding_shape_v_chilling_power,
    'Did the SRP/KPD bans establish a clear, bounded doctrine for constitutional defense, or did their formulation create a template for expanding the ban instrument to movements with weaker claims to existential threat?',
    'Historical trajectory of ban doctrine: compare the explicit grounds in the SRP/KPD judgments to the grounds deployed in subsequent cases (NPD, DVU, etc.). Track the scope creep of ''unconstitutional aims'' and ''hostile intent'' criteria. Analyze whether successor bans required new institutional violence or merely ideological hostility.',
    'If founding shape is clear and bounded: the doctrine provides genuine protection against anti-system movements without arbitrary expansion. If chilling power expands: the founding precedents set a template that has enabled disciplining of increasingly moderate movements, contradicting the reading''s claim that the founding shape was set by proven threat on both flanks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_shape_v_chilling_power, empirical, 'Whether founding bans created bounded doctrine or expandable chilling template').

omega_variable(
    reading_contest_constitution,
    'This reading instantiates ONE interpretation of the contested party_ban_instrument kernel. What is the structural relationship between this founding_precedents_reading and its sibling readings?',
    'Meta-level analysis: the founding_precedents_reading claims that symmetric early bans (SRP/KPD) set a clear, bounded founding shape for the doctrine. The chilling_critique_reading claims the ban power expands to chill all political edges. The potentiality_threshold_reading claims the doctrine was refined to require both hostile aims AND organizational capacity. These are different instantiations of the same kernel (the text of Article 21, GG, and the court''s authority to apply it). The reading contest is about what the founding decision ESTABLISHED — a clear boundary, an expandable template, or a refined threshold.',
    'The three readings cannot all be true simultaneously in describing the founding shape. Either early bans were symmetrical (founding_precedents), or they established asymmetrical chilling (chilling_critique), or they left the threshold ambiguous pending later refinement (potentiality_threshold). The empirical and conceptual omegas above decompose this contest into resolvable questions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_constitution, conceptual, 'Structural contest between three readings of the party_ban_instrument kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_ban_instrument__founding_precedents_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pbfp_theater_founding_moment, party_ban_instrument__founding_precedents_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(pbfp_theater_post_bans, party_ban_instrument__founding_precedents_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(pbfp_theater_doctrine_settled, party_ban_instrument__founding_precedents_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(pbfp_extraction_founding_moment, party_ban_instrument__founding_precedents_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pbfp_extraction_post_srp_ban, party_ban_instrument__founding_precedents_reading, base_extractiveness, 2, 0.41).
narrative_ontology:measurement(pbfp_extraction_post_kpd_ban, party_ban_instrument__founding_precedents_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pbfp_extraction_doctrine_settled, party_ban_instrument__founding_precedents_reading, base_extractiveness, 10, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(pbfp_suppression_founding_moment, party_ban_instrument__founding_precedents_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(pbfp_suppression_post_bans, party_ban_instrument__founding_precedents_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(pbfp_suppression_doctrine_settled, party_ban_instrument__founding_precedents_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_ban_instrument__founding_precedents_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_ban_instrument__founding_precedents_reading, party_ban_instrument__chilling_critique_reading).
narrative_ontology:affects_constraint(party_ban_instrument__founding_precedents_reading, party_ban_instrument__potentiality_threshold_reading).
narrative_ontology:affects_constraint(party_ban_instrument__founding_precedents_reading, npd_banning_precedent__organizational_capacity).

% DUAL FORMULATION NOTE:
% The party_ban_instrument kernel is instantiated as three separate constraint stories, one per reading (founding_precedents, chilling_critique, potentiality_threshold). Each reading is a structurally distinct claim about what the founding SRP/KPD bans established and how the doctrine evolved. Constraint family linkage: founding_precedents_reading (this file) influences chilling_critique_reading and potentiality_threshold_reading — the founding bans are cited as evidence/precedent by all three readings, but interpreted differently. The npd_banning_precedent story represents downstream application of the founding doctrine to a movement (NPD) with weaker existential threat claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
