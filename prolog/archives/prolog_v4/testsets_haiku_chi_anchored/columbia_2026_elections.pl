% ============================================================================
% CONSTRAINT STORY: columbia_2026_elections
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_columbia_2026_elections, []).

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
 *   constraint_id: columbia_2026_elections
 *   human_readable: 2026 Colombian Presidential Election Structure
 *   domain: political/electoral_systems
 *
 * SUMMARY:
 *   The 2026 Colombian presidential election operates within a constitutional
 *   framework that mandates executive transition through a one-term limit
 *   (2022-2026), creating a structural constraint that shapes candidate
 *   emergence, coalition formation, and democratic renewal capacity. The
 *   constraint exhibits genuine coordination (the mandatory transition
 *   preserves legitimacy through power rotation) alongside significant
 *   extraction mechanisms (establishment control of candidate selection,
 *   media gatekeeping, regional disempowerment). The one-term limit is often
 *   framed as a natural safeguard against authoritarianism, yet it functions
 *   equally as a mechanism through which incumbent coalitions select
 *   successors and outsider candidates face systematic barriers to
 *   competitive entry. The constraint's theater ratio (0.65) reflects that
 *   electoral forms (universal suffrage, secret ballot, multiparty
 *   competition) mask pre-electoral distortions (media concentration,
 *   campaign finance asymmetries, establishment coalition gatekeeping). For
 *   the political establishment, the election is legitimate coordination:
 *   mandatory succession prevents autocratic drift while preserving party
 *   continuity through institutional succession. For regional autonomy
 *   movements and outsider candidates, the same election is extraction: they
 *   participate in a framework whose rules they did not design and cannot
 *   exit, bearing costs of exclusion while validating a system that
 *   reproduces their marginalization. The analytical observer risks
 *   naturalizing this as an immutable constitutional law when it is actually
 *   a design choice specific to Colombia's post-1991 institutional
 *   settlement.
 *
 * KEY AGENTS:
 *   - Political Establishment Coalition: Primary beneficiary (institutional/arbitrage) — designs electoral architecture, controls successor candidate selection, benefits from mandatory transition without losing power concentration
 *   - Regional Autonomy Movements: Primary victim (moderate/constrained) — participate in election aggregating regional voice but face central authority that ignores regional mandates; cannot exit participation
 *   - Outsider Candidates: Secondary victim (powerless/trapped) — compete within rules designed to preserve incumbent advantage; limited funding, media gatekeeping, establishment coalition unity make competitive entry structurally difficult
 *   - Democratic Reform Coalition: Organized agent (organized/mobile) — civil society, transparency NGOs, youth movements building alternative institutional pathways (campaign finance reform, media access, redistricting); see election as temporary problem with sunset
 *   - Electoral Administration: Institutional actor (institutional/arbitrage) — maintains performative neutrality while operating within framework that pre-determines unequal competition; theater_ratio=0.65 reflects fairness rituals masking structural inequalities
 *   - Constitutional Framework: Analytical viewpoint (analytical/analytical) — one-term limit appears as immutable natural law but is contingent institutional design; risks naturalizing extraction as inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(columbia_2026_elections, 0.38).
domain_priors:suppression_score(columbia_2026_elections, 0.52).
domain_priors:theater_ratio(columbia_2026_elections, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(columbia_2026_elections, extractiveness, 0.38).
narrative_ontology:constraint_metric(columbia_2026_elections, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(columbia_2026_elections, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(columbia_2026_elections, tangled_rope).
narrative_ontology:human_readable(columbia_2026_elections, "2026 Colombian Presidential Election Structure").
narrative_ontology:topic_domain(columbia_2026_elections, "political/electoral_systems").

domain_priors:requires_active_enforcement(columbia_2026_elections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(columbia_2026_elections, political_establishment).
narrative_ontology:constraint_beneficiary(columbia_2026_elections, centrist_coalition).
narrative_ontology:constraint_victim(columbia_2026_elections, outsider_candidates).
narrative_ontology:constraint_victim(columbia_2026_elections, regional_autonomy).
narrative_ontology:constraint_victim(columbia_2026_elections, democratic_renewal).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL OUTSIDER CANDIDATE (SNARE) — Faces structural barriers to competitive entry: limited funding access, media gatekeeping, establishment coalition dominance. Must compete within rules designed to preserve incumbent party advantage. No viable exit from participation; inability to exit produces high extraction cost. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(columbia_2026_elections, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL AUTONOMY MOVEMENT (TANGLED ROPE) — Benefits from electoral participation as coordination mechanism (aggregates regional voice) but bears extraction through centralized authority structures that ignore regional mandates. Constrained exit — regions cannot secede but can reduce participation. d≈0.72, f(d)≈1.12, σ=0.9 → χ≈0.40.
constraint_indexing:constraint_classification(columbia_2026_elections, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: POLITICAL ESTABLISHMENT COALITION (ROPE) — Experiences constitutional one-term limit as coordination mechanism that preserves legitimacy through mandatory transition while maintaining party continuity. Arbitrage exit available — can field successor candidates aligned with establishment interests. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(columbia_2026_elections, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DEMOCRATIC REFORM COALITION (SCAFFOLD) — Organized actors (civil society, transparency NGOs, youth movements) see electoral architecture as temporary institutional problem with a sunset: campaign finance reform, media access rules, and redistricting could reduce extraction mechanisms. Currently constrained by establishment resistance, but structural pathways to reform exist (constitutional amendment). d≈0.48, f(d)≈0.58, σ=1.0 → χ≈0.22. Low effective extraction because coalition has institutional agency.
constraint_indexing:constraint_classification(columbia_2026_elections, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL ADMINISTRATION APPARATUS (PITON) — Constitutional framework for elections is largely performative in enforcing fairness: campaign finance rules lack enforcement mechanisms, media regulation is absent, ballot access barriers persist despite legal neutrality. Electoral authority maintains the ritual of neutral administration while knowing structural inequalities persist. theater_ratio=0.65 reflects performative compliance (counting votes fairly while ignoring pre-election distortions). Institutional inertia sustains the apparatus despite degraded fairness function. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(columbia_2026_elections, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL REALIST / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the one-term limit is an immutable constitutional law of Colombian politics — no incumbent can circumvent it. This perspective risks naturalizing what is actually a contingent institutional design (single-term limits exist in some democracies but not others; Colombia's is a political choice, not a law of nature). The structural data (ε=0.38, suppression=0.52, theater=0.65) reveals this as a false summit.
constraint_indexing:constraint_classification(columbia_2026_elections, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(columbia_2026_elections_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(columbia_2026_elections, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(columbia_2026_elections, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(columbia_2026_elections, TR),
    TR >= 0.70.

:- end_tests(columbia_2026_elections_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The establishment benefits from electoral participation that confers legitimacy while maintaining power concentration through successor selection. The extraction is real but constrained — regional voters still aggregate voice, outsiders can still compete despite barriers, and succession does prevent indefinite incumbent control. The reduced value (vs earlier 0.52 assessment) reflects that the constraint combines genuine coordination (mandatory transition) with extraction mechanisms (gatekeeping). Suppression (0.52): Moderate-high. Significant barriers to outsider entry include: limited campaign finance access (media advertising costs concentrate among well-funded candidates), media concentration in establishment-aligned outlets, establishment coalition unity that fragments outsider opposition, and regional/provincial political structures biased toward established parties. But suppression is not total — outsiders have run competitive campaigns (2022 elected Petro, an outsider), and social movements can mobilize. Theater ratio (0.65): Moderate-high. Electoral processes are substantially performative: ballot is cast fairly, votes are counted accurately (technical neutrality), but pre-electoral distortions (media access, campaign finance, coalition gatekeeping) determine viable candidate set before voting begins. The theater has increased as establishment strategies focus on pre-electoral advantage rather than post-electoral legitimacy contestation. Theater progression from 0.48→0.65 reflects rising performance burden as establishments invest in facade maintenance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates major perspectival divergence around a single electoral event. The political establishment sees legitimate coordination (mandatory succession prevents authoritarianism while preserving institutional continuity) and experiences the election as rope — they participate in and benefit from a mechanism that ensures power rotation within their coalition. Regional movements and outsiders see the same election as snare and tangled rope — they are trapped in a framework they did not design, compete under unequal conditions, and bear costs of exclusion while the process claims legitimacy from their participation. The democratic reform coalition sees a temporary problem with a sunset — electoral architecture can be reformed (campaign finance, media access, redistricting) within the constitutional framework, and structural pathways to reform exist despite establishment resistance. The electoral administration sees its own ritual as degraded (piton) — procedures are followed but the fairness they promise is undermined by pre-electoral distortions that administers cannot and will not address. The constitutional realist risks seeing an immutable natural law (mountain) — the one-term limit appears as an untouchable safeguard — but this naturalizes a design choice specific to Colombia's 1991 constitutional settlement, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Political establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can arbitrage by fielding successor candidates aligned with establishment interests. Outsider candidates: Victim + trapped → d≈0.90, f(d)≈1.38. Maximum extraction. Cannot exit participation without surrendering political viability; compete within rules designed to preserve incumbent advantage. Regional movements: Victim + constrained → d≈0.72, f(d)≈1.12. Significant extraction. Participate to aggregate regional voice (coordination function) but face central authority that ignores regional mandates; can reduce participation (abstention) but cannot exit meaningfully. Democratic reform coalition: Organized + mobile → d≈0.48, f(d)≈0.58. Moderate effective extraction. Have agency through institutional pathways (constitutional amendment, regulation), retain mobility through alternative organizing (parallel civic engagement). Electoral administration: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification comes from theater gate (≥0.70 not met, but 0.65 approaches it), not from high chi. Can arbitrage by maintaining appearance of fairness while operating within pre-designed framework. Constitutional framework: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (analyst naturalizes constraint); the engine's false summit detector catches this based on structural data (ε=0.38, suppression=0.52, theater=0.65 indicating contingency, not physical law).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_inclusivity_threshold,
    'What level of establishment coalition inclusivity constitutes genuine democratic participation vs tokenistic incorporation of outsiders?',
    'Analysis of coalition composition pre- and post-election; tracking of outsider candidate platform incorporation into winning candidate''s agenda; post-election policy alignment with outsider constituencies',
    'If threshold low (token inclusion acceptable): extraction is moderate, scaffold sunset more plausible. If threshold high (substantive incorporation required): extraction is higher, reform coalition must achieve deeper structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_inclusivity_threshold, conceptual, 'Threshold for genuine vs tokenistic coalition inclusivity').

omega_variable(
    media_gatekeeping_mechanism,
    'Is media coverage concentration in Colombia a structural constraint on ballot access or a contingent policy outcome that can be reformed independently of electoral system?',
    'Comparative analysis of media ownership concentration vs ballot access in countries with similar constitutional frameworks; measurement of coverage disparity between establishment and outsider candidates; assessment of feasibility and political will for media regulation reform',
    'If structural (independent of reform): extraction persists regardless of electoral changes. If contingent: campaign finance and media transparency reforms could reduce extraction, strengthening scaffold perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(media_gatekeeping_mechanism, empirical, 'Whether media gatekeeping is structural or policy-contingent').

omega_variable(
    regional_franchise_enforcement,
    'Can regions enforce meaningful policy mandates on a centralized executive, or is regional electoral participation purely performative?',
    'Longitudinal tracking of elected executive compliance with regional electoral mandates; measurement of regional authority over budget allocation and service delivery; comparison of resource distribution to regional electoral support',
    'If regions can enforce: tangled rope classification confirmed, regional coordination function is real. If purely performative: snare perspective grows stronger, regional participation is extraction by central authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_franchise_enforcement, empirical, 'Whether regions can enforce policy mandates on centralized executive').

omega_variable(
    successor_candidate_independence,
    'Do establishment-backed successor candidates represent genuine party ideology or are they instruments of continuity that extract legitimacy from electoral process while maintaining power concentration?',
    'Comparison of successor candidate platform to incumbent administration policies; measurement of policy continuity vs ideological shift; tracking of successor accountability to party base vs establishment faction interests; post-election analysis of rival within party',
    'If independent: rope classification valid, electoral mechanism genuinely coordinates succession. If instrumental: establishment extraction mechanism is higher, rope perspective is beneficiary''s self-serving narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_candidate_independence, conceptual, 'Whether successor candidates are independent or establishment instruments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(columbia_2026_elections, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(col26_tr_t0, columbia_2026_elections, theater_ratio, 0, 0.48).
narrative_ontology:measurement(col26_tr_t12, columbia_2026_elections, theater_ratio, 12, 0.58).
narrative_ontology:measurement(col26_tr_t24, columbia_2026_elections, theater_ratio, 24, 0.65).

% Extraction over time
narrative_ontology:measurement(col26_be_t0, columbia_2026_elections, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(col26_be_t12, columbia_2026_elections, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(col26_be_t24, columbia_2026_elections, base_extractiveness, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(columbia_2026_elections, enforcement_mechanism).
narrative_ontology:affects_constraint(columbia_2026_elections, colombian_campaign_finance_system).
narrative_ontology:affects_constraint(columbia_2026_elections, regional_autonomy_framework).
narrative_ontology:affects_constraint(columbia_2026_elections, media_concentration_colombia).

% DUAL FORMULATION NOTE:
% The 2026 election is a downstream constraint in the Colombian institutional family. Upstream constraints (campaign finance inequality, media concentration, regional disempowerment) have their own ε values reflecting the severity of structural barriers; the election constraint (ε=0.38) captures the synthesis of these barriers as they manifest in candidate competition and voter choice within a single electoral cycle. Decomposition: campaign_finance_inequality (ε≈0.45, snare for outsiders) + media_concentration (ε≈0.42, snare for regional voice) + regional_disempowerment (ε≈0.36, tangled rope) converge to produce the election constraint (ε=0.38, tangled rope). The election is not separable from these upstream constraints; it is their structural manifestation at the political decision point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(columbia_2026_elections, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
