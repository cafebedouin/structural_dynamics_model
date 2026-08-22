% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Pathway: Participatory Rebuilding of Communal Life under Divine Blessing
 *   domain: religious/political_theology/social
 *
 * SUMMARY:
 *   This story instantiates the jerusalem_reading of the
 *   human_transcendence_pathway kernel: authentic community rebuilt through
 *   patient, participatory labor under divine blessing, integrating plurality
 *   into communion rather than uniformity. The arrangement organizes communal
 *   life around shared worship, rotating common labor, mutual aid, and
 *   deliberative discernment, with deliberately weighted flows toward
 *   returning exiles and the marginalized. Constraint-family decomposition:
 *   the colloquial label 'the pathway to human transcendence and community'
 *   covers three structurally distinct claims with different extraction
 *   profiles — babel_reading (consolidated uniform system, high extraction
 *   from the absorbed), this reading (plural communion, low extraction), and
 *   technocratic_vs_incarnational_reading (limit-elimination versus
 *   grace-received-in-vulnerability). The babel and technocratic claims are
 *   frequently cited as evidence against this reading's patience (speed and
 *   scale achievements); this reading's durability findings feed back as
 *   critique. The epsilon referent here is the standing arrangement the story
 *   describes — the Jerusalem-pattern community as it actually operates,
 *   assessed by this reading's own lights — never the babel or technocratic
 *   alternatives. Claim and metrics are independent: the claimed type is what
 *   the structure appears to be; the metrics describe low-but-nonzero
 *   extraction (formation demands, institutional aging) and are not tuned to
 *   any predicted engine output.
 *
 * KEY AGENTS:
 *   - community_elders_and_formators: agenda-setter (organized/identity_locked) — administer formation and common labor; standing rises and falls with community health
 *   - participating_households: primary beneficiary (organized/constrained) — contribute labor and resources, receive belonging and mutual aid
 *   - returning_exiles_and_marginalized: protected beneficiary (moderate/constrained) — deliberate reintegration flows run toward them
 *   - efficiency_preferring_members: cost-bearing beneficiary (moderate/mobile) — bear the opportunity cost of patience; exit is open
 *   - surrounding_technocratic_society: excluded critic (institutional/arbitrage) — ambient rival arrangement outside the deliberation
 *   - catholic_social_teaching_magisterium: analytical observer (institutional/analytical) — evaluates fidelity to the transmitted doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.2).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Pathway: Participatory Rebuilding of Communal Life under Divine Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political_theology/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '257a908f-2eb3-4b69-847d-0160aa5bd6e0').
narrative_ontology:cs_kernel_codification('257a908f-2eb3-4b69-847d-0160aa5bd6e0', fixed_text).
narrative_ontology:cs_authority_grounding('257a908f-2eb3-4b69-847d-0160aa5bd6e0', lineage).
narrative_ontology:cs_interpretation_layer_present('257a908f-2eb3-4b69-847d-0160aa5bd6e0').
narrative_ontology:cs_reading_relation('257a908f-2eb3-4b69-847d-0160aa5bd6e0', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('257a908f-2eb3-4b69-847d-0160aa5bd6e0', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('257a908f-2eb3-4b69-847d-0160aa5bd6e0', foundational, transcendence_received_not_engineered).
narrative_ontology:cs_axiom_status(transcendence_received_not_engineered, holdable).
narrative_ontology:cs_axiom_grounding('257a908f-2eb3-4b69-847d-0160aa5bd6e0', transcendence_received_not_engineered, theological).
narrative_ontology:cs_axiom('257a908f-2eb3-4b69-847d-0160aa5bd6e0', foundational, plurality_constitutive_of_communion).
narrative_ontology:cs_axiom_status(plurality_constitutive_of_communion, holdable).
narrative_ontology:cs_axiom_grounding('257a908f-2eb3-4b69-847d-0160aa5bd6e0', plurality_constitutive_of_communion, deontological).
narrative_ontology:cs_axiom('257a908f-2eb3-4b69-847d-0160aa5bd6e0', secondary, patient_process_over_imposed_blueprint).
narrative_ontology:cs_axiom_status(patient_process_over_imposed_blueprint, holdable).
narrative_ontology:cs_axiom_grounding('257a908f-2eb3-4b69-847d-0160aa5bd6e0', patient_process_over_imposed_blueprint, instrumental).
narrative_ontology:cs_reference_frame('257a908f-2eb3-4b69-847d-0160aa5bd6e0', patient_participatory_communion_under_divine_blessing).
narrative_ontology:cs_drift_state('257a908f-2eb3-4b69-847d-0160aa5bd6e0', contemporary_technocratic_paradigm, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('257a908f-2eb3-4b69-847d-0160aa5bd6e0', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, participating_households).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles_and_marginalized).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, efficiency_preferring_members).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, efficiency_preferring_members).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, grace_builds_on_nature).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, plurality_integrated_into_communion).
narrative_ontology:constraint_vindicates(human_transcendence_pathway__jerusalem_reading, preferential_option_for_the_marginalized).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach, organize the rotating common labor, preside over worship, and facilitate the discernment circles through which decisions are made. They draw no salary premium and hold standing only insofar as the community flourishes; their vocation and personal identity are formed inside the community they serve, and stepping away would mean relinquishing the role that constitutes their life's work.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, community_elders_and_formators, agenda_setter,
    organized, generational, identity_locked, regional).

% Contribute a share of income, labor days, and presence to the common life; receive mutual aid in illness and hardship, a dense web of belonging, and a voice in communal discernment. Leaving is possible and occasionally happens, typically for work or marriage, but means surrendering the relationships and support structure that daily life runs on.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, participating_households, beneficiary,
    organized, biographical, constrained, local).

% Returned migrants, refugees, and others previously on the margins whom the community deliberately folds into its common life: first call on aid funds, sponsored apprenticeships in the common labor, and reserved seats in discernment circles. Their participation is treated as constitutive rather than decorative; the community counts their presence as the measure of its own health. Exit is possible but would return them to the isolation they came from.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles_and_marginalized, beneficiary,
    moderate, biographical, constrained, regional).

% Members who chafe at consensus cycles, multi-year rebuilding timelines, and the refusal to adopt faster managerial tools. They receive the same belonging and aid as everyone else and give the same contributions, but experience the pace as a running opportunity cost. Nothing binds them: they are free to join or found faster arrangements nearby, and some do, which the community treats as a cost of its own choices rather than a betrayal.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, efficiency_preferring_members, payer,
    moderate, immediate, mobile, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, efficiency_preferring_members, beneficiary).

% The ambient technological-managerial culture surrounding the community — employers, platforms, planning regimes — which regards patient participatory rebuilding as sentimental inefficiency and would redesign the community's life around optimization if consulted. It stands outside the community's deliberations and encounters the arrangement only as friction at the boundary.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, surrounding_technocratic_society, excluded,
    institutional, civilizational, arbitrage, global).

% Teachers of the social tradition who evaluate whether communities of this pattern embody the doctrine they transmit — watching for paternalism disguised as care, uniformity disguised as unity, and stagnation disguised as patience. They authorize no budgets and enforce no rules here; their seat is evaluative.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, catholic_social_teaching_magisterium, observer,
    institutional, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(human_transcendence_pathway__jerusalem_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rebuilds durable common life among dispersed and diverse persons: shared worship synchronizes time, rotating common labor produces shared goods, mutual aid pools risk, and deliberative discernment converts disagreement into decisions without silencing minority positions.
% TRANSFER_FUNCTION: Circulates labor, money, and attention from every member into common funds of care and rebuilding, and recognition and belonging back to every member — with deliberately weighted flows toward returning exiles and the marginalized. Nothing accumulates at a controlling seat; the circulation is the point.
% ABSENT_VOICES: The surrounding technocratic-managerial culture would object that the timeline wastes human potential and that scale problems demand centralized tools; departed members would testify to the real costs of slow consensus from outside the affection that made it bearable. Neither is seated in the community's discernment.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strand the aid networks, halt the rebuilding projects mid-wall, and leave the reintegrated exiles without their channel back into common life; households would drift into private provision and the surrounding managerial culture would absorb the young within a generation. The arrangement is maintained, not natural — its absence rearranges everything it touches.
% FOUNDING_PROBLEM: How a scattered, demoralized people rebuilds authentic common life after displacement — without the imperial shortcut of forced uniformity and without dissolving into isolated households — the problem posed in the exile-and-return narratives the reading takes as its pattern.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: communitarian sociology and social-capital research attest that participatory, slow institution-building outperforms imposed blueprints for durability; refugee-reintegration practitioners report the same asymmetry in resettlement work; and political theologians in traditions outside this one independently describe the uniformity-versus-communion dilemma in their own terms. No seat inside the community is the source of the attestation.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.20 at interval end) because flows run toward the least powerful and no seat captures disproportionate gain; the residual reflects real formation demands — time, tithe, deference to communal discernment — and the mild hardening institutions show with age. Suppression is low (0.15): the arrangement operates by persuasion, catechesis, and social expectation, with no barrier to departure; the scalar is static because the story does not trace enforcement-capacity change. Theater is low (0.12): worship and ritual are the coordinating acts themselves, not performance substituting for function, though routines formalize slightly late in the interval. Accessibility collapse is low (0.25): the babel and technocratic alternatives remain live, legible options the pathway claims to surpass but does not foreclose. Resistance is moderate (0.45): internal impatience, external technocratic critique, and periodic departures. The measurement series share one time grid; both tracked metrics show a mild U-shape — founding intensity, mature equilibrium, late institutional hardening — a lifecycle curve, not an oscillatory extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the elders' position the arrangement is a vocation fulfilled — identity-locked, but locked into a beneficial relationship. From the efficiency-preferring members' position the same structure is a running tax on speed they are free to refuse by walking out, which keeps their experienced burden real but bounded. From the exiles' position it is a lifeline: the flows most deliberately favor them. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared structural relationships run toward the beneficiary end: two beneficiary groups are declared and no victims, because under this reading's structure no group is targeted for extraction — the efficiency cost is universal, chosen, and borne by every seat including the elders. Exit modulation differentiates within the beneficiary side: mobile exit keeps the efficiency-preferring members nearest the beneficiary pole despite their payer role; constrained exit raises household and exile directionality slightly; the elders' identity lock amplifies whichever relationship exists, and here the relationship is beneficial. The exile-weighted transfers invert the usual extraction direction — movement runs toward the least powerful seat. No directionality overrides are needed: the derivation from beneficiary declarations plus exit options already lands each seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconstructing common life after displacement without forced uniformity or atomistic dissolution — remains live: displacement, migration, and institutional atomization persist, so there is no dead mandate animating a husk. The classification discipline cuts both ways here. The efficiency sacrifice could be misread as asymmetric extraction (yielding a hybrid coordination/extraction verdict) if its burden distribution skews; the efficiency_burden_distribution omega tracks exactly that possibility. Conversely, the low metrics must not be mistaken for natural-law status: the arrangement is cultivated, decays without labor, and would leave a rearranged world if it vanished — it is a maintained human construction, not a discovered one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the human_transcendence_pathway kernel (jerusalem_reading). What would change structurally if a sibling reading were instantiated instead?',
    'Compare against the sibling stories: babel_reading converts the community''s plurality into a consolidated uniform system (members become instruments; the slow and the plural become targets); technocratic_vs_incarnational_reading splits on whether limits are eliminated by technique or received as the site of grace.',
    'Under babel, today''s beneficiaries become raw material and extraction rises sharply; under the technocratic half of the third reading, formation gives way to optimization and the exile-preferential flows dissolve; under the incarnational half, structure converges toward this reading''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, where the sibling disagreement is located.').

omega_variable(
    voluntariness_vs_internalized_formation,
    'Is the low measured suppression genuine voluntariness, or formation so thorough that members cannot conceive of exit?',
    'Track departure trajectories and post-departure testimony across a cohort: if leavers report identity rupture and relational collapse comparable to coercive-settings literature, the internalized share is large; if they relocate with intact selves and continued goodwill, the formation is voluntary.',
    'If largely internalized, lived suppression exceeds the structural measure and member-seat classifications drift toward harder types; if voluntary, current metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_vs_internalized_formation, empirical, 'Structural versus internalized component of formation pressure.').

omega_variable(
    efficiency_burden_distribution,
    'Is the efficiency sacrificed for solidarity distributed evenly across strata, or do senior members retain speed for themselves while delegating slowness downward?',
    'Audit decision latency and burden allocation by stratum: compare how long elders'' own initiatives take versus rank-and-file requests, and who performs the deferred maintenance labor.',
    'Skewed burden would introduce asymmetric extraction through the same structure and push classification toward a hybrid coordination/extraction profile; even burden sustains the low-extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_burden_distribution, empirical, 'Whether the solidarity cost is symmetric or quietly shifted down.').

omega_variable(
    blessing_attribution_ambiguity,
    'Are the community''s durability and cohesion effects attributable to divine blessing as the reading claims, or fully to the participatory process itself?',
    'Comparative study of structurally identical participatory communities lacking the transcendent-reference layer: if outcomes match, the process suffices and the blessing claim is interpretive; if they diverge, the reading''s distinctive claim carries causal weight.',
    'If process alone explains outcomes, the arrangement functions identically but vindicates a narrower proposition set; classification barely moves, while the reading''s self-understanding and its polemic against the siblings weaken.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(blessing_attribution_ambiguity, conceptual, 'Whether the theological attribution is load-bearing for the arrangement''s effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(huma_tr_t6, human_transcendence_pathway__jerusalem_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(huma_tr_t12, human_transcendence_pathway__jerusalem_reading, theater_ratio, 12, 0.06).
narrative_ontology:measurement(huma_tr_t18, human_transcendence_pathway__jerusalem_reading, theater_ratio, 18, 0.07).
narrative_ontology:measurement(huma_tr_t24, human_transcendence_pathway__jerusalem_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__jerusalem_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(huma_be_t6, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 6, 0.14).
narrative_ontology:measurement(huma_be_t12, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(huma_be_t18, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(huma_be_t24, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 24, 0.18).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(human_transcendence_pathway__jerusalem_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the single colloquial label 'pathway to human transcendence/community' decomposes into three structurally distinct constraints per the epsilon-invariance principle. babel_reading (uniform consolidation; high extraction from absorbed members), human_transcendence_pathway__jerusalem_reading (this file; plural communion; low extraction), and technocratic_vs_incarnational_reading (limit-elimination versus grace-received-in-vulnerability). Upstream/downstream: babel and technocratic achievements are cited as evidence against this reading's patience, while this reading's durability data feed back as critique of both siblings. Each story carries its own epsilon, beneficiaries, and failure modes; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
