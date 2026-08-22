% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: religious/theological/social
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) stands at the intersection of three competing
 *   readings: as a binding metaphysical orthodoxy (strict_orthodox_reading),
 *   as a historically contingent confessional witness
 *   (symbolic_confessional_reading), and—the reading instantiated here—as a
 *   performative habituation mechanism that establishes and maintains
 *   Christian community identity through liturgical recitation independent of
 *   metaphysical assent. Under the liturgical_habituation_reading, the
 *   creed's authority is social and embodied rather than doctrinal and
 *   coercive. The constraint is the creed's persistent recitation in
 *   corporate worship as an identity boundary marker. This reading does not
 *   adjudicate whether the creed's metaphysical claims are true or binding;
 *   it observes that the creed's primary social function is performative—it
 *   coordinates communal identity through repetition and ritual
 *   participation. Extractiveness is very low (0.08) because the creed, under
 *   this reading, extracts nothing from participants; it offers them
 *   membership in a tradition. Suppression is minimal (0.15) because
 *   participation is self-perpetuating once habituated; dissenters must
 *   actively choose to stay—they are not externally forced out. Theater ratio
 *   is high (0.72) because the functional meaning of creedal recitation is
 *   increasingly its performative role in maintaining continuity rather than
 *   its doctrinal truth-content (the metaphysical readings do the doctrinal
 *   work). This reading feeds both the strict and symbolic readings by
 *   providing the embodied social substrate through which those doctrinal
 *   contests are enacted.
 *
 * KEY AGENTS:
 *   - liturgical_community — participants who experience the creed as identity boundary through repeated ritual performance (identity_locked, generational time horizon)
 *   - ordained_leadership — institutional agenda-setters who structure and maintain the liturgical performance (institutional power, mobile exit)
 *   - catechetical_educators — mediators between performative participation and explicit teaching (organized, constrained exit)
 *   - theological_interpreters — scholars working within the habituation framework to offer diverse metaphysical readings (organized, mobile exit)
 *   - dissident_believers — those whose private views diverge but who remain liturgically present; structurally excluded from defining creedal authority (powerless, trapped exit)
 *   - analytical_observer — structural analyst attending to the coordination mechanism independent of metaphysical adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "religious/theological/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '86ce2d62-e266-4ac5-8235-346eacf2f914').
narrative_ontology:cs_kernel_codification('86ce2d62-e266-4ac5-8235-346eacf2f914', fixed_text).
narrative_ontology:cs_authority_grounding('86ce2d62-e266-4ac5-8235-346eacf2f914', lineage).
narrative_ontology:cs_interpretation_layer_present('86ce2d62-e266-4ac5-8235-346eacf2f914').
narrative_ontology:cs_reading_relation('86ce2d62-e266-4ac5-8235-346eacf2f914', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('86ce2d62-e266-4ac5-8235-346eacf2f914', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('86ce2d62-e266-4ac5-8235-346eacf2f914', foundational, creedal_authority_performative_not_metaphysical).
narrative_ontology:cs_axiom_status(creedal_authority_performative_not_metaphysical, holdable).
narrative_ontology:cs_axiom_grounding('86ce2d62-e266-4ac5-8235-346eacf2f914', creedal_authority_performative_not_metaphysical, instrumental).
narrative_ontology:cs_axiom('86ce2d62-e266-4ac5-8235-346eacf2f914', foundational, participation_independent_of_private_assent).
narrative_ontology:cs_axiom_status(participation_independent_of_private_assent, holdable).
narrative_ontology:cs_axiom_grounding('86ce2d62-e266-4ac5-8235-346eacf2f914', participation_independent_of_private_assent, empirically_contingent).
narrative_ontology:cs_reference_frame('86ce2d62-e266-4ac5-8235-346eacf2f914', creedal_performance_as_identity_habituation).
narrative_ontology:cs_drift_state('86ce2d62-e266-4ac5-8235-346eacf2f914', contemporary_post_1950s_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86ce2d62-e266-4ac5-8235-346eacf2f914', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, catechetical_educators).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, theological_interpreters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members who participate in liturgical recitation of the Nicene Creed. They experience the creed as a performative marker of communal belonging—its authority derives from repeated embodied participation in a tradition rather than from logical metaphysical assent. The recitation's meaning is constituted through habit, rhythm, and collective presence. Their exit from the practice would mean departure from the worshipping community itself.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_community, beneficiary,
    organized, generational, identity_locked, global).

% Presides over and structures the liturgical performance. Maintains the creed as part of the prescribed order of service. Under this reading, their function is to facilitate the habituation process and sustain the communal boundary through performance—not to enforce doctrinal orthodoxy. They can reshape liturgical practice and interpretation within their authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ordained_leadership, agenda_setter,
    institutional, generational, mobile, global).

% Teach the creed's recitation and its history. Under the habituation reading, their role is to explain the creed as a boundary marker and historical witness rather than as a set of metaphysical truths requiring assent. They mediate between the performative liturgical function and explicit cognitive teaching.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, catechetical_educators, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, catechetical_educators, agenda_setter).

% Scholars and theologians who write about the creed's meaning. Under the habituation reading, they operate within a framework where the creed's authority is performative and socially constituted rather than metaphysically binding. They can offer diverse theological interpretations while maintaining creedal participation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theological_interpreters, beneficiary,
    organized, biographical, mobile, global).

% Those whose private metaphysical views diverge from creedal formulations but who continue liturgical participation out of communal commitment, family bonds, or cultural identity. They are structurally excluded from the conversation about creedal authority itself—they perform without full cognitive assent but are not present in defining what the creed means. The habituation reading permits their presence but does not name their situation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, dissident_believers, excluded,
    powerless, biographical, trapped, global).

% The abstract function of historical witness: the creed's recitation carries forward a 1,700-year continuity of Christian tradition. This is not an actor but a vindicated proposition under this reading: liturgical habituation sustains doctrinal memory and community identity across time.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, historical_continuity_witness, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(nicene_creed_authority__liturgical_habituation_reading, historical_continuity_witness).

% Takes a structural view of how the creed functions as a coordination mechanism independent of the truth-claims it makes. Attends to the embodied, performative, communal aspects without adjudicating the metaphysical questions the creed addresses.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, analytical_observer, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__liturgical_habituation_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__liturgical_habituation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a bounded worshipping community through shared ritualized performance. The creed's recitation creates a common identity marker—those who participate belong; those who do not are outside. This coordination function operates independently of whether participants privately assent to the creed's metaphysical content. The coordination problem it solves: how does a tradition maintain communal continuity and boundary while members hold diverse private beliefs?
% TRANSFER_FUNCTION: Transfers cultural capital, social belonging, and historical authority from the institutional church to participants through their embodied participation in the ritual performance. What flows is not extraction but recognition of membership. Those who recite the creed are recognized as part of the tradition; those who do not are marked as outside.
% ABSENT_VOICES: Dissident believers whose private metaphysical views diverge from creedal formulations but who participate liturgically are present in body but absent from the conversation about what creedal authority means. Their perspective—that the creed can be performed without full assent—is structurally what this reading claims, but they are not typically consulted in defining creedal authority. Also absent: historical voices of non-reciting Christians (Quakers, some branches of Protestantism) who argue the creed is not necessary for community identity.
% DISAPPEARANCE_RATIONALE: If the liturgical recitation of the Nicene Creed disappeared overnight, the Christian tradition would reorganize around alternative identity markers—perhaps other creeds, contemporary confessions, or purely catechetical teaching. The worshipping community would need a different boundary ritual. The tradition's continuity would not cease, but a significant coordinating mechanism would be lost, and the three-reading contest over creedal authority would lose its primary performative substrate.
% FOUNDING_PROBLEM: Early Christian communities needed a way to distinguish themselves from pagan society and from deviant interpretations of Christian faith. The Council of Nicaea produced a verbal formula—the creed—that could be recited as a test of orthodoxy and as a unifying statement of faith. The creed's original function was partly doctrinal (binding all believers to Nicene Christology) and partly performative (a shared statement recited in communal worship).
% FOUNDING_PROBLEM_CORROBORATION: Church historians attest that early creeds served both doctrinal and liturgical functions. Historians of Christian practice (e.g., Bradshaw on early Christian worship) document that the creed's liturgical role—recitation in corporate worship—is as ancient as its doctrinal role. However, the Orthodox and Catholic churches emphasize the creed's metaphysical binding force (strict_orthodox_reading corroboration), while Protestant denominations and modern biblical scholars question whether the creed's truth-claims bind all believers (symbolic_confessional_reading corroboration). No corroborating party from outside the benefiting tradition (the church itself) attests the founding problem—the question is entirely internal to Christian self-understanding. What is corroborated by external scholarship is the historical fact that the creed has functioned liturgically since its adoption.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness remains at 0.08 across the interval because the habituation reading locates creedal authority in social coordination, not in rent collection or coercive gate-keeping. The creed offers membership and historical continuity; it does not extract payment, labor, or cognitive submission. Suppression is lowest in the modern period (0.15 at 2026) because the habituation mechanism is now self-sustaining—participants choose participation; it is no longer enforced by ecclesial sanction or social pressure on the scale it was in medieval Christendom. Theater ratio rises from 0.55 to 0.72 because the performative function becomes increasingly salient relative to the doctrinal function: in the early church, the creed's metaphysical content was actively contested and enforced (Arian controversy, Nestorian controversy). In the modern period, most mainstream Christian communities accept the creed's basic metaphysical claims without contest, and the creed's primary social work is habituating community members into the tradition through repeated participation. The rising theater ratio captures this drift from contested doctrinal enforcement toward performative identity maintenance. Accessibility_collapse is moderate (0.42) because alternatives to creedal participation exist and are salient—non-creedal Christian traditions, secular culture, religious switching. The creed's boundary is maintained through habituation and social bonding, not through foreclosure of alternatives. Resistance is moderate (0.38) because dissident believers resist the implicit claim that private metaphysical assent is required, even as they continue participation; also because some Christian traditions explicitly reject creeds as unnecessary.
 *
 * PERSPECTIVAL GAP:
 *   The ordained leadership and the liturgical community should compute differently under this reading. For ordained leadership, the creed is an agenda-setting tool—they structure and maintain the performance, which requires institutional authority and mobility. From the liturgical community's perspective, the creed is experienced as an identity boundary into which they are habituated; their exit is identity_locked (leaving the ritual means leaving the community). The analytical observer sits outside both, noting the structural coordination that neither the leadership nor the participants need to explicitly acknowledge for the mechanism to work. The engine will compute per-seat directionality from these structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The liturgical_community is the structural beneficiary (d near 0.0): they receive membership, continuity, and identity from participation; there is no extraction from them, only integration. The ordained_leadership has moderate beneficiary directionality (d ~ 0.2–0.3): they benefit from having an established framework for community cohesion and self-definition, but they do not extract from participants. Their role is custodial, not extractive. The catechetical_educators have mixed directionality (d ~ 0.4): they both benefit from and contribute to the habituation process; they have some institutional authority but are constrained by tradition. Theological_interpreters have near-beneficiary directionality (d ~ 0.15): they benefit from the creed's continued recitation (it gives them something to interpret), but they contribute intellectual work and take on the risk of doctrinal controversy. The dissident_believers are structurally excluded from the directionality computation entirely under this reading because the reading does not name their situation as targets or beneficiaries—it structurally ignores the cost to those whose private metaphysical views diverge. This is itself a problem the reading leaves to omega variables.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: Has the founding problem (early Christian identity boundary and orthodoxy test) outlived its function? Under the strict_orthodox_reading, the founding problem is live—doctrinal precision still matters for Orthodox and Catholic Christianity. Under the symbolic_confessional_reading, the founding problem is dead—historical contingency has replaced metaphysical binding as the framing. Under the liturgical_habituation_reading, the founding problem is contested: the boundary function is still live (communities still need ways to mark and maintain identity), but the orthodoxy-test function has atrophied. The creed persists as a performative mechanism long after its doctrinal enforcement has loosened. This classification prevents mislabeling the creed as pure extraction (snare) by keeping the coordination function visible. It also prevents premature claim of obsolescence (world_unchanged) by showing that the habituation mechanism genuinely does work to maintain community identity—if the creed vanished, the community would need a replacement boundary ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_assent_vs_performative_participation,
    'How much private metaphysical assent to creedal content is required for a participant to count as a member of the creedal community? Does the habituation reading permit dissenters whose private views diverge from creedal formulations?',
    'Interview data from creed-reciting Christians whose explicit beliefs diverge from creedal content (e.g., those who affirm universalist eschatology while reciting language of eternal judgment, or who hold non-Nicene Christologies while reciting Nicene formulations). Historical study of post-reformation Protestant churches that retained creeds while permitting doctrinal diversity within the congregation.',
    'If assent is not required, the habituation reading is robust—the creed coordinates identity independent of metaphysical commitment. If assent is implicitly required, the reading collapses into the strict_orthodox_reading, and extractiveness (coercive cognitive demand) rises to 0.4+. This is the boundary question between rope (coordination) and tangled_rope (coordination + extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_assent_vs_performative_participation, empirical, 'Whether creedal participation requires metaphysical assent or is purely performative.').

omega_variable(
    dissident_believer_cost_invisibility,
    'What is the actual cost to dissident believers who participate in creedal recitation while privately dissenting? Is this cost structural (enforced by communal sanction) or internalized (the dissidents police their own silence)?',
    'Ethnographic study of post-recitation conversations in mainline Protestant churches where heterodox private beliefs are known. Historical documentation of sanction or silence directed at known dissenters. Psychological research on cognitive dissonance in religious ritual participation.',
    'If the cost is structural and non-trivial, extractiveness rises; the creed extracts conformity-performance from dissenters. If the cost is internalized and negotiable, the reading holds—the creed''s low extractiveness reflects that dissenters choose to absorb the cost themselves. The habituation reading as authored assumes the cost is internalized/minimal; if structural, the reading mischaracterizes the mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissident_believer_cost_invisibility, empirical, 'The cost structure for those whose private beliefs diverge from creedal formulations.').

omega_variable(
    strict_vs_habituation_authority_foreclosure,
    'Does the strict_orthodox_reading''s claim that the creed binds all believers to one metaphysical ontology logically foreclose the habituation_reading''s claim that creedal participation is independent of assent?',
    'Historical and contemporary documentation of how each reading community handles dissenters: if strict Orthodox treatment of heresy involves enforcement (anathema, excommunication), while habituation-reading communities permit continued participation despite divergence, the readings coexist structurally. If both traditions enforce metaphysical binding, they coexist by suppressing the question of dissidence.',
    'If the readings foreclose each other, the constraint family''s structure changes: one reading rules out the other. The current authoring assumes coexistence, which requires that enforcement differs between reading communities. This omega documents that assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_vs_habituation_authority_foreclosure, conceptual, 'Whether strict metaphysical binding logically forecloses habituation-independent participation.').

omega_variable(
    historical_contingency_of_habituation_mechanism,
    'Is the habituation mechanism (learning identity through repeated ritual) historically contingent on the creed''s specific formulation, or would any shared ritual formula serve the same coordination function?',
    'Comparative study of creedal and non-creedal Christian communities: do non-creedal traditions (Quakers, many evangelical churches) develop alternative habituation mechanisms (shared worship style, scriptural literacy, testimonial form)? Do shifts in creedal formulation (ecumenical prayer texts, contemporary liturgies that rewrite or supplement the creed) change the coordination function measurably?',
    'If the habituation function is independent of the creed''s specific content, extractiveness remains low regardless of what formula is recited—the creed is merely a vehicle for the mechanism. If the function is contingent on this specific formula (its 1,700-year authority, its historical continuity), then it has irreplaceable symbolic value, and what appears as low extractiveness might reflect the monopoly of a particular tradition''s performance of identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_contingency_of_habituation_mechanism, conceptual, 'Whether the habituation coordination is specific to the Nicene Creed or generalizable to other ritual formulas.').

omega_variable(
    rising_theater_ratio_interpretation,
    'Does the rising theater_ratio from 0.55 to 0.72 indicate that the creed''s functional meaning has shifted from doctrinal enforcement to performative maintenance, or does it indicate that the creed''s doctrinal claims have become less salient because fewer people take them seriously (which would be degradation toward piton status)?',
    'Time-series analysis of sermon and teaching content from mainstream Christian congregations (1970s, 2000, 2026): what proportion of teaching time is devoted to explaining the creed''s metaphysical content versus to teaching the creed''s liturgical role and historical significance? Do congregations with high theater_ratio still treat the creed as doctrinally binding, or have they drifted toward treating it as a cultural artifact?',
    'If the ratio indicates functional shift (from contested doctrine to performative identity), the reading holds and extractiveness stays low. If the ratio indicates degradation (the creed is recited but neither understood nor enforced doctrinally), the constraint moves toward piton status: it persists through theatrical repetition rather than active function. This changes the classified type from rope to piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rising_theater_ratio_interpretation, empirical, 'Whether high theater_ratio indicates functional shift or institutional degradation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 325, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 325, 0.55).
narrative_ontology:measurement(nice_tr_t600, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 600, 0.65).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1200, 0.68).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.7).
narrative_ontology:measurement(nice_tr_t2000, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2000, 0.71).
narrative_ontology:measurement(nice_tr_t2026, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2026, 0.72).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 325, 0.12).
narrative_ontology:measurement(nice_be_t600, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 600, 0.1).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1200, 0.08).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.07).
narrative_ontology:measurement(nice_be_t2000, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(nice_be_t2026, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2026, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement(nice_su_t600, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 600, 0.28).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1200, 0.22).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.18).
narrative_ontology:measurement(nice_su_t2000, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2000, 0.15).
narrative_ontology:measurement(nice_su_t2026, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2026, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.06).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The Nicene Creed authority constraint family decomposes into three structurally distinct claims with different ε values and beneficiary structures. The strict_orthodox_reading (high extractiveness, coercive metaphysical binding) and symbolic_confessional_reading (moderate extractiveness, historical contingency) operate at the level of truth-claim adjudication. The liturgical_habituation_reading (very low extractiveness, performative coordination) operates at the level of social mechanism. All three readings share the same kernel (the creed as a historical text), but they produce different constraints because they answer different questions: What does the creed obligate believers to believe? What authority does the creed carry? How does the creed function to maintain Christian community? The three readings coexist in contemporary Christianity because they are held by different stakeholder communities and address different explanatory levels. A single believer or community can hold all three simultaneously without internal contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
