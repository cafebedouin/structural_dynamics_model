% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Divine Mandate Reading of Ma'at: Royal Embodiment Order
 *   domain: political/theological (ancient Egypt)
 *
 * SUMMARY:
 *   In the divine-mandate instantiation, Ma'at — the ordering principle —
 *   flows from the created cosmos through the person of the king alone into
 *   society; the king embodies it and cannot act against it by definition.
 *   The arrangement organizes the Nile valley's political economy: grain
 *   assessments, corvée rotation, quarrying and mining expeditions, and
 *   peripheral tribute all move under the justification that maintaining the
 *   king maintains the world. Because royal action is definitionally
 *   conforming, no Ma'at-based claim can be brought against the throne;
 *   accountability language survives only in windows of central weakness.
 *   This file instantiates ONE reading of the maat_order_principle kernel.
 *   The reciprocity reading (the king owes justice, stability, and
 *   distribution and can fail) and the distributed maintenance reading (all
 *   actors sustain order in their station) are separate constraint stories
 *   with different victim sets and different epsilon values; they are linked
 *   through network.affects_constraints, not folded into this file. The
 *   epsilon referent here is the standing divine-mandate arrangement as it
 *   operated, assessed by this reading's own lights — never the
 *   accountability-bearing arrangement the reciprocity sibling would
 *   institute. KEY AGENTS (by structural relationship):
 *   pharaoh_royal_household — source-seat administrator
 *   (institutional/identity_locked), positioned outside the arrangement it
 *   runs; temple_priesthood — enforcing beneficiary
 *   (organized/identity_locked); scribal_administrative_class — administering
 *   beneficiary (moderate/constrained); peasant_laborers — primary target
 *   (powerless/trapped); corvee_conscripted_workers — primary target
 *   (powerless/trapped); peripheral_tribute_peoples — excluded remitters
 *   (powerless/trapped); modern_egyptologists — analytical observer
 *   (analytical/analytical).
 *
 * KEY AGENTS:
 *   - pharaoh_royal_household: agenda-setting beneficiary (institutional / identity_locked) — occupies the source seat; royal action defines conformity, so the arrangement cannot bind its own administrator
 *   - temple_priesthood: beneficiary (organized / identity_locked) — performs ritual maintenance, interprets oracles, holds estate income; supplies the enforcement backbone when royal power weakens
 *   - scribal_administrative_class: beneficiary (moderate / constrained) — runs census, assessment, and conscription; exempt from the burdens it allocates
 *   - peasant_laborers: primary target (powerless / trapped) — deliver grain assessment and corvée bodies under cosmic-necessity justification
 *   - corvee_conscripted_workers: primary target (powerless / trapped) — quarry, mine, haul, and build under expedition conditions
 *   - peripheral_tribute_peoples: excluded remitters (powerless / trapped) — conquered populations paying tribute with no standing in the order's discourse
 *   - modern_egyptologists: analytical observer (analytical / analytical) — reads the full structure from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.72).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.8).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Divine Mandate Reading of Ma'at: Royal Embodiment Order").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "political/theological (ancient Egypt)").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '7ef8083e-8590-4b0d-a70b-e247437edd35').
narrative_ontology:cs_kernel_codification('7ef8083e-8590-4b0d-a70b-e247437edd35', formalized).
narrative_ontology:cs_authority_grounding('7ef8083e-8590-4b0d-a70b-e247437edd35', lineage).
narrative_ontology:cs_interpretation_layer_present('7ef8083e-8590-4b0d-a70b-e247437edd35').
narrative_ontology:cs_reading_relation('7ef8083e-8590-4b0d-a70b-e247437edd35', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('7ef8083e-8590-4b0d-a70b-e247437edd35', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('7ef8083e-8590-4b0d-a70b-e247437edd35', foundational, royal_action_definitionally_maat_conforming).
narrative_ontology:cs_axiom_status(royal_action_definitionally_maat_conforming, holdable).
narrative_ontology:cs_axiom_grounding('7ef8083e-8590-4b0d-a70b-e247437edd35', royal_action_definitionally_maat_conforming, theological).
narrative_ontology:cs_axiom('7ef8083e-8590-4b0d-a70b-e247437edd35', secondary, contest_of_royal_mandate_constitutes_isfet).
narrative_ontology:cs_axiom_status(contest_of_royal_mandate_constitutes_isfet, holdable).
narrative_ontology:cs_axiom_grounding('7ef8083e-8590-4b0d-a70b-e247437edd35', contest_of_royal_mandate_constitutes_isfet, theological).
narrative_ontology:cs_reference_frame('7ef8083e-8590-4b0d-a70b-e247437edd35', royal_embodiment_of_cosmic_order).
narrative_ontology:cs_drift_state('7ef8083e-8590-4b0d-a70b-e247437edd35', post_first_intermediate_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7ef8083e-8590-4b0d-a70b-e247437edd35', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh_royal_household).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, temple_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_administrative_class).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, peasant_laborers).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, corvee_conscripted_workers).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, divine_kingship_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, maat_cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(maat_order_principle__divine_mandate_reading, royal_infallibility_presumption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the throne as son of the creator sun-god and sole channel through which cosmic order reaches the Two Lands. Sets tax rates, summons corvée, commissions quarries, mines, and tombs, and receives grain, labor, and tribute at the palace before redistributing shares to temples and officials. The office's identity is fused with the order it administers: coronation installs the king as embodiment of Ma'at, abdication or deposition is ritually indistinguishable from death, and nothing the occupant does can count as a breach of the order by definition. Leaving the role would mean ceasing to be what the role is.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh_royal_household, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaoh_royal_household, beneficiary).

% Staffs the cults that perform the daily ritual maintenance of cosmic order, interprets oracles, keeps the calendars and festival cycle, and administers temple estates that rank among the largest landholdings in the valley. Priestly income, exemption from labor dues, and hereditary office all depend on the theology in which the king's correct ritual conduct sustains the world. Priestly identity is constituted by the cult; abandoning it forfeits office, estate income, and burial provision. When royal authority weakens, temple hierarchies are the surviving enforcement backbone of the order's claims.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, temple_priesthood, beneficiary,
    organized, generational, identity_locked, national).

% Runs the census, grain assessment, labor conscription rolls, and expedition logistics that convert the theology into collections. Literacy is monopolized within this class, and scribal instruction literature explicitly contrasts the scribe's protected life with the plowman's and laborer's. Members are exempt from the burdens they allocate and advance by fidelity to the assessment system. Exit would mean surrendering the only route out of manual labor the society offers.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_administrative_class, beneficiary,
    moderate, biographical, constrained, national).

% Work the flood-fed fields, deliver the grain assessment after harvest, and supply the bodies from which corvée gangs are drawn. The assessment is justified as the price of the king maintaining the inundation, the sun's course, and the defeat of chaos. There is no tribunal in which the assessment can be contested as excessive, since the king's conduct is by definition correct; complaint exists only as literary convention voiced by others. Flight beyond the valley's administration is possible at the margins but means loss of village, kin, and burial, and the desert offers nothing to flee to.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, peasant_laborers, payer,
    powerless, biographical, trapped, local).

% Are rotated through quarrying, mining, hauling, and construction service on royal monuments, often at distant sites under expedition conditions with ration provisioning controlled by the dispatching officials. Service duration and site are set administratively; refusal is punishable, and work crews at sites like the royal necropolis have petitioned and struck when rations failed, the one recorded mode of pushback. Return home depends on completion of the levy term.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, corvee_conscripted_workers, payer,
    powerless, biographical, trapped, regional).

% Conquered populations in Nubia and the Levantine marches remit gold, cattle, timber, and captives under the same justification: the king's order extends over the Nine Bows, and tribute is the outer edge of Ma'at's reach. They have no standing in the order's discourse, no cult share, and no channel through which the terms of remittance could be renegotiated; rebellion is met with campaign and deportation.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, peripheral_tribute_peoples, excluded,
    powerless, biographical, trapped, continental).

% Read the full structure from outside the tradition: administrative papyri, ration ledgers, strike rosters, Nilometer records, complaint literature, and monument programs. They can compare the theology's claims against its fiscal and logistical operation across three millennia and identify where the stated justification and the recorded transfers diverge, though they command no standing inside the arrangement they study.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaoh_royal_household).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single legitimating frame under which a long, flood-dependent river valley solves coordination problems once and centrally: basin-scale grain storage and famine relief, inundation response, calendar and festival timekeeping, dispute arbitration across dozens of nomes, and mobilization of labor for hydraulic and monumental works.
% TRANSFER_FUNCTION: Moves surplus grain, corvée labor days, quarry and mining output, and peripheral tribute from peasant producers, conscripted workers, and subject peoples to the royal household, temple estates, and the scribal administrative class, priced as the cost of the king maintaining cosmic order.
% ABSENT_VOICES: The producing peasantry has no seat in theological formulation and no literate voice of its own; subject peripheral peoples remit tribute entirely outside the discourse. Reciprocity-style objection — that the king owes justice and can fail — surfaces only in elite-authored complaint and instruction literature, written by the same scribal class that administers collection, and mostly in windows of central weakness.
% DISAPPEARANCE_RATIONALE: If the mandate framework vanished overnight, taxation and corvée lose their cosmic justification and become bare coercion, the redistributive apparatus loses its legitimacy story, succession loses its arbiter, and the festival-calendar spine of civic time dissolves. The Intermediate Periods show the preview: whenever central legitimacy failed, the valley fragmented into nome warlordism, famine followed stored-grain collapse, and monumental enterprise stopped — the whole civilizational operating system reorganizes around whatever replaces it.
% FOUNDING_PROBLEM: Unification-era Nile valley: an unpredictable flood regime feeding a dense population along a thousand miles of river, with no mechanism to coordinate storage, relief, arbitration, and labor mobilization across autonomous nomes. Divine kingship offered a single point through which basin-wide cooperation could be compelled and justified.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem itself is attested independently of the theology: Nilometer and hydrological records, granary archaeology, and famine stelae document flood variability and the mortality of storage failure. Complaint literature from outside the palace (the Admonitions of Ipuwer, the Prophecy of Neferti) attests that contemporaries tied royal failure to cosmic disorder, though it is elite-authored and no peasant-voiced corroboration exists — a gap that is itself signal. Modern egyptological scholarship corroborates the hydraulic-coordination function while disputing whether the divine-mandate form remained necessary once administrative alternatives existed.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the transfer is large and decoupled from any reciprocal provision the throne can be sued for: the embodiment clause deletes exactly the accountability mechanism the reciprocity reading would supply, so assessment levels are set by administrative capacity rather than by obligation. Suppression is higher still (0.80) because persistence depends on machinery, not uptake: a literacy monopoly held by the beneficiary scribal class, a ritual calendar administered by the beneficiary priesthood, and the doctrinal classification of alternative readings as isfet — chaos itself — so that contesting the mandate is not disagreement but cosmic crime. Theater ratio is moderate (0.40): the ritual and redistributive apparatus performed real calendrical and storage coordination, but a growing share of activity across the interval consisted of proclaiming royal infallibility and raising restoration monuments after each crisis — maintenance of the reading rather than of the order. Accessibility collapse is 0.62: for anyone inside the valley no alternative order was reachable and apostasy was social annihilation, yet reciprocity-flavored objection repeatedly resurfaced in complaint literature, so alternatives collapsed incompletely. Resistance is 0.45: the Deir el-Medina strikes, tomb robbery waves, flight from levies, and elite complaint texts show recurring friction concentrated where enforcement thinned. CYCLICAL PATTERN: the series traces crisis -> restoration -> accumulation cycles keyed to flood variability and central capacity. Extractiveness dips when enforcement machinery fragments (the Intermediate-Period analogues around T10-T30 — extraction is enforced, not self-executing) and peaks after imperial reconcentration (T40, the New Kingdom analogue with tribute inflow and the Amarna-era enforcement surge). Suppression ratchets upward across cycles — each restoration rebuilt enforcement stronger than before, an intermittent-reinforcement dynamic in which crisis itself renewed the mandate claim. Theater trends upward across the whole span as performative maintenance accumulates. All three series share one time grid; end-state values match base_properties.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute opposite structures from the same tradition. From the peasant and conscript seats the arrangement is a compulsory transfer with no contestable terms; from the priestly and scribal seats it is a legitimate order they staff, interpret, and profit from. The king's seat is structurally singular: positioned as the arrangement's source rather than its subject, the throne cannot register the arrangement as binding at all — the per-seat computation from the throne approaches pure subsidy with zero exposure. This is the reading's defining delta against its siblings, and it is computed from the authored positions, not adjudicated by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations place the royal household nearest the subsidy end (collects first and defines conformity), with the priesthood slightly above it (shares gains but bears enforcement labor) and the scribal class above that (gains status and exemption, bears administrative toil). The victim declarations place peasants and conscripted workers near the full-target end, and trapped exit holds them there — there is no arbitrage route out of the valley's order. Peripheral tribute peoples sit at the extreme target end despite formal absence from the discourse: tribute flows outward under the same justification with no return channel whatsoever. National-to-continental scopes raise verification difficulty, which scales effective extraction further upward for the paying seats. No directionality overrides are authored: the derivation from beneficiary/victim data plus exit options reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mislabel risk runs in both directions. Read as pure cosmic law, the arrangement's beneficiaries vanish and its enforcement machinery disappears from view — the classic false-summit shape, which is why the naturalness question is carried as an omega and why beneficiaries are declared despite the theology's self-presentation as natural fact. Read as pure extraction, the genuine coordination achievement — basin-scale grain storage, flood response, calendar regulation, cross-nome arbitration — vanishes, and three millennia of civilizational operation become inexplicable. Tangled rope holds both halves: the founding hydraulic-coordination problem was real and repeatedly proved its reality (every fragmentation produced famine and disorder), while the embodiment clause converted a coordination solution into an unaccountable transfer machine whose justification progressively outlived the parts of its function it originally covered. The R5 mismatch surface is live here: founding problem contested, disappearance verdict world_rearranges — the arrangement persists well past the point where its original justification fully covers its operation, which is precisely where the reading's drift-denial function (failure redefined away, never acknowledged) becomes load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the operative arrangement the divine-mandate reading authored here (king as source, outside the constraint), or do the sibling readings better describe practice — reciprocity (king bound by obligations he can fail) or distributed maintenance (all actors sustain order in their station)?',
    'Compare behavior at royal-failure junctures: First Intermediate complaint literature, the Deir el-Medina strike petitions, and succession-usurpation narratives. If subjects and elites treated royal failure as Ma''at failure carrying claims AGAINST the throne, the reciprocity reading governed practice; if failure was defined away (the usurper was never the true king), the mandate reading governed.',
    'If a sibling reading governed practice, this file''s epsilon and victim set are misattributed: under reciprocity the king sits INSIDE the arrangement as an accountable party and the structure computes closer to conditional coordination; under distributed maintenance the obligated set widens to every actor disciplined out of station.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Ma''at kernel actually governed practice.').

omega_variable(
    cosmic_order_naturalness,
    'Is Ma''at a discovered cosmic order that the royal theology reports, or a constructed political theology whose presentation as natural law serves identifiable beneficiaries?',
    'Test the theology''s content against its exceptions across reigns: if what counts as Ma''at tracks royal interest (assessment levels, corvée scale, campaign timing all read as conforming), the constructed reading is supported; stable moral content invariant across regimes and dynasties would support discovery.',
    'If constructed, the arrangement is a false summit — presented as natural law while benefiting the palace, temples, and scribes — and the natural-law self-presentation is itself part of the enforcement machinery rather than a report about the world.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_order_naturalness, conceptual, 'Natural law versus constructed theology in the Ma''at claim.').

omega_variable(
    embodiment_tautology_scope,
    'Does ''the king cannot violate Ma''at by definition'' mean royal action defines conformity (making the clause vacuous for the throne), or that Ma''at defines legitimate kingship (with failures explained away as usurpation)?',
    'Examine usurpation and erasure narratives: when a predecessor was damned and expunged, the tradition conceded the throne HAD been occupied contrary to Ma''at — implying the clause binds the office conditionally rather than excusing every occupant.',
    'On the vacuous reading, no Ma''at claim can ever constrain royal action and the arrangement is unaccountable transfer; on the conditional reading, a latent accountability mechanism exists that the reciprocity sibling makes explicit, changing the computed asymmetry substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodiment_tautology_scope, conceptual, 'Scope of the embodiment clause''s non-bindingness on royal action.').

omega_variable(
    acquiescence_mechanism,
    'Does the modest recorded resistance reflect internalized acceptance of the mandate framing among the paying population, or structural suppression of articulation — literacy and record-keeping monopolized by the beneficiary scribal class?',
    'Compare literate complaint texts (which voice reciprocity-style objection readily when central control loosens) with non-literate action records: strike rosters, flight from corvée levies, tomb robbery waves, settlement abandonment.',
    'If internalized, suppression travels with agents beyond enforcement reach and outlasts the machinery; if structural, dismantling the literacy and ritual monopoly would release articulated objection quickly, and the measured suppression profile would drop sharply without any change in belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acquiescence_mechanism, empirical, 'Structural versus internalized suppression of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_divine_mandate_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(maat_divine_mandate_tr_t10, maat_order_principle__divine_mandate_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(maat_divine_mandate_tr_t20, maat_order_principle__divine_mandate_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(maat_divine_mandate_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement(maat_divine_mandate_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(maat_divine_mandate_tr_t50, maat_order_principle__divine_mandate_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(maat_divine_mandate_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement(maat_divine_mandate_be_t10, maat_order_principle__divine_mandate_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(maat_divine_mandate_be_t20, maat_order_principle__divine_mandate_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement(maat_divine_mandate_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(maat_divine_mandate_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(maat_divine_mandate_be_t50, maat_order_principle__divine_mandate_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(maat_divine_mandate_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(maat_divine_mandate_su_t10, maat_order_principle__divine_mandate_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(maat_divine_mandate_su_t20, maat_order_principle__divine_mandate_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(maat_divine_mandate_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(maat_divine_mandate_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(maat_divine_mandate_su_t50, maat_order_principle__divine_mandate_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, resource_allocation).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Ma'at' covers three structurally distinct arrangements that differ on the king's position relative to the order. The divine-mandate reading (this file) places the king outside the arrangement as its source, yielding an unaccountable transfer structure with high suppression of alternatives. The reciprocity reading places the king inside as an accountable obligor (justice, stability, distribution owed, breach possible), yielding a conditional-coordination structure with a different victim set. The distributed maintenance reading distributes maintenance across all stations, widening the obligated set to every actor. The upstream/downstream pressure runs from this reading toward both siblings: royal concentration of interpretive authority shapes the legitimacy conditions under which the sibling readings can be voiced, which is why the edges are declared from this file. Each member carries its own epsilon, beneficiaries, and victims; none averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
