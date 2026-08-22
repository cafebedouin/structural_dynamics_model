% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ABOLISHED_MEIJI_SHINBUTSU_BUNRI_1868]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion Order (Kami as Traces of Original Buddhas)
 *   domain: religious/historical
 *
 * SUMMARY:
 *   From roughly the ninth century to 1868, the dominant framework for
 *   relating Japan's kami cult to Buddhism identified every kami with a
 *   Buddhist original: the kami is a local trace (suijaku) of an original
 *   ground (honji), a buddha or bodhisattva appearing in native guise. This
 *   story instantiates the ontological_fusion_reading of the
 *   simultaneous_veneration kernel: the claim that the identification
 *   captures metaphysical truth, and that the institutional order built on it
 *   — shrine-temple pairings, clerical appointments at shrines, kami given
 *   Buddhist names and bodhisattva ranks — is the proper expression of that
 *   truth. The arrangement solved a real integration problem for a millennium
 *   while concentrating interpretive authority in monastic hands and
 *   progressively re-describing kami identity in vocabulary shrines did not
 *   control. Per the epsilon-referent rule, epsilon here assesses the
 *   STANDING ARRANGEMENT UNDER CONTEST — the enforced honji-suijaku order as
 *   it actually operated — not the post-separation or kami-autonomous
 *   alternative this reading's rivals would install; the reading's
 *   endorsement of the ontology does not zero the cost profile of the
 *   institution that enforced it. Claim and metrics are authored
 *   independently: claimed_type records my structural judgment (genuine
 *   coordination plus asymmetric, enforced extraction); the metrics record
 *   the arrangement's observed operation. Sibling readings
 *   (domain_partition_reading, pragmatic_incoherence_reading) are separate
 *   constraint files linked via network.affects_constraints; the contest
 *   between readings is carried in the omega variables, not averaged into
 *   this one.
 *
 * KEY AGENTS:
 *   - - buddhist_monastic_establishments: Agenda-setter and principal collector (institutional/arbitrage) — articulates the doctrine, installs clergy at shrines, defines what any kami is, receives offerings and jurisdiction
 *   - - shrine_buddhist_clergy: Secondary beneficiary (organized/mobile) — occupies shrine offices, performs esoteric rites, draws stipends from shrine revenues
 *   - - court_and_warrior_governments: Co-agenda-setter (institutional/arbitrage) — sanctions ranks and pairings, gains legitimation and social peace, bears policing costs
 *   - - prestige_shrine_institutions: Mixed-position beneficiary-payer (powerful/constrained) — trades doctrinal independence for estates, titles, and protection; Ise refuses and pays politically
 *   - - hereditary_shrine_priest_lineages: Primary target (moderate/identity_locked) — loses interpretive authority over their own deities; office bound to ancestry and locality
 *   - - local_kami_veneration_communities: Diffuse target with incidental benefit (powerless/constrained) — devotion to local kami progressively reframed by others
 *   - - kokugaku_nativist_scholars: Excluded voice (organized/constrained) — attacks the identification from outside doctrinal administration
 *   - - comparative_religion_historians: Analytical observer (analytical/analytical) — reconstructs the full structure from outside the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.75).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.55).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion Order (Kami as Traces of Original Buddhas)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/historical").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, 'ebaa34d1-2af8-48d7-beed-7251047804d9').
narrative_ontology:cs_kernel_codification('ebaa34d1-2af8-48d7-beed-7251047804d9', formalized).
narrative_ontology:cs_authority_grounding('ebaa34d1-2af8-48d7-beed-7251047804d9', lineage).
narrative_ontology:cs_interpretation_layer_present('ebaa34d1-2af8-48d7-beed-7251047804d9').
narrative_ontology:cs_reading_relation('ebaa34d1-2af8-48d7-beed-7251047804d9', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('ebaa34d1-2af8-48d7-beed-7251047804d9', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('ebaa34d1-2af8-48d7-beed-7251047804d9', foundational, kami_are_trace_manifestations_of_original_buddhas).
narrative_ontology:cs_axiom_status(kami_are_trace_manifestations_of_original_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('ebaa34d1-2af8-48d7-beed-7251047804d9', kami_are_trace_manifestations_of_original_buddhas, theological).
narrative_ontology:cs_axiom('ebaa34d1-2af8-48d7-beed-7251047804d9', secondary, kami_veneration_incomplete_without_buddhist_ground).
narrative_ontology:cs_axiom_status(kami_veneration_incomplete_without_buddhist_ground, holdable).
narrative_ontology:cs_axiom_grounding('ebaa34d1-2af8-48d7-beed-7251047804d9', kami_veneration_incomplete_without_buddhist_ground, instrumental).
narrative_ontology:cs_reference_frame('ebaa34d1-2af8-48d7-beed-7251047804d9', honji_suijaku_cosmic_hierarchy).
narrative_ontology:cs_drift_state('ebaa34d1-2af8-48d7-beed-7251047804d9', post_meiji_separation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ebaa34d1-2af8-48d7-beed-7251047804d9', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, shrine_buddhist_clergy).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, prestige_shrine_institutions).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, hereditary_shrine_priest_lineages).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_kami_veneration_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, court_and_warrior_governments).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, local_kami_veneration_communities).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, prestige_shrine_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great monastic complexes (Enryakuji, Onjoji, Toji and their branch networks) articulate the doctrinal identification of kami with buddhas, train and appoint clergy to serve at shrines, register shrine rites within their liturgical calendars, and receive shares of shrine offerings and endowment income. They decide what any kami is; their exit position is trivial because the interpretive instrument is theirs — a kami reclassified is a kami re-described.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments, beneficiary).

% Monks installed at shrines (betto, shaso) perform esoteric rites, maintain kami images as Buddhist icons, administer shrine property, and draw stipends and offerings from shrine revenues. They move between postings within the monastic network; their livelihood depends on the pairing system continuing.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shrine_buddhist_clergy, beneficiary,
    organized, biographical, mobile, national).

% The court grants shrine ranks and confirms temple-shrine pairings; warrior governments endow shrine-temples with estates and call on both institutions for prayers and legitimation. They gain social peace and ritual prestige and bear the administrative cost of policing the boundary between the two systems.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, court_and_warrior_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, court_and_warrior_governments, beneficiary).

% Major shrines with court rank and landed endowments trade doctrinal independence for estates, titles, and protection: their deities receive Buddhist names and bodhisattva ranks, and temple complexes grow inside their precincts. Some — Ise above all — refuse the pairing and keep their rites free of Buddhist vocabulary at real political cost, demonstrating that the autonomy surrendered had a price.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, prestige_shrine_institutions, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, prestige_shrine_institutions, payer).

% Hereditary priestly families hold office by descent and conduct the shrine's daily rites. Across the interval their deities are progressively re-described from outside: kami receive Buddhist originals, priests are pressed toward ordination or subordinate office under resident monks, and shrine theology is written in a vocabulary they do not control. Leaving the office means extinguishing a duty tied to ancestors and locality; staying means administering a deity whose identity is defined elsewhere.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, hereditary_shrine_priest_lineages, payer,
    moderate, generational, identity_locked, regional).

% Village and neighborhood worship associations maintain local shrines, fund festivals, and approach their kami for rain, harvest, healing, and protection. Their devotional life is gradually reframed — kami as manifestations of buddhas, rites supplemented with esoteric elements — a reframing they did not author and cannot veto, though festival life and this-worldly benefits continue and are often enriched.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_kami_veneration_communities, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, local_kami_veneration_communities, beneficiary).

% Nativist philologists reconstruct an ancient Way of the kami from the classics and argue the Buddhist identification is a later accretion obscuring it. They publish, teach, and gather networks of disciples, but hold no office in the doctrinal administration their work attacks; their influence reaches policy only at the very end of the interval, when the Meiji state acts on conclusions they reached generations earlier.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, kokugaku_nativist_scholars, excluded,
    organized, generational, constrained, national).

% Modern historians of Japanese religion reconstruct the arrangement's rise, operation, and abolition from documents, comparing it with other cases of religious synthesis. They bear none of its costs and collect none of its benefits; their seat sits outside the interval entirely.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_religion_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Housed two complete religious systems — an indigenous cult of numinous locality and a comprehensive imported salvation religion — within one polity: unified ritual calendars, a shared soteriology (kami attain buddhahood; buddhas manifest as kami), institutional templates for temple-shrine coexistence, and a common doctrinal language spanning elite scholastic and popular practice.
% TRANSFER_FUNCTION: Moved interpretive authority over kami identity, clerical offices, and offering and revenue streams from shrine institutions to Buddhist monastic complexes; moved doctrinal prestige, textual resources, and soteriological standing back toward shrines — asymmetrically, since the kami received an identity defined elsewhere while the temple received jurisdiction.
% ABSENT_VOICES: Kami-centered voices: hereditary shrine priesthoods defending autochthonous theology had no seat in doctrinal administration, and nativist scholars circulated objections in print from outside the establishment until the Meiji state made their position official. Lay venerators expressed preference through practice and festival, never through doctrine — the conversation about what the kami really are was conducted almost entirely within monastic institutions under court sanction.
% DISAPPEARANCE_RATIONALE: The medieval religious economy was organized around the fusion: temple-shrine complexes, monastically staffed shrines, clerical appointment chains, land endowments, and festival calendars all presupposed it. Overnight disappearance would have unraveled jurisdiction, revenue, and ritual life across the archipelago — as the actual 1868 abolition demonstrated, rearrangement was achievable but took the form of widespread temple destruction, forced laicization of clergy, and the compulsory re-description of every kami back out of Buddhist vocabulary.
% FOUNDING_PROBLEM: How a single polity could sustain both an indigenous cult of numinous locality and a comprehensive imported salvation religion — two totalizing systems with rival claims on ritual, resources, and ultimate truth — without schism, persecution, or collapse of the court's ritual order.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Kuroda Toshio and the historiography descending from him attest that the fusion was the medieval mainstream whose coordinating problem dissolved once both systems were domesticated in the early-modern period; contemporaneous nativist scholarship (Motoori Norinaga, Hirata Atsutane) attested from outside the establishment that the founding problem no longer justified the hierarchy; the Meiji separation edicts acted on that assessment. No party inside the Buddhist establishment corroborated obsolescence — institutional testimony uniformly defended continued necessity — which is itself signal.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is high (0.75 at interval end) because the arrangement's central flow — interpretive authority over kami identity, plus clerical offices and offering shares — ran from shrine institutions to monastic complexes, and the rate of that flow was set by the monastic side, not negotiated. Suppression (0.55) is authored as a RAW STRUCTURAL property, unscaled: it reflects legal rank subordination of shrines, temple jurisdiction over shrine finances, court/bakufu decrees enforcing pairings, and pressure toward clerical ordination of priestly lineages; only extractiveness is scaled by directionality and scope in the engine's computation. Theater_ratio (0.40) is moderate: the ritual apparatus kept performing real integrative work throughout (festivals, rites, calendrical coordination), but by the late Edo period a growing share of doctrinal maintenance defended the arrangement out of institutional interest after elite conviction had eroded — performance without the function it once performed, short of full atrophy. Accessibility_collapse (0.52): alternatives never fully collapsed — partition thinking persisted among practitioners, Ise maintained a Buddhist-free rite, quiet kami-centrism survived locally — but no rival framework could be institutionalized inside the order. Resistance (0.62): sustained nativist philology from Keichū through Hirata Atsutane, Ise's costly distancing, domain-level separation experiments, culminating in the Meiji abolition — a construct that must ultimately be abolished by state violence is not meeting negligible resistance. The three measurement series share ONE six-point grid (900/1100/1300/1500/1700/1867) so no metric row is silently substituted. The suppression_requirement series is authored deliberately: enforcement capacity BUILT UP through medieval institutionalization (peak ~1300–1500) and DECAYED in the late Edo period — decay, not liberalization (see omega enforcement_decay_or_liberalization). Coalition potential among the powerless targets was thin: local worship communities were geographically dispersed, ritually dependent, and offered no coordination channel that the monastic network did not mediate.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the monastic seat, the arrangement is the natural metaphysical order it administers: the kami genuinely IS the buddha's trace, and installing clergy at shrines is completing the kami's own truth. From the hereditary-priest seat, the same structure is the alienation of a deity the lineage serves by ancestral right — the kami's name for itself replaced by one issued from elsewhere. Prestige shrines straddle: they collected estates and rank precisely by accepting the re-description, so their grievance is priced and partially settled. The engine computes these per-seat classifications from the structural data; this story does not adjudicate which seat is right, and the divergence between the agenda-setter's experience and the payer's experience is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Monastic establishments sit nearest the beneficiary pole (d near 0): they author the terms, capture the flow (see gain_flow), and hold arbitrage-grade exit — any kami can be re-described because the interpretive instrument is theirs. Shrine clergy inherit low d through their positions. Court and warrior governments derive moderately low d: genuine legitimation gains against real enforcement costs. Hereditary priest lineages derive high d, amplified toward the full-target end by identity_locked exit — the office is ancestral, the deity locality-bound, and exit means extinguishing a lineage duty. Local worship communities derive high-moderate d with incidental-benefit damping (their festival life continued and often enriched). One override is authored: prestige_shrine_institutions are the only powerful-seat agents, and the structural derivation from their beneficiary declaration would place them near the beneficiary pole; their true position is mixed — they paid doctrinal autonomy for resources, and Ise's counterexample shows the autonomy had a market price — so d is overridden to 0.4. Kokugaku scholars are excluded rather than coordinated: outside the conversation, they feed no chi. National spatial scope makes verification of local practice harder across provinces, which the engine reflects as a modest upward scaling of effective extraction; suppression is left unscaled by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. Reading the millennium as pure extraction erases the genuine coordination function — two totalizing religious systems were housed in one polity without schism for nine centuries, and several seats (prestige shrines, lay communities) verifiably collected — a pure-extraction story cannot explain why so many parties defended the arrangement. Reading it as pure coordination erases the enforced hierarchy: identifiable lineages lost interpretive authority over their own deities, and the arrangement required active enforcement (decrees, ordination pressure, financial jurisdiction) to hold. Mandatrophy: the founding problem — housing kami cult and Buddhism without schism — was effectively dead by the late Edo period; both systems had long been domesticated, coexistence was routinized, and the schism risk the arrangement managed had receded. Yet the structure persisted on institutional interest and inertia through its final century, with rising theater and decaying enforcement, until an exogenous regime rupture abolished it. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) flags exactly this zombie character of the pre-abolition decades; because the arrangement WAS abolished, no ongoing zombie persists — the flag dates the drift, it does not describe the present. Identity-lock dynamics: the hereditary priests' lock was to lineage-office, not to Buddhist doctrine as such — once the Meiji state broke the frame, priestly lineages flipped to kami-centrism with remarkable speed, confirming that what bound them was ancestral obligation, not conviction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_domain_partition,
    'This constraint instantiates the ontological_fusion_reading of the simultaneous_veneration kernel; if the domain_partition_reading (kami and buddhas as functionally distinct entities governing separate jurisdictions) better describes the arrangement, does the measured subordination of kami identity — and hence epsilon — collapse?',
    'Compare doctrinal texts against practitioner self-understanding across the interval: votive inscriptions, oracle records, liturgical manuals, and elite diaries indicating whether venerators treated kami and buddhas as one class of beings or two.',
    'If partition describes practice, the fusion reading''s victim (denied kami autonomy) largely evaporates and the arrangement reads as complementary specialization with low extraction, closer to a coordination mechanism; if fusion describes practice, the authored high epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_domain_partition, conceptual, 'Whether the sibling domain-partition reading displaces this reading''s victim structure.').

omega_variable(
    kernel_sibling_pragmatic_incoherence,
    'If the pragmatic_incoherence_reading is correct — simultaneous veneration was never a coherent constraint, only unenforced contradictory beliefs held without resolution — does this story''s subject (an enforced ontological-unity arrangement with an asymmetric flow of authority and revenue) exist at all?',
    'Trace enforcement instruments directly: ordination records of shrine priests, court decrees mandating shrine-temple pairing, property documents binding shrines to temple jurisdiction, and punishment records for kami-centered practice that refused Buddhist framing.',
    'Abundant enforcement instruments confirm a single operative constraint and this story stands; their absence dissolves the arrangement into plural private beliefs and routes the analysis to the sibling story with epsilon near zero.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_pragmatic_incoherence, empirical, 'Whether enforcement evidence suffices to treat the fusion as one operative constraint rather than uncoordinated belief.').

omega_variable(
    kami_autonomy_baseline,
    'How autonomous was the pre-Buddhist kami cult baseline against which the loss of indigenous kami autonomy is measured?',
    'Archaeological and documentary reconstruction of shrine practice before sustained Buddhist contact (before the seventh century), assessing whether an independent theological authority structure existed to be displaced.',
    'If the autonomous baseline is thinner than the victim declaration assumes, part of the measured cost is the ordinary price of literacy, textuality, and state incorporation rather than monastic advantage-taking, lowering epsilon; if robust, the victim declaration stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_autonomy_baseline, conceptual, 'Counterfactual baseline for the kami-autonomy victim claim.').

omega_variable(
    enforcement_decay_or_liberalization,
    'Does the falling suppression_requirement after 1500 reflect enforcement decay (the arrangement becoming brittle ahead of forced abolition) or genuine liberalization (voluntary acceptance making coercion unnecessary)?',
    'Late-Edo domain-level separation experiments (Mito, Okayama, Fukuyama), records of anti-Buddhist agitation, and bakufu responses distinguishing tolerated dissent from prosecuted dissent.',
    'The decay reading supports classification drift toward inertial, theatrically maintained operation in the final century; the liberalization reading would instead suggest the arrangement was stabilizing and the 1868 rupture exogenous rather than end-stage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_or_liberalization, empirical, 'Interpretation of the late-interval suppression decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 900, 1867).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t900, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement(simu_tr_t1100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(simu_tr_t1300, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1300, 0.18).
narrative_ontology:measurement(simu_tr_t1500, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1500, 0.24).
narrative_ontology:measurement(simu_tr_t1700, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1700, 0.34).
narrative_ontology:measurement(simu_tr_t1867, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 1867, 0.4).

% Extraction over time
narrative_ontology:measurement(simu_be_t900, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 900, 0.4).
narrative_ontology:measurement(simu_be_t1100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement(simu_be_t1300, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1300, 0.7).
narrative_ontology:measurement(simu_be_t1500, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1500, 0.73).
narrative_ontology:measurement(simu_be_t1700, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1700, 0.76).
narrative_ontology:measurement(simu_be_t1867, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 1867, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t900, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement(simu_su_t1100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1100, 0.48).
narrative_ontology:measurement(simu_su_t1300, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1300, 0.62).
narrative_ontology:measurement(simu_su_t1500, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1500, 0.64).
narrative_ontology:measurement(simu_su_t1700, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1700, 0.58).
narrative_ontology:measurement(simu_su_t1867, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 1867, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'simultaneous veneration of kami and buddhas' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon values, victim sets, and enforcement profiles: domain_partition_reading (functional specialization, low extraction), ontological_fusion_reading (this file — enforced ontological hierarchy, high extraction), and pragmatic_incoherence_reading (no coherent constraint at all, near-zero epsilon). The fusion reading is upstream: it supplied the doctrinal apparatus and institutional machinery that the partition reading qualifies and the incoherence reading denies. Each member links to the others via affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__ontological_fusion_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
