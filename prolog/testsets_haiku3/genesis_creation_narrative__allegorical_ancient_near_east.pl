% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   Genesis 1–2, read as Ancient Near Eastern mythopoetic literature,
 *   decouples the text from scientific authority claims. The constraint is
 *   that Genesis conveys theological and cosmological vision through mythic
 *   narrative that is intelligible within its 8th–6th century BCE literary
 *   context and comparative tradition (Enuma Elish, Atrahasis), NOT as an
 *   empirical chronicle of creation timescale, mechanisms, or Earth age. This
 *   reading is ONE of three live interpretations of the contested kernel
 *   'genesis_creation_narrative'; the other readings (literal young-earth,
 *   theistic evolutionary) make structurally different claims about the same
 *   text. This story instantiates the allegorical-ancient-near-east reading
 *   only — it does not adjudicate between readings, but models the constraint
 *   that emerges when this reading holds institutional authority.
 *
 * KEY AGENTS:
 *   - Historical-critical scholars: institutional agents who benefit from the reading's compatibility with source criticism and form analysis; their interpretive methods (genre classification, redaction history, comparative literature) find legitimate application.
 *   - Theistic scientists: organized agents who benefit from the decoupling of Genesis from scientific claims; they can hold evolutionary biology and cosmological consensus without textual tension.
 *   - Young-earth literalists: organized agents who experience the reading as a loss of textual authority and hermeneutical framework; the reading challenges their epistemological foundation.
 *   - Literalist seminary faculty: institutional agenda-setters who administer literalist hermeneutics and face pressure to gatekeep against this reading.
 *   - Scientific community: external observer; benefits from reduced friction and institutional conflict.
 *   - Lay believers: excluded from formal theological discourse; their lived experience of textual meaning is not systematically incorporated.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.12).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.08).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.12).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/hermeneutics/science_religion_interface").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, 'a6a224e6-617e-47a8-b57b-90eee0e7c126').
narrative_ontology:cs_kernel_codification('a6a224e6-617e-47a8-b57b-90eee0e7c126', fixed_text).
narrative_ontology:cs_authority_grounding('a6a224e6-617e-47a8-b57b-90eee0e7c126', expertise).
narrative_ontology:cs_interpretation_layer_present('a6a224e6-617e-47a8-b57b-90eee0e7c126').
narrative_ontology:cs_reading_relation('a6a224e6-617e-47a8-b57b-90eee0e7c126', genesis_creation_narrative__literal_young_earth, coexists_with).
narrative_ontology:cs_reading_relation('a6a224e6-617e-47a8-b57b-90eee0e7c126', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('a6a224e6-617e-47a8-b57b-90eee0e7c126', foundational, text_makes_no_empirical_claims).
narrative_ontology:cs_axiom_status(text_makes_no_empirical_claims, holdable).
narrative_ontology:cs_axiom_grounding('a6a224e6-617e-47a8-b57b-90eee0e7c126', text_makes_no_empirical_claims, empirically_contingent).
narrative_ontology:cs_axiom('a6a224e6-617e-47a8-b57b-90eee0e7c126', foundational, ancient_near_eastern_genre_context).
narrative_ontology:cs_axiom_status(ancient_near_eastern_genre_context, holdable).
narrative_ontology:cs_axiom_grounding('a6a224e6-617e-47a8-b57b-90eee0e7c126', ancient_near_eastern_genre_context, empirically_contingent).
narrative_ontology:cs_reference_frame('a6a224e6-617e-47a8-b57b-90eee0e7c126', textual_analysis_historical_context).
narrative_ontology:cs_drift_state('a6a224e6-617e-47a8-b57b-90eee0e7c126', contemporary_scientific_consensus_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a6a224e6-617e-47a8-b57b-90eee0e7c126', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, theistic_scientists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_literalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Biblical scholars, Ancient Near Eastern specialists, and comparative religionists who benefit from a reading that permits form-critical analysis, source criticism, and redaction history. The reading legitimizes their methods and frameworks — genre classification, historical contextualization, literary parallels with cuneiform texts. They have mobile exit: they could adopt literalism or other readings but choose the allegorical frame because it enables their scholarly practice.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_scholars, beneficiary,
    institutional, generational, mobile, global).

% Scientists and theologians who want to hold evolutionary biology, cosmological consensus, and stellar nucleosynthesis alongside religious faith. The reading eliminates tension: Genesis makes NO empirical claims about timescale or mechanism, so there is no text-evidence conflict. They benefit from the decoupling. Exit is mobile: they could adopt literalism (inducing tension) or agnosticism (abandoning faith) but choose this reading because it permits both intellectual integrity and faith commitment.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theistic_scientists, beneficiary,
    organized, biographical, mobile, global).

% Organized communities (churches, creation-science organizations, some homeschooling networks) for whom literalism is constitutive of religious identity and epistemology. The allegorical reading is experienced as a loss: Genesis no longer adjudicates empirical truth, inerrancy is weakened, the text's authority is diminished. Exit from literalism is identity-fused; many adherents cannot leave without experiencing radical identity rupture. They bear the cost of defending literalism against institutional pressure.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_literalists, payer,
    organized, generational, identity_locked, global).

% Faculty at evangelical and fundamentalist seminaries who teach literalist hermeneutics and maintain institutional commitment to inerrancy. They administer gatekeeping: curriculum choices, hiring decisions, theological statements that shape what young clergy are trained to believe. They encounter the allegorical reading as a competing interpretive authority that threatens institutional mission. Exit is constrained: they could adopt the reading but doing so undermines their institution's identity and constituency.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_seminary_faculty, agenda_setter,
    institutional, generational, constrained, national).

% Observes from outside religious institutional frameworks. The reading eliminates a persistent source of public friction: young people no longer need to choose between accepting evidence and accepting Genesis as authority. Reduces institutional tension without scientific institutions needing to adjudicate theological claims. Benefits without participating in hermeneutical choices.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community, observer,
    institutional, civilizational, analytical, global).

% Believers outside formal theological institutions who rely on Genesis for spiritual meaning and identity formation. They would have something to say about whether reading it as mythopoetic preserves its power to transform and sustain. They are not systematically incorporated into the institutional hermeneutical debate. Their voices are excluded; the readings are formalized by scholars and seminary faculty who do not answer to lay intuition.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, lay_believers_unaffiliated, excluded,
    powerless, biographical, identity_locked, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_scholars).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates textual interpretation across comparative ancient Near Eastern literature: locates Genesis within a shared genre (cosmogonic myth), permits form-critical analysis, and recovers the historical context of the text's composition and function in ancient Israel. Solves the coordination problem of how to read Genesis as meaningful to its original audience and interpretive tradition rather than through modern scientific or historical-chronicle frames.
% TRANSFER_FUNCTION: Transfers hermeneutical authority from literalist (historical-scientific chronicle) readings to historical-critical (genre-contextual) readings. Moves the text from the category 'adjudicates empirical claims about cosmology and biology' to 'conveys theological and cosmological vision through mythic narrative'. This is a redistributive move: literalist institutions lose gatekeeping authority over the text's meaning; historical-critical scholars and theistic scientists gain interpretive legitimacy.
% ABSENT_VOICES: Lay believers, indigenous communities with their own creation cosmologies, feminist interpreters of the dominion mandate — these parties would contest what the allegorical reading loses: spiritual authority, embodied meaning, narrative performativity, the text's role in shaping how humans relate to creation and to each other. They are largely absent from the institutional hermeneutical discourse.
% DISAPPEARANCE_RATIONALE: If the allegorical-ancient-near-east reading disappeared (i.e., became unrepresentable in institutional religious discourse), fundamentalist and literalist institutions would consolidate their hermeneutical authority. Scientific and secular institutions would face renewed institutional friction and young people would experience renewed tension between scriptural authority claims and empirical evidence. The arrangement would not vanish — literalism would simply expand unchallenged. Contested because benefiting parties dispute whether the constraint adds epistemic value or merely shifts power; paying parties dispute whether its disappearance would represent intellectual loss or liberation.
% FOUNDING_PROBLEM: The founding problem was the emergence of historical-critical biblical scholarship in 18th–19th century Europe: how to read Genesis in light of comparative ancient literature, source criticism, and the growing body of Mesopotamian creation myths? Literalist readings could not easily accommodate the discovery that Genesis shared narrative motifs with Enuma Elish and Atrahasis. The allegorical-ancient-near-east reading solved the coordination problem by relocating Genesis into a literary genre frame where such similarities are expected and meaningful rather than threatening.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religious scholars, Ancient Near Eastern specialists, and historians of the Bible attest the founding problem remains live: new discoveries of cuneiform texts, ongoing refinement of dating for Genesis composition, and persistent questions about transmission and redaction all keep the comparative framework methodologically active. Literalist scholars actively dispute this; they argue the founding problem was a false problem resting on rationalist assumptions. Corroboration from outside literalist institutions: archaeological evidence of Iron Age Israel and textual parallels support the comparative-genre framing.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, contested).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 endpoint) because the reading does not extract rents or suppress alternatives through coercion — it is a scholarly and theological argument that competes in the open marketplace of interpretations. Suppression is minimal (0.08) because the reading spreads through institutional pedagogy (seminaries, universities, published scholarship) rather than through enforcement machinery. Theater is near-zero (0.05) because the constraint's function (comparative genre analysis, historical contextualization) is genuinely its operation — there is little performative maintenance. Accessibility collapse is low (0.22) because alternatives (literalism, theistic evolution) remain live and accessible; the reading must continuously persuade rather than rely on naturalness. Resistance is high (0.71) because literalist institutions mount sustained, organized counterargument; the reading faces real pushback. The measurements show mild trajectory: extractiveness rises slightly as institutional penetration increases (more scholars adopt the frame), but suppression and theater remain flat because the reading operates through intellectual merit, not coercion. This is a rope-type constraint: genuine coordination function (how to read Genesis in light of comparative ancient literature) without substantial extraction or enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (literalists) and the beneficiary seats (scholars, scientists) should compute dramatically different types from their local perspectives. From the literalist seminary's position, the reading is an attempted usurpation of hermeneutical authority and a capitulation to rationalism — it looks like a snare: extraction of textual control, suppression of literalist interpretation, active enforcement through institutional gatekeeping (university hiring, seminary curricula, publishing gatekeeping). From the scholar's position, it is a genuine rope: coordination of how to read Genesis intelligibly within its historical context, with no coercive overhead, genuine beneficiaries. The engine computes this divergence from power + exit: literalists are organized and institutional but face identity-lock (high d → high χ); scholars are institutional and mobile (low d → low χ). The reading instantiates the scholar's seat analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (historical-critical scholars, theistic scientists) gain from the reading because it permits their preferred interpretive methods and resolves apparent tensions with empirical knowledge. Their exits are mobile — they could use other readings or epistemic frameworks but choose this one because it is genuinely helpful to their work. Payers (literalists) lose hermeneutical authority; their exit is identity-locked because literalism constitutes a comprehensive epistemological framework tightly coupled to religious identity for many. Literalist institutions (seminaries, publishing houses) are constrained: they could adopt the reading but doing so would undermine their institutional mission and constituency.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by maintaining genuine coordination function: the founding problem (how to read Genesis in light of comparative ancient literature and source criticism) remains live, and the reading's function (genre-contextual analysis) remains essential to solving that problem. There is no degradation of function masked by theater. The reading does not persist because of institutional inertia; it spreads because scholars find it analytically useful and because it genuinely permits both theistic commitment and scientific consensus. This is a sustained rope, not a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_classification_stability,
    'Is the genre classification of Genesis 1-2 as ''mythopoetic literature'' stable across scholarly tradition, or does it rest on interpretive choices that different scholarly schools contest?',
    'Meta-analysis of form-critical scholarship: do scholars agree on the boundaries of the mythic genre, or do disagreements about genre reflect deeper hermeneutical commitments?',
    'If genre is stable and well-defined, the reading''s basis is robust. If genre classification varies across schools of criticism, the reading itself carries latent contestability. A fragile genre category weakens the reading''s universalizability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_classification_stability, conceptual, 'Whether genre-classification of Genesis as mythopoetic is a stable scholarly achievement or reflects latent hermeneutical choice.').

omega_variable(
    theological_authority_loss,
    'When Genesis is read as mythopoetic rather than as historical chronicle or empirical authority, what is lost from the text''s function as theological authority and spiritual meaning-making for communities that rely on it?',
    'Phenomenological study of believers who adopt vs. reject the allegorical reading: do they report sustained sense of textual meaningfulness, or do they experience hermeneutical loss? Longitudinal study of communities that shift hermeneutics.',
    'If substantial spiritual/theological meaning is lost, the reading may be trading hermeneutical coherence for scientific consonance — a real cost obscured by the scholarly framing. If meaning sustains or transforms, the reading''s viability as a lived theological framework is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_authority_loss, empirical, 'Whether decoupling from empirical authority entails loss of theological meaningfulness.').

omega_variable(
    metaphorical_dominion_mandate,
    'If the dominion mandate (Genesis 1:28, 2:15) is read as metaphor rather than as normative creation-management charter, what interpretive constraints apply to its metaphorical content? Does ''dominion'' metaphorically authorize extractive or exploitative human relation to creation, or does it reframe human role in ways that constrain extraction?',
    'Systematic study of how the allegorical reading handles dominion in ecological ethics and environmental theology. Do allegorical readings produce systematically different environmental ethics than literalist readings, or is this independent of hermeneutics?',
    'If allegorical reading strongly constrains extractive readings of dominion, the decoupling from empirical authority carries downstream ethical consequences. If the reading permits both exploitative and stewardship interpretations of the metaphor, ethical content is underdetermined by hermeneutics itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphorical_dominion_mandate, empirical, 'Whether allegorical reading of dominion produces determinate ethical constraints.').

omega_variable(
    literalist_sibling_relationship,
    'Does the allegorical-ancient-near-east reading logically foreclose the literal-young-earth reading, or do they coexist as live options held by different communities and scholarly traditions?',
    'Examine whether a single coherent intellectual framework can hold both readings (e.g., some interpreters who accept historical-critical methods while also maintaining literalist faith commitments), or whether the readings are held in mutually exclusive institutional contexts.',
    'If they coexist without logical foreclosure, both readings remain live within different communities and the kernel contest is genuinely unresolved. If one foreclosure relation holds, the contest is structurally asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_sibling_relationship, conceptual, 'The structural relationship between this reading and the literal-young-earth sibling.').

omega_variable(
    authority_grounding_institutional_capture,
    'The allegorical-ancient-near-east reading is grounded in institutional scholarly authority (universities, seminaries, peer review). Does this authority derive from genuine expertise in comparative ancient literature and form criticism, or does it rest partly on institutional power and gatekeeping that privileges secular scholarship over faith-based hermeneutics?',
    'Meta-analysis: do scholars from literalist traditions who adopt historical-critical methods reach the same conclusions as secular scholars? Are there systematic differences in interpretive conclusions tied to institutional affiliation or faith commitment?',
    'If conclusions are method-driven (same methods produce same results regardless of scholar''s background), authority is rooted in expertise. If conclusions vary by institutional or faith context even when methods are identical, authority partly reflects institutional gatekeeping.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_institutional_capture, empirical, 'Whether scholarly authority derives from expertise or from institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.02).
narrative_ontology:measurement(gene_tr_t5, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 5, 0.03).
narrative_ontology:measurement(gene_tr_t10, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 10, 0.04).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 20, 0.04).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 30, 0.05).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gene_be_t5, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(gene_be_t10, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(gene_be_t30, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.04).
narrative_ontology:measurement(gene_su_t5, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 5, 0.05).
narrative_ontology:measurement(gene_su_t10, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 20, 0.07).
narrative_ontology:measurement(gene_su_t30, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.03).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The genesis_creation_narrative kernel decomposes into three constraint stories, each instantiating a different reading with distinct ε values and beneficiary/victim structures. ALLEGORICAL_ANCIENT_NEAR_EAST (this story): low extraction, low suppression, rope-type; scholarly coordination function. LITERAL_YOUNG_EARTH: high extraction, high suppression, snare-type; institutional enforcement of inerrantist hermeneutics. THEISTIC_EVOLUTIONARY: moderate extraction, moderate suppression, tangled-rope-type; hybrid coordination (science-theology synthesis) with asymmetric costs. The three readings are not measurements of one constraint; they are three structurally distinct constraints arising from different interpretations of the same kernel text. The network links capture the downstream pressure each reading exerts on the others: allegorical reading influences (but does not foreclose) theistic-evolutionary; literal-young-earth coexists with both but faces increasing institutional pressure from scientific consensus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
