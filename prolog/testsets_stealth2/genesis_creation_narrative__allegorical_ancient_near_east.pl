% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Allegorical Ancient Near Eastern Reading of Genesis 1-2
 *   domain: religious/hermeneutical/science-religion-interface
 *
 * SUMMARY:
 *   Within mainline denominations and academic biblical studies, Genesis 1-2
 *   is read as ancient Near Eastern mythopoetic literature — genre-kin to
 *   Enuma Elish and Atrahasis — making no historical-scientific claims. This
 *   reading is not a passive opinion; it is an administered standard, encoded
 *   in seminary curricula, ordination requirements, lectionaries, and study
 *   Bibles, and maintained against rival readings by hiring, credentialing,
 *   and doctrinal discipline. Its genuine coordination achievement is large:
 *   it lets communities inhabit both the tradition and modern science without
 *   forcing a choice, and it withdraws the text from cosmology-and-biology
 *   adjudication, lowering conflict with science education. Its costs are
 *   real but bounded: legitimate meaning becomes accessible chiefly through
 *   credentialed mediation, plain-sense lay reading is recast as
 *   pre-critical, inherited devotional practices that assumed historicity are
 *   partially invalidated, and ethical projects built on the dominion charge
 *   of Genesis 1:28 lose their proof-text. KEY AGENTS (by structural
 *   relationship): - mainline_denominational_teaching_offices: Agenda-setter
 *   and beneficiary (institutional/arbitrage) — administers the standard,
 *   absorbs enforcement costs - academic_biblical_scholars: Primary
 *   beneficiary (institutional/mobile) — careers and authority ride on the
 *   critical paradigm - plain_sense_lay_believers: Primary target
 *   (powerless/constrained) — bear mediated access and delegitimized naive
 *   reading - traditional_devotional_communities: Target
 *   (moderate/constrained) — inherited practice partially ruled out of bounds
 *   - dominion_ethics_theologians: Target (moderate/mobile) — lose the
 *   normative force of Genesis 1:28 - science_education_institutions:
 *   Beneficiary (institutional/mobile) — gain reduced creationism conflict,
 *   administer nothing - literalist_creationist_ministries: Excluded
 *   (organized/trapped) — barred from the rooms where the reading is fixed -
 *   hermeneutics_of_religion_analysts: Analytical observer — sees the full
 *   allocation structure
 *
 * KEY AGENTS:
 *   - mainline_denominational_teaching_offices: agenda-setter and beneficiary (institutional/arbitrage) — sets and enforces the reading, pays enforcement costs
 *   - academic_biblical_scholars: primary beneficiary (institutional/mobile) — collects interpretive authority and career rents from the critical paradigm
 *   - plain_sense_lay_believers: primary target (powerless/constrained) — bear the cost of mediated access and delegitimized plain reading
 *   - traditional_devotional_communities: target (moderate/constrained) — bear partial invalidation of inherited devotional practice
 *   - dominion_ethics_theologians: target (moderate/mobile) — bear loss of normative force in Genesis 1:28
 *   - science_education_institutions: beneficiary (institutional/mobile) — collect reduced science-religion conflict without administering the standard
 *   - literalist_creationist_ministries: excluded (organized/trapped) — would contest the reading but are structurally outside the conversation
 *   - hermeneutics_of_religion_analysts: analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.34).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.3).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.34).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Allegorical Ancient Near Eastern Reading of Genesis 1-2").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious/hermeneutical/science-religion-interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '7ccf39c7-1514-4c8b-a862-002533917077').
narrative_ontology:cs_kernel_codification('7ccf39c7-1514-4c8b-a862-002533917077', fixed_text).
narrative_ontology:cs_authority_grounding('7ccf39c7-1514-4c8b-a862-002533917077', expertise).
narrative_ontology:cs_interpretation_layer_present('7ccf39c7-1514-4c8b-a862-002533917077').
narrative_ontology:cs_reading_relation('7ccf39c7-1514-4c8b-a862-002533917077', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('7ccf39c7-1514-4c8b-a862-002533917077', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('7ccf39c7-1514-4c8b-a862-002533917077', foundational, text_makes_no_historical_scientific_claims).
narrative_ontology:cs_axiom_status(text_makes_no_historical_scientific_claims, holdable).
narrative_ontology:cs_axiom_grounding('7ccf39c7-1514-4c8b-a862-002533917077', text_makes_no_historical_scientific_claims, empirically_contingent).
narrative_ontology:cs_axiom('7ccf39c7-1514-4c8b-a862-002533917077', secondary, dominion_metaphor_non_normative).
narrative_ontology:cs_axiom_status(dominion_metaphor_non_normative, holdable).
narrative_ontology:cs_axiom_grounding('7ccf39c7-1514-4c8b-a862-002533917077', dominion_metaphor_non_normative, empirically_contingent).
narrative_ontology:cs_reference_frame('7ccf39c7-1514-4c8b-a862-002533917077', ane_mythopoetic_genre_frame).
narrative_ontology:cs_drift_state('7ccf39c7-1514-4c8b-a862-002533917077', contemporary_expansion_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('7ccf39c7-1514-4c8b-a862-002533917077', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominational_teaching_offices).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_education_institutions).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, plain_sense_lay_believers).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, traditional_devotional_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, dominion_ethics_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets ordination standards, seminary curricula, lectionaries, and catechetical materials that encode the allegorical ancient Near Eastern reading, and maintains it through hiring, credentialing, and doctrinal discipline. Gains institutional coherence and scientific credibility from the standard; pays the costs of defending it when literalist factions challenge it from within. Defines the terms of the game and so can reshape it, though reshaping risks schism.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominational_teaching_offices, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, mainline_denominational_teaching_offices, beneficiary).

% Produces the comparative-genre scholarship — Enuma Elish and Atrahasis parallels, functional-cosmology readings — on which the standard rests, and staffs the seminaries and departments that transmit it. Careers, journals, and professional authority depend on the critical paradigm remaining the legitimate approach to the text. Can move between theology, religious studies, and humanities posts if a given institution turns hostile.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars, agenda_setter).

% Reads Genesis 1-2 devotionally in its apparent plain sense. Under the administered standard, that unmediated understanding is recast as naive or pre-critical, and legitimate meaning becomes accessible mainly through expert mediation they did not choose and cannot easily evaluate. Leaving means exiting a congregation that anchors friendships and family identity; staying means accepting the mediated reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, plain_sense_lay_believers, payer,
    powerless, biographical, constrained, global).

% Communities whose inherited practice — pre-critical commentaries, harmonization traditions, liturgical habits that assume the events narrated occurred — predates the critical paradigm. Parts of that inheritance are ruled out of bounds by the standard, and they must either retrain their teachers across a generation or accept that cherished readings are obsolete. Their sunk investment spans generations, which makes retraining costly and exit wrenching.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, traditional_devotional_communities, payer,
    moderate, generational, constrained, continental).

% Builds ecological, political, or vocational ethics on the dominion charge of Genesis 1:28. Under the standard the verse is ancient royal-ideology metaphor carrying no normative force, so their projects lose their proof-text and must be rebuilt on other scriptural or philosophical grounds. Rebuilding is feasible — their training travels — but the specific resource is gone.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, dominion_ethics_theologians, payer,
    moderate, generational, mobile, global).

% Benefits whenever adopting denominations withdraw the text from cosmology and biology adjudication: fewer school-board fights, less pressure on curricula, fewer students arriving with collapsed trust in science. Administers nothing of the standard and bears none of its internal costs; its benefit is purely the removed conflict.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_education_institutions, beneficiary,
    institutional, generational, mobile, national).

% Young-earth and concordist ministries barred from mainline teaching offices, seminary faculties, and publication venues by the same standards that encode the allegorical reading. They would argue the reading concedes empirical ground the text actually holds, but their objection is structurally outside the room where the reading is fixed; their route back runs through the very credentialing the standard controls.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_creationist_ministries, excluded,
    organized, biographical, trapped, global).

% Studies how religious communities allocate authority among text, tradition, and science; takes testimony from every seat, traces the genealogy of the reading from the nineteenth-century crisis forward, and neither administers the standard nor pays into it.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, hermeneutics_of_religion_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_scholars).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the faith-science collision for adopting communities: it allocates the text's authority to theological and liturgical domains, concedes empirical domains to science, and thereby lets members inhabit both modernity and the tradition without choosing between them. It also standardizes ministerial training so that a scattered denomination teaches one coherent account of the text.
% TRANSFER_FUNCTION: Moves interpretive authority — the recognized right to say what the text means and what it claims — from lay plain-sense readers and inherited devotional practice to credentialed specialists (seminary-trained clergy and academic biblical scholars); moves the burden of reconciling text and science from individual believers to institutions.
% ABSENT_VOICES: Literalist movements and the plain-sense devotional majorities of earlier generations are absent from the seminary and academic rooms where the reading is fixed; they would object that the reading concedes ground the text actually holds and severs it from its plain claims. The text's ancient composers are also absent and unavailable — their intent is reconstructed by the very specialists whose authority depends on the reconstruction.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, seminaries, ordination requirements, lectionaries, and study Bibles would require rewriting; mainline bodies would face an immediate forced choice between re-literalizing (splitting their science-facing members) and further relativizing the text; the managed boundary between pulpit and laboratory would dissolve and the science-religion conflict would return inside those bodies rather than between them.
% FOUNDING_PROBLEM: Built to solve the post-Enlightenment collision: geology, evolutionary biology, and historical criticism made the literal chronicle untenable for educated believers, threatening either mass defection or anti-scientific entrenchment. The allegorical ancient Near Eastern reading preserved the text's religious use while ending its career as an empirical adjudicator.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of the period document the nineteenth- and twentieth-century crises (Essays and Reviews, the Briggs trial, the Scopes controversy) as the reading's occasion; science-education organizations and survey sociology attest that the collision the reading manages is real and recurring, visible in persistent evolution-acceptance gaps between mainline and literalist bodies. No corroborating source attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.34, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate (0.34): the reading takes little that its own lights count as true — it only relinquishes claims it says the text never made — but it does concentrate interpretive authority in a credentialed class and impose real costs on plain-sense and traditional readers, so it is not negligible. Suppression (0.30) is a raw structural property, unscaled by power or scope: enforcement is soft (credentialing, hiring, social marking rather than coercion) and exit is genuinely available — other denominations, secularism — which caps it well below snares. Theater ratio (0.22) reflects a partly routinized comparative-mythology industry that republishes familiar parallels, alongside a real hermeneutical service. Accessibility collapse is low (0.25): the literal reading remains fully live for millions, so alternatives do not collapse once this reading is understood. Resistance is substantial (0.60): creationist movements, traditionalist backlash, and slippery-slope objections actively contest the standard. The temporal series run on ONE shared grid (t=0..140, eight points, every tracked metric authored at every point). The suppression_requirement series is authored deliberately because this story traces enforcement-capacity change: high early enforcement (confessional tests, heresy trials such as the Briggs case, Scopes-era defense) decaying across the interval as literalists sorted into separate denominations and enforcement needs fell. Extractiveness rose as the paradigm professionalized and gatekept, then plateaued; theater crept up as the scholarship industrialized. Final series values equal the base_properties scalars by construction of the interval endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute differently. From the teaching offices and the academy, the arrangement is a hard-won peace they built and maintain: the text freed from falsification, the community freed from the science war. From the plain-sense lay seat, the same structure operates as a credential gate over meaning they once accessed directly; from traditional devotional seats, as the quiet invalidation of inherited practice; from dominion-ethics seats, as the quiet amputation of a normative resource. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidy end: teaching offices collect coherence and credibility (damped further by their real enforcement costs), scholars collect authority and careers, science-education institutions collect conflict reduction at zero administrative burden. Targets sit toward the full-target end: lay believers bear the transfer of interpretive authority with constrained exit (communal identity binds, though denomination-switching remains possible — hence constrained, not identity_locked), traditional communities bear invalidation costs with generational sunk investment, and dominion-ethics theologians bear a specific normative loss but retain mobile exit by rebuilding on other grounds. The excluded literalist ministries feed no derivation: they are outside the arrangement's operation, not extracted from by it. Larger scopes (global scholar networks, continental denominations) modestly amplify effective extraction for targets by making verification of fair mediation harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the post-Enlightenment collision between the literal chronicle and geology, evolution, and historical criticism — is still live wherever the collision recurs, so this is not a mandatrophy case: status=live crossed with disappearance_verdict=world_rearranges produces no capture/zombie flag. The classification work the type performs is bidirectional. From the academy's seat the reading presents as pure rope — emancipation, decoupling, peace — and naming the credential-gate victims prevents mislabeling it as costless coordination. From literalist seats it presents as a snare — capitulation dressed as scholarship — and the genuine decoupling function, the low suppression, and the easy exit prevent mislabeling it as pure extraction. Tangled rope holds both truths: real coordination and real, bounded, asymmetrically distributed cost through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates one reading (allegorical_ancient_near_east) of the contested kernel genesis_creation_narrative; how would the constraint''s structure change under the sibling readings?',
    'Generate the sibling stories (genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__theistic_evolutionary) and compare epsilon, victim sets, and enforcement profiles across the kernel family.',
    'Under literal_young_earth the text regains adjudicative authority over cosmology and biology, science educators become targets, and epsilon rises sharply with a different classification; under theistic_evolutionary partial theological adjudication survives and the structure sits between this reading and literalism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a three-reading kernel; the disagreement is located in whether the text asserts truth-claims about natural history.').

omega_variable(
    genre_classification_stability,
    'Is the ancient Near Eastern mythopoetic genre classification of Genesis 1-2 stable under ongoing comparative evidence, or could new tablet finds or reassessments of the Enuma Elish and Atrahisis parallels shift the genre verdict?',
    'Track Assyriological publication and peer response; a durable revision of the parallel corpus or of functional-cosmology readings would signal instability.',
    'If the genre classification weakens, the reading''s foundational axiom loses its empirical grounding and foreclosure pressure from the literal reading rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_classification_stability, empirical, 'Stability of the evidential basis for the reading''s core genre claim.').

omega_variable(
    enforcement_decay_selection_ambiguity,
    'Does the falling suppression requirement reflect genuine liberalization within mainline bodies, or selection effect — literal-leaning members exiting to separate denominations so that less internal enforcement is needed?',
    'Denominational demographic series: if internal dissent falls faster than membership declines, and decline concentrates among literal-leaning members, selection dominates.',
    'If selection drives the decay, the arrangement persists by segregation rather than consensus, and its coordination function narrows to the remnant community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_selection_ambiguity, empirical, 'Whether enforcement decay is persuasion or sorting.').

omega_variable(
    mediation_service_vs_rent,
    'Is the credential gate around legitimate meaning priced near the real cost of the philological and historical training it certifies, or does it carry an authority premium beyond service cost?',
    'Compare curricular cost and time-to-ordination against measured deference: do lay readers with equivalent primary-source access receive different uptake of their interpretations?',
    'A large authority premium would raise effective extraction above the authored base and create pressure toward a snare-side computed classification; rough parity supports the tangled-rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediation_service_vs_rent, empirical, 'Whether specialist mediation of the text''s meaning is service or rent.').

omega_variable(
    decoupling_durability_under_new_frontiers,
    'Will the no-adjudication settlement hold as new empirical frontiers (genomics, neuroscience, artificial intelligence) reopen territory adjacent to the text''s theological claims?',
    'Observe whether adopting denominations extend the decoupling principle to new domains or resume selective adjudication when doctrinal interests are engaged.',
    'Resumed selective adjudication would erode the reading''s core axiom and shift its structure toward the theistic_evolutionary sibling''s configuration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_durability_under_new_frontiers, preference, 'Durability of the decoupling settlement against future frontier conflicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_ane_allegorical_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.1).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t20, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 20, 0.12).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.15).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t60, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 60, 0.18).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t80, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 80, 0.2).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t100, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 100, 0.21).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t120, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 120, 0.22).
narrative_ontology:measurement(genesis_ane_allegorical_tr_t140, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 140, 0.22).

% Extraction over time
narrative_ontology:measurement(genesis_ane_allegorical_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(genesis_ane_allegorical_be_t20, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(genesis_ane_allegorical_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(genesis_ane_allegorical_be_t60, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(genesis_ane_allegorical_be_t80, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 80, 0.33).
narrative_ontology:measurement(genesis_ane_allegorical_be_t100, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 100, 0.34).
narrative_ontology:measurement(genesis_ane_allegorical_be_t120, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 120, 0.34).
narrative_ontology:measurement(genesis_ane_allegorical_be_t140, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 140, 0.34).

% Suppression requirement over time
narrative_ontology:measurement(genesis_ane_allegorical_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(genesis_ane_allegorical_su_t20, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(genesis_ane_allegorical_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(genesis_ane_allegorical_su_t60, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(genesis_ane_allegorical_su_t80, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(genesis_ane_allegorical_su_t100, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 100, 0.32).
narrative_ontology:measurement(genesis_ane_allegorical_su_t120, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 120, 0.3).
narrative_ontology:measurement(genesis_ane_allegorical_su_t140, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 140, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Genesis 1-2 claims' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — the three readings of the kernel genesis_creation_narrative. Each has its own epsilon, beneficiary/victim structure, and enforcement profile: this allegorical reading (no historical-scientific claims, complete decoupling), literal_young_earth (full adjudicative authority over cosmology, science educators as targets), and theistic_evolutionary (partial theological adjudication, intermediate structure). The readings are linked rather than merged because measuring the text's authority through one reading's observable yields a different epsilon than through another's — the confusion lives in the shared label, not in the structure. Historically the literal reading was upstream (the default until the nineteenth-century crisis); this reading's genre scholarship now exerts downstream influence on the theistic_evolutionary sibling's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
