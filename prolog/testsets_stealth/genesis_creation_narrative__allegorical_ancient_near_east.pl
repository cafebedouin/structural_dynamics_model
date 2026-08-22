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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Allegorical Ancient Near Eastern Reading Norm for Genesis 1-2
 *   domain: religious/hermeneutical/institutional
 *
 * SUMMARY:
 *   Within academic biblical studies and mainline religious education, the
 *   allegorical ancient-Near-Eastern reading of Genesis 1-2 operates as an
 *   enforced interpretive norm: the text is taught as mythopoetic literature
 *   of its milieu, making no historical-scientific claims, and work that
 *   reads it as cosmological chronicle is gated out of mainstream venues. The
 *   frame solves a real problem — it genuinely illuminates the text through
 *   comparative material and dissolves the scripture-science collision for
 *   the institutions that adopt it — while simultaneously transferring
 *   interpretive legitimacy away from confessional literalist communities and
 *   concordist scholars, who bear the delegitimation and respond with
 *   parallel institutions rather than conversion. This story is ONE READING
 *   of the genesis_creation_narrative kernel: the
 *   allegorical_ancient_near_east reading, instantiated here as a clean
 *   epsilon-invariant constraint. The sibling readings (literal_young_earth,
 *   theistic_evolutionary) are separate constraints with their own epsilon
 *   values, victim sets, and classifications, linked through network edges
 *   and documented in omega variables — the contest between readings is NOT
 *   folded into this file's classification. The epsilon referent is the
 *   standing arrangement under contest — the allegorical frame's own
 *   institutional operation — never the literalist alternative this reading
 *   argues against.
 *
 * KEY AGENTS:
 *   - - academic_biblical_studies_guild: agenda-setting administrator (institutional/constrained) — sets and polices the genre verdict; collects interpretive authority
 *   - - mainline_seminary_educators: beneficiary (institutional/mobile) — collect credibility and science-conflict immunity
 *   - - science_education_advocates: beneficiary (organized/mobile) — collect removal of scriptural warrant from creation-science disputes
 *   - - general_religious_laity: dual-positioned beneficiary/payer (moderate/constrained) — gain a usable scripture, lose the plain sense
 *   - - confessional_literalist_communities: primary target (organized/identity_locked) — bear delegitimation; exit means leaving the community's epistemic world
 *   - - concordist_apologetics_scholars: target (moderate/constrained) — careers confined to parallel channels
 *   - - postcritical_theologians: excluded voice (moderate/constrained) — would restore theological adjudication; held at the frame's edge
 *   - - religious_studies_analysts: analytical observer (analytical/analytical) — sees the full structure from outside every party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.58).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.5).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.58).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Allegorical Ancient Near Eastern Reading Norm for Genesis 1-2").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious/hermeneutical/institutional").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '557ce167-9ebb-4f4d-9b04-3d34c5d515a3').
narrative_ontology:cs_kernel_codification('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', fixed_text).
narrative_ontology:cs_authority_grounding('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', expertise).
narrative_ontology:cs_interpretation_layer_present('557ce167-9ebb-4f4d-9b04-3d34c5d515a3').
narrative_ontology:cs_reading_relation('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', foundational, ane_genre_excludes_scientific_assertion).
narrative_ontology:cs_axiom_status(ane_genre_excludes_scientific_assertion, holdable).
narrative_ontology:cs_axiom_grounding('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', ane_genre_excludes_scientific_assertion, empirically_contingent).
narrative_ontology:cs_axiom('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', secondary, complete_science_scripture_decoupling).
narrative_ontology:cs_axiom_status(complete_science_scripture_decoupling, holdable).
narrative_ontology:cs_axiom_grounding('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', complete_science_scripture_decoupling, instrumental).
narrative_ontology:cs_reference_frame('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', ane_mythopoetic_literature).
narrative_ontology:cs_drift_state('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', contemporary_academy, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('557ce167-9ebb-4f4d-9b04-3d34c5d515a3', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_studies_guild).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_seminary_educators).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_education_advocates).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, general_religious_laity).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, confessional_literalist_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, concordist_apologetics_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, general_religious_laity).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ane_comparative_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretive norms through peer-reviewed journals, doctoral training, society program units, and textbook adoption. Administers the genre verdict that Genesis 1-2 is ancient Near Eastern mythopoetic literature making no historical-scientific claims, and reviews submissions against it. A century of accumulated method, curricula, and career structures rests on the frame; abandoning it would unravel the discipline's standing and its members' training.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_studies_guild, agenda_setter,
    institutional, generational, constrained, global).

% Teach scripture to clergy candidates under accreditation and denominational expectations. The allegorical frame lets them present Genesis 1-2 as theologically serious without exposing graduates to ridicule on cosmology or biology, and without requiring them to defend a young earth. They collect credibility and conflict-avoidance; shifting to another hermeneutical frame would carry moderate but survivable cost.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_seminary_educators, beneficiary,
    institutional, generational, mobile, regional).

% Campaign for evolution-only science standards in public schools and litigate against creation-science insertion. The allegorical reading removes the scriptural warrant that creation-science plaintiffs cite, supplying them with religious allies who testify that the text itself makes no scientific claims. Their benefit is indirect but recurring in every standards fight.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_education_advocates, beneficiary,
    organized, generational, mobile, national).

% Sit in pews and read the text devotionally. They gain a usable scripture that no longer collides with what their children learn in school, but they also lose the plain-sense reading many were raised on, and they experience expert re-description of 'what the text really is' as having their Bible taken away. Individually they have little voice in the committees that set the frame; they respond mainly by staying, leaving, or quietly ignoring the dispute.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, general_religious_laity, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, general_religious_laity, payer).

% Hold the plain-sense chronological reading as part of communal identity and doctrinal subscription. Under the allegorical establishment their reading is classified as pre-critical error in universities, mainline media, and public education, and their children encounter it framed that way. Leaving the literal reading would mean leaving the community's epistemic world, so they bear the delegitimation rather than exit; they have responded by building parallel institutions — colleges, publishers, museums — rather than converting.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, confessional_literalist_communities, payer,
    organized, generational, identity_locked, global).

% Trained to harmonize Genesis with geology, cosmology, or biology, they find mainstream journals, presses, and hiring committees closed to concordist work regardless of its quality. Their realistic options are conversion to the dominant frame at the cost of prior training and standing, or publication in parallel confessional channels with reduced reach. Some migrate; most operate permanently in the secondary market.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, concordist_apologetics_scholars, payer,
    moderate, biographical, constrained, national).

% Work in the theological interpretation of Scripture movement and argue that a purely historical-comparative reading flattens the text's theological voice and its role in the canon. They hold faculty posts adjacent to the guild but their program units and journals remain marginal; they would restore theological adjudication to the reading and are kept at the frame's edge by the same methodological rules that police concordism.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, postcritical_theologians, excluded,
    moderate, biographical, constrained, national).

% Study the hermeneutical dispute itself from religious studies and history-of-science chairs: how the genre verdict was reached, what it enforces, whom it advantages. They take testimony from every seat, trace the comparative method's history, and publish analyses none of the parties controls.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, academic_biblical_studies_guild).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates scholarly and educational discourse about Genesis 1-2 by fixing a shared genre frame — ancient Near Eastern mythopoetic literature comparable to Enuma Elish and Atrahasis — so that seminaries, universities, and mainline faith communities can teach the text without each reader relitigating cosmology, and so that comparative philology can be applied consistently.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy from literalist and concordist traditions toward the academic guild and the mainline institutions aligned with it; moves the text's public meaning away from cosmological assertion and toward literary-theological witness.
% ABSENT_VOICES: Confessional literalist communities appear in the story only as those affected; they are absent from the journal boards, curriculum committees, and accrediting bodies where the frame is administered. Traditionalist laity who treat the plain sense as authoritative rarely sit in those rooms either. The pre-modern Jewish and Christian interpreters whose reading practice the frame supersedes are described by scholars but cast no vote.
% DISAPPEARANCE_RATIONALE: If the allegorical frame vanished overnight, seminary curricula, doctoral training, and the science-faith interface would reorganize immediately: the text would revert to contested adjudicative status over cosmology and biology, the creation-science litigation posture would collapse into open conflict, and mainline institutions would lose the settlement that lets them teach scripture credibly alongside science.
% FOUNDING_PROBLEM: The collision between nineteenth-century geology and Darwinian biology and the plain-sense chronological reading of Genesis 1-2, which threatened to make scripture scientifically false and drove educated believers out of the churches; the allegorical and comparative reading was built to preserve the text's religious significance without asserting anything science could refute.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of science document the nineteenth-century crisis and its settlement; creationist organizations themselves attest the problem is live — their entire institutional existence presupposes it; and the American court record from Scopes (1925) through Kitzmiller v. Dover (2005) attests that the scripture-science collision the frame was built to manage remains an active public dispute.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58 at interval end) because the frame's enforcement transfers interpretive legitimacy from identifiable parties to the guild and its allies, and the transfer is not matched by equivalent benefit flowing back to those who pay; it stops short of snare levels because the comparative method delivers real illumination and the conflict-resolution benefit is broadly distributed. Suppression is moderate (0.50) and purely structural — peer review, hiring norms, accreditation, and curricular control — with no coercive or legal mechanism; the persistence of large parallel creationist institutions is direct evidence that alternatives remain reachable, which caps accessibility_collapse at 0.40. Resistance is substantial (0.60): organized, funded, generational, and international. Theater is low (0.22): most activity is functional philological and literary analysis, though a rising share of 'myth' vocabulary functions as boundary-marking ritual rather than analysis. The temporal series run on one shared six-point grid (every tracked metric authored at every point). The suppression_requirement series is deliberately non-monotonic and is authored because this story specifically tracks enforcement-capacity change: the frame required escalating active defense as its enforcement machinery matured (1890-1985, peaking during the creation-science fights), then enforcement effort declined as the frame became self-enforcing within its institutions — the machinery normalized, not the boundary. Extractiveness rises monotonically across the same interval: rent accrued as the liberating minority reading became establishment orthodoxy.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter and beneficiary seats the frame is a hard-won epistemic achievement: a century of philology that freed the text from false scientific embarrassment, experienced as coordination they built and maintain. From the payer seats the identical structure operates as dispossession: their reading is not refuted but ruled out of order, by bodies they do not staff, in terms ('myth') they experience as insult rather than description. The identity-locked payer seat should compute the harshest type — identity fusion with the plain-sense reading converts professional disagreement into existential threat — while the mobile beneficiary seats compute the mildest. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The guild sits nearest the beneficiary pole (d near 0.0): it writes the rules and collects the authority. Seminary educators and science advocates hold low d — they receive credibility and litigation advantage without administering anything. General laity sit near symmetric: genuine relief from the science conflict, genuine loss of the plain sense, diffuse and individually small on both sides. Confessional literalist communities sit near the full-target pole, amplified by identity lock: they bear the delegitimation and cannot exit without exiting their community's world. Concordist scholars hold high d with constrained exit — trained into a method the mainstream venues exclude, their realistic exits are costly conversion or permanent secondary-market operation. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already produce these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the scripture-science collision — is still live wherever communities read Genesis chronologically, so this is not a case of mandate outliving function; the R5 mismatch flag (dead status x world_rearranges verdict) does not fire. The classification matters in both directions: recognizing the genuine coordination function (comparative illumination, conflict settlement, a teachable text) prevents mislabeling the frame as pure extraction, while naming the asymmetric transfer (legitimacy flows one way, enforcement is real, victims are identifiable) prevents romanticizing it as pure coordination. The piton failure mode is distant — the frame's analytic function is heavily used, not ritually maintained — but the slow theater_ratio rise tracks the portion of 'myth' talk that has become credential-signaling rather than analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (allegorical_ancient_near_east) of the contested genesis_creation_narrative kernel; what changes structurally if a sibling reading — literal_young_earth or theistic_evolutionary — gains institutional dominance?',
    'Track institutional indicators: seminary accreditation standards, public-school science-standard litigation outcomes, denominational teaching authorities, and journal editorial policy shifts across the sibling readings'' strongholds.',
    'Under literal_young_earth dominance the text regains adjudicative authority over cosmology and biology and the victim set expands to include science educators and mainstream research institutions; under theistic_evolutionary dominance partial scripture-science coupling returns and this reading''s complete-decoupling axiom loses its distinguishing force. Either shift rewrites this story''s beneficiary/victim structure and epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three live readings of a fixed-text kernel; sibling readings are separate constraints, not measurement parameters of this one.').

omega_variable(
    extraction_rent_or_hygiene_cost,
    'Is the measured extraction — the closure of mainstream venues to concordist scholarship — an avoidable rent of guild self-protection, or an unavoidable cost of maintaining evidential standards for genre claims?',
    'Compare fields with weaker interpretive gatekeeping for analogous quality outcomes; audit rejection records for concordist submissions that met the frame''s own evidential thresholds and were refused on frame-membership grounds alone.',
    'If the closure is rent, pluralist review reforms could reduce effective extraction without sacrificing the coordination function; if it is hygiene, part of the measured extraction is the irreducible price of the frame''s epistemic service and the tangled_rope reading strengthens against snare drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_rent_or_hygiene_cost, empirical, 'Whether venue closure reflects quality control or boundary protection.').

omega_variable(
    dominion_normativity_residual,
    'Does the allegorical reading actually strip the dominion mandate of Genesis 1:28 of normative force, as its structural delta predicts, or does ecological and bioethical argument continue to draw normative weight from it in practice?',
    'Survey denominational environmental statements, eco-theology literature, and bioethics briefs for appeals to Genesis 1:28 that trade on its normative force rather than treating it as descriptive of an ancient worldview.',
    'If residual normativity persists, the reading''s decoupling is incomplete — the text retains adjudicative reach beyond cosmology into ethics — and the frame''s epsilon and victim structure shift accordingly; if the delta holds, the reading is internally consistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominion_normativity_residual, empirical, 'Whether the predicted loss of normative force for the dominion metaphor has actually occurred.').

omega_variable(
    category_split_anachronism,
    'Is the verdict that Genesis 1-2 makes no historical-scientific claims a discovered property of the text''s genre, or an artifact of imposing modern discipline boundaries (history vs. theology vs. science) on an ancient text that did not separate them?',
    'Historiography of ancient Israelite and wider ANE scribal categories: how the producers and earliest transmitters of such texts classified them, and whether the modern assertive-content distinction maps onto any native category.',
    'If the split is anachronistic, the reading''s foundational axiom weakens from discovery to stipulation, its foreclosure of the literal_young_earth sibling softens, and part of the frame''s authority rests on a category imposition rather than textual evidence — raising the extraction attributable to the guild''s own boundary-work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_split_anachronism, conceptual, 'Whether the no-scientific-claims axiom is a textual property or a modern categorical projection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1890, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_ane_reading_tr_t1890, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(genesis_ane_reading_tr_t1925, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(genesis_ane_reading_tr_t1960, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(genesis_ane_reading_tr_t1985, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(genesis_ane_reading_tr_t2005, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(genesis_ane_reading_tr_t2025, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(genesis_ane_reading_be_t1890, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1890, 0.32).
narrative_ontology:measurement(genesis_ane_reading_be_t1925, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1925, 0.4).
narrative_ontology:measurement(genesis_ane_reading_be_t1960, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1960, 0.47).
narrative_ontology:measurement(genesis_ane_reading_be_t1985, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1985, 0.53).
narrative_ontology:measurement(genesis_ane_reading_be_t2005, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2005, 0.56).
narrative_ontology:measurement(genesis_ane_reading_be_t2025, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(genesis_ane_reading_su_t1890, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement(genesis_ane_reading_su_t1925, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1925, 0.5).
narrative_ontology:measurement(genesis_ane_reading_su_t1960, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(genesis_ane_reading_su_t1985, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(genesis_ane_reading_su_t2005, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(genesis_ane_reading_su_t2025, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2025, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what Genesis 1-2 says' decomposes into three structurally distinct constraints — one per reading of the fixed-text kernel. This member (allegorical_ancient_near_east) carries the lowest coupling to scientific claims and a victim set defined by interpretive delegitimation; the literal_young_earth sibling carries maximal adjudicative coupling and a victim set including scientific institutions; the theistic_evolutionary sibling sits between with partial coupling. Each file authors its own epsilon over the same kernel text; the upstream comparative-method consensus established by this reading supplies the genre vocabulary the theistic-evolutionary sibling borrows, hence the influence edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
