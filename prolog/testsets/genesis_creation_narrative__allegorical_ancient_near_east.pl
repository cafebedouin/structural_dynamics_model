% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Text (Allegorical Reading)
 *   domain: religious/hermeneutical/epistemological
 *
 * SUMMARY:
 *   Genesis 1-2 in this reading is understood as Ancient Near Eastern
 *   mythopoetic literature: a theological narrative structured through
 *   Babylonian cosmogonic patterns (Enuma Elish parallels,
 *   primordial-chaos-to-order structure, naming-as-creation), designed to
 *   establish Israel's cosmological place and humanity's vocation within
 *   divine order, without making empirical claims about natural history or
 *   the age of the earth. The constraint operates at the level of
 *   hermeneutical authority: it asserts the text is theology/literature, not
 *   science, and therefore makes no adjudicative claim over biology,
 *   cosmology, or geology. This reading decouples Genesis from evolutionary
 *   science, creating institutional space for both religious meaning-making
 *   and scientific inquiry to proceed without hermeneutical conflict. The
 *   constraint is claimed as rope (genuine coordination: separating
 *   magisteria permits both to function without boundary violation) and the
 *   measurements describe modest but real extractiveness—the reading extracts
 *   epistemic authority FROM the literal reading and its communities,
 *   transferring it to science and critical scholarship.
 *
 * KEY AGENTS:
 *   - Critical biblical scholars (powerless-to-organized depending on institutional seat; mobile exit): interpret Genesis through comparative mythology and form-critical methods; benefit from the reading's elevation of their hermeneutical authority.
 *   - Evolutionary biologists and cosmologists (organized, mobile): protected from hermeneutical contestation; benefit from the non-overlap principle; pursue science without negotiating theology.
 *   - Liberal theological seminaries (organized, constrained exit): teach Genesis allegorically; benefit from coherence between scholarship and faith; credential clergy in this reading.
 *   - Literal-reading evangelical/fundamentalist communities (organized, identity-locked exit): bear the cost of this reading's dominance in academic and institutional contexts; marginalized as 'prescientific'; maintain their reading through counter-institutional effort.
 *   - Science educators and public-school administrators (institutional, constrained): use the reading to exclude creationism from science curricula; benefit from institutional coherence; depend on the reading's acceptance in courts and policy.
 *   - Religious fundamentalist institutions (organized, identity-locked agenda-setter): actively resist and contest this reading; suffer epistemic delegitimation; maintain literal reading through schools and publishing.
 *   - Theistic evolutionists (moderate, constrained, largely excluded): occupy middle ground; agree the reading is valid but contest that it is the only valid Genesis reading; absent from formal institutional articulation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.31).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.28).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.31).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Text (Allegorical Reading)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious/hermeneutical/epistemological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, 'b566f3e9-d9b7-43e2-921f-d263fe4128eb').
narrative_ontology:cs_kernel_codification('b566f3e9-d9b7-43e2-921f-d263fe4128eb', fixed_text).
narrative_ontology:cs_authority_grounding('b566f3e9-d9b7-43e2-921f-d263fe4128eb', lineage).
narrative_ontology:cs_interpretation_layer_present('b566f3e9-d9b7-43e2-921f-d263fe4128eb').
narrative_ontology:cs_reading_relation('b566f3e9-d9b7-43e2-921f-d263fe4128eb', genesis_creation_narrative__literal_young_earth, coexists_with).
narrative_ontology:cs_reading_relation('b566f3e9-d9b7-43e2-921f-d263fe4128eb', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('b566f3e9-d9b7-43e2-921f-d263fe4128eb', foundational, genesis_ancient_near_east_text).
narrative_ontology:cs_axiom_status(genesis_ancient_near_east_text, holdable).
narrative_ontology:cs_axiom_grounding('b566f3e9-d9b7-43e2-921f-d263fe4128eb', genesis_ancient_near_east_text, empirically_contingent).
narrative_ontology:cs_axiom('b566f3e9-d9b7-43e2-921f-d263fe4128eb', foundational, text_makes_no_cosmological_claims).
narrative_ontology:cs_axiom_status(text_makes_no_cosmological_claims, holdable).
narrative_ontology:cs_axiom_grounding('b566f3e9-d9b7-43e2-921f-d263fe4128eb', text_makes_no_cosmological_claims, deontological).
narrative_ontology:cs_reference_frame('b566f3e9-d9b7-43e2-921f-d263fe4128eb', genesis_as_ancient_theology).
narrative_ontology:cs_drift_state('b566f3e9-d9b7-43e2-921f-d263fe4128eb', contemporary_scientific_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b566f3e9-d9b7-43e2-921f-d263fe4128eb', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholarship).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_education_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, evolutionary_biologists_cosmologists).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, liberal_theological_seminaries).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_education_administrators).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_communication_professionals).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, fundamentalist_institutional_leaders).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, text_genre_sensitivity_principle).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ancient_near_eastern_literary_conventions).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, theology_science_non_overlap_magisteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in ancient languages, comparative mythology, Akkadian, Ugaritic, and form criticism read Genesis through Ancient Near Eastern literary conventions. They publish in peer-reviewed biblical studies journals (JBL, VT, JAOS), teach in university departments and liberal seminaries, and shape academic consensus on compositional history and genre. The allegorical reading elevates their hermeneutical authority and validates their interpretive methods as the scholarly standard.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, beneficiary,
    organized, generational, mobile, global).

% Researchers in evolutionary biology, paleontology, astrophysics, and geology operate under the constraint's assertion that Genesis makes no scientific claims. This removes Genesis from the domain of empirical adjudication: evolution, deep time, Big Bang cosmology proceed without negotiating theology. They publish in Science, Nature, peer-reviewed disciplinary journals, and communicate publicly with the confidence that they need not address hermeneutical objections from literal Genesis readings.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, evolutionary_biologists_cosmologists, beneficiary,
    organized, generational, mobile, global).

% Mainline Protestant (Methodist, Presbyterian, Episcopalian, United Church of Christ), Roman Catholic, and Jewish theological seminaries teach Genesis 1-2 through historical-critical and literary-mythological methods. They credential clergy, shape pulpit interpretation, and influence educated congregational understanding. The constraint permits them to teach deep theology (God as creator, vocation, covenant, Sabbath sanctification) without requiring inerrantism or defending literalism against science.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, liberal_theological_seminaries, beneficiary,
    organized, generational, constrained, regional).

% Evangelical, fundamentalist, Seventh-day Adventist, and some Orthodox Christian communities, plus Orthodox Jewish yeshiva communities, teach Genesis as straightforward historical record with 24-hour creation days and recent creation. They bear the cost of the allegorical reading's institutional dominance: their hermeneutics are delegitimized in academic and scientific discourse, their children encounter contradiction between home teaching and school/university teaching, and they must actively defend their reading against scholarly consensus presented as authority.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_communities, payer,
    organized, generational, constrained, regional).

% Public school science departments, state education boards, and national science curriculum standards (Next Generation Science Standards) operate under the constraint's framework: science class teaches empirically grounded natural history, and theology is a different domain. This permits them to teach evolution and cosmology without requiring them to attack religion or forbid private belief. Courts (Edwards v. Aguillard, Dover v. School District) have upheld the magisteria-separation as the constitutional basis for excluding creationism from science curricula.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_education_administrators, beneficiary,
    institutional, generational, constrained, national).

% Bible colleges, creation-science organizations (Answers in Genesis, Institute for Creation Research), evangelical publishing houses, and fundamentalist think tanks actively maintain and defend the literal reading through curriculum, books, conferences, and social-media presence. They set the agenda for their institutional constituencies and bear the cost of scholarly delegitimation and legal exclusion from public education. Their institutional identity is fused with biblical literalism; accepting the allegorical reading would mean institutional dissolution.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, fundamentalist_institutional_leaders, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, fundamentalist_institutional_leaders, payer).

% Science writers, museum educators, public-engagement scientists, and science journalists deploy the allegorical reading rhetorically when addressing religious audiences on evolution, cosmology, or deep time. The reading permits them to educate scientifically while respecting religious identity—avoiding the appearance of scientific atheism and positioning science and religion as compatible. This is a coordination win if audiences accept the epistemic frame.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_communication_professionals, beneficiary,
    moderate, biographical, mobile, global).

% Courts, legislatures, and school boards in pluralistic democracies interpret the allegorical reading (and the magisteria-separation frame) as the constitutional basis for scientific education. Courts have ruled that excluding creationism from science class does not violate religious freedom because science and theology are separate domains; evolution is taught as science, not as anti-religious claim. They do not author the reading but depend on it for institutional coherence.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, educational_policy_makers, observer,
    institutional, generational, analytical, national).

% A diffuse constituency within mainline Christianity (BioLogos, theistic-evolution theologians like John Polkinghorne, Alister McGrath) and Jewish thought that accepts evolutionary science as the true natural history and reads Genesis as theologically compatible with but not predictive of science (days as epochs, text as using metaphor not chronology). They are largely absent from the formal articulation of the constraint; their reading is rarely centered as a primary alternative. They are excluded not by explicit rejection but by invisibility in mainstream discourse.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionists, excluded,
    moderate, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates academic, scientific, and secular-institutional authority around a unified principle: Genesis 1-2 is Ancient Near Eastern mythopoetic literature making theological claims (human vocation, divine ordering, covenant) but no empirical claims about natural history. This decouples science education and evolutionary biology from hermeneutical contestation. Removes institutional boundary conflict: universities and public schools can teach evolution without attacking religion; religious communities can maintain faith without denying science.
% TRANSFER_FUNCTION: Transfers epistemic authority from literal biblical interpretation (as adjudicator of cosmology/biology) to evolutionary biology and cosmology, and transfers hermeneutical authority from proof-text fundamentalism to historical-critical biblical scholarship. In exchange, assigns theology (meaning-making, vocation, covenant) to the biblical text and protects religious identity-construction from scientific delegitimation. A quid pro quo: different institutional domains accept non-overlapping epistemic magisteria.
% ABSENT_VOICES: Fundamentalist and evangelical pastors and theologians who read Genesis literally are present but not authorized to speak in scientific or public-education contexts—their exclusion is structural, enforced through institutional gatekeeping (peer review in academic publishing, curriculum standards in schools, scientific credentialing). Literalist lay believers in churches and homeschools are entirely absent from scholarly discourse. Theistic evolutionists occupy an ambiguous middle: they agree the reading is legitimate but contest that it is the ONLY legitimate reading; they are neither fully present (rarely centered as primary voice) nor fully absent (academic publications exist).
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight (hypothetically replaced by literal or theistic-evolutionary dominance), the primary rearrangement would be renewed legal and policy conflict: literal creationism would re-enter school-board disputes, courts would re-litigate Establishment Clause questions (can Genesis literalism be taught as science?), and the working détente permitting 'both religious and scientific authority in the same institutions' would collapse. Publishers, textbook writers, educators, and theologians would lose the shared frame permitting peaceful institutional coexistence. Religious freedom and scientific freedom would be repositioned as competitors rather than as occupants of separate domains.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century conflict: Darwin's evolution collided with literal Genesis teaching; churches rejected Darwinism, scientists attacked biblical authority, and public schools faced irresolvable curriculum disputes (Scopes Trial, modern creationism battles). The constraint was developed by liberal theologians, critical biblical scholars, and science-minded clergy to dissolve the conflict by asserting Genesis was never making scientific claims—the conflict is a category mistake, not a genuine disagreement.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is LIVE: literal creationism remains taught in evangelical churches, fundamentalist schools, and some Orthodox communities; evolution remains contested in public school boards; court cases continue (Dover v. School District, Kansas Board of Education). The resolution (magisteria-separation) is corroborated by: (1) evolutionary biologists (evolutionary theory remains empirically unchallenged and is the foundation of modern biology); (2) academic biblical scholars (comparative mythology and form criticism confirm ancient-near-eastern literary patterns in Genesis); (3) secular-education advocates and scientists (testifying that evolution and religious belief are compatible when the text is not treated as scientific). Literal-reading communities dispute the resolution (they affirm both Genesis and deny evolution or insist Genesis constrains evolution interpretation) but do not deny the original conflict existed or that the magisteria-separation frame achieves institutional peace.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).

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
 *   Extractiveness is modest (0.31 at interval end) because the constraint is genuinely coordinate: it does solve the magisteria-separation problem and permits institutional coexistence that was previously conflicted. However, it is not zero because the reading extracts epistemic authority from literal interpretation and transfers it to academic scholarship and science—a redistribution benefiting some seats over others. The trajectory shows slow growth (0.18→0.31 over 50 years) because the reading's institutional grip has deepened: it is now taught in most mainstream seminaries, affirmed in Catholic doctrine, and entrenched in science-education policy. Suppression is modest (0.28) because the constraint does not require coercion of individual belief—fundamentalist communities can and do maintain literal readings—but it does require active defense of the literal reading against scholarly consensus. Theater (0.18) reflects the reading's genuine functional coherence: the separation of magisteria is performative in some contexts (courts citing it to avoid establishment-clause violation) but also genuine—the hermeneutical practice of historical-critical scholarship is not mere theater. Accessibility collapse (0.42) is moderate: once the reading is known, alternatives (literal, theistic-evolution) do persist and gain institutional support; the reading is not inevitable. Resistance (0.67) is substantial: strong counter-institutional resistance from fundamentalist churches, creation-science organizations, and some Orthodox communities maintain literalism as a live position.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between seats that experience the reading as enabling (scientists, scholars, liberal clergy) and seats that experience it as constraining (fundamentalist communities, fundamentalist institutions). Enablement comes from removal of boundary conflict; constraint comes from delegitimation of one's own hermeneutics. The gap is STRUCTURAL, not factual: both seats are describing the same constraint accurately from their positions, but the positions create opposed interests. The engine's per-seat computation will show this divergence in the d values and in the terminal type assignment: a rope for beneficiaries, a tangled-rope or snare for high-d payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for critical biblical scholars: low d (~0.2) because they are net beneficiaries—their hermeneutical authority is elevated, their careers advance through acceptance of this reading, and they have arbitrage-level exit (can teach anywhere the reading is dominant, which is most mainstream institutions). Directionality for evolutionary biologists/cosmologists: low d (~0.15) because they benefit from the magisteria boundary—it removes hermeneutical constraint from their work, and they have mobile-to-arbitrage exit (pursue science in any modernized institution). Directionality for science educators: moderate d (~0.4) because they benefit from the reading's institutional validity (it justifies excluding creationism from curricula) but are constrained by school-board politics and parent objections; moderate exit because switching to a more literal framework would require institutional work. Directionality for evangelical/fundamentalist payers: high d (~0.75) because they bear the cost of epistemic delegitimation, must actively defend their reading against consensus, and have constrained-to-identity-locked exit (leaving the reading means leaving the community/institution). Directionality for fundamentalist agenda-setters: extreme d (~0.85) because they actively maintain a reading against institutional pressure, extract nothing from the constraint (they are its targets), and have identity-locked exit (institutional dissolution if they concede the reading). No directionality override is needed because the structural data (beneficiaries vs. payers/victims, power atoms, exit options) derives d correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (science/theology conflict in 19th-20th century) is LIVE: literal creationism continues to clash with evolution in school boards, courts, and public discourse. The constraint's mandate (separate magisteria to remove conflict) remains ACTIVE and FUNCTIONAL in institutional contexts where both religious and scientific authority operate (universities, courts, public education). However, a mandatrophy shadow exists: for fundamentalist communities that have not accepted the reading, the constraint does not resolve the conflict—instead it imposes a hermeneutical frame from outside their tradition. The constraint works (mandatrophy is not present) ONLY for parties who accept its epistemic premises (that theology and science are separable magisteria, that Genesis is not making scientific claims). For parties that reject those premises, the constraint is experienced not as coordination but as imposition. This is NOT mandatrophy (the constraint still solves the stated founding problem: it does remove conflict at the institutional level, even if fundamentalist communities must actively resist it). But it is a WARNING: the constraint's success depends on widespread acceptance of a particular epistemology (non-overlapping magisteria), and that epistemology is itself contested, especially in communities that practice integrated reading of scripture and science.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisteria_boundary_assumption,
    'Is the non-overlapping magisteria (NOMA) framework itself a coherent epistemology, or does it rest on unexamined assumptions about how meaning, truth, and authority relate across domains?',
    'Philosophical critique of NOMA''s internal consistency; empirical observation of whether scientists and theologians in practice actually maintain separate magisteria or whether they rhetorically invoke the boundary when convenient and blur it when it suits their argument.',
    'If NOMA is epistemologically unstable, the constraint''s foundation shifts: it would be a pragmatic institutional accommodation rather than a true separation, which would lower its perceived legitimacy and increase its theater ratio. If NOMA is robust, the constraint genuinely coordinates distinct domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisteria_boundary_assumption, conceptual, 'Whether the constraint''s foundational epistemology is internally coherent.').

omega_variable(
    literal_reading_identity_lock,
    'To what extent is the literal reading of Genesis maintained by fundamentalist and evangelical communities through identity fusion (exit-costs rooted in community/vocation identity) versus through genuine rational conviction that the reading is true?',
    'Ethnographic study of fundamentalist communities; post-exit trajectories of people raised in literal-reading cultures who later accept the allegorical reading; analysis of whether accepting the allegorical reading requires leaving one''s religious community.',
    'If identity-lock is primary, the literal-reading communities are trapped by social structure rather than rational belief, raising the oppression diagnosis; if rational conviction is primary, the constraint is genuinely contested on epistemic grounds. The measured suppression score would be re-evaluated: if suppression is primarily internalized (community pressure, identity threat), it is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_reading_identity_lock, empirical, 'Whether resistance to the allegorical reading is rooted in identity-lock or rational conviction.').

omega_variable(
    science_authority_asymmetry,
    'Does the allegorical reading genuinely separate Genesis from science, or does it embed science as the superior interpretive frame (science defines what counts as ''real'' claims, and Genesis is relegated to ''merely'' theological because it makes no scientific claims)?',
    'Rhetorical analysis of how the reading is invoked: does it position theology and science as peers in separate domains, or does it position science as the arbiter of what is ''real'' and theology as interpretation of what is ''meaningful'' (a subordinate category)? Comparison of how scientists speak about their work versus how theologians speak about theirs in institutional contexts.',
    'If the reading embeds science as superior authority, it is functionally a Snare for theology and theistic reading communities—they are told their reading is valid but implicitly relegated to meaning-making while science owns reality. If the reading genuinely separates domains as peers, it is Rope coordination. The extraction score (0.31) assumes a middle position; this omega would clarify whether extraction is higher (science dominance) or lower (genuine parity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(science_authority_asymmetry, conceptual, 'Whether the constraint establishes peer magisteria or science-dominant hierarchy.').

omega_variable(
    theistic_evolution_exclusion,
    'Is the absence of theistic-evolutionary readings from the primary institutional articulation of this constraint accidental (they simply have less institutional presence) or structural (the allegorical reading actively forecloses theistic-evolution as a legitimate third option)?',
    'Historical analysis of biblical scholarship discourse: are theistic-evolutionist readings cited as legitimate alternatives or dismissed as compromise positions? Do universities and seminaries teach all three readings as live options or present the allegorical and literal readings as the primary poles?',
    'If exclusion is structural, the constraint is narrower than it appears—it eliminates a third reading and polices hermeneutical options more than it appears to. If exclusion is accidental, the constraint is more open than characterized; theistic-evolution is simply less institutionally visible, not forbidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theistic_evolution_exclusion, empirical, 'Whether the constraint forecloses theistic-evolutionary readings or merely marginalizes them.').

omega_variable(
    commission_kernel_contest,
    'Is this reading truly one reading of a single contested kernel (genesis_creation_narrative), or are these three fundamentally DIFFERENT constraints masquerading as alternative readings of one text because they operate under different ε-invariance conditions?',
    'Structural comparison: do all three readings make claims about THE SAME CONSTRAINT (the text Genesis 1-2 as a cultural/religious artifact), or do they each construct a different object (Genesis as scientific chronicle vs. Genesis as theological metaphor vs. Genesis as compatible-with-science framework)? If the objects differ, apply the ε-invariance principle: write three separate constraint stories, not three readings of one story.',
    'If ε truly varies across readings such that the same measurement would yield different extractiveness values, these are not readings but different constraints. The current authoring treats them as readings of one kernel; if the ε-invariance test fails, the kernel frame collapses and each reading should be its own standalone story (not linked as readings but as a constraint family via network.affects_constraints).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commission_kernel_contest, conceptual, 'Whether the three readings are genuinely readings of one kernel or structurally distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gene_tr_t0, projected).
narrative_ontology:measurement(gene_tr_t8, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 8, 0.1).
narrative_ontology:measurement_basis(gene_tr_t8, observed).
narrative_ontology:measurement(gene_tr_t16, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(gene_tr_t16, observed).
narrative_ontology:measurement(gene_tr_t24, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 24, 0.15).
narrative_ontology:measurement_basis(gene_tr_t24, observed).
narrative_ontology:measurement(gene_tr_t32, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 32, 0.17).
narrative_ontology:measurement_basis(gene_tr_t32, observed).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(gene_tr_t40, observed).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(gene_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(gene_be_t0, projected).
narrative_ontology:measurement(gene_be_t8, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(gene_be_t8, observed).
narrative_ontology:measurement(gene_be_t16, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(gene_be_t16, observed).
narrative_ontology:measurement(gene_be_t24, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 24, 0.29).
narrative_ontology:measurement_basis(gene_be_t24, observed).
narrative_ontology:measurement(gene_be_t32, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 32, 0.3).
narrative_ontology:measurement_basis(gene_be_t32, observed).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(gene_be_t40, observed).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(gene_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(gene_su_t0, projected).
narrative_ontology:measurement(gene_su_t8, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(gene_su_t8, observed).
narrative_ontology:measurement(gene_su_t16, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 16, 0.21).
narrative_ontology:measurement_basis(gene_su_t16, observed).
narrative_ontology:measurement(gene_su_t24, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 24, 0.24).
narrative_ontology:measurement_basis(gene_su_t24, observed).
narrative_ontology:measurement(gene_su_t32, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 32, 0.27).
narrative_ontology:measurement_basis(gene_su_t32, observed).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(gene_su_t40, observed).
narrative_ontology:measurement(gene_su_t50, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(gene_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel genesis_creation_narrative. The kernel has three instantiations: allegorical_ancient_near_east (this story), literal_young_earth, and theistic_evolutionary. Each reading declares a different structural relationship to the text Genesis 1-2 and makes different claims about the text's epistemic authority. The three readings are linked via network.affects_constraints because they are sibling readings of a single kernel; they are not separate constraint families but rather three incompatible truth-claims about how the same text should be read. See cs_structure.reading_relations for the formal logical relationships among them (forecloses, coexists_with, influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
