% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Genesis 1-2 Read as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This story authors ONE reading within the Genesis creation kernel
 *   contest: Genesis 1-2 understood as Ancient Near Eastern mythopoetic
 *   literature, structurally parallel to Enuma Elish and Atrahasis, making no
 *   historical-scientific claims about cosmological or biological origins.
 *   Under this reading the text has zero adjudicative authority over geology,
 *   cosmology, or evolutionary biology; the six-day structure is read as a
 *   liturgical/theological schema (a 'temple inauguration' or polemical
 *   cosmology asserting monotheism against Mesopotamian polytheism) rather
 *   than a chronological claim; and the dominion mandate (Gen 1:28) is read
 *   as ancient royal-ideology language transposed onto humanity generally,
 *   carrying theological but not scientific or environmental-policy normative
 *   force in any direct sense. This reading does not describe or average over
 *   the sibling readings (literal_young_earth, theistic_evolutionary) — each
 *   is a separate constraint with its own epsilon, beneficiaries, and
 *   victims, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - critical_biblical_scholars: institutional agenda-setters who administer the reading through seminary curricula and peer review
 *   - science_compatible_clergy: beneficiaries who use the reading to avoid science/faith conflict in congregational life
 *   - interfaith_dialogue_institutions: beneficiaries who need a non-literal text for ecumenical bridging
 *   - biblical_literalist_congregants: payers whose historicist faith formation is institutionally displaced
 *   - denominational_minorities_favoring_inerrancy: powerless payers with the least institutional recourse
 *   - young_earth_creationist_organizations: excluded contestants pushed to fringe institutional status
 *   - religious_studies_observers: analytical seat studying the contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.42).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.38).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.42).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 Read as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '6713f483-a0f2-4a7d-93c3-376d4cab10b8').
narrative_ontology:cs_kernel_codification('6713f483-a0f2-4a7d-93c3-376d4cab10b8', fixed_text).
narrative_ontology:cs_authority_grounding('6713f483-a0f2-4a7d-93c3-376d4cab10b8', expertise).
narrative_ontology:cs_interpretation_layer_present('6713f483-a0f2-4a7d-93c3-376d4cab10b8').
narrative_ontology:cs_reading_relation('6713f483-a0f2-4a7d-93c3-376d4cab10b8', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('6713f483-a0f2-4a7d-93c3-376d4cab10b8', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('6713f483-a0f2-4a7d-93c3-376d4cab10b8', foundational, text_has_no_cosmological_adjudicative_authority).
narrative_ontology:cs_axiom_status(text_has_no_cosmological_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('6713f483-a0f2-4a7d-93c3-376d4cab10b8', text_has_no_cosmological_adjudicative_authority, conventional).
narrative_ontology:cs_axiom('6713f483-a0f2-4a7d-93c3-376d4cab10b8', foundational, genre_determines_truth_conditions_not_literal_sequence).
narrative_ontology:cs_axiom_status(genre_determines_truth_conditions_not_literal_sequence, holdable).
narrative_ontology:cs_axiom_grounding('6713f483-a0f2-4a7d-93c3-376d4cab10b8', genre_determines_truth_conditions_not_literal_sequence, empirically_contingent).
narrative_ontology:cs_axiom('6713f483-a0f2-4a7d-93c3-376d4cab10b8', secondary, dominion_mandate_is_ancient_royal_ideology_not_direct_policy_norm).
narrative_ontology:cs_axiom_status(dominion_mandate_is_ancient_royal_ideology_not_direct_policy_norm, holdable).
narrative_ontology:cs_axiom_grounding('6713f483-a0f2-4a7d-93c3-376d4cab10b8', dominion_mandate_is_ancient_royal_ideology_not_direct_policy_norm, conventional).
narrative_ontology:cs_reference_frame('6713f483-a0f2-4a7d-93c3-376d4cab10b8', historical_critical_comparative_ane_scholarship).
narrative_ontology:cs_drift_state('6713f483-a0f2-4a7d-93c3-376d4cab10b8', post_dead_sea_scrolls_and_ugaritic_discovery_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('6713f483-a0f2-4a7d-93c3-376d4cab10b8', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_compatible_clergy).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, interfaith_dialogue_institutions).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, biblical_literalist_congregants).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, denominational_minorities_favoring_inerrancy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish and teach the comparative-mythology reading, placing Genesis alongside Enuma Elish and the Atrahasis epic. Control seminary curricula, peer-reviewed journals, and academic biblical studies departments. Their professional standing and institutional funding depend on this reading being treated as the scholarly consensus.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, beneficiary).

% Preach and teach this reading to reconcile congregational faith with mainstream science education, avoiding conflict with members who are scientists or have scientifically-educated children. Gain credibility with educated, urban, or academically-adjacent congregations by adopting this frame.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_compatible_clergy, beneficiary,
    organized, biographical, mobile, national).

% Use the allegorical reading as a bridge for ecumenical and interfaith conversation, since a non-literal text is easier to harmonize across traditions and with secular academic partners. Their funding and legitimacy partly depend on the text being treated as compatible with pluralism.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, interfaith_dialogue_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Hold that Genesis records historical events; when denominational leadership or seminary-trained clergy adopt the allegorical frame, their reading is displaced from institutional teaching, textbooks, and sermons. Exit means leaving the denomination or forming breakaway congregations, at real social and familial cost.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, biblical_literalist_congregants, payer,
    moderate, biographical, constrained, national).

% Smaller, often rural or working-class congregations that hold inerrantist convictions but lack the institutional power to contest curricular or denominational shifts toward the allegorical reading. They experience the reading's ascendance as their tradition being overridden by academically credentialed elites, with little recourse beyond exit or silent dissent.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, denominational_minorities_favoring_inerrancy, payer,
    powerless, biographical, trapped, regional).

% Actively contest the allegorical reading in publications and legal advocacy but are largely excluded from mainstream biblical scholarship and science education institutions; their arguments are treated as fringe within the academy this reading dominates.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationist_organizations, excluded,
    organized, biographical, constrained, national).

% Study the sociology and history of the interpretive contest itself, analyzing how each reading gains or loses institutional footing without personally holding a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, diffuse).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive frame that lets religious communities retain the text's theological and liturgical value while avoiding direct contradiction with geology, cosmology, and evolutionary biology — solving the coordination problem of reconciling faith communities with modern science education and public discourse.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy away from literalist clergy, denominational structures, and lay traditions that hold historical readings, and toward academically credentialed scholars, seminary faculties, and clergy trained in historical-critical method; also moves social and identity costs onto congregants whose faith formation assumed historicity.
% ABSENT_VOICES: Literalist congregants in smaller or less-resourced denominations rarely have seats on seminary curriculum committees, academic hiring panels, or interfaith dialogue boards where this reading is adopted as normative; their objections surface mainly in separate, often disparaged institutional channels (creationist organizations, independent congregations).
% DISAPPEARANCE_RATIONALE: If the allegorical reading vanished from mainstream institutions overnight, seminaries and academic biblical studies would lose a settled frame for teaching Genesis without reopening conflict with science education; literalist communities would regard this as restoration rather than loss. Scholars and interfaith bodies would experience real institutional disruption; literalist communities would experience vindication. The two constituencies genuinely disagree about whether the world would rearrange or merely correct itself.
% FOUNDING_PROBLEM: The historical-critical method emerged to address genuine philological and archaeological findings — the discovery of older Mesopotamian creation texts with parallel structure, and the geological/biological evidence undermining a young-earth chronology — that made a straightforwardly historical reading of Genesis 1-2 increasingly difficult to sustain within an intellectually credible institutional framework.
% FOUNDING_PROBLEM_CORROBORATION: Archaeologists and comparative-literature scholars outside any faith tradition (e.g. Assyriologists working on Enuma Elish and Atrahasis independent of theological commitment) corroborate the textual parallels that motivate the mythopoetic reading. Geologists and evolutionary biologists, also outside the interpretive dispute, corroborate the founding empirical problem (young-earth chronology's incompatibility with the physical record). No outside corroboration exists for the further claim that the allegorical reading is the ONLY theologically legitimate response to that evidence — that step is asserted primarily by the benefiting scholarly and clerical class itself.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, contested).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) because the reading genuinely solves a coordination problem (reconciling faith communities with mainstream science) but does so by displacing an entrenched alternative reading and imposing real costs on communities whose theological identity depended on historicity. Suppression is comparably moderate (0.38): enforcement runs through institutional gatekeeping (seminary hiring, curriculum committees, credentialing) rather than coercive force, but it is real and growing as academic biblical studies has professionalized. Theater ratio rose modestly (0.10 to 0.30) as the reading's institutional entrenchment increasingly generates performative signaling (denominational statements affirming 'compatibility with science') independent of the underlying textual-critical work. Accessibility collapse is moderate (0.35): a literalist reading remains available and practiced but is increasingly marginalized in credentialed institutional spaces. Resistance is substantial (0.55) because literalist communities and young-earth organizations actively contest the reading in publications, legal advocacy, and separate institution-building.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (critical_biblical_scholars), this reading is coordination: a defensible historical-critical consensus resolving a real interpretive problem. From the payer seat (biblical_literalist_congregants, denominational_minorities_favoring_inerrancy), the same institutional dominance is experienced as extraction of interpretive authority and displacement of an inherited faith tradition, imposed by academically credentialed elites who control seminary and denominational gatekeeping. The engine computes these divergent seat classifications from the structural power/exit data; the claim (tangled_rope) is authored independently of any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical biblical scholars and interfaith institutions sit near the full-beneficiary end: they set the interpretive agenda, control credentialing, and gain professional and institutional legitimacy from the reading's ascendance. Science-compatible clergy are secondary beneficiaries with more mobility (they can relocate to congregations receptive to the frame). Literalist congregants and especially denominational minorities favoring inerrancy sit near the full-target end: they bear the cost of displaced tradition with constrained or trapped exit (leaving a denomination carries real social and familial cost). Young-earth organizations are excluded rather than coordinated — they contest the reading from outside the institutions that adopted it, which is why they are marked excluded rather than payer: their conflict is with being locked out of legitimacy-granting institutions, not primarily a resource transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling textual-critical and scientific evidence with a credible institutional theology) remains genuinely live — the Mesopotamian parallels and the geological/biological evidence against young-earth chronology have not gone away. This distinguishes the constraint from a pure mandatrophy case where an institution persists after its founding problem is dead. However, the further claim that the allegorical reading is theologically obligatory (not merely one available response to the evidence) is where extraction concentrates: that stronger claim serves the professional and institutional interests of the reading's beneficiaries beyond what the founding evidentiary problem itself requires, which is why this reading computes as tangled_rope rather than pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_identification_certainty,
    'Is the ANE-mythopoetic genre identification (parallel structure to Enuma Elish/Atrahasis, temple-inauguration cosmology) itself a settled comparative-literature finding, or a contestable interpretive framework that could be revised by future textual or archaeological discovery?',
    'Continued comparative Semitic philology and Mesopotamian archaeology; convergence or divergence among scholars working outside any confessional commitment to a particular reading''s theological implications.',
    'If genre identification is robust and independent of theological motive, the reading''s coordination function is well-grounded; if genre identification is itself shaped by a prior commitment to compatibility with science, the reading''s extraction component is larger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_identification_certainty, empirical, 'Whether the ANE genre classification is independently secure or theologically motivated.').

omega_variable(
    obligatory_vs_permissible_reading,
    'Does the founding evidentiary problem (Mesopotamian parallels, geological/biological evidence) require adopting the allegorical reading specifically, or does it merely rule out strict literalism while leaving multiple non-literal readings (including theistic_evolutionary) equally available?',
    'Examine whether institutions that adopt the allegorical reading as normative can articulate a non-question-begging reason to prefer it over theistic_evolutionary, beyond institutional convenience or scholarly consensus-signaling.',
    'If the evidence only rules out literalism without specifically requiring this reading over theistic_evolutionary, then the institutional narrowing to this specific reading is where most of the extraction is concentrated, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligatory_vs_permissible_reading, conceptual, 'Whether this specific reading is evidentially required or merely one permissible option among several non-literal readings.').

omega_variable(
    kernel_committer_framing,
    'Is the underlying kernel best modeled as a single ambiguous text (Genesis 1-2) with three competing readings, or are the readings themselves symptomatic of three distinct interpretive communities each constructing their own effective ''kernel'' retroactively?',
    'Reception-history analysis: track whether the interpretive communities cite a shared textual kernel they disagree about, or whether each community''s reading practices have diverged so far that they are no longer functionally interpreting the same object.',
    'If the communities no longer share an effective kernel, the forecloses/coexists_with/influences relations authored here would need re-evaluation, since ''sibling readings of one kernel'' presumes a shared referent that reception-history divergence could undermine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_framing, conceptual, 'Whether the three readings genuinely share one kernel or have diverged into three effectively separate objects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 20, 0.15).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.2).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 60, 0.24).
narrative_ontology:measurement(gene_tr_t80, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 80, 0.28).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(gene_be_t80, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 80, 0.4).
narrative_ontology:measurement(gene_be_t100, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.3).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 60, 0.33).
narrative_ontology:measurement(gene_su_t80, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 80, 0.36).
narrative_ontology:measurement(gene_su_t100, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.1).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language label 'the Genesis creation narrative' per the epsilon-invariance principle: measuring the constraint's adjudicative claim over cosmology/biology yields radically different epsilon depending on which reading is in view (near-zero adjudicative claim here vs. strong claims under literal_young_earth). Each reading is authored as its own constraint with its own epsilon, beneficiaries, victims, and classification; the family is linked so contamination/legitimacy analysis can propagate across the kernel contest without conflating the readings into one averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
