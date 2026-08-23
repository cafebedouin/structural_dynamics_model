% ============================================================================
% CONSTRAINT STORY: john_1_1_logos__orthodox_christological
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_john_1_1_logos__orthodox_christological, []).

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
 *   constraint_id: john_1_1_logos__orthodox_christological
 *   human_readable: Nicene Boundary on the Johannine Logos (Orthodox Christological Reading)
 *   domain: theology/christology/biblical_hermeneutics
 *
 * SUMMARY:
 *   The orthodox christological reading of John 1:1-18 operates, once
 *   institutionally adopted, as a boundary on Christian belief and belonging:
 *   the Logos is confessed as ontologically divine, preexistent,
 *   consubstantial with the Father, and incarnate in 1:14. The confession
 *   genuinely coordinates the identity, worship, and sacramental life of
 *   roughly two billion believers; the same structure extracts from
 *   non-Trinitarian readers, who bear anathema, exclusion from communion and
 *   ecumenical bodies, denial of recognition, and - from the late fourth
 *   century to the early modern period - confiscation, exile, and death.
 *   Enforcement rose with establishment, peaked in the confessional age, and
 *   decayed after disestablishment into ecclesial and social exclusion. This
 *   file instantiates ONE reading of the john_1_1_logos kernel; the
 *   subordinationist and non-incarnational-monotheist readings are separate
 *   constraints with their own epsilon values, victim sets, and histories,
 *   linked through network.affects_constraints. The claim/metric gap is
 *   deliberate: the reading CLAIMS the status of guarded revealed truth while
 *   the authored metrics describe the boundary's actual operation, including
 *   its costs to dissenters - the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - episcopal_hierarchy: Primary agenda-setter (institutional/identity_locked) - defines, administers, and enforces the rule of faith
 *   - - creedal_denominations: Institutional beneficiary (institutional/constrained) - constituted by the shared confession
 *   - - ordained_sacramental_clergy: Beneficiary with fused identity (organized/identity_locked) - sacramental commission derives from the incarnation claim
 *   - - academic_trinitarian_theologians: Secondary beneficiary (moderate/constrained) - careers and curricula ride on the settlement
 *   - - orthodox_laity: Broad beneficiary (moderate/identity_locked) - receives identity and belonging, funds the institutions, absorbs division costs
 *   - - nontrinitarian_christians: Principal target (organized/constrained) - bears exclusion, anathema, and recognition-denial
 *   - - arian_communities: Historical target (organized/trapped) - suppressed by imperial law after 381
 *   - - early_modern_antitrinitarians: Historical target (powerless/trapped) - executed and expelled in the 16th-17th centuries
 *   - - secular_states: Former co-enforcer, now abstainer (institutional/arbitrage)
 *   - - seminary_dissenters: Excluded voice (powerless/constrained) - objects within institutions that require signed assent
 *   - - doctrine_historians: Analytical observer (analytical/analytical) - sees the full construction history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(john_1_1_logos__orthodox_christological, 0.42).
domain_priors:suppression_score(john_1_1_logos__orthodox_christological, 0.3).
domain_priors:theater_ratio(john_1_1_logos__orthodox_christological, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, extractiveness, 0.42).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(john_1_1_logos__orthodox_christological, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(john_1_1_logos__orthodox_christological, tangled_rope).
narrative_ontology:human_readable(john_1_1_logos__orthodox_christological, "Nicene Boundary on the Johannine Logos (Orthodox Christological Reading)").
narrative_ontology:topic_domain(john_1_1_logos__orthodox_christological, "theology/christology/biblical_hermeneutics").

domain_priors:requires_active_enforcement(john_1_1_logos__orthodox_christological).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(john_1_1_logos__orthodox_christological, '6728e56d-58a0-4cdc-8b33-c7e5f37b96e5').
narrative_ontology:cs_kernel_codification('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', fixed_text).
narrative_ontology:cs_authority_grounding('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', lineage).
narrative_ontology:cs_interpretation_layer_present('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5').
narrative_ontology:cs_reading_relation('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', john_1_1_logos__subordinationist, forecloses).
narrative_ontology:cs_reading_relation('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', john_1_1_logos__non_incarnational_monotheist, forecloses).
narrative_ontology:cs_axiom('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', foundational, logos_consubstantial_with_father).
narrative_ontology:cs_axiom_status(logos_consubstantial_with_father, holdable).
narrative_ontology:cs_axiom_grounding('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', logos_consubstantial_with_father, theological).
narrative_ontology:cs_axiom('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', foundational, incarnation_is_god_become_flesh).
narrative_ontology:cs_axiom_status(incarnation_is_god_become_flesh, holdable).
narrative_ontology:cs_axiom_grounding('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', incarnation_is_god_become_flesh, theological).
narrative_ontology:cs_axiom('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', secondary, sacramental_efficacy_rests_on_incarnation).
narrative_ontology:cs_axiom_status(sacramental_efficacy_rests_on_incarnation, holdable).
narrative_ontology:cs_axiom_grounding('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', sacramental_efficacy_rests_on_incarnation, theological).
narrative_ontology:cs_reference_frame('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', nicene_incarnational_frame).
narrative_ontology:cs_drift_state('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', historical_critical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6728e56d-58a0-4cdc-8b33-c7e5f37b96e5', '').
narrative_ontology:cs_kernel_id(john_1_1_logos__orthodox_christological, john_1_1_logos).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, creedal_denominations).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, ordained_sacramental_clergy).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, academic_trinitarian_theologians).
narrative_ontology:constraint_beneficiary(john_1_1_logos__orthodox_christological, orthodox_laity).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, nontrinitarian_christians).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, arian_communities).
narrative_ontology:constraint_victim(john_1_1_logos__orthodox_christological, early_modern_antitrinitarians).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, homoousion_consubstantiality).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, eternal_generation_of_the_son).
narrative_ontology:constraint_vindicates(john_1_1_logos__orthodox_christological, incarnational_sacramental_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, councils, and magisterial offices define the rule of faith, convene synods, draft anathemas, and decide who may teach, celebrate, and commune. The office's authority grew with the settlement it guards: presiding over the boundary made conciliar judgment the church's highest court. Leaving the office means leaving the institution that constitutes the vocation.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, episcopal_hierarchy, agenda_setter,
    institutional, generational, identity_locked, global).

% Catholic, Orthodox, and historic Protestant communions are constituted by shared confession of the Nicene formula. The confession gives them a common identity across languages and centuries, a basis for mutual recognition of orders and sacraments, and the membership criterion for joint bodies. Renouncing it would dissolve the bond that makes them one communion rather than a loose federation.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, creedal_denominations, beneficiary,
    institutional, generational, constrained, global).

% Priests and pastors act in the person of Christ at the altar; their commission to celebrate rests on the claim that God personally entered flesh and matter. Livelihood, standing, and self-understanding are bound to the doctrine; departure typically ends the vocation, the community ties, and the identity formed around it.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, ordained_sacramental_clergy, beneficiary,
    organized, biographical, identity_locked, global).

% Scholars in confessional faculties and seminaries build research programs, curricula, and careers explicating the classical doctrine. Chairs, journals, and conference circuits presuppose the settlement; publishing against it costs position and standing within the guild.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, academic_trinitarian_theologians, beneficiary,
    moderate, biographical, constrained, global).

% Ordinary members receive belonging, a named identity, liturgical continuity, and assurance that their worship addresses the true God. They also fund the institutions through giving and volunteer labor, and absorb the practical costs of division the boundary produces: split families, severed friendships, and a fragmented denominational landscape.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, orthodox_laity, beneficiary,
    moderate, biographical, identity_locked, global).

% Unitarians, Christadelphians, Jehovah's Witnesses, Oneness Pentecostals, and Latter-day Saints confess readings the creed rejects. Mainstream communions bar them from communion, often decline to recognize their baptisms or ordinations, exclude their bodies from ecumenical councils, and in common speech deny them the name Christian. Penalties ran to confiscation, exile, and death in earlier eras; today they are ecclesial and social. Forming their own congregations is open to them, at the price of permanent separation from the majority's fellowship, facilities, and resources.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, nontrinitarian_christians, payer,
    organized, generational, constrained, global).

% Fourth-century followers of Arius and allied bishops taught the Son to be the first and highest creature. After Nicaea and again after Constantinople 381, imperial law stripped their churches, exiled their clergy, and banned their assemblies. Under Constantius II the tide briefly reversed; Gothic kingdoms carried the reading for another two centuries before absorption or conquest ended it.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, arian_communities, payer,
    organized, generational, trapped, regional).

% Sixteenth- and seventeenth-century anti-Trinitarians such as Michael Servetus, the Polish Brethren, and the Socinians printed arguments that the Son is not consubstantial with the Father. Servetus was burned in Geneva in 1553; the Polish Brethren were expelled from Poland in 1658; Italian and Swiss dissenters fled or recanted. Presses, university posts, and safe passage were closed to them.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, early_modern_antitrinitarians, payer,
    powerless, biographical, trapped, continental).

% Imperial and royal governments converted conciliar rulings into civil law: the edicts of Theodosius made Nicene confession the empire's legal standard, medieval crowns executed heresy statutes, and Geneva's council burned Servetus. Since the disestablishment era most states have withdrawn from doctrinal enforcement altogether, leaving the boundary to the churches; a few retain blasphemy or apostasy laws that touch it obliquely.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, secular_states, agenda_setter,
    institutional, generational, arbitrage, continental).

% Students and junior faculty inside confessional institutions who privately read the Prologue with subordinationist or non-incarnational eyes. Speaking the objection aloud forfeits admission, ordination tracks, or employment; statements of faith are signed, not discussed. Their objections surface only anonymously or after departure.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, seminary_dissenters, excluded,
    powerless, biographical, constrained, national).

% Historians of doctrine study how the Prologue was read before, during, and after the fourth-century settlements: Wisdom-tradition backgrounds, Arius's sources, the politics of Nicaea. They take testimony from all parties and hold no communion seat; their analyses feed both confessional and dissenting accounts.
narrative_ontology:constraint_stakeholder(john_1_1_logos__orthodox_christological, doctrine_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(john_1_1_logos__orthodox_christological, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(john_1_1_logos__orthodox_christological, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The confession solves a real collective-action problem for a dispersed, multilingual movement: it supplies a single shared identity marker, stabilizes worship and scriptural reading practice, enables mutual recognition of orders and sacraments across autonomous congregations, and gives joint bodies a membership criterion.
% TRANSFER_FUNCTION: Moves recognition and status (the name Christian, communion access, valid orders, ecumenical membership, and historically civil toleration) from those who confess the Nicene formula toward confessors and away from non-confessors; also moves material support (tithes, endowments, institutional funding) toward the bodies that administer the boundary.
% ABSENT_VOICES: Subordinationist and non-incarnational readers inside confessional institutions (seminarians, junior faculty, doubting laity) cannot speak without forfeiting position or membership; historically, Arian voices were silenced by imperial edict rather than answered. Their objections live on in the sibling readings' communities, outside the room where the boundary is administered.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, ecumenical structures would lose their membership criterion and reorganize, denominational identities built on the confession would shift, sacramental theology would lose its grounding claim, and non-Trinitarian movements would press for re-entry into mainstream communion and recognition. The largest coordinated religious identity in the world would rearrange around a different or absent marker.
% FOUNDING_PROBLEM: The fourth-century crisis of the Logos: competing readings of the Prologue threatened the church's cohesion, and once the empire aligned with the church, imperial unity rode on the answer. Nicaea was convened to settle whether the Son is co-eternal with the Father, fixing a rule of faith against subordinationist readings and defining the object of Christian worship.
% FOUNDING_PROBLEM_CORROBORATION: Historians of late antiquity working outside confessional institutions corroborate that the founding problem was a real cohesion-and-governance dispute, not a retroactive invention. Surviving non-Trinitarian communities attest the problem is live, treating the Nicene settlement as a wrong answer rather than a resolution; the settlement's finality is attested only by the benefiting parties themselves.
narrative_ontology:disappearance_verdict(john_1_1_logos__orthodox_christological, world_rearranges).
narrative_ontology:founding_problem_status(john_1_1_logos__orthodox_christological, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(john_1_1_logos__orthodox_christological, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(john_1_1_logos__orthodox_christological, 'none', 1).
narrative_ontology:epsilon_provenance(john_1_1_logos__orthodox_christological, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(john_1_1_logos__orthodox_christological_tests).
:- end_tests(john_1_1_logos__orthodox_christological_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 for the current arrangement: the boundary's present costs fall on non-Trinitarians as exclusion, recognition-denial, and institutional barring, real but no longer civil or lethal in most jurisdictions; the temporal series shows the historical peak (0.88 at 1553, the confessional age of burnings and expulsions) and the post-disestablishment decline. Suppression is 0.30: enforcement is now ecclesial discipline and social pressure, the residue of a machinery that once ran through imperial edict and heresy statute. Theater ratio is 0.55: in mainline contexts creedal recitation is substantially ritualized - the congregation affirms homoousios without engaging it - while in live-dispute contexts (evangelical engagement with non-Trinitarian movements, Global South confessionalism) the boundary remains functional; the series dips at 1553 when the dispute was live again. Accessibility collapse is 0.45: alternative readings are foreclosed inside confessional institutions (signed statements of faith, employment and ordination gates) but persist openly outside them (Unitarian bodies, Jehovah's Witnesses, Christadelphians), so alternatives are narrowed, not eliminated. Resistance is 0.55: organized dissenting movements endure, and intra-institutional dissent persists privately; coalition power among the targeted groups is limited because they are theologically opposed to one another as much as to the creed. Claim and metrics are independent authored facts: the tangled_rope claim rests on the presence of BOTH a genuine coordination function (identity, worship, sacramental coherence) AND asymmetric extraction with active enforcement, not on tuning to a predicted output. All three tracked metrics share one time grid (325-2025, eight points) so the engine samples a complete row at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergently. From the episcopal seat the arrangement reads as guardianship of revealed truth and the price of communal coherence; from the non-Trinitarian seats the same structure reads as enforced exclusion with a lethal historical record; from the historian's seat it reads as a contingent fourth-century political settlement that acquired sacral immutability. Clergy and laity sit between: sincere conviction fused with institutional dependence. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate which experience is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality for the hierarchy, denominations, clergy, theologians, and laity; the laity sit slightly higher than the other beneficiaries because they also fund the institutions and absorb the division costs, but remain net beneficiaries. Victim declarations map the non-Trinitarian class, the suppressed Arian communities, and the executed/expelled early modern anti-Trinitarians toward the full-target end, with the trapped historical classes nearest it - they had no exit at all. Secular states are the ambiguous seat: historically they collected enforcement legitimacy alongside the hierarchy, but modern disestablishment moved them toward symmetry; the derivation from their agenda_setter role and arbitrage exit handles this without an override, and no directionality_overrides are authored because the beneficiary/victim-plus-exit derivation already differentiates the seats correctly (power-atom-keyed overrides would collide here, since beneficiaries and victims share power levels). Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, and the boundary's global scope modestly amplifies effective extraction by making verification of 'sufficient' confession harder across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. A pure-rope label would erase the documented victim class - anathema, executions, expulsions are not coordination overhead. A pure-snare label would erase the genuine, voluntarily affirmed coordination that sustains the world's largest religious identity for believers who pay nothing and lose nothing by the boundary. The R5 interview shows the founding problem contested rather than dead: the orthodox parties hold it settled, the surviving dissenting communities hold it mis-settled, so no dead-mandate zombie signature fires, and the boundary still actively governs who may teach, commune, and belong. The watch condition is drift toward piton: if enforcement keeps decaying while conviction thins, the boundary persists as theatrical recitation - the theater_ratio series (0.08 to 0.55) tracks exactly that transition, and the persistence omega below names the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    john_1_1_kernel_reading_commitment,
    'This constraint is one reading of the john_1_1_logos kernel: does the Prologue, read on its own terms, identify the Logos as a preexistent divine hypostasis who becomes flesh (this reading), as a created subordinate agent (subordinationist), or as figurative wisdom-language (non_incarnational_monotheist) - and where exactly does the text underdetermine the choice?',
    'Historical-critical reconstruction of the Prologue''s composition (Wisdom-tradition background, Johannine community setting, reception history before Nicaea); no confessional adjudication can settle the question for parties outside that tradition.',
    'Resolution toward a sibling reading dissolves this constraint''s victim structure (anathema loses its object) and removes the incarnation ground of sacramental authority; the boundary''s enforcement rationale collapses with the reading that grounds it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(john_1_1_kernel_reading_commitment, conceptual, 'Committer structure: which reading the kernel text itself supports, and where the readings diverge.').

omega_variable(
    persistence_conviction_vs_inertia,
    'Is the boundary sustained primarily by living conviction or by institutional inertia and career/discipline structures?',
    'Compare belief retention and boundary salience across voluntary-affiliation and established-church contexts; track whether boundary enforcement persists where sanctions are absent.',
    'If inertia dominates, the constraint drifts piton-ward as enforcement decays - creed recited, boundary unfelt; if conviction dominates, the tangled_rope classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_conviction_vs_inertia, empirical, 'Conviction versus inertia as the persistence mechanism behind the boundary.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissent structural (institutional discipline, exclusion, historical penalties) or internalized (identity fusion formed by lifelong doctrinal formation, fear of lost belonging and salvation)?',
    'Post-exit trajectory of former members and clergy: if self-censorship and fear persist after leaving enforcement reach, the internalized share is substantial.',
    'Internalized suppression keeps the boundary effective after formal enforcement ends, raising effective suppression above the structural measure and slowing decay of the victim class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a doctrinal community.').

omega_variable(
    identity_framing_extraction_cover,
    'Does the identity-coordination framing of the confession launder exclusionary enforcement - is the coordination benefit separable from the anathema?',
    'Examine communions and federations that relaxed or dropped the boundary while retaining shared worship structures: if cohesion persists without exclusion, the coordination function is separable from the extraction.',
    'If separable, the exclusion component is pure extraction riding on genuine identity coordination, confirming tangled_rope with the extraction side dominant; if inseparable, part of the measured extraction is the constitutive price of the boundary itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_framing_extraction_cover, empirical, 'Guard against identity-framing cover: whether the coordination function requires the exclusionary enforcement.').

omega_variable(
    victim_class_temporal_boundaries,
    'Who counts as the constraint''s victims now that civil enforcement has lapsed in most jurisdictions - only groups under active ecclesial sanction, or all non-Trinitarians under soft exclusion and recognition-denial?',
    'Comparative mapping of enforcement intensity by era and jurisdiction: baptism recognition, communion access, ecumenical membership, employment in confessional institutions.',
    'A narrow victim class lowers current effective extraction toward rope-like coordination; a broad class keeps extraction substantial and the tangled_rope asymmetry load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_class_temporal_boundaries, conceptual, 'Temporal and jurisdictional bounds of the victim class under soft enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(john_1_1_logos__orthodox_christological, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(john_tr_t325, john_1_1_logos__orthodox_christological, theater_ratio, 325, 0.08).
narrative_ontology:measurement(john_tr_t381, john_1_1_logos__orthodox_christological, theater_ratio, 381, 0.12).
narrative_ontology:measurement(john_tr_t600, john_1_1_logos__orthodox_christological, theater_ratio, 600, 0.18).
narrative_ontology:measurement(john_tr_t1000, john_1_1_logos__orthodox_christological, theater_ratio, 1000, 0.28).
narrative_ontology:measurement(john_tr_t1300, john_1_1_logos__orthodox_christological, theater_ratio, 1300, 0.36).
narrative_ontology:measurement(john_tr_t1553, john_1_1_logos__orthodox_christological, theater_ratio, 1553, 0.26).
narrative_ontology:measurement(john_tr_t1800, john_1_1_logos__orthodox_christological, theater_ratio, 1800, 0.44).
narrative_ontology:measurement(john_tr_t2025, john_1_1_logos__orthodox_christological, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(john_be_t325, john_1_1_logos__orthodox_christological, base_extractiveness, 325, 0.4).
narrative_ontology:measurement(john_be_t381, john_1_1_logos__orthodox_christological, base_extractiveness, 381, 0.52).
narrative_ontology:measurement(john_be_t600, john_1_1_logos__orthodox_christological, base_extractiveness, 600, 0.63).
narrative_ontology:measurement(john_be_t1000, john_1_1_logos__orthodox_christological, base_extractiveness, 1000, 0.74).
narrative_ontology:measurement(john_be_t1300, john_1_1_logos__orthodox_christological, base_extractiveness, 1300, 0.83).
narrative_ontology:measurement(john_be_t1553, john_1_1_logos__orthodox_christological, base_extractiveness, 1553, 0.88).
narrative_ontology:measurement(john_be_t1800, john_1_1_logos__orthodox_christological, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(john_be_t2025, john_1_1_logos__orthodox_christological, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(john_su_t325, john_1_1_logos__orthodox_christological, suppression_requirement, 325, 0.42).
narrative_ontology:measurement(john_su_t381, john_1_1_logos__orthodox_christological, suppression_requirement, 381, 0.56).
narrative_ontology:measurement(john_su_t600, john_1_1_logos__orthodox_christological, suppression_requirement, 600, 0.66).
narrative_ontology:measurement(john_su_t1000, john_1_1_logos__orthodox_christological, suppression_requirement, 1000, 0.74).
narrative_ontology:measurement(john_su_t1300, john_1_1_logos__orthodox_christological, suppression_requirement, 1300, 0.82).
narrative_ontology:measurement(john_su_t1553, john_1_1_logos__orthodox_christological, suppression_requirement, 1553, 0.86).
narrative_ontology:measurement(john_su_t1800, john_1_1_logos__orthodox_christological, suppression_requirement, 1800, 0.48).
narrative_ontology:measurement(john_su_t2025, john_1_1_logos__orthodox_christological, suppression_requirement, 2025, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(john_1_1_logos__orthodox_christological, identity_coordination).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__subordinationist).
narrative_ontology:affects_constraint(john_1_1_logos__orthodox_christological, john_1_1_logos__non_incarnational_monotheist).

% DUAL FORMULATION NOTE:
% The colloquial question 'what does John 1:1 teach?' decomposes into three structurally distinct constraints - one per reading of the kernel - because measuring the Prologue's claim through the orthodox exegetical frame yields a different epsilon, victim set, and enforcement history than measuring it through the subordinationist or non-incarnational frames. The orthodox reading is institutionally upstream: its anathemas and membership criteria shaped the resource environment (churches, presses, academic posts) in which the sibling readings survived marginally. Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
