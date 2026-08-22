% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__modalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__modalist_reading, []).

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
 *   constraint_id: biblical_divine_nature__modalist_reading
 *   human_readable: Modalist Christology: Sequential Divine Modes
 *   domain: theology/doctrinal
 *
 * SUMMARY:
 *   Modalism (modalismus, Sabellian modalism, modal-monism) is a reading of
 *   the biblical divine nature in which Father, Son, and Spirit are
 *   sequential modes or roles of a single divine subject rather than three
 *   simultaneous persons. The reading emerges in response to the founding
 *   problem: how to maintain strict monotheism (God is one) while honoring
 *   biblical evidence that Father, Son, and Spirit act as distinct agents and
 *   while sustaining Jesus-centered worship and agency-attribution. Modalism
 *   preserves both monotheism and full Jesus-divinity without invoking the
 *   philosophical substance-language (hypostasis, ousia) of later trinitarian
 *   councils. It is rejected by mainstream trinitarian theology as Sabellian
 *   heresy (subordinating the Son/Spirit's independent personhood to modal
 *   sequence) and by unitarian theology as insufficiently monotheistic
 *   (ascribing divine agency to the Son beyond creaturely function). The
 *   constraint story models modalist theology as a tangled rope: it
 *   coordinates genuine piety and exegetical adequacy (beneficiary function)
 *   while suppressing systematic philosophical coherence and mainstream
 *   trinitarian authority (extractive/suppressive function). Enforcement
 *   depends on maintaining textual and experiential credibility against both
 *   trinitarian philosophical apparatus and unitarian reduction.
 *
 * KEY AGENTS:
 *   - Modalist theologians: institutional seats; maintain the reading against conciliar trinitarianism and unitarian challenge; identity fused with reading coherence
 *   - Jesus-centered piety practitioners: moderate power; benefit from devotional access without philosophical apparatus; constrained exit (either trinitarian complexity or unitarian reduction)
 *   - Systematic philosophy adherents: powerful institutional seats; bear cost of modal inadequacy (simultaneous-action problem); high exit options (arbitrage to trinitarian language)
 *   - Subordinationist communities: organized but trapped; experience modalism as heretical disguise of subordinationism; constrained exit
 *   - Trinitarian councils: institutional agenda-setters (excluded); formally condemned modalism; cannot negotiate without abandoning conciliar settlement
 *   - Exegetical communities: analytical observers; assess Father/Son/Spirit language patterns; outside enforcement machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, 0.68).
domain_priors:suppression_score(biblical_divine_nature__modalist_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__modalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(biblical_divine_nature__modalist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__modalist_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__modalist_reading, "Modalist Christology: Sequential Divine Modes").
narrative_ontology:topic_domain(biblical_divine_nature__modalist_reading, "theology/doctrinal").

domain_priors:requires_active_enforcement(biblical_divine_nature__modalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__modalist_reading, '1a908371-33af-4ae2-bcbd-ba4b1880c58e').
narrative_ontology:cs_kernel_codification('1a908371-33af-4ae2-bcbd-ba4b1880c58e', fixed_text).
narrative_ontology:cs_authority_grounding('1a908371-33af-4ae2-bcbd-ba4b1880c58e', lineage).
narrative_ontology:cs_interpretation_layer_present('1a908371-33af-4ae2-bcbd-ba4b1880c58e').
narrative_ontology:cs_reading_relation('1a908371-33af-4ae2-bcbd-ba4b1880c58e', biblical_divine_nature__trinitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a908371-33af-4ae2-bcbd-ba4b1880c58e', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('1a908371-33af-4ae2-bcbd-ba4b1880c58e', foundational, divine_modes_not_simultaneous_persons).
narrative_ontology:cs_axiom_status(divine_modes_not_simultaneous_persons, holdable).
narrative_ontology:cs_axiom_grounding('1a908371-33af-4ae2-bcbd-ba4b1880c58e', divine_modes_not_simultaneous_persons, empirically_contingent).
narrative_ontology:cs_axiom('1a908371-33af-4ae2-bcbd-ba4b1880c58e', foundational, exegetical_priority_over_philosophical_apparatus).
narrative_ontology:cs_axiom_status(exegetical_priority_over_philosophical_apparatus, holdable).
narrative_ontology:cs_axiom_grounding('1a908371-33af-4ae2-bcbd-ba4b1880c58e', exegetical_priority_over_philosophical_apparatus, conventional).
narrative_ontology:cs_reference_frame('1a908371-33af-4ae2-bcbd-ba4b1880c58e', apostolic_narrative_sequence).
narrative_ontology:cs_drift_state('1a908371-33af-4ae2-bcbd-ba4b1880c58e', post_nicene_conciliar_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a908371-33af-4ae2-bcbd-ba4b1880c58e', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__modalist_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__modalist_reading, jesus_centered_piety_practitioners).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, systematic_philosophy_adherents).
narrative_ontology:constraint_victim(biblical_divine_nature__modalist_reading, subordinationist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defend and articulate the modalist reading against trinitarian and unitarian challengers. Maintain textual exegesis showing Father/Son/Spirit language as role-descriptions of a single divine subject across redemptive history. Professional identity and authority fused with the reading's coherence.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, modalist_theologians, agenda_setter,
    institutional, generational, identity_locked, regional).

% Experience Jesus as the full revelation and agency of God without needing to master philosophical hypostasis-language or reconcile apparent monotheism-violation. The modalist frame preserves direct devotional access while affirming biblical monotheism. Exit means adopting either trinitarian apparatus or unitarian reduction, both costly to piety-structure.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, jesus_centered_piety_practitioners, beneficiary,
    moderate, biographical, constrained, regional).

% Bear the cost of modalism's incompleteness: mode-language fails to resolve the simultaneous-action problem (how can Father and Son act at once if they are sequential modes?). Must either accept modal inadequacy or migrate to trinitarian substance-language. Their exit is philosophically easy but institutionally costly.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, systematic_philosophy_adherents, payer,
    powerful, generational, arbitrage, regional).

% Experience modalism as heretical subordination disguised as monotheism: if Father, Son, Spirit are modes of one person, the Son's agency is only apparent, not real. This denies both full trinitarian personhood and unitarian clarity. Trapped between modalist claims and their own coherence needs.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, subordinationist_communities, payer,
    organized, generational, constrained, regional).

% Formally condemned modalism (Sabellian heresy) as inadequate to scripture and logic. Their exclusion is structural: modalism's core claim (sequential modes, not simultaneous persons) directly contradicts the trinitarian hypostasis framework. Cannot negotiate with modalism without abandoning the conciliar settlement.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, trinitarian_councils, excluded,
    institutional, civilizational, trapped, continental).

% Study biblical language for Father/Son/Spirit across genres and contexts to assess whether role-language or person-language is primary. Their analysis affects credibility of modalist exegetical claims but they remain structurally outside the enforcement machinery.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__modalist_reading, exegetical_communities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__modalist_reading, modalist_theologians).
narrative_ontology:fixing_cost_class(biblical_divine_nature__modalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves monotheism while sustaining Jesus-centered worship and agency-attribution without invoking philosophical substance-duality. Coordinates strict monotheism, biblical narrative (Father acting in OT, Son in NT, Spirit in Acts onward), and devotional practices into one coherent reading without hypostatic apparatus.
% TRANSFER_FUNCTION: Moves credibility and institutional authority from philosophical coherence (systematic theology's main currency) to exegetical adequacy (biblical language's primary concern). Winners: practitioners who experience the piety-function directly. Losers: systematic theologians who must defend incomplete philosophical apparatus; subordinationists who see their position absorbed into a heretical framing.
% ABSENT_VOICES: Mainstream trinitarian institutional power (councils, bishops defending conciliar settlement) is structurally excluded by modalism's core claim. Unitarian reformers also excluded: they would argue modalism gives too much divinity to the Son while trinitarians give too much independence. Both see modalism as incoherent middle ground.
% DISAPPEARANCE_RATIONALE: Trinitarians would say the world rearranges—the heresy suppressed by councils returns and destabilizes conciliar Christology. Modalists would say the world remains unchanged—the biblical text continues to use mode-language whether or not the reading is institutionally authorized. The dispute is over what 'disappearance' of a doctrinal authority means.
% FOUNDING_PROBLEM: How to sustain monotheism (God is one) while honoring biblical evidence of Father, Son, Spirit as distinct agents and the lived practice of Jesus-worship without inventing philosophical categories foreign to scripture itself.
% FOUNDING_PROBLEM_CORROBORATION: Modern biblical scholars outside modalist circles attest the founding problem is live: the language of Father/Son/Spirit IS sequential/narrative in biblical genres, the apparent monotheism-violation IS textually present, and philosophical apparatus (hypostasis, ousia) IS absent from scripture. Modalists cite this exegetical consensus as vindication. Trinitarians attest the problem is *solved* by conciliar settlement; modalism rejects the solution, not the problem.
narrative_ontology:disappearance_verdict(biblical_divine_nature__modalist_reading, contested).
narrative_ontology:founding_problem_status(biblical_divine_nature__modalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__modalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__modalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__modalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__modalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__modalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__modalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (early modalism, exegetically plausible, low institutional cost) to 0.68 (later modalism, carrying the suppression cost of defending against both trinitarianism and unitarianism) over the interval 0–1800 (covering early church modalism through medieval recovery attempts to modern modalist theology). The rise models institutional entrenchment: as trinitarian conciliar authority hardened (Council of Nicaea onward), modalism's cost to maintain increased; exegetical adequacy alone could not offset the institutional exclusion. Theater ratio rises similarly (0.12 to 0.41), modeling the shift from substantive exegetical debate to performative reaffirmation within isolated modalist communities. Suppression requirement (0.42 to 0.72) models enforcement intensity: early modalism faced debate; later modalism faced heresy charge and institutional exclusion. The plateau after t=1200 reflects stabilization of the constraint at the level of sectarian communities outside mainstream trinitarian authority—no further rise but no collapse either. The shared time grid (t ∈ {0, 300, 600, 900, 1200, 1500, 1800}) ensures all three metrics measure the same interval.
 *
 * PERSPECTIVAL GAP:
 *   The modalist theologian (agenda-setter) and the piety practitioner (beneficiary) would compute similarly: both experience the reading as genuine coherence without unacceptable cost. The systematic philosopher and the trinitarian councilor would compute modalism as extractive: the reading forces them either to accept philosophical incompleteness or to migrate to trinitarian apparatus. The subordinationist would compute modalism as suppressive heresy. The engine computes these divergences from structural data (power atoms, exit options, role declarations); the authored claim (tangled_rope) already asserts a cost-bearing structure, so the perspectival gap is built into the type.
 *
 * DIRECTIONALITY LOGIC:
 *   Modalist theologians are the agenda-setters (d near 0.0, beneficiary end): they set the exegetical agenda, enforce textual reading standards, and maintain the reading's credibility. Jesus-centered practitioners benefit (d near 0.0): they gain devotional access and coherence. Systematic philosophers and subordinationists are targets (d near 1.0): they bear the cost of philosophical incompleteness or suppressed subordinationism. The trinitarian institutional exclusion (not a stake-holder role here but a structural constraint on exit) drives the suppression metric upward—modalists cannot simply leave the church; they must maintain the reading against institutional pressure. This is classic tangled-rope asymmetry: coordination function (exegetical coherence, piety-sustainability) rides on an extractive structure (suppression of alternative readings, enforcement against trinitarian authority).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (monotheism + Jesus-divinity + exegetical adequacy) is live but contested. The trinitarian reading solves it via philosophical substance-language; the unitarian reading solves it via subordination; modalism solves it via sequential modes. Each reading vindicates a different aspect: modalism vindicates strict monotheism and biblical narrative-order; trinitarianism vindicates philosophical coherence and simultaneous-action capacity; unitarianism vindicates numerical singularity and rationalism. Mandatrophy (mandate outliving function) does NOT apply here: the mandate—to sustain Jesus-worship within monotheistic faith—is as live in modalist theology as in trinitarian and unitarian readings. The constraint persists because the founding problem persists, not because the mandate is dead. The classification as tangled_rope (not piton) reflects this: the suppression and extraction are real, but the coordination function is genuinely live, not merely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modal_sequence_simultaneity_problem,
    'Can mode-language coherently explain simultaneous divine action (Father electing, Son redeeming, Spirit sanctifying all in the same moment) if the modes are sequential?',
    'Logical analysis of modal metaphysics: does ''mode'' allow for simultaneous manifestation in different spheres, or does it require strict temporal sequence? Comparison with modern modal logic and actuality operators.',
    'If mode-language can accommodate simultaneity, modalism gains philosophical coherence and extraction drops (suppression no longer needed for logical defense). If not, modalism remains philosophically incomplete and extraction persists as the cost of maintaining the reading despite logical gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modal_sequence_simultaneity_problem, conceptual, 'Whether modalist mode-language can resolve simultaneous divine action.').

omega_variable(
    exegetical_versus_philosophical_authority,
    'When exegetical adequacy (biblical language-use) and philosophical coherence (logical necessity) conflict, which authority grounds the reading''s legitimacy?',
    'Historical analysis of which seat (modalists, trinitarians, unitarians) can command broader institutional support when the conflict is explicit. Contemporary theological method debates (biblical vs. systematic priority).',
    'If exegetical authority dominates, modalism gains credibility and extraction decreases. If philosophical authority dominates, modalism remains marginal and extraction persists as suppression cost. The reading''s persistence depends partly on this boundary dispute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exegetical_versus_philosophical_authority, preference, 'Authority hierarchy between exegetical and philosophical groundings in doctrinal reading.').

omega_variable(
    subordinationism_versus_modalism_boundary,
    'Is modalism (sequential modes of one person) structurally distinguishable from subordinationism (Son/Spirit as divine but subordinate to Father) or does modalism collapse into subordinationism once enforcement pressure rises?',
    'Close textual analysis of early modalist vs. early subordinationist literature; examination of whether modalists can maintain ''same person in different modes'' rather than ''same essence in hierarchical rank.''',
    'If the boundary holds, modalism retains coherence as a distinct reading. If it collapses, modalists cannot defend against the subordinationist charge, and subordinationist communities gain grounds for merger-pressure, lowering extraction. If the boundary weakens, theater_ratio rises (performative reaffirmation of distinctness becomes necessary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinationism_versus_modalism_boundary, empirical, 'Whether modalism remains logically/textually distinct from subordinationism under pressure.').

omega_variable(
    conciliar_authority_versus_exegetical_rediscovery,
    'Can a reading formally condemned by ecumenical councils (Sabellian heresy at Nicaea) ever recover institutional legitimacy through exegetical rediscovery, or does conciliar exclusion create permanent suppression?',
    'Historical precedent: instances of recovered/rehabilitated positions in Christian tradition post-condemnation. Modern biblical scholarship''s capacity to shift denominational teaching without formal council reversal.',
    'If recovery is possible, suppression requirement can decrease as exegetical case strengthens; if permanent, suppression is structural and extraction plateaus. This determines whether the constraint can resolve mandatrophy (founding problem) or remains trapped in performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conciliar_authority_versus_exegetical_rediscovery, conceptual, 'Whether conciliar condemnation creates permanent or revisable institutional suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__modalist_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__modalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t300, biblical_divine_nature__modalist_reading, theater_ratio, 300, 0.18).
narrative_ontology:measurement(bibl_tr_t600, biblical_divine_nature__modalist_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement(bibl_tr_t900, biblical_divine_nature__modalist_reading, theater_ratio, 900, 0.35).
narrative_ontology:measurement(bibl_tr_t1200, biblical_divine_nature__modalist_reading, theater_ratio, 1200, 0.39).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__modalist_reading, theater_ratio, 1500, 0.41).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__modalist_reading, theater_ratio, 1800, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__modalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t300, biblical_divine_nature__modalist_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement(bibl_be_t600, biblical_divine_nature__modalist_reading, base_extractiveness, 600, 0.58).
narrative_ontology:measurement(bibl_be_t900, biblical_divine_nature__modalist_reading, base_extractiveness, 900, 0.64).
narrative_ontology:measurement(bibl_be_t1200, biblical_divine_nature__modalist_reading, base_extractiveness, 1200, 0.66).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__modalist_reading, base_extractiveness, 1500, 0.68).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__modalist_reading, base_extractiveness, 1800, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__modalist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bibl_su_t300, biblical_divine_nature__modalist_reading, suppression_requirement, 300, 0.54).
narrative_ontology:measurement(bibl_su_t600, biblical_divine_nature__modalist_reading, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(bibl_su_t900, biblical_divine_nature__modalist_reading, suppression_requirement, 900, 0.7).
narrative_ontology:measurement(bibl_su_t1200, biblical_divine_nature__modalist_reading, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__modalist_reading, suppression_requirement, 1500, 0.72).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__modalist_reading, suppression_requirement, 1800, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__modalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__modalist_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__modalist_reading, biblical_divine_nature__unitarian_reading).

% DUAL FORMULATION NOTE:
% The biblical divine nature kernel generates three constraint stories, one per major reading. Each reading authors a different ε, different beneficiaries/victims, different suppression mechanisms. Modalism (this story) carries moderate extraction due to its institutional exclusion; trinitarianism carries lower extraction (philosophical apparatus solves logical problems); unitarianism carries different extraction direction (targets trinitarian and Jesus-centered practitioners). The three stories form a constraint family linked by kernel identity and mutual influence. Modalism influences both siblings: the trinitarian response was partly driven by modalist challenge; the unitarian response was partly driven by trinitarianism's response to modalism. No single reading forecloses another—all three remain live in contemporary theology. All three coexist across different faith communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
