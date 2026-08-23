% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__unitarian_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Unitarian Reading of Biblical Divine Nature
 *   domain: theological/doctrinal
 *
 * SUMMARY:
 *   The unitarian reading asserts that the God of the Bible is numerically
 *   one — the Father alone — and that the Son and Spirit are subordinate or
 *   created. This reading has operated as a living constraint since the Arian
 *   controversy (4th century), surviving imperial suppression, medieval
 *   inquisition, Reformation persecution, and modern marginalization. It
 *   coordinates communities (Socinians, Unitarians, Biblical Unitarians,
 *   Christadelphians, some Restorationist groups) around a shared doctrinal
 *   core while extracting legitimacy from the trinitarian institutional
 *   hierarchy that dominates global Christianity. The constraint claims to
 *   restore apostolic monotheism; its opponents claim it abandons the
 *   apostolic Christ. The engine will compute per-seat classifications from
 *   the structural data below; the claimed_type (tangled_rope) reflects the
 *   author's judgment that the reading both coordinates a genuine community
 *   AND asymmetrically extracts from the trinitarian establishment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.62).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.45).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Biblical Divine Nature").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theological/doctrinal").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '7ecc290b-cd87-43bb-9ddd-e450fda93a4d').
narrative_ontology:cs_kernel_codification('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', fixed_text).
narrative_ontology:cs_authority_grounding('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', lineage).
narrative_ontology:cs_reading_relation('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', foundational, father_alone_is_god).
narrative_ontology:cs_axiom_status(father_alone_is_god, holdable).
narrative_ontology:cs_axiom_grounding('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', father_alone_is_god, theological).
narrative_ontology:cs_axiom('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', foundational, son_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', son_subordinate_or_created, theological).
narrative_ontology:cs_reference_frame('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', apostolic_monotheism).
narrative_ontology:cs_drift_state('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', post_nicene_creedal_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('7ecc290b-cd87-43bb-9ddd-e450fda93a4d', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_congregations).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, anti_trinitarian_theologians).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, biblical_unitarians).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_enforcers).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, episcopal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, biblical_unitarians).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, numerical_monotheism).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, biblical_sufficiency).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, apostolic_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Local worshipping communities organized around strict numerical monotheism; they govern themselves congregationally without bishops, ordain their own ministers, and regard the creeds as human additions. Members join voluntarily and can leave for other denominations; the constraint coordinates their identity and worship but does not trap them.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_congregations, beneficiary,
    organized, generational, mobile, regional).

% Scholars and ministers who articulate the unitarian reading — writing commentaries, engaging in public debate, training ministers. They set the doctrinal agenda for the movement. Their professional standing depends on the reading's plausibility; they can publish in academic presses outside the movement, giving them arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, anti_trinitarian_theologians, agenda_setter,
    moderate, biographical, arbitrage, global).

% Individual believers who hold the unitarian reading within mixed or hostile denominational settings (e.g., mainline Protestant, Catholic, Orthodox contexts). They gain theological coherence but pay social and institutional costs — exclusion from communion, blocked ordination, disciplinary proceedings. Exit means leaving their ecclesiastical home.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, biblical_unitarians, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, biblical_unitarians, payer).

% Bishops, councils, and magisterial structures whose authority rests on the creedal definition of God. The unitarian reading delegitimizes their teaching office and jurisdictional claims. They cannot exit the conflict without abandoning their institutional identity — the reading strikes at the foundation of their authority.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_institutional_hierarchy, payer,
    institutional, civilizational, identity_locked, global).

% Congregations for the Doctrine of the Faith, synodical courts, heresy-trial mechanisms, and academic gatekeepers who police trinitarian boundaries. The unitarian reading creates a persistent target that requires monitoring, censure, and exclusion — their enforcement apparatus is occupied by a challenge they cannot assimilate.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_enforcers, payer,
    institutional, civilizational, identity_locked, global).

% The episcopal office itself — the claim that bishops guard apostolic doctrine. Unitarianism's flat ecclesiology and scriptural sufficiency undercut the necessity of the episcopate. Bishops can adapt (some Anglican provinces accommodate unitarian-leaning clergy) but the structural pressure is toward irrelevance.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, episcopal_authority, payer,
    institutional, civilizational, constrained, global).

% Trinitarian believers who find themselves in unitarian-dominated congregations or regions (historically: Transylvania, Poland, New England). They would object to the denial of Christ's divinity but have no voice in the governance; exit means leaving family and community.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_laity_in_unitarian_space, excluded,
    powerless, biographical, constrained, local).

% Scholars of patristics and Christian origins who trace the development of trinitarian language. They neither collect nor pay in the doctrinal contest but their work is cited by all sides — Harnack's 'Hellenization' thesis supports unitarians; the Cappadocians' terminology supports trinitarians.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, early_church_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a Christian community around strict numerical monotheism and biblical authority over creedal tradition, providing a shared doctrinal identity without hierarchical enforcement.
% TRANSFER_FUNCTION: Moves theological authority from creedal institutions (councils, magisterium, episcopal teaching office) to individual and congregational scriptural interpretation; moves ecclesial power from hierarchy to gathered congregation.
% ABSENT_VOICES: Trinitarian laity who might prefer unitarian theology but remain in trinitarian churches for cultural/familial reasons; early church figures (Athanasius, the Cappadocians, Augustine) whose writings are claimed by both sides but who cannot speak for themselves; Jewish and Islamic interlocutors for whom the debate is unintelligible on its own terms.
% DISAPPEARANCE_RATIONALE: If the unitarian constraint vanished overnight, unitarian congregations would either dissolve, merge into trinitarian denominations, or adopt creedal statements; the trinitarian institutional hierarchy would lose a persistent structural challenger that forces continual boundary-maintenance; anti-trinitarian theologians would lose their institutional base.
% FOUNDING_PROBLEM: The problem of creedal additions to biblical teaching — the Nicean (325) and Chalcedonian (451) definitions introduced non-scriptural terminology (homoousios, hypostasis, ousia) that went beyond the apostolic witness to one God, the Father.
% FOUNDING_PROBLEM_CORROBORATION: Patristic historians (Harnack, Kelly, Lienhard) document that trinitarian terminology develops post-Nicea and is absent from the New Testament; biblical scholars (Dunn, Bauckham, Hurtado) debate whether NT Christology entails later orthodoxy or reflects early high Christology within monotheism; trinitarian theologians (Athanasius, Gregory of Nyssa, Aquinas) attest the creeds were forced by Christological necessity, not innovation — no single external corroboration resolves the dispute.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the reading's structural displacement of trinitarian authority — where it governs, the episcopal/credal apparatus loses its rationale. Suppression (0.45) is moderate: historically severe (execution, exile, anathema) but declining as state enforcement withdrew; today suppression is mainly social and institutional (denial of orders, communion, academic posts). Theater ratio (0.18) is low — the reading is genuinely held, not performative. Accessibility collapse (0.52) is partial: trinitarian alternatives remain globally dominant and accessible, but within unitarian space they are excluded. Resistance (0.68) is high from the institutional hierarchy, which treats the reading as a boundary violation requiring perpetual policing.
 *
 * PERSPECTIVAL GAP:
 *   From the unitarian seat, the constraint is a rope — pure coordination around biblical truth, liberating believers from creedal bondage. From the trinitarian bishop's seat, it is a snare — a cover for Christological reductionism that destroys the grammar of salvation. From the biblical unitarian in a trinitarian church, it is a tangled rope — coordination with fellow dissenters, but extraction by the institution that disciplines them. The engine computes this divergence; the author does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian congregations and theologians are structural beneficiaries (d near 0.0) — the constraint subsidizes their coherence and identity. Biblical unitarians in hostile denominations are payers (d ~0.6) — they bear costs without full benefit. Trinitarian hierarchy and enforcers are full targets (d near 1.0) — the constraint extracts their legitimating rationale. The derivation chain: beneficiaries declared → low d; victims declared → high d; exit options modulate (mobile beneficiaries, identity_locked victims). No overrides needed — the structural data captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creedal addition to Scripture) remains contested — unitarians say the problem persists, trinitarians say the creeds faithfully express Scripture. The arrangement has not atrophied; it persists as a live option with growing scholarly support (historical-critical biblical studies, social-trinitarian critiques of classical theism). Mandatrophy is not resolved — the constraint's mandate (restore biblical monotheism) is still claimed as unfulfilled by its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the biblical_divine_nature kernel a single constraint with multiple readings, or are the readings structurally distinct constraints that only share a label?',
    'Apply ε-invariance test: if measuring the constraint via trinitarian observables (creedal conformity, sacramental validity) yields different ε than unitarian observables (scriptural citation count, congregational autonomy), they are distinct constraints.',
    'If distinct, each reading gets its own constraint story with independent ε; the kernel_id becomes a family label, not a shared referent. If single, the ε must be reading-invariant — which this story denies by authoring a reading-specific ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel frame masks multiple constraints or genuinely identifies one constraint with contested readings.').

omega_variable(
    extraction_direction_contingency,
    'Does the unitarian constraint''s extraction from the trinitarian institution flow from the reading''s internal logic, or from the institution''s reactive suppression?',
    'Counterfactual: in a world where trinitarian institutions ignored unitarianism (no heresy trials, no creedal tests), would the reading still extract legitimacy from them? Historical comparison: Socinian Poland (tolerated) vs. Calvinist Geneva (suppressed).',
    'If extraction is institution-reactive, the unitarian constraint is lower-ε than measured; if internal, the ε stands. Affects whether the constraint is tangled_rope (coordination + extraction) or rope (coordination only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_direction_contingency, empirical, 'Whether the asymmetric extraction is endogenous to the reading or imposed by the victim''s response.').

omega_variable(
    flat_ecclesiology_necessity,
    'Is the unitarian reading''s flat ecclesiology (congregational polity, no bishops) a necessary consequence of its theology, or a historical accident of its persecution history?',
    'Compare unitarian groups with episcopal polities (e.g., some Anglican unitarians, Polish Brethren''s brief superintendency) — does the theology permit hierarchy or forbid it?',
    'If necessary, the low institutional authority is structural (low power → low enforcement capacity); if contingent, the reading could develop institutional authority without changing its type — affecting whether requires_active_enforcement is intrinsic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flat_ecclesiology_necessity, conceptual, 'Whether flat ecclesiology is entailed by the reading or imposed by its marginalization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal disability, institutional exclusion) or internalized (unitarians absorbing trinitarian normative judgments)?',
    'Post-exit trajectory: do former unitarians who join trinitarian churches report persistent internalized suppression, or does it dissolve with institutional change?',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the agent after exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a doctrinal minority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.05).
narrative_ontology:measurement(bibl_tr_t500, biblical_divine_nature__unitarian_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(bibl_tr_t1000, biblical_divine_nature__unitarian_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(bibl_tr_t1800, biblical_divine_nature__unitarian_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(bibl_tr_t1900, biblical_divine_nature__unitarian_reading, theater_ratio, 1900, 0.16).
narrative_ontology:measurement(bibl_tr_t2000, biblical_divine_nature__unitarian_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(bibl_tr_t2025, biblical_divine_nature__unitarian_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement(bibl_be_t500, biblical_divine_nature__unitarian_reading, base_extractiveness, 500, 0.25).
narrative_ontology:measurement(bibl_be_t1000, biblical_divine_nature__unitarian_reading, base_extractiveness, 1000, 0.35).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(bibl_be_t1800, biblical_divine_nature__unitarian_reading, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(bibl_be_t1900, biblical_divine_nature__unitarian_reading, base_extractiveness, 1900, 0.58).
narrative_ontology:measurement(bibl_be_t2000, biblical_divine_nature__unitarian_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(bibl_be_t2025, biblical_divine_nature__unitarian_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.85).
narrative_ontology:measurement(bibl_su_t500, biblical_divine_nature__unitarian_reading, suppression_requirement, 500, 0.75).
narrative_ontology:measurement(bibl_su_t1000, biblical_divine_nature__unitarian_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.55).
narrative_ontology:measurement(bibl_su_t1800, biblical_divine_nature__unitarian_reading, suppression_requirement, 1800, 0.45).
narrative_ontology:measurement(bibl_su_t1900, biblical_divine_nature__unitarian_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement(bibl_su_t2000, biblical_divine_nature__unitarian_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(bibl_su_t2025, biblical_divine_nature__unitarian_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.08).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, christological_definition__chalcedonian).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, ecclesial_authority__episcopal_governance).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, creedal_subscription__nicene_constantinopolitan).

% DUAL FORMULATION NOTE:
% This constraint is one member of the biblical_divine_nature constraint family. The trinitarian_reading (three hypostases, one ousia) and modalist_reading (one person, three modes) are sibling constraints with distinct ε, beneficiary/victim structures, and claimed_types. The unitarian_reading forecloses both siblings structurally (core premises contradict) while coexisting with them sociologically (different parties hold each). The family shares the kernel_id but each story is ε-invariant per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
