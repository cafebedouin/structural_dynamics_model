% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: biblical_divine_nature__unitarian_reading
 *   human_readable: Numerical Singularity of God (Unitarian Reading)
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint instantiates the UNITARIAN READING of the contested
 *   biblical divine nature kernel. The reading asserts that God is
 *   numerically singular—the Father alone is God in the strict sense, while
 *   the Son and Holy Spirit are subordinate, created, or derivative beings.
 *   This reading opposes the trinitarian consensus that emerged in the
 *   4th-5th centuries and the modalist alternative that treats
 *   Father/Son/Spirit as sequential modes of one person. The unitarian
 *   reading vindicates strict monotheism (one God, numerically) by denying
 *   the Son's co-eternality and co-equality with the Father. As a constraint,
 *   this reading functions as a doctrinal gate that subordinates
 *   institutional hierarchy and institutional credal authority: it claims the
 *   Father's singularity is the true biblical doctrine and delegitimizes the
 *   hierarchical, credal-orthodox institutional infrastructure that
 *   consolidated trinitarian dogma. The extraction is high because this
 *   reading's assertion depends on delegitimizing and suppressing the
 *   institutional-orthodoxy constraint it competes with—the reading must
 *   attack the very authority structures that would defend the alternative
 *   reading. Suppression is high because institutional gatekeepers (bishops,
 *   councils, synods, inquisitorial bodies) actively exclude unitarian voices
 *   from pulpits, councils, and canonical standing. Theater ratio rises over
 *   the interval because the institutional assertion of orthodoxy
 *   increasingly becomes theatrical (ritual restatement of the creed,
 *   formulaic anathemas) even as institutional enforcement capacity and
 *   doctrinal confidence in the constraint itself decay.
 *
 * KEY AGENTS:
 *   - unitarian_theological_communities — claim numerical singularity is the true biblical reading; bear suppression from institutional gatekeepers
 *   - institutional_church_hierarchy — enforce credal orthodoxy, defend trinitarian consensus, gatekeep canonical and institutional standing
 *   - credal_orthodoxy_defenders — theologians, councils, bishops defending the Nicene/Chalcedonian synthesis; identify institutional stability with trinitarian doctrine
 *   - biblical_literalist_communities — agents who read OT monotheism language as univocal and contest NT passages that trinitarian readings interpret as co-eternal persons
 *   - modalist_theological_communities — alternative reading that coexists with both unitarian and trinitarian; treat Father/Son/Spirit as modes, preserving numerical singularity differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.68).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.72).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Numerical Singularity of God (Unitarian Reading)").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7').
narrative_ontology:cs_kernel_codification('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', fixed_text).
narrative_ontology:cs_authority_grounding('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', lineage).
narrative_ontology:cs_interpretation_layer_present('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7').
narrative_ontology:cs_reading_relation('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', foundational, numerical_singularity_of_god).
narrative_ontology:cs_axiom_status(numerical_singularity_of_god, holdable).
narrative_ontology:cs_axiom_grounding('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', numerical_singularity_of_god, deontological).
narrative_ontology:cs_axiom('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', foundational, subordination_principle_over_essence_unity).
narrative_ontology:cs_axiom_status(subordination_principle_over_essence_unity, holdable).
narrative_ontology:cs_axiom_grounding('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', subordination_principle_over_essence_unity, empirically_contingent).
narrative_ontology:cs_reference_frame('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', strict_biblical_monotheism).
narrative_ontology:cs_drift_state('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', post_nicene_institutional_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f081bb6b-eeb6-4eb0-8761-0d9e75bc3eb7', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, unitarian_theological_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodoxy_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, credal_orthodoxy_defenders).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, biblical_literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theological communities asserting that the Father alone is God and that the Son and Holy Spirit are subordinate or created beings. They read the OT monotheism language (Shema Israel, One God) as univocal and literal, and interpret NT passages about the Father, Son, and Spirit through that lens. Their assertion directly delegitimizes trinitarian orthodoxy and the institutional hierarchy that defends it. Exit from this position is experienced as theological apostasy—doctrinal identity is fused with monotheistic strictness.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_theological_communities, agenda_setter,
    organized, civilizational, identity_locked, global).

% Bishops, councils, synods, and inquisitorial bodies that gatekeep doctrinal orthodoxy and enforce credal conformity. They defend trinitarian consensus (Nicene, Chalcedonian) as settled dogma and exclude unitarian voices from councils, pulpits, and canonical standing. They bear suppression from unitarian doctrinal assertion (delegitimization of their authority) and bear the cost of maintaining enforcement machinery (inquisitions, credal tests, institutional discipline).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_church_hierarchy, payer,
    institutional, generational, constrained, global).

% Theologians, church fathers, councils, and bishops who developed and defend the trinitarian synthesis. They identify institutional stability and theological truth with trinitarian orthodoxy (Nicene creed, Chalcedonian definition). They bear suppression from unitarian doctrinal contestation (which treats trinitarian consensus as corruption) and benefit from institutional enforcement that silences unitarian voices. Their exit is constrained: abandoning trinitarian defense means abandoning institutional legitimacy and theological authority.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodoxy_defenders, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, credal_orthodoxy_defenders, beneficiary).

% Communities that read OT monotheism language as univocal and literal (one God, no plural divine persons) and contest trinitarian interpretations of NT passages. They benefit from the unitarian reading's assertion that strict monotheism is the true biblical meaning. Their exit is more mobile than institutional hierarchy—they can adopt unitarian theology without losing material standing, though institutional acceptance is constrained.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, biblical_literalist_communities, beneficiary,
    moderate, biographical, mobile, regional).

% Theological communities that preserve numerical singularity through sequential modes or roles (Father, Son, Spirit are the same person manifest in different times or functions) rather than through subordination or createdness. They are structurally excluded from both unitarian and trinitarian orthodox gatekeeping—neither reading fully affirms modalism, and institutional trinitarian orthodoxy condemns modalism as heresy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, modalist_theological_communities, excluded,
    moderate, generational, constrained, regional).

% Councils, synods, and ecumenical bodies that seek unified doctrinal consensus across Christian communities. They observe the unitarian, trinitarian, and modalist contest and adjudicate which reading becomes institutionally binding. Their position is analytical: they referee the doctrinal contest and determine which reading is orthodoxy.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, theological_consensus_seekers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the referent of 'God' in monotheism by asserting that God is numerically singular (the Father alone). This reading solves the problem of preserving strict monotheism against theological formulations (trinitarian co-equality) that the reading reads as implying three Gods. The coordination is doctrinal: it offers a coherent interpretation of biblical language that maintains monotheistic integrity.
% TRANSFER_FUNCTION: Transfers legitimacy and doctrinal authority FROM institutional trinitarian orthodoxy (trinitarian consensus, creedal conformity, institutional gatekeeping) TO unitarian assertion of biblical singularity. The reading demands subordination of trinitarian creedal authority to the principle of Father's singularity. It moves institutional standing and theological authority from credal defenders to unitarian theological communities (who claim superior biblical fidelity).
% ABSENT_VOICES: Modalist theological communities are partially excluded—they agree with unitarians on numerical singularity but disagree on the mechanism (modes vs. subordination). They would argue that numerical singularity can be preserved without subordinating the Son, and that both unitarian and trinitarian readings misread the biblical modalism. Lay believers whose faith was shaped by trinitarian catechesis are not directly in this conversation—their internalized orthodoxy is maintained through liturgy and catechesis, not through theological debate. Rival monotheistic religions (Islam, Judaism) that preserve numerical singularity are not parties to the Christian doctrinal contest, though their existence as coherent monotheisms outside trinitarian Christianity is implicit pressure on the constraint.
% DISAPPEARANCE_RATIONALE: If the unitarian reading's assertion of numerical singularity vanished (the claim that Father alone is God disappeared), institutional trinitarian orthodoxy would consolidate unopposed, unitarian theological communities would dissolve or convert to trinitarian faith, and the doctrinal synthesis defending trinitarian creedal authority would persist without contestation. The world rearranges because doctrinal authority, institutional gatekeeping, and theological education would all rest on trinitarian consensus without the structural pressure the unitarian reading applies.
% FOUNDING_PROBLEM: How can the God of Israel (numerically one, as asserted in the Shema and OT monotheism passages) be reconciled with the NT's appearance of Father, Son, and Holy Spirit as distinct persons in relation? The unitarian reading solves this by asserting that the Son and Spirit are not truly God in the same sense as the Father—they are subordinate or created—thus preserving numerical singularity of God.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is asserted as live by unitarian theological communities (strict monotheism must be preserved against trinitarian innovation). The founding problem is asserted as dead or solved by trinitarian orthodoxy defenders (the Nicene and Chalcedonian councils settled the issue—trinitarian co-equality preserves true monotheism through essence-unity, not numerical singularity). Early church historians outside the benefiting parties (Eusebius of Caesarea, Athanasius, the Nicene council records) attest that the problem was genuinely live and hotly contested in the 4th century. Contemporary religious historians (Bart Ehrman, Maurice F. Wiles) attest that the founding problem persists in theological scholarship and that the trinitarian 'solution' remains contested by unitarian and historical-critical readings. Modalist communities attest that the founding problem can be solved differently—through modes, not essence-unity or subordination.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures the degree to which the unitarian reading subordinates institutional hierarchy and credal authority (the Father's singularity = the reading's core vindication; institutional trinitarian consensus = the victim). At t=0 (early institutional formation), extractiveness is moderate (0.42) because unitarian communities are nascent and institutional enforcement machinery is immature. By t=20 (post-Council-of-Nicaea consolidation), extractiveness rises to 0.68 as institutional enforcement hardens and unitarian assertion becomes structurally incompatible with institutional standing. Suppression is high throughout (0.58–0.72) because institutional gatekeeping is active: exclusion from councils, condemnation of unitarian texts, institutional penalties for heterodox teaching. Theater ratio rises (0.22 → 0.41) because institutional defense of orthodoxy becomes increasingly formulaic and ritually performative (rote recitation of anathemas, liturgical affirmations of the creed) even as doctrinal coherence and lived enforcement decay. The one shared time grid ensures every metric is authored at every examined point; the cyclical pattern visible in theater (steady rise, flattening toward t=20) reflects the constraint's transition from active doctrinal contestation (early councils) to consolidated institutional ritual (post-Nicene establishment).
 *
 * PERSPECTIVAL GAP:
 *   From the unitarian theological seat, this constraint is genuine opposition to institutional corruption: the Father's singularity is the true biblical doctrine, and institutional trinitarian consensus is a departure from scripture. From the institutional-orthodoxy seat, the constraint is schismatic disruption: unitarian assertion destabilizes the doctrinal synthesis that unified the institutional church and threatens the authority structures built on credal orthodoxy. The engine computes these seats' different classifications from the structural data: the unitarian reading asserts a competing claim to biblical authority and therefore operates as extraction (subordination of institutional hierarchy); the institutional seat experiences this as victimhood (delegitimization of the creed it defends). Both experience high suppression — unitarian voices are suppressed by institutional gatekeeping; institutional authority is suppressed by unitarian doctrinal assault. The perspectival gap is the core structural fact: the same constraint (numerical singularity of God) is coordinating function from the unitarian seat (vindicating strict monotheism, clarifying biblical truth) and extraction from the institutional seat (delegitimizing trinitarian orthodoxy, subordinating hierarchical authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Unitarian theological communities are the targets of this constraint: they assert the Father's singularity and therefore must suppress or subordinate the trinitarian institutional-hierarchy constraint. Their directionality is high (near 1.0) — they bear the suppression, carry identity-lock (theological identity fused with unitarian assertion, exit means apostasy), and have constrained exit (institutional gatekeeping bars their participation in official theological discourse). Institutional hierarchy and credal orthodoxy are the victims: the unitarian reading's assertion treats their consolidation as corruption of biblical truth and demands their subordination to the Father's singularity principle. The reading's beneficiary is strict biblical monotheism (vindicated, not a real actor): the unitarian framing treats OT language about God's singularity as univocal and literal, and reads the NT through that lens, vindicating the proposition that monos theos (one God) means numerical singularity. No agent collects from this constraint — the benefit is to a vindicated proposition (the reading's theology), not to a stakeholder. This directionality structure is asymmetric: the reading must delegitimize institutional authority to assert itself, which makes suppression structural and high, and makes the reading's operation deeply entangled with institutional contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   The unitarian reading's founding problem is preserving strict biblical monotheism against the institutional trinitarian consensus that it reads as a corruption of that monotheism. The founding problem is LIVE from the unitarian seat (strict monotheism is eternally true and eternally contested by institutional trinitarian doctrine) and DEAD from the institutional seat (trinitarian orthodoxy is established, institutionally consolidated, theologically developed, no longer a question — the institutional reading treats unitarian contestation as heresy, not as a live problem to be solved). This mandatrophy mismatch is deliberate: the constraint's persistence depends on treating the founding problem as dead (institutional gatekeepers view unitarian contestation as error to be suppressed, not as a live problem requiring coordination). The theater ratio rising (0.22 → 0.41) supports the mandatrophy signal: institutional affirmation of orthodoxy becomes increasingly performative as the problem it was meant to solve (doctrinal clarity, institutional unification) is no longer live — what remains is ritual restatement of the creed, liturgical affirmation, formulaic anathemas. The constraint persists, but its functional rationale decays; it is maintained by institutional inertia and identity-lock rather than by active problem-solving.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_contested,
    'Is this a single kernel (biblical divine nature) instantiated by three competing readings, or three independent doctrinal claims mistaken for variations on one text?',
    'Textual genealogy: trace which Biblical passages each reading cites as the kernel''s ground. If all three readings cite overlapping sets of the same passages as authoritative, the kernel is shared; if they cite disjoint passage-sets treated as authoritative only within each reading''s tradition, the kernel is distributed or multiple.',
    'If kernel is shared, this reading forecloses the trinitarian reading (contradictory monotheism axioms). If kernel is distributed, they coexist as alternative canonical grounds with no logical contradiction at the framework level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_contested, conceptual, 'Whether the three readings share a single contested kernel or instantiate independent doctrinal claims.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of unitarian voices structural (institutional exclusion, credal gatekeeping, inquisitorial force) or internalized (unitarian believers accept subordination as theologically correct)?',
    'Post-disestablishment trajectory: in jurisdictions where institutional suppression machinery is disabled (no state enforcement of creedal orthodoxy, no institutional penalties for heterodoxy), does unitarian theological assertion persist and grow, or does it remain suppressed by voluntary alignment with institutional teaching?',
    'If structural, the constraint''s effective suppression is the authored scalar. If internalized, the constraint carries suppressive force independent of institutional machinery; victims may not experience exit as available even when institutional barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of suppression in the unitarian-orthodoxy constraint.').

omega_variable(
    reading_kernel_framing_stability,
    'Does the unitarian reading treat Biblical passages (patristic synthesis of OT monotheism + NT christology) as a FIXED kernel that its reading interprets, or does the reading''s own tradition revise which passages count as kernel-definitive?',
    'Historical textual criticism within unitarian theology: do 19th–20th century unitarian biblical scholarship re-weight passage significance, elevate new passages as foundational, or demote previously central passages? Reconstruction of the reading''s canon-within-canon over time.',
    'If the kernel is truly fixed, this reading''s authority derives from superior interpretation of a shared text (lineage + expertise framing). If unitarian theology has revised the kernel''s composition, this reading''s authority is distributed (each generation re-selects which passages ground the claim), and the constraint''s codification status shifts from fixed_text to distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_framing_stability, conceptual, 'Stability of the biblical kernel under unitarian theological revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bibl_tr_t4, biblical_divine_nature__unitarian_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(bibl_tr_t8, biblical_divine_nature__unitarian_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(bibl_tr_t12, biblical_divine_nature__unitarian_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(bibl_tr_t16, biblical_divine_nature__unitarian_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(bibl_tr_t20, biblical_divine_nature__unitarian_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bibl_be_t4, biblical_divine_nature__unitarian_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(bibl_be_t8, biblical_divine_nature__unitarian_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(bibl_be_t12, biblical_divine_nature__unitarian_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(bibl_be_t16, biblical_divine_nature__unitarian_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(bibl_be_t20, biblical_divine_nature__unitarian_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bibl_su_t4, biblical_divine_nature__unitarian_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(bibl_su_t8, biblical_divine_nature__unitarian_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(bibl_su_t12, biblical_divine_nature__unitarian_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(bibl_su_t16, biblical_divine_nature__unitarian_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(bibl_su_t20, biblical_divine_nature__unitarian_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__unitarian_reading, 0.12).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories instantiate three readings of the biblical divine nature kernel. The unitarian, trinitarian, and modalist readings share the same kernel (contested biblical passages on monotheism and the divine persons) but interpret the kernel's meaning differently. Each reading asserts a different monotheism: numerical singularity (unitarian), essence-unity (trinitarian), sequential modes (modalist). Each reading is instantiated as a separate constraint with its own ε, beneficiary/victim structure, and authority grounding. They are linked via network.affects_constraints to indicate family relationships. The unitarian reading forecloses the trinitarian reading logically (incompatible monotheisms) and coexists with the modalist reading (both preserve numerical singularity). Sibling stories: biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
