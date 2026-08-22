% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__trinitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_divine_nature__trinitarian_reading, []).

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
 *   constraint_id: biblical_divine_nature__trinitarian_reading
 *   human_readable: Trinitarian Divine Nature Doctrine (Essence-Unity Formulation)
 *   domain: theological/religious_authority
 *
 * SUMMARY:
 *   The trinitarian reading of the biblical divine nature claims that God
 *   eternally subsists in three distinct hypostases (Father, Son, Holy
 *   Spirit) while maintaining numerical unity of ousia (essence/substance).
 *   Institutionalized by the Council of Nicaea (325 CE) and clarified at
 *   Constantinople I (381 CE), this doctrine became the test of orthodoxy:
 *   non-Trinitarians (Arians, Unitarians, Oneness believers) were
 *   anathematized and excluded from sacramental communion. The constraint
 *   story describes this reading AS INSTANTIATED—the specific theological
 *   commitment, its institutional enforcement structure, and the extraction
 *   it accomplishes by making trinitarian affirmation the price of Christian
 *   institutional membership. This is ONE reading of the contested kernel
 *   'biblical divine nature'; sibling readings (Arian/modalist/Unitarian) are
 *   OTHER constraints, not folded into this one. The ε value (0.81 at
 *   interval endpoint) assesses extractiveness of THIS reading's enforcement,
 *   not an average across readings.
 *
 * KEY AGENTS:
 *   - institutional_orthodox_church: Maintains trinitarian orthodoxy as non-negotiable; derives ecclesiastical authority from conciliar decisions anathematizing alternatives
 *   - trinitarian_councils_and_magisterium: Codified and defend trinitarian doctrine; extract authority by defining Christian identity itself
 *   - arian_communities: Denied co-eternity of Son; exiled and excluded after Nicaea; trapped by institutional power asymmetry
 *   - unitarian_believers: Affirm God's numerical singularity; face systematic institutional exclusion; constrained exit options
 *   - non_trinitarians_broadly: All who cannot conscientiously affirm three co-eternal hypostases; victims of doctrinal anathema and institutional exclusion
 *   - scriptural_interpreters: Analytical seat documenting exegetical ambiguity; document how doctrine emerges from philosophical synthesis, not direct scriptural assertion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, 0.81).
domain_priors:suppression_score(biblical_divine_nature__trinitarian_reading, 0.89).
domain_priors:theater_ratio(biblical_divine_nature__trinitarian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(biblical_divine_nature__trinitarian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__trinitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__trinitarian_reading, "Trinitarian Divine Nature Doctrine (Essence-Unity Formulation)").
narrative_ontology:topic_domain(biblical_divine_nature__trinitarian_reading, "theological/religious_authority").

domain_priors:requires_active_enforcement(biblical_divine_nature__trinitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__trinitarian_reading, '549e0f25-3746-400e-84a0-315285ad4013').
narrative_ontology:cs_kernel_codification('549e0f25-3746-400e-84a0-315285ad4013', fixed_text).
narrative_ontology:cs_authority_grounding('549e0f25-3746-400e-84a0-315285ad4013', lineage).
narrative_ontology:cs_interpretation_layer_present('549e0f25-3746-400e-84a0-315285ad4013').
narrative_ontology:cs_reading_relation('549e0f25-3746-400e-84a0-315285ad4013', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_reading_relation('549e0f25-3746-400e-84a0-315285ad4013', biblical_divine_nature__unitarian_reading, coexists_with).
narrative_ontology:cs_axiom('549e0f25-3746-400e-84a0-315285ad4013', foundational, three_eternally_distinct_hypostases).
narrative_ontology:cs_axiom_status(three_eternally_distinct_hypostases, holdable).
narrative_ontology:cs_axiom_grounding('549e0f25-3746-400e-84a0-315285ad4013', three_eternally_distinct_hypostases, deontological).
narrative_ontology:cs_axiom('549e0f25-3746-400e-84a0-315285ad4013', foundational, essence_unity_preserves_monotheism).
narrative_ontology:cs_axiom_status(essence_unity_preserves_monotheism, holdable).
narrative_ontology:cs_axiom_grounding('549e0f25-3746-400e-84a0-315285ad4013', essence_unity_preserves_monotheism, empirically_contingent).
narrative_ontology:cs_axiom('549e0f25-3746-400e-84a0-315285ad4013', secondary, conciliar_authority_supreme_in_doctrine).
narrative_ontology:cs_axiom_status(conciliar_authority_supreme_in_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('549e0f25-3746-400e-84a0-315285ad4013', conciliar_authority_supreme_in_doctrine, conventional).
narrative_ontology:cs_reference_frame('549e0f25-3746-400e-84a0-315285ad4013', nicene_trinitarian_orthodoxy).
narrative_ontology:cs_drift_state('549e0f25-3746-400e-84a0-315285ad4013', contemporary_post_ecumenical_reformation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('549e0f25-3746-400e-84a0-315285ad4013', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_divine_nature__trinitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, institutional_orthodox_church).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__trinitarian_reading, trinitarian_councils_and_magisterium).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, arian_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, unitarian_believers).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, oneness_pentecostals).
narrative_ontology:constraint_victim(biblical_divine_nature__trinitarian_reading, non_trinitarians_broadly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Catholic, Orthodox, and most Protestant churches administer trinitarian doctrine as the criterion of orthodoxy. They control doctrinal curriculum, ordination requirements, and sacramental access. Trinitarian affirmation is enforced through catechesis, liturgy, and councils. The institutional church's identity is constituted by trinitarian tradition; the doctrine is inseparable from institutional continuity and authority.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, institutional_orthodox_church, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Ecumenical councils (Nicaea 325, Constantinople I 381, Ephesus 431, Chalcedon 451, etc.) codified trinitarian doctrine as binding dogma. The magisterium (teaching authority) claims continuity with these councils and derives legitimacy from fidelity to conciliar tradition. Control over doctrinal orthodoxy consolidates ecclesiastical power: councils defined what it means to be Christian; dissent = heresy = exclusion. The magisterium benefits materially from this power—it appoints bishops, controls education, and collects tithes contingent on maintaining doctrinal unity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, trinitarian_councils_and_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__trinitarian_reading, trinitarian_councils_and_magisterium, beneficiary).

% Arian theologians and congregations (denying the Son's co-eternity with the Father, affirming His subordination) were declared heretical at Nicaea 325 CE and systematically suppressed. Their bishops were exiled (Athanasius and others imprisoned), their churches closed, their writings burned. Arian communities persisted in Ostrogothic Italy, Visigothic Spain, and among Germanic tribes until forcibly converted or absorbed. Exit: recant Arian theology, accept institutional subordination, or flee. Most were eventually trapped.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, arian_communities, payer,
    organized, biographical, trapped, regional).

% From the Reformation onwards (Socinus, Polish Brethren, English and American Unitarians), those who read Scripture as affirming God's numerical singularity and the Son's subordinate status were excluded from mainstream institutional Christianity. Unitarian communities organized separately (Unitarian churches, Universalist congregations). They maintain intellectual credibility but cannot access institutional legitimacy within trinitarian frameworks. Exit: form separate institutions (achieved in some jurisdictions) or accept marginalization.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, unitarian_believers, payer,
    moderate, biographical, constrained, national).

% Oneness Pentecostals (rejecting eternally distinct hypostases, performing baptism 'in the name of Jesus' only) emerged from revivals but are barred from mainstream ecumenical recognition and trinitarian Pentecostal denominations. They maintain organized congregations but face doctrinal pressure and institutional exclusion. Exit: recant Oneness theology, accept trinitarian conformity, or maintain separate institutional structure.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, oneness_pentecostals, payer,
    organized, biographical, constrained, global).

% Any believer or theologian who conscientiously cannot affirm three eternally distinct hypostases in one ousia faces institutional exclusion from Christian communion. In pre-Reformation and medieval periods, this exclusion carried legal penalties (property confiscation, exile, execution under Christian rulers). In modern secular societies, exclusion is primarily institutional and social. Exit options: (1) recant (deny conscience), (2) accept exclusion from institutional Christianity, (3) join non-trinitarian communities (where permitted). All options impose significant cost.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, non_trinitarians_broadly, payer,
    powerless, biographical, trapped, global).

% 16th–17th century reformers (Socinus, Servetus, Castellio) who questioned trinitarian orthodoxy and advocated Scripture-only authority were systematically suppressed: Servetus burned (1553), Socinus exiled, Castellio's works banned. Their exegetical arguments were never admitted as legitimate theological positions; they were treated as heretics rather than interlocutors. Excluded from Church decision-making before the conversation began.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, early_reformist_theologians, excluded,
    moderate, biographical, constrained, regional).

% Modern biblical scholars, historians of early Christianity, and comparative religionists study trinitarian doctrine's scriptural foundations and institutional development. They document that no passage in Scripture explicitly states 'three co-eternal hypostases in one ousia'; the doctrine emerges from post-biblical philosophical synthesis (Neoplatonic metaphysics, Aristotelian categories) applied to resolve theological tensions. Their analytical seat demonstrates how institutional authority shaped theological consensus rather than Scripture univocally entailing it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, scriptural_interpreters, observer,
    analytical, biographical, analytical, global).

% Islamic theologians (al-Ghazali, Ibn Sina, al-Kindi) developed rigorous critiques of trinitarian logic as philosophically incoherent: three substances cannot be numerically one; the ousia/hypostasis distinction smuggles in contradiction. Their arguments were structurally excluded from Christian theological discourse—treated as attacks from outside the faith rather than challenges requiring response from within. Their exclusion was institutional and geographical; Islamic philosophers could not participate in Church councils.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, medieval_islamic_philosophers, excluded,
    institutional, generational, trapped, regional).

% 18th-century rationalists (Spinoza, Hume, Kant) questioned whether trinitarian doctrine can be rationally justified. Spinoza was excommunicated and exiled (1656); Hume's writings on miracles and design challenged the rational foundations of Christian orthodoxy. Their critiques were excluded from institutional theological discourse (treated as atheism rather than philosophy). Modern secular academia now hosts their arguments, but mainstream institutional Christianity still excludes them.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__trinitarian_reading, enlightenment_rationalists, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__trinitarian_reading, institutional_orthodox_church).
narrative_ontology:fixing_cost_class(biblical_divine_nature__trinitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved the genuine coordination problem of early Christianity: how to maintain numerical monotheism (essential to Jewish convert confidence and pagan philosophical respectability) while preserving the Father, Son, and Spirit as distinct, active agents in the economy of salvation (incarnation, atonement, Pentecost). The ousia/hypostasis distinction provided a framework allowing both: unity at the level of divine essence, multiplicity at the level of hypostatic persons. Without this framework, the Church faced a dilemma: either deny the Son and Spirit true divinity (Arianism, Unitarianism), or assert three Gods (tritheism). Trinitarian doctrine appeared to thread the needle.
% TRANSFER_FUNCTION: Transfers authority from Scripture (ambiguous, plural-readable) to the councils and magisterium (custodians of trinitarian orthodoxy). Non-Trinitarians must either accept the doctrine (intellectual submission), undergo recantation (denial of conscience), or face exclusion from sacramental communion and ecclesiastical authority. The extraction flows upward to the institutional hierarchy: councils and church officials consolidate power over defining Christian identity itself. Whoever controls doctrine controls membership, priesthood, and salvation (control of sacraments). The constraint embeds doctrinal authority in institutional structure: agreement with trinitarian orthodoxy is the price of membership.
% ABSENT_VOICES: Arian bishops were outvoted at Nicaea and subsequent councils; their representatives were few and lacked procedural power. Unitarian and Socinian thinkers had no seat at conciliar deliberations (they emerged centuries later). Islamic philosophers' critiques of trinitarian logic were never admitted as valid theological interlocution—they were excluded by institutional geography and religious boundary. Modalist and Pneumamachian (Spirit-denying) factions were suppressed at Constantinople I. Indigenous Christian communities (Syriac, Egyptian, Ethiopian) with different theological emphases were marginalized by Constantinople-Rome hegemony. The councils were trinitarian-supermajority bodies voting on whether to allow trinitarian alternatives; the outcome was predetermined.
% DISAPPEARANCE_RATIONALE: If trinitarian orthodoxy ceased to be enforced as binding dogma, Christian institutional structures would reorganize: (1) Unitarian and Oneness communities would claim ecumenical legitimacy and access to sacramental authority within mainstream denominations. (2) Scripture reading would revert to plural hermeneutics; no single reading would be mandatory for membership. (3) The conciliar authority structure would lose its monopoly on doctrine; theologians and communities could hold plural views on the divine nature. (4) Ecumenical recognition would no longer require trinitarian affirmation; dialogue with non-Trinitarian traditions would proceed as equals rather than heresy-correction. (5) The institutional church's authority over Christian identity would fragment; 'Christianity' would become a broader coalition with different doctrinal emphases.
% FOUNDING_PROBLEM: In the 4th century CE, as Christianity transitioned from persecuted minority to state religion, the Church faced urgent coordination crisis: diverse communities read Scripture differently regarding the divine nature and Christ's relation to the Father. Alexandria (Arius and Alexander), Antioch, and Rome held different emphases. Pastoral disputes (were Arian clergy legitimate? Could Arians receive communion?) risked schism. Emperor Constantine convened Nicaea to determine a single authoritative doctrine. The founding problem was institutional unity under theological pluralism.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox/institutional sources affirm the founding problem is solved: trinitarian doctrine provides stable theological identity and ecclesiastical unity. Non-trinitarian historians and theologians (Unitarians, Oneness historians, secular scholars) attest the founding problem was genuine but argue the trinitarian solution was not the only coherent path—other frameworks (Arianism, Unitarianism, modalism) also provided coherence and coordination, yet were excluded by institutional power rather than refuted by exegesis or philosophy. Modern ecumenical theologians from outside trinitarian-supermajority traditions testify that the 'problem' was partly constructed: the imposed consensus-testing created the crisis it claimed to solve.
narrative_ontology:disappearance_verdict(biblical_divine_nature__trinitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__trinitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__trinitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_divine_nature__trinitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__trinitarian_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__trinitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_divine_nature__trinitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_divine_nature__trinitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.31 at Nicaea to 0.81 by 1685) reflects accumulating institutional enforcement and scope expansion. At Nicaea, trinitarian orthodoxy was a new requirement with limited enforcement machinery; by the medieval period, institutional inquisitions, property confiscations, and systematic exclusion of non-Trinitarians had matured. Suppression is even higher (0.89) and grows faster than extractiveness because the constraint's persistence depends critically on preventing alternative readings from gaining institutional legitimacy—the anathema is maintained by active enforcement, not voluntary compliance. Theater_ratio (0.42) indicates that a substantial fraction of trinitarian enforcement activity by 1685 is performative: conciliar processes, creedal recitations, and doctrinal elaborations that defend the constraint's legitimacy rather than solve the original coordination problem. By the 17th century, alternative readings had been suppressed so effectively that the 'coordination problem' of diverse readings no longer exists—the theater is what remains. Accessibility_collapse (0.78) reflects that once trinitarian orthodoxy is declared binding, alternatives become intellectually inaccessible within institutional Christianity; exit requires conscious rejection of institutional Christianity itself. Resistance (0.72) is substantial because non-Trinitarians and reformers (Socinus, Enlightenment rationalists) actively refused the doctrine on exegetical and philosophical grounds; their dissent was suppressed, not eliminated.
 *
 * PERSPECTIVAL GAP:
 *   The institutional-church seat (agenda_setter, identity_locked) experiences trinitarian orthodoxy as genuine coordination—a solved problem, a foundation of Christian identity. From the Arian or Unitarian seat (payer, trapped), the same constraint operates as enforced extraction: a powerful institution has criminalized a coherent reading of Scripture and demands intellectual submission as the price of membership. The analytical seat (scriptural interpreter) perceives the coordination problem as partly constructed: early Christianity's plural readings could have coexisted under different institutional arrangements. The engine computes these divergent per-seat types from the structural data (power asymmetry, exit-option constraint, victim set definition); the authored claim (tangled_rope) reflects the objective structure: genuine coordination function (resolving theological pluralism) paired with asymmetric extraction (institutional consolidation of authority over doctrine and Christian identity).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional church and magisterium are near-beneficiary (d ≈ 0.08–0.15): they collect authority, define orthodoxy, and exclude competitors. Non-Trinitarians are full targets (d ≈ 0.85–1.0): they must absorb, recant, or accept exclusion; exit is trapped or identity_locked (recanting requires denying their conscientious reading of Scripture). The ousia/hypostasis distinction emerges from post-biblical philosophical systems (Neoplatonism, Aristotelian metaphysics), not from Scripture directly; this asymmetry of doctrinal tools means the institutional authorities (who control the philosophical vocabulary through education and council authority) have structural advantage in demonstrating 'orthodoxy' and marking alternatives as 'heresy.' The constraint's persistence depends on suppression precisely because the scriptural text does not unambiguously resolve the matter—without enforcement, alternative readings would resurface.
 *
 * MANDATROPHY ANALYSIS:
 *   The trinitarian doctrine exhibits features consistent with mandatrophy (mandate outlived by function): (1) The founding problem (4th-century coordination crisis among diverse Christian communities) was solved by Nicaea's establishment of a conciliar decision procedure, not by trinitarian metaphysics per se. (2) By the medieval period, institutional structures had achieved sufficient consolidation that plural reading was no longer a live threat—enforcement became maintenance of an established hierarchy rather than solving an active coordination problem. (3) Reformation scrutiny of conciliar authority (sola scriptura challenge) directly targets the mandate: if Scripture is sufficient, councils are unnecessary. (4) Modern ecumenical movements seek to reestablish communion across trinitarian and non-trinitarian traditions, implying the mandated exclusion is no longer functional. However, mandatrophy is NOT the sole or primary classification: the doctrine simultaneously persists as a genuine coordination achievement for trinitarian institutional Christianity and as extractive enforcement for non-Trinitarians. The claim (tangled_rope) reflects both: coordination + extraction, active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ousia_hypostasis_distinction_coherence,
    'Is the ousia/hypostasis distinction (essence vs. hypostatic person) philosophically coherent, or is it a post-hoc rationalization masking deeper conceptual incoherence in trinitarian metaphysics?',
    'Formal analytical philosophy of metaphysics: does the distinction carry through under scrutiny of mereology, identity, and properties? Do contemporary analytic theologians accept it as coherent, or do they revise/supplement it?',
    'If incoherent: the doctrine is held by institutional power, not by rational necessity. The constraint becomes pure snare for non-Trinitarians; the tangled_rope classification inverts to snare. If coherent: the constraint retains genuine coordination function; tangled_rope stands. The coherence question directly determines whether non-Trinitarians are victims of institutional suppression or victims of rational necessity they refuse to accept.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ousia_hypostasis_distinction_coherence, empirical, 'Philosophical coherence of the ousia/hypostasis distinction.').

omega_variable(
    scriptural_univocality_vs_polysemy,
    'Does Scripture univocally affirm trinitarian doctrine, or does it admit genuinely plural readings that cannot all be harmonized with trinitarian metaphysics?',
    'Exegetical consensus among non-trinitarian-benefiting scholars (Islamic, Jewish, secular historians of early Christianity): do they find exegetical paths through Scripture that cannot be reconciled with trinitarian doctrine without ad-hoc interpretive moves?',
    'If Scripture is genuinely polysemous: the trinitarian reading is one valid reading, not the reading—non-Trinitarians are not irrational dissidents but conscientious alternative interpreters. Institutional exclusion becomes more clearly extraction. If Scripture univocally entails trinitarian doctrine: non-Trinitarians are denying what the text clearly states; their suppression has rational warrant. The constraint may remain tangled_rope but with justified asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scriptural_univocality_vs_polysemy, empirical, 'Whether Scripture univocally supports trinitarian doctrine or admits plural readings.').

omega_variable(
    kernel_contest_foreclosure_or_coexistence,
    'Do the trinitarian and unitarian readings logically foreclose each other within a single theological framework, or do they merely represent competing institutional positions that have both persisted?',
    'Formal logical analysis: can a theologian hold both ''three eternally distinct hypostases'' and ''one person, numerically singular God'' without contradiction? Or is the contradiction unavoidable?',
    'If foreclosure: the constraint''s enforcement is not arbitrary; one reading must be selected for institutional coherence. If coexistence: the constraint represents institutional power imposing one reading, not resolving a logical necessity. This determines whether anathema is justified doctrine-policing or unjustified exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_or_coexistence, conceptual, 'Whether trinitarian and unitarian readings are logically foreclosed or merely competing positions.').

omega_variable(
    institutional_identity_lock_mechanism,
    'Is the institutional church''s identity constitutively trinitarian, or has trinitarian orthodoxy become internalized through socialization such that questioning it feels identity-threatening but could be revised institutionally?',
    'Historical-institutional analysis: could the Catholic or Orthodox church renounce trinitarian doctrine and reorganize around alternative frameworks without ceasing to be ''the church''? Or is trinitarian identity genuinely essential to institutional continuity?',
    'If constitutive: the constraint is largely intractable through institutional change alone; reform requires theological revolution. If internalized-socialization: the constraint could be softened through institutional dialogue and ecumenical reframing. This determines whether mandatrophy can be resolved or is locked into institutional identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_identity_lock_mechanism, preference, 'Whether institutional identity is constitutively trinitarian or contingently socialized into it.').

omega_variable(
    anathema_enforcement_mechanism,
    'Is the anathema of non-Trinitarians maintained by active institutional enforcement (threat of exclusion, property confiscation, authority denial), or by cognitive/identity internalization such that non-Trinitarians themselves accept the stigma as deserved?',
    'Post-exclusion trajectory study: when non-Trinitarians gain institutional power or exit the constraint (formation of separate denominations), does suppression persist or evaporate? Do they retain stigma internalization?',
    'If primarily active enforcement: suppression is structural and can be reduced through institutional reform. If primarily internalized: the constraint persists even after exit; the extraction is deep in cognitive patterns and cultural memory. Theater_ratio may underestimate functional extraction if internalization dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anathema_enforcement_mechanism, empirical, 'Whether anathema enforcement is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__trinitarian_reading, 325, 1685).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__trinitarian_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement(bibl_tr_t451, biblical_divine_nature__trinitarian_reading, theater_ratio, 451, 0.28).
narrative_ontology:measurement(bibl_tr_t800, biblical_divine_nature__trinitarian_reading, theater_ratio, 800, 0.35).
narrative_ontology:measurement(bibl_tr_t1200, biblical_divine_nature__trinitarian_reading, theater_ratio, 1200, 0.39).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__trinitarian_reading, theater_ratio, 1500, 0.41).
narrative_ontology:measurement(bibl_tr_t1685, biblical_divine_nature__trinitarian_reading, theater_ratio, 1685, 0.42).

% Extraction over time
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__trinitarian_reading, base_extractiveness, 325, 0.31).
narrative_ontology:measurement(bibl_be_t451, biblical_divine_nature__trinitarian_reading, base_extractiveness, 451, 0.52).
narrative_ontology:measurement(bibl_be_t800, biblical_divine_nature__trinitarian_reading, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(bibl_be_t1200, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1200, 0.75).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1500, 0.81).
narrative_ontology:measurement(bibl_be_t1685, biblical_divine_nature__trinitarian_reading, base_extractiveness, 1685, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__trinitarian_reading, suppression_requirement, 325, 0.42).
narrative_ontology:measurement(bibl_su_t451, biblical_divine_nature__trinitarian_reading, suppression_requirement, 451, 0.71).
narrative_ontology:measurement(bibl_su_t800, biblical_divine_nature__trinitarian_reading, suppression_requirement, 800, 0.84).
narrative_ontology:measurement(bibl_su_t1200, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1200, 0.88).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1500, 0.89).
narrative_ontology:measurement(bibl_su_t1685, biblical_divine_nature__trinitarian_reading, suppression_requirement, 1685, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__trinitarian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_divine_nature__trinitarian_reading, 0.22).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__modalist_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, biblical_divine_nature__unitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, nicene_creed_institutional_authority).
narrative_ontology:affects_constraint(biblical_divine_nature__trinitarian_reading, sacramental_access_gate__trinitarian_conformity).

% DUAL FORMULATION NOTE:
% The 'biblical divine nature' kernel decomposes into three structurally distinct constraint stories: trinitarian_reading (three hypostases, one ousia; this file), modalist_reading (sequential modes, not simultaneous persons), and unitarian_reading (numerical singularity of God). Each reading instantiates a different ε value, different victim set, and different institutional stance. The readings are linked by network.affects_constraints because institutional enforcement of trinitarian orthodoxy directly suppresses the modalist and unitarian readings. No reading is 'the truth'—each reading is ε-invariant within its own framework. The contest is located in the kernel (which reading coheres with Scripture), not in external facts. The three stories together model how a single textual source (Bible) generates three structurally distinct constraints through different interpretive commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__trinitarian_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
