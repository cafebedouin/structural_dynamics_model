% ============================================================================
% CONSTRAINT STORY: biblical_divine_nature__unitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Unitarian Reading of Divine Nature: Numerical Singularity, Father Alone
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   The kernel biblical_divine_nature — what the Christian scriptures
 *   disclose about God's numerical constitution — is read three ways, and
 *   each reading emits a different constraint. This story instantiates the
 *   unitarian reading: God is numerically one, the Father alone is God, and
 *   the Son and Spirit are subordinate or created rather than consubstantial
 *   persons. As a governing arrangement the reading has been actually
 *   operative: the Transylvanian church founded under the Edict of Torda
 *   (1568), the Polish Brethren of Rakow, British and American Unitarian
 *   bodies, and today's congregational Biblical Unitarian networks. Its
 *   structure inverts the credal settlement's: authority is low and
 *   distributed (scripture's plain sense, read without magisterial
 *   mediation), ecclesiology is flat (congregational self-governance), and
 *   the parties who bear its costs are the institutional hierarchy and the
 *   credal orthodoxy whose arbitral office it declines to recognize. FAMILY
 *   NOTE (epsilon decomposition): the trinitarian reading's constraint
 *   carries high institutional authority, credal beneficiaries, and
 *   dissenters as victims, with epsilon assessed far higher on the credal
 *   settlement's enforcement record; the modalist reading emits a third
 *   structure with its own victim set. This story's epsilon (0.34) is
 *   authored for the unitarian arrangement as it actually operates —
 *   establishment-era tithes in early Transylvania, boundary enforcement,
 *   confiscated credal arbitral authority, bounded member worship grammar —
 *   not for an idealized liberation. The three files are linked because each
 *   reads the same fixed text and each defines itself against the others;
 *   textual and archaeological findings that shift one reading shift the
 *   others' plausibility.
 *
 * KEY AGENTS:
 *   - lay_believers: primary beneficiary (moderate/mobile) — gain unmediated interpretive standing and relief from credal assent duties
 *   - congregational_communities: beneficiary (organized/constrained) — receive the doctrinal finality the credal office once monopolized
 *   - synod_superintendents: agenda-setter (organized/constrained) — ordain ministers, supervise doctrine, police the boundary (Deva 1579)
 *   - ecclesial_institutional_hierarchy: primary payer (institutional/identity_locked) — arbitral authority and anathema power lose purchase where the norm governs
 *   - credal_orthodox_establishment: payer (institutional/identity_locked) — bears continuous delegitimation of the necessity claim beneath its teaching office
 *   - non_adorationist_reformers: payer (moderate/trapped) — internal overshooters disciplined by the community's own enforcement
 *   - trinitarian_leaning_members: excluded voice (powerless/mobile) — may attend, may not teach
 *   - historians_of_doctrine: analytical observer — documents the record from outside every communion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_divine_nature__unitarian_reading, 0.34).
domain_priors:suppression_score(biblical_divine_nature__unitarian_reading, 0.26).
domain_priors:theater_ratio(biblical_divine_nature__unitarian_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(biblical_divine_nature__unitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, tangled_rope).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature: Numerical Singularity, Father Alone").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, 'cbae13b7-23a8-472d-9b5a-fa4a77ea58ba').
narrative_ontology:cs_kernel_codification('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', fixed_text).
narrative_ontology:cs_authority_grounding('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', distributed).
narrative_ontology:cs_reading_relation('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', biblical_divine_nature__modalist_reading, forecloses).
narrative_ontology:cs_axiom('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', foundational, father_alone_numerical_singularity).
narrative_ontology:cs_axiom_status(father_alone_numerical_singularity, holdable).
narrative_ontology:cs_axiom_grounding('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', father_alone_numerical_singularity, theological).
narrative_ontology:cs_axiom('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', foundational, son_spirit_subordinate_or_created).
narrative_ontology:cs_axiom_status(son_spirit_subordinate_or_created, holdable).
narrative_ontology:cs_axiom_grounding('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', son_spirit_subordinate_or_created, theological).
narrative_ontology:cs_reference_frame('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', apostolic_unitary_monotheism).
narrative_ontology:cs_drift_state('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', contemporary_post_nicene_settlement, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('cbae13b7-23a8-472d-9b5a-fa4a77ea58ba', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, lay_believers).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, congregational_communities).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, ecclesial_institutional_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, credal_orthodox_establishment).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, non_adorationist_reformers).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, numerical_monotheism_of_the_father).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, plain_sense_sola_scriptura_hermeneutics).
narrative_ontology:constraint_vindicates(biblical_divine_nature__unitarian_reading, anti_credal_subordinationist_christology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Read the scriptures without credal mediation and address worship to the Father alone, honoring Jesus as Messiah, Lord, and exalted Son of God rather than as God himself. They gain direct interpretive standing — no council or magisterium stands between them and the text — and are spared the duty of affirming formulae they find unintelligible. They bear the social cost of heterodoxy where trinitarian churches dominate, and their prayer grammar is bounded: petition goes to the Father, with thanks through Christ as mediator in the majority adorationist practice.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, lay_believers, beneficiary,
    moderate, biographical, mobile, global).

% Self-governing congregations and fellowships — from the Transylvanian church to modern Biblical Unitarian assemblies — that own their buildings, call their own ministers, and teach from the Racovian Catechism lineage or equivalent statements. They receive the doctrinal finality the credal establishment once monopolized: they decide who teaches and what is sound. Exit for a community means dissolution or absorption into another body, which prices leaving above what most members will pay.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, congregational_communities, beneficiary,
    organized, generational, constrained, continental).

% Elected superintendents and synods — the Transylvanian office founded under Ferenc David, and analogous fellowship committees elsewhere — ordain ministers, supervise doctrine, and police the boundary of acceptable teaching. They administer the arrangement rather than merely enjoying it; their office exists only while the community holds the unitarian norm, and the Deva proceedings of 1579, in which the synod prosecuted David for carrying non-adorationism further than the synod would go, mark the hard edge of their authority.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, synod_superintendents, agenda_setter,
    organized, generational, constrained, national).

% Bishops, councils, and magisterial offices whose arbitral authority over divine-nature doctrine is exactly what the unitarian norm declines to recognize. Wherever the reading gains ground, their interpretive monopoly and their power to define and anathematize lose purchase. They cannot relinquish the credal office without ceasing to be what they are, so they defend it instead — historically by banishment, statute, and worse.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, ecclesial_institutional_hierarchy, payer,
    institutional, generational, identity_locked, global).

% The creed-and-confession apparatus descended from Nicaea and Chalcedon — liturgical formulae, confessional subscriptions, seminary curricula — which the unitarian reading classes as post-biblical accretion. It bears continuous delegitimation pressure: every plain-sense argument against the three-person formula erodes the necessity claim on which its teaching authority rests. Its identity is fused with the formulae; abandoning them would dissolve the establishment itself.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, credal_orthodox_establishment, payer,
    institutional, civilizational, identity_locked, global).

% Internal radicals who pressed the reading past its settled line — most famously Ferenc David, who concluded that Christ should not be invoked or adored at all. The community's enforcement turned on them: David was tried by his own synod at Deva in 1579 and died in imprisonment there. Later overshooters face disfellowship rather than prison, but the pattern holds: the arrangement's boundary is enforced hardest against those who accept most of it and exceed it.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, non_adorationist_reformers, payer,
    moderate, biographical, trapped, regional).

% Members, seekers, and neighboring Christians who read the same scriptures and reach trinitarian conclusions. Inside unitarian congregations they may attend but not teach: pulpit and teaching office are closed to their reading, and fellowship statements bar it outright. Their recourse is silence or exit, and because exit is open, their objection rarely registers inside the community's deliberations.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_leaning_members, excluded,
    powerless, biographical, mobile, global).

% Academic patristics and Reformation historians who document the pre-Nicene theological landscape, the political mechanics of the fourth-century settlements, and the persecution record of unitarian movements. They collect testimony from every seat, publish outside any communion's control, and neither gain nor lose with the reading's fortunes.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, lay_believers).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monotheistic worship and doctrine around numerical singularity: one object of worship (the Father), one exalted lord-mediator (Jesus), one hermeneutic (the plain sense of a fixed text), enabling congregational self-governance without credal arbitration of paradox.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal finality from credal institutions — councils, magisteria, confessional establishments — to lay readers and self-governing congregations; simultaneously restricts the worship-grammar rights of members, bounding prayer address to the Father.
% ABSENT_VOICES: Trinitarian-leaning members and seekers inside unitarian congregations would object that the same scriptures warrant their reading, but the teaching office is closed to them and their recourse is exit. The credal establishment itself sits wholly outside the conversation and would contest the reading's genealogy of Nicaea. Historically, the non-adorationist radicals were the silenced internal voice after Deva.
% DISAPPEARANCE_RATIONALE: If the unitarian norm vanished overnight, the Transylvanian church and the global congregational networks would reorganize their worship grammar, ordination standards, and catechetical curricula immediately; the credal establishment would lose the standing counter-voice that disciplines its self-understanding and forces continual restatement of its foundations; the religious marketplace would lose one of its oldest recurring competitors.
% FOUNDING_PROBLEM: The arrangement was built to close the gap its holders perceive between the New Testament's plain unitary witness — one God, the Father; Jesus as Lord, Messiah, and exalted Son — and the post-apostolic credal elaboration (homoousios, three hypostases), and to remove the enforcement machinery that credal mystery made possible: the anathemas, statutes, and punishments visited on those who read otherwise.
% FOUNDING_PROBLEM_CORROBORATION: Academic patristics scholarship — seated outside every benefiting party — corroborates the historical problem: the diversity of pre-Nicene divine-nature teaching and the political character of the fourth-century settlement are documented independently of unitarian advocacy. The credal establishment corroborates liveness from the opposing seat by continuing to publish refutations of unitarian exegesis four centuries after the arrangement's founding. No source outside the dispute attests the reading's correctness; the normative claim remains contested, and this record does not pretend otherwise.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_divine_nature__unitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_divine_nature__unitarian_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_divine_nature__unitarian_reading_tests).
:- end_tests(biblical_divine_nature__unitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.34) because the real cost flows are authority and status rather than material rent: the largest single transfer is the confiscation of credal arbitral authority, alongside the bounded worship grammar imposed on members and the enforcement burdens of boundary maintenance. The series opens elevated (0.36) because the early Transylvanian church was an established body receiving state-backed support; the 1658 dip marks the expulsion of the Polish Brethren, after which the arrangement survives only in voluntary communities with little to collect. Suppression (0.26) is real but light: ministerial subscription, catechetical standardization along the Racovian lineage, fellowship-statement bars on trinitarian teaching, and the Deva prosecution of Ferenc David mark the hard edge, but there is no anathema-and-execution machinery, and the tradition's own toleration record (the Edict of Torda's multi-communion legality) caps it. Theater (0.15) is low: commemorations and identity maintenance exist, but the core teaching and worship functions are performed, not performed-at. Accessibility collapse (0.42) is moderate-low because alternatives remain fully live — the trinitarian reading won nearly everywhere, so this constraint competes rather than forecloses. Resistance (0.58) records four centuries of fierce external opposition (the Brethren's expulsion, statutory criminality of trinity-denial in Britain until the 1813 relief, persistent social cost) against low internal resistance. Claim and metrics are independent: tangled_rope is claimed from the structure (genuine coordination function, named payers, real if light enforcement); the metrics are authored descriptively; any divergence is the engine's measurement, not an error to reconcile. All three tracked series run on one shared seven-point grid (1568, 1605, 1658, 1774, 1825, 1989, 2026); the 1774 suppression bump tracks the era in which trinity-denial remained statutorily punishable in Britain, preceding the 1813 relief.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute opposites. From the credal establishment's position, this reading is an assault on transmitted authority: it strips the offices that guard the deposit and rewards whoever walks away from the guardians. From the lay believer's position, it is the removal of an impost — a duty to affirm what no one can explain, enforced by people who profit from the impossibility. The excluded seat (trinitarian-leaning members) experiences a milder structural image of what dissenters experienced under the credal settlement — barred from teaching, free to leave — and each side weaponizes exactly that comparison. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (lay_believers, congregational_communities) derive low directionality for those seats; the victim declarations (ecclesial_institutional_hierarchy, credal_orthodox_establishment, non_adorationist_reformers) derive high directionality, amplified for the two identity_locked institutional seats whose exit is priced at self-dissolution and damped for no one here — no seat holds arbitrage-grade exit from the dispute itself. One override is authored: powerless agents pinned at d=0.65, because the only powerless seat (trinitarian_leaning_members) carries real conformity costs — barred from the teaching office, silent in deliberations — that the derivation chain cannot see, since excluded voices receive no beneficiary/victim declaration and the per-power-atom fallback would misplace a seat whose exit is open but whose voice is not. Granularity limitation noted: synod_superintendents and congregational_communities share the organized power atom, so per-seat differentiation between administrator and community exceeds the override mechanism's resolution; the role declarations carry that distinction instead.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings are prevented. Romanticizing the reading as pure liberation (pure coordination) would erase the named payers: the hierarchy's confiscated arbitral office and the disciplined internal radicals are real cost-bearers through the same structure that coordinates lay worship. Demonizing it as mere heresy-collection (pure extraction) would erase the genuine coordination function — coherent monotheistic worship, scriptural intelligibility, congregational self-governance — and the light enforcement footprint relative to the credal settlement it opposes. The hybrid computation holds both truths at once. On obsolescence: the founding problem — the exegetical gap between the plain unitary witness its holders see and the credal formulae — remains live, evidenced by continuing scholarship and continuing apologetic production on both sides, so no resolved-mandatrophy declaration is authored; the arrangement persists because the problem persists, not because its function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the biblical_divine_nature kernel; what changes structurally if a sibling reading (trinitarian_reading, modalist_reading) is adopted instead?',
    'Adoption of a sibling flips the entire structural surface: the victim set becomes dissenting readers rather than the hierarchy and credal orthodoxy, the hierarchy moves to the beneficiary/agenda-setter side, effective extraction rises sharply on the credal settlement''s enforcement record, and the classification migrates toward the enforcement-heavy end. The disagreement is located in the numerical-constitution claim itself, not in application or emphasis.',
    'Sibling adoption converts this story''s payers into beneficiaries and vice versa; the two files must never be merged into one constraint with a blended victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    epsilon_referent_under_kernel_reading,
    'Does this story''s epsilon measure the operative unitarian arrangement itself (with its real cost flows: confiscated credal arbitral authority, bounded worship grammar, boundary enforcement, early establishment-era tithes), or the credal settlement as assessed from the unitarian seat?',
    'Corpus-level ruling on kernel-reading referents for advocacy readings whose endorsed arrangement has actual historical instances. This file authors the operative arrangement, matching the declared victim set, and documents the alternative referent in the family note.',
    'If the referent were the credal settlement as the unitarian sees it, epsilon would rise sharply and the victim set would flip to dissenting readers — that is a different constraint file, not a re-measurement of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_under_kernel_reading, conceptual, 'Referent discipline: what the authored epsilon is about.').

omega_variable(
    pre_nicene_mainstream_status,
    'Was pre-Nicene Christian teaching substantially subordinationist, such that the reading''s reference frame approximates the actual early state rather than a later reconstruction?',
    'Patristic scholarship independent of unitarian advocacy: studies of Origen, Tertullian, the Monarchian controversies, and the political mechanics of the fourth-century settlements.',
    'Affirmation strengthens the genealogical anchor of the reference frame and reframes the measured drift as restoration-deficit; denial recasts the reference frame as retrojection and weakens the reading''s founding-problem corroboration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_nicene_mainstream_status, empirical, 'Whether the apostolic-unitary reference frame is historically proximate or constructed.').

omega_variable(
    enforcement_intensity_calibration,
    'Is the light-enforcement picture accurate across all bodies holding this reading, or do disfellowship practices and negative confession clauses in fellowship statements constitute heavier suppression than the scalar encodes?',
    'Comparative audit of disciplinary records across Transylvanian, British, American, and lay-movement bodies, benchmarked against credal establishments'' enforcement records over the same intervals.',
    'Materially higher suppression would push the computed classification toward the harder hybrid boundary and raise effective extraction for internal dissenters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intensity_calibration, empirical, 'Calibration of the enforcement footprint across the movement''s bodies.').

omega_variable(
    status_dispossession_extraction_status,
    'Does the credal elite''s loss of arbitral authority count as cost-bearing through the structure (hybrid-relevant), or as ordinary competitive displacement between rival communions (coordination-pure)?',
    'Conceptual ruling on whether dispossessed rivals'' authority losses enter the effective-transfer ledger, or only losses borne by parties governed by the arrangement.',
    'Decides the boundary between the pure-coordination and hybrid computations for this story; the declared victim set presumes the former.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_dispossession_extraction_status, conceptual, 'Whether dispossession of credal elites is extraction or displacement.').

omega_variable(
    adoration_line_stability,
    'Where exactly does the reading''s own boundary sit on invoking Christ in worship — thanks through Christ as mediator permitted, or non-adorationism — and is that line stable?',
    'Survey of current fellowship statements, catechetical materials, and teaching practice on invocatory prayer across the movement''s bodies.',
    'The devotional cost borne by members and the internal-enforcement record (the Deva prosecution is the historical hard case) both shift with the line''s placement; instability indicates the arrangement polices a moving boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoration_line_stability, conceptual, 'Stability of the internal worship-grammar boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 1568, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1568, biblical_divine_nature__unitarian_reading, theater_ratio, 1568, 0.1).
narrative_ontology:measurement_basis(bibl_tr_t1568, observed).
narrative_ontology:measurement(bibl_tr_t1605, biblical_divine_nature__unitarian_reading, theater_ratio, 1605, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t1605, observed).
narrative_ontology:measurement(bibl_tr_t1658, biblical_divine_nature__unitarian_reading, theater_ratio, 1658, 0.18).
narrative_ontology:measurement_basis(bibl_tr_t1658, observed).
narrative_ontology:measurement(bibl_tr_t1774, biblical_divine_nature__unitarian_reading, theater_ratio, 1774, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t1774, observed).
narrative_ontology:measurement(bibl_tr_t1825, biblical_divine_nature__unitarian_reading, theater_ratio, 1825, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t1825, observed).
narrative_ontology:measurement(bibl_tr_t1989, biblical_divine_nature__unitarian_reading, theater_ratio, 1989, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t1989, observed).
narrative_ontology:measurement(bibl_tr_t2026, biblical_divine_nature__unitarian_reading, theater_ratio, 2026, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1568, biblical_divine_nature__unitarian_reading, base_extractiveness, 1568, 0.36).
narrative_ontology:measurement_basis(bibl_be_t1568, observed).
narrative_ontology:measurement(bibl_be_t1605, biblical_divine_nature__unitarian_reading, base_extractiveness, 1605, 0.33).
narrative_ontology:measurement_basis(bibl_be_t1605, observed).
narrative_ontology:measurement(bibl_be_t1658, biblical_divine_nature__unitarian_reading, base_extractiveness, 1658, 0.24).
narrative_ontology:measurement_basis(bibl_be_t1658, observed).
narrative_ontology:measurement(bibl_be_t1774, biblical_divine_nature__unitarian_reading, base_extractiveness, 1774, 0.27).
narrative_ontology:measurement_basis(bibl_be_t1774, observed).
narrative_ontology:measurement(bibl_be_t1825, biblical_divine_nature__unitarian_reading, base_extractiveness, 1825, 0.31).
narrative_ontology:measurement_basis(bibl_be_t1825, observed).
narrative_ontology:measurement(bibl_be_t1989, biblical_divine_nature__unitarian_reading, base_extractiveness, 1989, 0.33).
narrative_ontology:measurement_basis(bibl_be_t1989, observed).
narrative_ontology:measurement(bibl_be_t2026, biblical_divine_nature__unitarian_reading, base_extractiveness, 2026, 0.34).
narrative_ontology:measurement_basis(bibl_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1568, biblical_divine_nature__unitarian_reading, suppression_requirement, 1568, 0.3).
narrative_ontology:measurement_basis(bibl_su_t1568, observed).
narrative_ontology:measurement(bibl_su_t1605, biblical_divine_nature__unitarian_reading, suppression_requirement, 1605, 0.28).
narrative_ontology:measurement_basis(bibl_su_t1605, observed).
narrative_ontology:measurement(bibl_su_t1658, biblical_divine_nature__unitarian_reading, suppression_requirement, 1658, 0.2).
narrative_ontology:measurement_basis(bibl_su_t1658, observed).
narrative_ontology:measurement(bibl_su_t1774, biblical_divine_nature__unitarian_reading, suppression_requirement, 1774, 0.3).
narrative_ontology:measurement_basis(bibl_su_t1774, observed).
narrative_ontology:measurement(bibl_su_t1825, biblical_divine_nature__unitarian_reading, suppression_requirement, 1825, 0.26).
narrative_ontology:measurement_basis(bibl_su_t1825, observed).
narrative_ontology:measurement(bibl_su_t1989, biblical_divine_nature__unitarian_reading, suppression_requirement, 1989, 0.22).
narrative_ontology:measurement_basis(bibl_su_t1989, observed).
narrative_ontology:measurement(bibl_su_t2026, biblical_divine_nature__unitarian_reading, suppression_requirement, 2026, 0.26).
narrative_ontology:measurement_basis(bibl_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (biblical_divine_nature), three readings emitting three distinct constraints. The trinitarian reading's file carries high institutional authority, credal beneficiaries, and dissenters as victims, with epsilon far above this story's on the credal settlement's enforcement record; the modalist reading's file carries a third victim set. This file's epsilon (0.34) is authored for the operative unitarian arrangement, matching its declared victim set (institutional hierarchy and credal orthodoxy). The family is linked because all three readings read the same fixed text corpus and each defines itself against the others; evidentiary shifts propagate across the edges in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_divine_nature__unitarian_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
