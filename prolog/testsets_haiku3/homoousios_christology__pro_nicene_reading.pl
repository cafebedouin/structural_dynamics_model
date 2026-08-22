% ============================================================================
% CONSTRAINT STORY: homoousios_christology__pro_nicene_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__pro_nicene_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: homoousios_christology__pro_nicene_reading
 *   human_readable: Pro-Nicene Homoousios Christology (Identical Divine Substance)
 *   domain: ecclesiastical/theological/political
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) and its successors establish that Christ
 *   is homoousios ('of one substance') with the Father—sharing identical
 *   divine essence, unqualified, eternal, uncreated. This reading is one
 *   instantiation of a contested kernel (the identity and status of Christ in
 *   Christian theology). The pro-Nicene reading asserts this formula as
 *   apostolic truth, enforced through anathema and imperial law against Arian
 *   (subordinationist), semi-Arian (compromise), and alternative
 *   Christological traditions. The constraint combines genuine theological
 *   coordination (unifying Christian doctrine around a single Christological
 *   claim) with substantial extraction (concentrating interpretive authority
 *   in the imperial-ecclesiastical apparatus, suppressing rival theologies,
 *   binding dissenting clergy and communities through identity-lock and legal
 *   disability). The story instantiates THIS reading only—the pro-Nicene
 *   position—and authors its ε, suppression, and stakeholder structure as the
 *   reading itself understands them.
 *
 * KEY AGENTS:
 *   - Imperial-ecclesiastical alliance: Constantine and imperial successors + Nicene bishops (esp. Athanasius, later Cyril). Agenda-setters and beneficiaries of unified doctrine and consolidated authority.
 *   - Arian clergy: Arius, Eusebius of Nicomedia, later Gothic and Vandal clergy. Targets of anathema and exile; identity-locked to suppressed theology.
 *   - Semi-Arian communities: Theological mediators holding homoiousios (similar substance); pushed to conform or be condemned.
 *   - Unaffiliated believers: Lay Christians whose doctrine is determined by imperial-ecclesiastical pronouncement; beneficiaries of unified faith and costbearers of suppressed traditions.
 *   - Alternative Christological traditions: Nestorian, Monophysite, and other non-Nicene schools; excluded from institutional legitimacy.
 *   - Imperial legal apparatus: Governors, courts, military enforcing homoousios through law, exile, and temple confiscation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, 0.68).
domain_priors:suppression_score(homoousios_christology__pro_nicene_reading, 0.76).
domain_priors:theater_ratio(homoousios_christology__pro_nicene_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(homoousios_christology__pro_nicene_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__pro_nicene_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__pro_nicene_reading, "Pro-Nicene Homoousios Christology (Identical Divine Substance)").
narrative_ontology:topic_domain(homoousios_christology__pro_nicene_reading, "ecclesiastical/theological/political").

domain_priors:requires_active_enforcement(homoousios_christology__pro_nicene_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__pro_nicene_reading, '5d61900b-6ad9-46bc-8d53-481a363cefb7').
narrative_ontology:cs_kernel_codification('5d61900b-6ad9-46bc-8d53-481a363cefb7', fixed_text).
narrative_ontology:cs_authority_grounding('5d61900b-6ad9-46bc-8d53-481a363cefb7', extraction).
narrative_ontology:cs_interpretation_layer_present('5d61900b-6ad9-46bc-8d53-481a363cefb7').
narrative_ontology:cs_reading_relation('5d61900b-6ad9-46bc-8d53-481a363cefb7', homoousios_christology__arian_reading, forecloses).
narrative_ontology:cs_reading_relation('5d61900b-6ad9-46bc-8d53-481a363cefb7', homoousios_christology__semi_arian_reading, coexists_with).
narrative_ontology:cs_axiom('5d61900b-6ad9-46bc-8d53-481a363cefb7', foundational, christ_homoousios_identical_substance).
narrative_ontology:cs_axiom_status(christ_homoousios_identical_substance, holdable).
narrative_ontology:cs_axiom_grounding('5d61900b-6ad9-46bc-8d53-481a363cefb7', christ_homoousios_identical_substance, deontological).
narrative_ontology:cs_axiom('5d61900b-6ad9-46bc-8d53-481a363cefb7', foundational, apostolic_tradition_continuity_through_episcopal_succession).
narrative_ontology:cs_axiom_status(apostolic_tradition_continuity_through_episcopal_succession, holdable).
narrative_ontology:cs_axiom_grounding('5d61900b-6ad9-46bc-8d53-481a363cefb7', apostolic_tradition_continuity_through_episcopal_succession, conventional).
narrative_ontology:cs_reference_frame('5d61900b-6ad9-46bc-8d53-481a363cefb7', apostolic_tradition_theological_consensus).
narrative_ontology:cs_drift_state('5d61900b-6ad9-46bc-8d53-481a363cefb7', post_theodosius_imperial_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d61900b-6ad9-46bc-8d53-481a363cefb7', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(homoousios_christology__pro_nicene_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_alliance).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, arian_clergy).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, semi_arian_communities).
narrative_ontology:constraint_victim(homoousios_christology__pro_nicene_reading, unaffiliated_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(homoousios_christology__pro_nicene_reading, unaffiliated_believers).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, apostolic_tradition_continuity).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, trinitarian_monotheism).
narrative_ontology:constraint_vindicates(homoousios_christology__pro_nicene_reading, incarnational_soteriology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops assembled in council and their successors enforce the homoousios formula as the single legitimate expression of Christology. They administer catechesis, define heresy, and deploy anathema (exclusion from communion) against rival formulations. They benefit from imperial backing that treats heterodoxy as sedition, and their institutional coherence depends on doctrinal uniformity. Exit for bishops means renouncing ecclesiastical office or explicit heresy charges.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, constrained, continental).

% The Roman Empire's executive (Constantine, his successors, the imperial administration) vests authority in the Council of Nicaea and enforces its creed through imperial law and territorial exile. The empire benefits from a unified Christian theology that delegitimizes factional splits—political unity is enforced through doctrinal unity. The emperor sits above the bishops but depends on their consensus to legitimate imperial rule as divinely ordained. Exit would require fracturing either the imperial apparatus or the church structure itself.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_alliance, agenda_setter,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, imperial_ecclesiastical_alliance, beneficiary).

% Clergy and bishops who affirm Christ's creation and subordination (Arian theology) are condemned as heretics, defrocked, exiled, or imprisoned. Their scriptural readings (John 14:28, 1 Corinthians 11:3, Colossians 1:15) are declared misinterpretations. Their ordinations are voided. They cannot exit without renouncing not only their theological position but their entire priestly identity and community—they are identity-locked. The constraint persists by making their core self-understanding incompatible with ecclesiastical existence.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, arian_clergy, payer,
    moderate, biographical, identity_locked, continental).

% Clergy and lay communities holding the homoiousios (similar substance) formula—a compromise attempting to bridge Nicene and Arian positions—are declared ambiguous, theologically unstable, and subject to coercion to affirm homoousios or face exclusion. Their historical role as mediators is erased; their position is reframed as evasion rather than mediation. Exit requires abandoning their theological genealogy and adopting the full homoousios formula, collapsing their constructed middle ground.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, semi_arian_communities, payer,
    moderate, biographical, identity_locked, regional).

% Lay believers outside the episcopal hierarchy receive a single, unified Christian doctrine that claims apostolic continuity and imperial endorsement. This simplifies religious instruction and provides a coherent identity. They also bear costs: believers whose family traditions align with Arian or semi-Arian theology face social ostracism, legal restrictions on worship, and the erasure of their inherited scriptural reading. Their exit options are constrained by social belonging, legal status, and geographic locality.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, unaffiliated_believers, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(homoousios_christology__pro_nicene_reading, unaffiliated_believers, payer).

% Doctrinal traditions emphasizing Christ's humanity (Nestorian), Christ's singular nature absorbing divinity (Monophysite), or other non-Nicene formulae are structurally barred from ecclesiastical office, imperial recognition, and territorial expansion. They persist in marginal communities but cannot compete for institutional legitimacy. Their exclusion is what the enforcement machinery exists to maintain. Exit from exclusion would require reversing the imperial-ecclesiastical alliance itself.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, alternative_christological_traditions, excluded,
    moderate, generational, trapped, continental).

% Imperial courts, governors, and military enforce the homoousios formula through law: exiling bishops, banning heterodox councils, confiscating temples, criminalizing heterodox preaching. The apparatus benefits from a delegitimized rival authority structure (fragmented Christianity would weaken imperial claim to divine ordination). Exit would mean abandoning a primary tool for consolidating imperial power. The apparatus is constrained by the need to maintain church cooperation.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, imperial_legal_apparatus, agenda_setter,
    institutional, generational, constrained, continental).

% Contemporary historians, theologians, and archivists analyze the Nicene settlement and its aftermath through textual evidence, council minutes, imperial edicts, and ecclesiastical commentaries. They operate outside the constraint's direct enforcement but provide the analytical frame for understanding its structure, persistence, and contestation.
narrative_ontology:constraint_stakeholder(homoousios_christology__pro_nicene_reading, historical_witnesses, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(homoousios_christology__pro_nicene_reading, historical_witnesses).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_christology__pro_nicene_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(homoousios_christology__pro_nicene_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine theological crisis within early Christianity: competing Christological formulae (Arian subordinationism vs. homoousios vs. semi-Arian compromise) created factional divisions that fractured both church and empire. The Nicene formula—'of one substance with the Father'—claims to settle the dispute by establishing a single authoritative reading of apostolic tradition, enabling unified catechesis, coherent liturgy, and a single ecclesiastical hierarchy aligned with imperial governance.
% TRANSFER_FUNCTION: Moves theological authority from decentralized regional bishops and grassroots exegesis to a centralized imperial-ecclesiastical council (Nicaea, subsequent ecumenical councils) and its enforcement structure. Transfers ecclesiastical power upward to metropolitans and the emperor's judicial arm. Transfers doctrinal authority from scriptural interpretation (where Arian and Nicene partisans had competing exegetical traditions) to conciliar pronouncement backed by anathema and imperial law. Extracts obedience from non-conforming clergy and communities through exile, defrocking, and legal disability.
% ABSENT_VOICES: Arian clergy and theologians are structurally excluded—anathematized and barred from councils. Semi-Arian mediators whose entire project was to bridge the divide are pushed to a binary choice (conform to homoousios or be condemned). Eastern regional churches with strong theological traditions independent of Rome are subordinated to conciliar authority. Lay believers whose scriptural inheritance aligns with Arian exegesis (Christ as firstborn creation, subordinate agent) are offered no legitimate channel to express this reading; their absence from the council floor is replaced by episcopal pronouncement on their behalf. Women mystics and contemplatives whose lived theology emphasized union with Christ are marginalized by the technical philosophical turn the homoousios formula requires.
% DISAPPEARANCE_RATIONALE: If the homoousios formula and its enforcement machinery vanished, early Christian communities would immediately fragment into competing Christological schools (Arian, semi-Arian, Apollinarian, Nestorian, Monophysite all regain institutional viability). The unified episcopal hierarchy collapses; regional churches revive independent authority. Imperial theology loses its single binding creed, forcing the state either to tolerate plurality or to suppress Christianity entirely. The constraint's persistence is not natural to Christianity—it is enforced by anathema and imperial law. Remove the enforcement and the world reorganizes around the suppressed alternatives.
% FOUNDING_PROBLEM: In the early 4th century, Arius of Alexandria taught that Christ, though divine, was created by the Father and is subordinate to the Father's infinite nature—a position held by significant clergy and lay believers across the eastern Mediterranean. This challenged the intuition among some bishops that Christ's divine status must be absolute and unqualified to sustain a coherent account of salvation and the Trinity. Local councils condemned Arius; the controversy spread. Different regions backed different readings. The empire faced a fractured Christianity where doctrinal disunity threatened political unity. The Council of Nicaea (325 CE) was convened to produce a single authoritative formula binding all bishops.
% FOUNDING_PROBLEM_CORROBORATION: Imperial bishops at the council and their successors (Athanasius, Cyril, later Chalcedon councils) attest the founding problem was real and live: Arianism represented a genuine theological error that threatened orthodox faith and required anathematization. Arian clergy and sympathizers (Eusebius of Nicomedia, later Gothic and Vandal bishops) attest that the problem was constructed—Nicene bishops used imperial power to suppress a legitimate scriptural reading held by many clergy and communities. Modern historians and theologians outside either constituency (Hanson, Williams, Ayres) document that both readings of Scripture had patristic precedent; the 'problem' was a selection by conciliar authority of one reading over another, backed by imperial force, not a resolution of contradiction discovered in doctrine itself.
narrative_ontology:disappearance_verdict(homoousios_christology__pro_nicene_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_christology__pro_nicene_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__pro_nicene_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__pro_nicene_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_christology__pro_nicene_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__pro_nicene_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_christology__pro_nicene_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_christology__pro_nicene_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint concentrates theological authority upward into the imperial-ecclesiastical hierarchy, displacing regional episcopal autonomy and exegetical pluralism. The rate rises steeply 325→380 as imperial backing hardens (Theodosius I's edict of 380 makes Nicene Christianity the empire's official religion) and rival schools lose institutional space. It plateaus 380→451 as enforcement becomes routine and alternative Christologies are driven underground or geographically marginal. Suppression (0.76) is high because persistence depends on active anathema (exclusion from communion, defrocking), imperial exile, and legal bans on heterodox preaching—the constraint cannot be sustained by participant preference alone. The suppression trajectory 0.38→0.76 tracks the imperial apparatus's buildup (Constantine convenes the council; Theodosius makes enforcement law; by 415, Cyril's expulsion of Nestorians from Alexandria is routine state action). Theater (0.42) is moderate: the Nicene formula performs real theological coordination, but a substantial share of enforcement activity defends the exclusivity of the formula against rivals rather than the coordination function itself. As alternatives are driven underground, less enforcement activity is needed for coordination, so theater rises—the performative component (recitation of creed, formal statements of orthodoxy) increases relative to active suppression. All measurements share one time grid (interval [325, 451]) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (imperial-ecclesiastical alliance, Nicene bishops) and the payer seats (Arian clergy, semi-Arians, dissenting believers) compute radically different constraint types from the same structure. From the agenda-setter's position, the constraint is rope or light tangled rope—a genuine coordination solution to doctrinal chaos that requires enforcement against the non-rational resistance of heretics. From the payer seats—especially Arian clergy and semi-Arian communities—the constraint is snare: a power grab dressed as theological truth, enforcing conformity through anathema and exile. The engine computes these divergent types per seat from the authored structural data (beneficiary/victim declarations, power differentials, exit options) without resolving the perspectival gap. The claim/metric independence is deliberate: the story claims tangled_rope; the metrics encode high extraction, high suppression, and active enforcement; the engine's per-seat computation will show whether the claim is supported or whether a higher-extraction type emerges from the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim declarations and exit options. Nicene bishops and the imperial apparatus are beneficiaries (collect authority, status, and institutional power; have arbitrage-grade exit options—they can walk away from the creed and remain powerful actors, though not as coordinators of Christianity). d for these seats ≈ 0.2 (near beneficiary end). Arian clergy and semi-Arians are victims (pay through identity-lock, exile, defrocking, and theological delegitimization; exit is exceptionally costly—renouncing their theological identity means renouncing their priestly identity and community belonging). d for these seats ≈ 0.85–0.90 (near full target end). Unaffiliated believers are symmetric or slightly victimized (benefit from unified doctrine and simplified catechesis; pay indirectly through suppressed inheritance traditions and social coercion). d for these seats ≈ 0.50–0.60. The engine amplifies effective extraction (χ) for target seats and dampens it for beneficiary seats using these d values; the result is a constraint whose extractiveness is very high from the target seats' perspective and minimal from the beneficiary seats' perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to solve the founding problem: theological disunity threatened both Christian coherence and imperial political unity. The pro-Nicene reading claims this problem remains live—Arianism is a persistent threat to Christian orthodoxy. The payer seats (Arian clergy, semi-Arians) attest the founding problem is dead or constructed: the 'threat' of Arianism was theological pluralism, suppressed by imperial force, not an error that required suppression for coherence. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) signals mandatrophy: if the constraint disappeared, the world would rearrange (factions would splinter, alternatives would re-emerge), but the constraint's persistence depends on continuous enforcement—it is not naturally stable. The constraint shows classic tangled-rope mandatrophy: genuine coordination function (unified creed) layered with extraction (concentrated authority, suppressed alternatives); the coordination benefits some seats (imperial-ecclesiastical alliance) while extraction harms others (Arian clergy, semi-Arians). A constraint with such asymmetric beneficiary structure, high suppression, and active identity-lock on payers is at high risk of misclassification as 'coordination' when the distribution of coordination benefit is so skewed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scripture_interpretation_ambiguity,
    'Do the foundational Christian scriptures (esp. Gospel of John, Pauline epistles, Hebrews) more naturally support a homoousios (identical substance) Christology or a subordinationist (Arian) Christology?',
    'Textual exegesis by historians of early Christian hermeneutics, comparing how Arian and Nicene theologians interpreted the same passages (John 14:28 ''the Father is greater than I''; 1 Corinthians 11:3 ''the head of Christ is God''; Colossians 1:15 ''firstborn of all creation''). Resolution requires acknowledging that BOTH readings find scriptural warrant; the ''problem'' solved by Nicaea was selection, not contradiction-resolution.',
    'If scriptures are genuinely ambiguous and both readings have patristic precedent, the constraint''s claim to resolve a doctrinal error is undermined—it becomes a political victory rather than a theological discovery. The constraint would reclassify from tangled_rope (genuine coordination + extraction) toward snare (extraction dressed as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scripture_interpretation_ambiguity, empirical, 'Whether the homoousios formula resolves genuine theological contradiction or imposes one reading over another.').

omega_variable(
    identity_lock_mechanism_ambiguity,
    'For Arian clergy and semi-Arian communities, is the suppression that binds them to non-conformity structural (loss of office, exile, legal disability) or internalized (they genuinely believe the pro-Nicene position and have changed their theology)?',
    'Historical tracking of post-anathema communities: if Arian clergy and believers recover doctrinal autonomy after imperial enforcement weakens (e.g., in Gothic kingdoms, Syrian monasteries), what theology do they return to? If they revert to Arian or semi-Arian positions, suppression was primarily structural; if they have internalized the homoousios formula, suppression was internalized.',
    'If suppression is primarily structural, the identity-lock is externally imposed and the constraint''s extraction is higher than measured (the target bears not only economic/legal costs but full cognitive capture). If suppression is internalized, the constraint''s theater component is higher—what appears as enforcement is partly routine compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_ambiguity, empirical, 'Whether Arian clergy remain suppressed through external force or internalized acceptance.').

omega_variable(
    kernel_contest_foreclosure_ambiguity,
    'Does the pro-Nicene axiom (homoousios = identical substance) logically foreclose the Arian axiom (Christ is created and subordinate), or are both salvageable within different philosophical frameworks?',
    'Philosophical analysis of the logical space: if ''identical substance'' and ''created subordinate'' are genuinely contradictory, foreclosure holds; if both can be true under different interpretations of ''substance'' or ''creation'' (e.g., substance as ousia vs. energeia, creation as temporal origin vs. causal dependence), the kernel remains open and the contest is political, not logical.',
    'If genuinely contradictory, the pro-Nicene reading forecloses Arianism and the sibling relationship is foreclosure (rare). If both are salvageable, the relationship is coexistence or influence, not foreclosure, and the constraint''s epistemic legitimacy rests on political decision, not logical necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclosure_ambiguity, conceptual, 'Whether the homoousios formula and Arian subordinationism are logically contradictory or both philosophically defensible.').

omega_variable(
    imperial_extraction_independence,
    'Would the pro-Nicene Christology have achieved unified ecclesiastical adoption without imperial backing, or does its dominance depend entirely on state enforcement?',
    'Comparative history: in regions where imperial backing is weak or withdrawn (e.g., Coptic and Syrian churches, post-imperial Germanic kingdoms), does pro-Nicene theology lose ground or persist? If it persists despite weak state support, the coordination function is genuine; if it collapses or undergoes revision, extraction depends on imperial apparatus.',
    'If pro-Nicene theology collapses without imperial backing, the constraint is purely extractive (the coordination claim is false—theologians would not naturally converge on homoousios). If it persists, the coordination function is genuine but the extraction (concentrated authority, suppressed alternatives) layers on top of real coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_extraction_independence, empirical, 'Whether homoousios theology would be naturally selected or requires imperial enforcement.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Was the pro-Nicene reading selected at Nicaea because it is the most defensible reading of apostolic tradition, or because it aligned with imperial preferences for unified doctrine?',
    'Historical inquiry into the council''s decision-making: did bishops argue from Scripture and patristic sources, or did Constantine''s authority frame the outcome? Did bishops with competing views have genuine persuasion, or was the homoousios formula imposed? Did post-Nicene development (Athanasius, Cyril) refine a naturally emerging consensus or defend a politically imposed settlement?',
    'If selection was evidence-driven and consensual, the reading''s legitimacy rests on theological merit. If selection was politically driven, the reading''s legitimacy rests on power and the constraint is extraction-primary. This determines whether mandatrophy obtains (founding problem dead, but constraint persists through inertia) or whether the constraint is still serving the founding coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Whether Nicene selection represents theological discovery or political power assertion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__pro_nicene_reading, 325, 451).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__pro_nicene_reading, theater_ratio, 325, 0.28).
narrative_ontology:measurement_basis(homo_tr_t325, projected).
narrative_ontology:measurement(homo_tr_t350, homoousios_christology__pro_nicene_reading, theater_ratio, 350, 0.35).
narrative_ontology:measurement_basis(homo_tr_t350, observed).
narrative_ontology:measurement(homo_tr_t380, homoousios_christology__pro_nicene_reading, theater_ratio, 380, 0.41).
narrative_ontology:measurement_basis(homo_tr_t380, observed).
narrative_ontology:measurement(homo_tr_t415, homoousios_christology__pro_nicene_reading, theater_ratio, 415, 0.44).
narrative_ontology:measurement_basis(homo_tr_t415, observed).
narrative_ontology:measurement(homo_tr_t451, homoousios_christology__pro_nicene_reading, theater_ratio, 451, 0.42).
narrative_ontology:measurement_basis(homo_tr_t451, projected).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__pro_nicene_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement_basis(homo_be_t325, projected).
narrative_ontology:measurement(homo_be_t350, homoousios_christology__pro_nicene_reading, base_extractiveness, 350, 0.55).
narrative_ontology:measurement_basis(homo_be_t350, projected).
narrative_ontology:measurement(homo_be_t380, homoousios_christology__pro_nicene_reading, base_extractiveness, 380, 0.68).
narrative_ontology:measurement_basis(homo_be_t380, observed).
narrative_ontology:measurement(homo_be_t415, homoousios_christology__pro_nicene_reading, base_extractiveness, 415, 0.7).
narrative_ontology:measurement_basis(homo_be_t415, observed).
narrative_ontology:measurement(homo_be_t451, homoousios_christology__pro_nicene_reading, base_extractiveness, 451, 0.68).
narrative_ontology:measurement_basis(homo_be_t451, projected).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__pro_nicene_reading, suppression_requirement, 325, 0.38).
narrative_ontology:measurement_basis(homo_su_t325, projected).
narrative_ontology:measurement(homo_su_t350, homoousios_christology__pro_nicene_reading, suppression_requirement, 350, 0.61).
narrative_ontology:measurement_basis(homo_su_t350, observed).
narrative_ontology:measurement(homo_su_t380, homoousios_christology__pro_nicene_reading, suppression_requirement, 380, 0.76).
narrative_ontology:measurement_basis(homo_su_t380, observed).
narrative_ontology:measurement(homo_su_t415, homoousios_christology__pro_nicene_reading, suppression_requirement, 415, 0.78).
narrative_ontology:measurement_basis(homo_su_t415, observed).
narrative_ontology:measurement(homo_su_t451, homoousios_christology__pro_nicene_reading, suppression_requirement, 451, 0.76).
narrative_ontology:measurement_basis(homo_su_t451, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__pro_nicene_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_christology__pro_nicene_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, homoousios_christology__semi_arian_reading).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, imperial_theology_legitimacy).
narrative_ontology:affects_constraint(homoousios_christology__pro_nicene_reading, episcopal_authority_centralization).

% DUAL FORMULATION NOTE:
% The homoousios christology kernel decomposes into three structurally distinct constraints, one per reading. The pro-Nicene reading (this story) establishes identity_coordination backed by imperial law and anathema. The Arian reading (sibling constraint) subordinates Christ and distributes authority more widely. The semi-Arian reading (sibling constraint) attempts mediation and has lower enforcement requirements. Each story carries its own ε, beneficiary/victim structure, and stakeholder seats. They are linked via network.affects_constraints because the success of one reading directly constrains the viability of others—imperial backing for pro-Nicene exclusion limits Arian institutional space; Arian persistence in peripheral regions influences pro-Nicene enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_christology__pro_nicene_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
