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
 *   human_readable: Unitarian Reading of Divine Nature: Father Alone is God
 *   domain: theology/religious_authority/doctrinal_history
 *
 * SUMMARY:
 *   This constraint instantiates the unitarian reading of the contested
 *   kernel 'biblical_divine_nature.' The kernel is a stabilized commitment to
 *   Scripture—the canonical texts are fixed—but their interpretation is
 *   contested. The unitarian reading asserts that Scripture teaches the
 *   Father's numerical singularity as God and the Son's subordination or
 *   createdness. This reading competes with trinitarian and modalist readings
 *   that derive different divine-nature claims from the same texts. The
 *   constraint is the institutional enforcement of trinitarian orthodoxy
 *   against unitarian exegeses. The unitarian reading benefits from a reading
 *   that positions itself as scriptural fidelity ('what Scripture plainly
 *   says') versus institutional corruption ('what the councils added'). The
 *   trinitarian hierarchy pays the cost of defending a doctrinally complex
 *   claim against the apparent simplicity of unitarian monotheism, and faces
 *   the existential threat that if unitarianism is vindicated, institutional
 *   authority is undermined. The constraint's persistence depends on
 *   suppressing unitarian exegeses in the academy, pulpit, and catechesis.
 *   Its classification as 'snare' reflects the asymmetric extraction:
 *   institutional beneficiaries (magisterial councils, trinitarian
 *   theologians, ordained hierarchy) enforce the constraint; victims include
 *   not only unitarian exegetes and movements (but they benefit from this
 *   reading's framework), but also the trinitarian institutional hierarchy
 *   itself (identity-locked, forced to continuously defend an exegetically
 *   contested doctrine). The ordinary believers in both traditions are also
 *   payers: they inherit the constraint's conflict.
 *
 * KEY AGENTS:
 *   - unitarian_exegetes: agenda-setters of the reading; face professional sanction; constrained exit
 *   - institutional_trinitarian_hierarchy: enforce orthodoxy; identity-locked to trinitarian dogma; payers of enforcement cost
 *   - anti_trinitarian_reform_movements: beneficiaries; mobile exit; gain legitimacy from unitarian reading framework
 *   - trinitarian_theologians: payers; forced to defend a complex doctrine continuously
 *   - ordinary_believers_trinitarian: beneficiary (belonging, unity) and payer (constrained understanding); constrained exit
 *   - ordinary_believers_unitarian: beneficiary (scriptural simplicity) and payer (minority status, discrimination); identity-locked
 *   - ecumenical_councils: agenda-setters; enforcement machinery
 *   - early_church_exegetes: excluded; their unitarian/subordinationist readings were suppressed
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
narrative_ontology:constraint_claim(biblical_divine_nature__unitarian_reading, snare).
narrative_ontology:human_readable(biblical_divine_nature__unitarian_reading, "Unitarian Reading of Divine Nature: Father Alone is God").
narrative_ontology:topic_domain(biblical_divine_nature__unitarian_reading, "theology/religious_authority/doctrinal_history").

domain_priors:requires_active_enforcement(biblical_divine_nature__unitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_divine_nature__unitarian_reading, '3b620818-12c1-4b8a-b6b5-b355d9b98941').
narrative_ontology:cs_kernel_codification('3b620818-12c1-4b8a-b6b5-b355d9b98941', fixed_text).
narrative_ontology:cs_authority_grounding('3b620818-12c1-4b8a-b6b5-b355d9b98941', extraction).
narrative_ontology:cs_interpretation_layer_present('3b620818-12c1-4b8a-b6b5-b355d9b98941').
narrative_ontology:cs_reading_relation('3b620818-12c1-4b8a-b6b5-b355d9b98941', biblical_divine_nature__trinitarian_reading, forecloses).
narrative_ontology:cs_reading_relation('3b620818-12c1-4b8a-b6b5-b355d9b98941', biblical_divine_nature__modalist_reading, coexists_with).
narrative_ontology:cs_axiom('3b620818-12c1-4b8a-b6b5-b355d9b98941', foundational, numerical_singularity_of_deity).
narrative_ontology:cs_axiom_status(numerical_singularity_of_deity, holdable).
narrative_ontology:cs_axiom_grounding('3b620818-12c1-4b8a-b6b5-b355d9b98941', numerical_singularity_of_deity, empirically_contingent).
narrative_ontology:cs_axiom('3b620818-12c1-4b8a-b6b5-b355d9b98941', secondary, apostolic_simplicity_doctrine).
narrative_ontology:cs_axiom_status(apostolic_simplicity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('3b620818-12c1-4b8a-b6b5-b355d9b98941', apostolic_simplicity_doctrine, conventional).
narrative_ontology:cs_reference_frame('3b620818-12c1-4b8a-b6b5-b355d9b98941', father_alone_monotheism_scriptural).
narrative_ontology:cs_drift_state('3b620818-12c1-4b8a-b6b5-b355d9b98941', modern_pluralism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3b620818-12c1-4b8a-b6b5-b355d9b98941', '').
narrative_ontology:cs_kernel_id(biblical_divine_nature__unitarian_reading, biblical_divine_nature).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, anti_trinitarian_reform_movements).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, orthodox_credal_councils).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, trinitarian_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, ordinary_believers_trinitarian).
narrative_ontology:constraint_beneficiary(biblical_divine_nature__unitarian_reading, ordinary_believers_unitarian).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, ordinary_believers_trinitarian).
narrative_ontology:constraint_victim(biblical_divine_nature__unitarian_reading, ordinary_believers_unitarian).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and clergy who read biblical texts as teaching the Father's unique divine status and the Son's subordination. They maintain scriptural interpretations centered on numerical monotheism and argue trinitarian doctrine is post-apostolic institutional addition. They set the interpretive agenda for unitarian communities but face professional sanction in trinitarian-dominated academic and clerical hierarchies. Their exit to secular academia or unitarian denominations costs institutional standing in mainstream Christianity.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, unitarian_exegetes, agenda_setter,
    organized, generational, constrained, global).

% The organized Church (Catholic, Orthodox, mainline Protestant) whose magisterial authority canonized trinitarianism as binding dogma through councils and creeds. Their legitimacy rests on the claim that trinitarianism is apostolic tradition, not institutional innovation. They enforce the constraint by controlling curriculum, ordination, and excommunication. But they are locked in: abandoning trinitarianism dissolves the institutional authority itself. They are payers because they must continuously defend a complex doctrine against the apparent simplicity of unitarian monotheism and must actively suppress unitarian exegeses.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy, payer,
    institutional, civilizational, identity_locked, global).

% Communities that adopt unitarian readings (Arian churches, Socinians, Unitarian Universalists, Christadelphians, some evangelical exegetes). They benefit from the unitarian reading framework: it organizes their theology around apparent scriptural simplicity, provides intellectual coherence for dissent against institutional orthodoxy, and establishes their religious identity as recovery of apostolic truth versus institutional corruption. Their exit has historically been high (establishing alternative denominations); regional variation is substantial (Unitarianism more accepted in some areas).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, anti_trinitarian_reform_movements, beneficiary,
    moderate, biographical, mobile, regional).

% Academic and church theologians whose professional status and intellectual framework depend on defending trinitarian orthodoxy. They publish defenses, counter unitarian arguments in peer-reviewed journals, maintain doctrinal coherence in seminaries and catechesis. The constraint requires their continuous labor (theological apologetics, institutional gatekeeping). They are identity-locked: their reputation and standing rest on trinitarian defense. If trinitarianism is abandoned, their career trajectory is invalidated.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, trinitarian_theologians, payer,
    institutional, generational, identity_locked, global).

% Believers in trinitarian-tradition churches taught that the Trinity is apostolic dogma and God's revealed truth. They benefit from doctrinal unity and institutional belonging; the Trinity is presented as beautiful mystery and binding orthodoxy. They are payers because their faith is constrained to require assent to a doctrinally complex claim; access to alternative readings is suppressed by homiletics and catechesis; their exit is low (leaving costs community, family, spiritual home).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, ordinary_believers_trinitarian, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, ordinary_believers_trinitarian, payer).

% Believers in unitarian traditions who read Scripture as teaching one God, the Father. They benefit from doctrinal simplicity and scriptural reading that confirms their faith. They are payers: their reading is branded heretical by large institutional bodies; they inherit the conflict of the doctrinal war; their children face discrimination in mixed-tradition contexts. Their exit is identity-locked (the unitarian faith is their identity; leaving means leaving themselves).
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, ordinary_believers_unitarian, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(biblical_divine_nature__unitarian_reading, ordinary_believers_unitarian, payer).

% The magisterial bodies (Nicaea 325, Constantinople 381, Chalcedon 451, Fourth Lateran 1215, Trent, Vatican I) that declared trinitarianism binding and anathematized unitarian positions. They function as the enforcement machinery's legitimating authority: their decrees define orthodoxy; their canons enforce it through institutional discipline and excommunication of heretics.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, ecumenical_councils, agenda_setter,
    institutional, civilizational, analytical, global).

% The diverse exegetical community of the second to fifth centuries (including Arians, Subordinationists, Apollinarians) who offered competing readings of divine nature and Father-Son relation. By post-Nicaean institutional consolidation, many were excluded from legitimate theological conversation. Their exclusion is the constraint's enforcement mechanism: the heresiological move that brackets unitarian readings as ipso facto illegitimate rather than as contending exegetical positions. Their voices remain absent from mainstream Christian discourse.
narrative_ontology:constraint_stakeholder(biblical_divine_nature__unitarian_reading, early_church_exegetes, excluded,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_divine_nature__unitarian_reading, institutional_trinitarian_hierarchy).
narrative_ontology:fixing_cost_class(biblical_divine_nature__unitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes the Church's self-understanding as apostolic and unchanged: by fixing trinitarian doctrine as apostolic tradition, the institutional Church coordinates its legitimacy claim around doctrinal continuity with the early apostles (as trinitarians read the early Church). The unitarian reading directly challenges this coordination: it claims the early Church was unitarian and the trinitarian doctrine is post-apostolic institutional addition. This reading does NOT offer an alternative coordination function; instead, it contests whether the institutional coordination is legitimate.
% TRANSFER_FUNCTION: Moves intellectual authority, interpretive control, and institutional legitimacy from dispersed scriptural interpretation to centralized magisterial bodies. Unitarian exegetes are forced to present their readings as dissent, heterodoxy, or marginal exegesis rather than as equally valid scriptural reading. Trinitarian institutions collect the authority to define orthodoxy and enforce it through ordination standards, seminary curricula, and excommunication.
% ABSENT_VOICES: Early Church exegetes whose unitarian and subordinationist readings were suppressed (Arius, Eusebius of Nicomedia, and the broader Arian tradition) are structurally absent from post-Nicaean orthodoxy. Their silence is maintained by the constraint: unitarian exegetes in modern contexts who attempt to recover these voices face professional sanction and institutional exclusion. The constraint operates to keep these historical alternatives de-legitimated.
% DISAPPEARANCE_RATIONALE: If this constraint (the institutional enforcement of trinitarian orthodoxy against unitarian readings) disappeared, the Church's legitimacy claim as apostolic would collapse—or rather, would be contested on open scriptural grounds rather than settled by magisterial decree. Denominations would splinter further around exegetical reading; seminary training would no longer enforce trinitarian dogma; believers could encounter unitarian readings as live scriptural options rather than heresies. The Church's institutional unity, as currently structured, rests substantially on enforced doctrinal conformity around the Trinity.
% FOUNDING_PROBLEM: In the early centuries post-Constantine, the Empire and the Church faced a unified-identity crisis: what makes Christian believers one people? Doctrinal orthodoxy became the institutional answer. Nicaea (325) and subsequent councils targeted doctrinal unity as the binding principle. Trinitarianism was selected (over Arianism) as the binding orthodoxy not because it was the exegetically strongest reading, but because it was the position of the imperial-backed coalition. The institutional problem was: how to enforce doctrinal conformity across a geographically dispersed Church. The constraint instantiates that enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Trinitarian historians attest the founding problem was real: early Church diversity required doctrinal settlement for institutional unity. Unitarian historians and exegetes contest the diagnosis: the early Church was less doctrinally uniform than trinitarians claim, and the constraint arose not from doctrinal necessity but from imperial politics. External corroboration comes from secular historians of Christianity (Brown, Pagels, Behr) who document Nicaea as a political act, not a purely theological one. Magisterial historians defend trinitarianism as apostolic; non-institutional exegetes deny it.
narrative_ontology:disappearance_verdict(biblical_divine_nature__unitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_divine_nature__unitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_divine_nature__unitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.68) is high because the constraint forces institutional recognition as the sole legitimate interpretation source: only the magisterial councils' reading of Scripture is orthodoxy; unitarian readings are automatic heresy. This is not a coordination problem solved (both readings offer interpretations of the same Scripture)—it is pure authority capture. Suppression (0.72) is high because the constraint's persistence depends on actively excluding unitarian readings from legitimate theological discourse: seminary curricula enforce trinitarian doctrine; heresiological arguments brand unitarianism as irrational or wicked rather than engaging it as a contending exegesis. Theater (0.41) is moderate: in high-medieval contexts, enforcement is routine and theatrical (catechism, creeds recited); in modern academic contexts, theological argument is more present. The measurement series shows extractiveness rising sharply post-Nicaea (0.15 → 0.52 at 325 CE) when institutional enforcement machinery was created, peaking in the high medieval period (0.68 at 800 CE), declining somewhat at Reformation (0.64 at 1500 CE) when monopoly was broken by denominational pluralism, and restabilizing in modernity (0.68 at 1700 CE). The suppression_requirement shows a similar arc: minimal before institutional enforcement, peak at 800 CE, declining at Reformation, restabilizing when mainstream institutions still enforce the constraint despite pluralism. Theater_ratio is low initially, rises as enforcement becomes routine (0.45 at 800 CE), and falls as theological argument resurfaces post-Reformation. This pattern reflects the constraint's lifecycle: initial contestation → institutional consolidation → monopoly enforcement → pluralism and re-opening of theological space → modern persistence through institutional rather than total control.
 *
 * PERSPECTIVAL GAP:
 *   From the trinitarian hierarchy's position: the constraint is legitimate enforcement of apostolic tradition; trinitarianism is what Scripture teaches; unitarian readings are misinterpretations. From the unitarian exegete's position: the constraint is institutional suppression of valid scriptural reading; trinitarianism is post-apostolic dogma; the hierarchy enforces a corruption. From the ordinary trinitarian believer's position: the Trinity is God's revealed truth, simple and beautiful, binding the Church. From the ordinary unitarian believer's position: the Trinity is a confusing doctrine the hierarchy forces on Scripture; the Father alone is God, as Scripture says. From the secular historian's position: Nicaea was a political act by the imperial Church, and trinitarianism won because power, not exegesis. Each seat computes a different effective extraction and a different type from the same structural data. The engine's per-seat computation captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary set (institutional trinitarian hierarchy, trinitarian theologians, ordinary trinitarian believers) collects authority, belonging, and doctrinal stability. The victim set (unitarian exegetes, orthodox councils forced to defend a complex doctrine, ordinary unitarian believers) pays the cost of suppressed readings, excluded positions, professional sanction, and constrained faith. The anti-trinitarian reform movements are beneficiaries: they gain a reading that legitimates dissent. But they are also constrained payers: their institutional options are limited; their believers live under discrimination. The exit_options differentiate power: institutional trinitarian hierarchy has high exit (they could abandon trinitarianism and reorganize as a different faith), but they are identity_locked (institutionally, they cannot—trinitarianism IS their legitimacy claim). Unitarian exegetes have constrained exit (they can move to unitarian denominations, but that costs institutional career). Anti-trinitarian movements have mobile exit (they can establish separate denominations, and historically have). The engine uses these structural facts to derive d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was institutional: unifying a geographically dispersed Church around doctrine. The constraint solved this by enforcing trinitarian orthodoxy through magisterial authority. The founding_problem_status is contested: trinitarians attest the problem is still live (doctrinal unity requires enforcement); unitarians attest the problem is dead (modern Church is already so fragmented that doctrinal enforcement is theater; alternatives exist). The constraint's persistence despite the problem's contestation suggests mandatrophy: the institutional machinery persists not because the coordination problem requires it, but because institutional actors benefit from it. The theater_ratio's rise post-Reformation (0.38 at 1500, 0.41 at 1700) supports mandatrophy: as denominational pluralism made doctrinal unity impossible, enforcement became more theatrical than functional. The constraint is neither purely rope (it was coordination once; now it is mostly enforcement without coordination function) nor pure snare (it retains enough coordination function—Church unity through doctrine—to avoid collapse). Tangled rope is possible, but the analysis here claims snare: the coordination function has substantially atrophied; enforcement persists because institutional actors benefit from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_vs_trinity_logical_precedence,
    'Is the unitarian reading''s core claim (numerical singularity of God) logically incompatible with the trinitarian claim (three persons in one essence), or do they differ in interpretation rather than logical structure?',
    'Formal logical analysis: does the trinitarian ousia-hypostasis distinction preserve numerical singularity at the level of essence (making readings semantically compatible), or does unitarian insistence on strict numerical monotheism create a hard logical contradiction?',
    'If logically compatible, both readings coexist as interpretive options. If logically incompatible, one reading forecloses the other. This determines whether the engine computes forecloses or coexists_with in the reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_vs_trinity_logical_precedence, conceptual, 'Whether unitarian and trinitarian claims are logically contradictory or semantically distinct.').

omega_variable(
    apostolic_teaching_historical_record,
    'What did the first-century apostolic Church actually teach about the Father-Son relation? Was unitarian monotheism or trinitarian theology the dominant apostolic position?',
    'Comparative exegetical analysis of early Christian texts (Paul, Gospel writers, early Fathers); historical reconstruction of second-century theological diversity (scholarship by Behr, Pagels, Hurtado, Griesbach). Did the unitarian reading recover an authentic early apostolic position?',
    'If the early Church was predominantly unitarian, institutional trinitarianism is post-apostolic innovation: the trinitarian hierarchy becomes a victim of the constraint (their legitimacy claim is false). If the early Church was already trinitarian, the constraint enforces accurate tradition and unitarian movements are the innovators. This omega gates the fundamental legitimacy of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apostolic_teaching_historical_record, empirical, 'The apostolic tradition on divine nature and the Father-Son relation.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of unitarian readings sustained primarily by institutional gatekeeping (seminary curricula, ordination standards, publishing monopolies), or by internalized orthodoxy (believers simply accept trinitarianism as revealed truth and suppress alternatives themselves)?',
    'Case studies of believers and exegetes who encounter unitarian readings in secular academic contexts (where structural suppression is minimal): do they adopt unitarian readings, or do they reject them based on internalized orthodoxy? Comparative analysis of institutional versus non-institutional intellectual contexts.',
    'If suppression is structural, removing institutional enforcement would enable unitarian readings to spread; the fixing_cost would be lower (institutional remedies suffice). If internalized, the constraint would persist even without institutional machinery; fixing_cost remains prohibitive (requires re-education of internalized beliefs). This affects the practical possibility of constraint removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is primarily structural or internalized.').

omega_variable(
    identity_fusion_trinitarian_hierarchy,
    'To what extent is the trinitarian hierarchy''s institutional identity fused with trinitarian doctrine itself? Would abandoning trinitarianism dissolve the Church''s sense of apostolic continuity and institutional legitimacy?',
    'Historical analysis: have any major Christian institutions shifted from trinitarian to unitarian theology without institutional collapse? (Unitarian Universalism; contemporary shifts in some evangelical churches.) What are the identity and institutional consequences? Theoretical analysis: can the Church reground its authority in other claims (Scripture alone, community practice) without trinitarianism?',
    'If identity is deeply fused, the trinitarian hierarchy faces existential institutional threat if trinitarianism is abandoned. This explains the high suppression_requirement and identity_locked exit status. If identity is separable, institutional flexibility is higher and the constraint might be reformable without institutional collapse. This omega gates the possibility of institutional acceptance of unitarian readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_fusion_trinitarian_hierarchy, conceptual, 'Whether institutional identity is fused with trinitarian dogma.').

omega_variable(
    reading_kernel_fixity_and_canon_closure,
    'Is the kernel (biblical texts and their interpretation) genuinely fixed, or is canon itself subject to revision and reinterpretation? Can the unitarian reading claim scriptural primacy if the scriptural canon was itself decided by the trinitarian hierarchy?',
    'Historical analysis of canon formation: was the New Testament canon fixed before or after Nicaea? Was canon selection politically influenced toward trinitarian texts? Theoretical analysis: if canon is fixed, can readings compete on exegetical grounds alone? If canon is revisable, is the unitarian reading constrained by canonical boundaries that favor trinitarianism?',
    'If canon is fixed and pre-trinitarian, the unitarian reading can claim direct scriptural support. If canon was selected after Nicaea to favor trinitarianism, the unitarian reading is constrained by a biased kernel, and the constraint is foundational (canon + institutional enforcement). This affects whether the constraint is truly about interpretation or about kernel access itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_fixity_and_canon_closure, empirical, 'Whether the biblical kernel is genuinely fixed or subject to canon revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_divine_nature__unitarian_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_divine_nature__unitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bibl_tr_t325, biblical_divine_nature__unitarian_reading, theater_ratio, 325, 0.25).
narrative_ontology:measurement(bibl_tr_t800, biblical_divine_nature__unitarian_reading, theater_ratio, 800, 0.45).
narrative_ontology:measurement(bibl_tr_t1500, biblical_divine_nature__unitarian_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement(bibl_tr_t1700, biblical_divine_nature__unitarian_reading, theater_ratio, 1700, 0.41).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_divine_nature__unitarian_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(bibl_be_t325, biblical_divine_nature__unitarian_reading, base_extractiveness, 325, 0.52).
narrative_ontology:measurement(bibl_be_t800, biblical_divine_nature__unitarian_reading, base_extractiveness, 800, 0.68).
narrative_ontology:measurement(bibl_be_t1500, biblical_divine_nature__unitarian_reading, base_extractiveness, 1500, 0.64).
narrative_ontology:measurement(bibl_be_t1700, biblical_divine_nature__unitarian_reading, base_extractiveness, 1700, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_divine_nature__unitarian_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(bibl_su_t325, biblical_divine_nature__unitarian_reading, suppression_requirement, 325, 0.65).
narrative_ontology:measurement(bibl_su_t800, biblical_divine_nature__unitarian_reading, suppression_requirement, 800, 0.78).
narrative_ontology:measurement(bibl_su_t1500, biblical_divine_nature__unitarian_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(bibl_su_t1700, biblical_divine_nature__unitarian_reading, suppression_requirement, 1700, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_divine_nature__unitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__trinitarian_reading).
narrative_ontology:affects_constraint(biblical_divine_nature__unitarian_reading, biblical_divine_nature__modalist_reading).

% DUAL FORMULATION NOTE:
% The kernel 'biblical_divine_nature' decomposes into three readings: unitarian (Father alone is God), trinitarian (three persons in one essence), and modalist (sequential modes of one person). Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different types. Unitarian reading (this story): snare, ε=0.68, focuses institutional enforcement against unitarian exegesis. Trinitarian reading: rope/tangled-rope, ε varies by institutional context, focuses legitimate doctrinal authority. Modalist reading: mountain or piton, ε low, focuses on degraded alternative. The three stories are linked: each's ε is measured against the standing arrangement under contest (what Scripture teaches), assessed by the reading's own lights. The readings' relationship to each other is captured in cs_structure.reading_relations; the kernel's constraint family structure is captured in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
