% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II Continuity Reading: Organic Development of Doctrine
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962-1965) represents one of the most consequential
 *   institutional events in modern Christianity: a council that produced 16
 *   documents authorizing sweeping reforms in liturgy, ecclesiology,
 *   ecumenism, religious liberty, and pastoral practice. The kernel is
 *   contested across three fundamentally different readings: the continuity
 *   reading (this constraint) holds that Vatican II represents organic
 *   development within an unchanging deposit of faith, with all reforms
 *   legitimate insofar as they faithfully express traditional doctrine in new
 *   pastoral contexts; the rupture reading holds that Vatican II introduced
 *   substantial doctrinal breaks or errors, creating irreconcilable tensions
 *   with prior teaching; the composite-overdetermination reading holds that
 *   Vatican II is not a coherent interpretable event but an overdetermined
 *   composite where multiple distinct doctrinal shifts have incompatible
 *   theological rationales, making both continuity and rupture readings false
 *   simplifications. This constraint instantiates the continuity reading as a
 *   specific structural configuration: beneficiaries are progressive
 *   reformers who can claim legitimacy for expansive changes while
 *   maintaining doctrinal orthodoxy; victims are both traditionalist agents
 *   (who experience extraction from hermeneutical reframing) and the
 *   pre-conciliar doctrinal framework itself (treated as superseded). The
 *   constraint is tangled_rope: genuine coordination function (reconciling
 *   reform with tradition) combined with asymmetric extraction (beneficiaries
 *   gain reform legitimacy; traditionalists lose interpretive standing).
 *   Theater ratio rises over the 60-year interval as the constant reassertion
 *   of 'continuity' becomes increasingly performative — the magisterium
 *   maintains the continuity frame not because the hermeneutical argument
 *   compels, but because breaking with it would shatter institutional
 *   narrative coherence.
 *
 * KEY AGENTS:
 *   - Progressive Episcopal Faction: Institutional beneficiaries (organized/arbitrage) — gain legitimacy for pastoral reforms, ecumenical openness, religious liberty while maintaining claim to doctrinal orthodoxy; experience constraint as pure coordination (rope perspective)
 *   - Traditionalist Communion: Moderate victims (moderate/constrained) — constrained but not trapped; experience extraction in hermeneutical reframing that privileges modernizing interpretation; have exit options (FSSP, traditionalist communities) but at spiritual/institutional cost
 *   - Pre-Conciliar Doctrinal Framework: Powerless abstraction (powerless/trapped) — cannot organize or advocate; treated as superseded doctrine under continuity reading despite reading's claim to preserve it; maximum extraction through hermeneutical suppression
 *   - Post-Conciliar Renewal Movements: Organized actors (organized/constrained) — see continuity reading as temporary scaffold for transition to stable post-conciliar orthodoxy; have genuine agency and clear sunset (once renewal stabilizes); moderate extraction through resource allocation
 *   - Institutional Magisterium: Beneficiary in maintenance mode (institutional/arbitrage) — continues to perform continuity reading as institutional ritual to maintain narrative coherence and doctrinal authority; increasingly piton-like (degraded, theater-dependent)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks treating the immutability-of-doctrine principle as a natural law when it may be a naturalized institutional claim; engine's false summit detector applies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.48).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II Continuity Reading: Organic Development of Doctrine").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'a82c8bb8-ec02-4cb9-9b82-1bd789337525').
narrative_ontology:cs_kernel_codification('a82c8bb8-ec02-4cb9-9b82-1bd789337525', formalized).
narrative_ontology:cs_authority_grounding('a82c8bb8-ec02-4cb9-9b82-1bd789337525', lineage).
narrative_ontology:cs_interpretation_layer_present('a82c8bb8-ec02-4cb9-9b82-1bd789337525').
narrative_ontology:cs_reading_relation('a82c8bb8-ec02-4cb9-9b82-1bd789337525', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('a82c8bb8-ec02-4cb9-9b82-1bd789337525', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('a82c8bb8-ec02-4cb9-9b82-1bd789337525', foundational, doctrinal_immutability_with_pastoral_development).
narrative_ontology:cs_axiom_status(doctrinal_immutability_with_pastoral_development, holdable).
narrative_ontology:cs_axiom_grounding('a82c8bb8-ec02-4cb9-9b82-1bd789337525', doctrinal_immutability_with_pastoral_development, deontological).
narrative_ontology:cs_axiom('a82c8bb8-ec02-4cb9-9b82-1bd789337525', foundational, hermeneutical_continuity_resolvable).
narrative_ontology:cs_axiom_status(hermeneutical_continuity_resolvable, holdable).
narrative_ontology:cs_axiom_grounding('a82c8bb8-ec02-4cb9-9b82-1bd789337525', hermeneutical_continuity_resolvable, conventional).
narrative_ontology:cs_reference_frame('a82c8bb8-ec02-4cb9-9b82-1bd789337525', unchanging_deposit_with_progressive_application).
narrative_ontology:cs_drift_state('a82c8bb8-ec02-4cb9-9b82-1bd789337525', contemporary_post_conciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a82c8bb8-ec02-4cb9-9b82-1bd789337525', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, modernizing_episcopal_faction).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_communion).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, pre_conciliar_certainty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Constrained but not trapped: traditionalists see Vatican II reforms as legitimate only insofar as they preserve prior doctrine. They experience genuine coordination (the continuity reading does coordinate old and new norms) but also extraction: the hermeneutical framework privileges modernizing interpretation over traditionalist readings. Exit options exist (FSSP, Ecclesia Dei) but carry spiritual and institutional costs. Moderate extraction with coordination function present.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Benefits from the continuity reading: they can claim legitimacy for sweeping reforms while maintaining doctrinal orthodoxy narrative. The reading solves their coordination problem (how to reform without appearing heretical). Low extraction experience — net beneficiaries with high agency and exit optionality. Experience the constraint as pure coordination.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Abstract but real: the prior stable doctrinal framework (pre-Vatican II teaching) cannot organize or advocate for itself. Trapped within the interpretive apparatus that superseded it. Maximum extraction — the certainty that preceded Vatican II is treated as superceded doctrine, yet the continuity reading claims to preserve it. Contradiction suppressed by hermeneutical complexity.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Organized agents (liturgical reform initiatives, catechetical projects, ecumenical dialogue committees) see the continuity reading as a temporary scaffolding for transitioning from pre-conciliar to post-conciliar ecclesiology. The sunset clause is implicit: once a stable post-conciliar orthodoxy is established and interiorized across the whole Church, the constant recitation that reforms are merely 'continuous development' becomes unnecessary. Theater ratio lower here (0.55) because these movements genuinely coordinate new pastoral realities. Moderate effective extraction because the organizations have agency and clear exit paths once transition completes.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The teaching authority of the Church increasingly performs the continuity reading as a ritual claim rather than as a live hermeneutical conviction. Subsequent pontiffs reassert continuity with Vatican II not because the hermeneutical argument compels but because breaking with it would fracture the institutional narrative. The performance persists through inertia — the magisterium cannot revise Vatican II backward without delegitimizing itself, so it maintains the 'continuity' claim even as pastoral practice diverges from both pre-conciliar and conciliar norms. Theater ratio high (0.70): the constant reaffirmation of continuity serves primarily to maintain institutional appearance of consistency.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational/universal perspective, if the deposit of faith is truly immutable and unchangeable, then Vatican II reforms are either continuous (logical necessity) or the deposit is not immutable (logical contradiction). This perspective treats the continuity relation as a necessary feature of how doctrine evolves — the constraint appears as an inherent property of theological epistemology itself. However, the structural data contradicts the mountain classification: genuine beneficiaries exist (progressive reformers), genuine victims exist (traditionalist communion), and active enforcement of the continuity frame is required. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_authority__continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low initial, rising to 0.38 by end interval. The continuity reading does provide genuine coordination value — it allows the institutional Church to enact substantive reforms while maintaining doctrinal authority narrative. But this coordination is asymmetric: beneficiaries gain legitimacy for preferred reforms while traditionalists experience interpretive loss. The extractiveness is not as high as pure snares (0.70+) because genuine coordination function exists and because the constraint's force depends on continuous active enforcement (if the magisterium stopped reasserting continuity, the frame would lose authority). The rising trajectory reflects increasing theater as enforcement intensifies over time. Suppression (0.48): Moderate-high. The continuity reading suppresses alternative readings (rupture, overdetermination) through institutional channels — bishops favoring traditionalist positions face resistance; theologians publishing outside official channels can explore rupture readings; the hermeneutical framework itself makes rupture appear incoherent or heretical. But suppression is not maximal (0.80+) because rupture and other alternative readings persist (traditionalist communities, critical theologians, underground traditions) and cannot be fully eliminated. The rising trajectory reflects institutional intensification of the continuity mandate over the 60-year interval. Theater ratio (0.65, rising to 0.70): High and rising. In the earliest post-conciliar period, the continuity reading could present itself as a live hermeneutical discovery; reformers could genuinely debate whether specific reforms expressed continuous development. By recent decades, the constant reaffirmation of continuity in magisterial documents appears increasingly performative — a ritual reassurance that the institution remains coherent rather than a persuasive hermeneutical argument. The rising trajectory reflects degradation from genuine hermeneutical claim to institutional performance.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading produces a sharp perspectival gap between beneficiaries and victims. The progressive episcopal faction (institutional/arbitrage) experiences the constraint as pure rope — a coordination mechanism that allows them to reform while remaining orthodox. They experience low chi, no extraction, and high agency. The traditionalist communion (moderate/constrained) experiences tangled_rope — the reading does coordinate old and new norms, but it privileges modernizing interpretation and constrains traditionalist options. Suppression is real (alternative readings marginalized) but not total (exit paths exist). The pre-conciliar framework itself (powerless/trapped) experiences snare — it cannot organize or argue for itself; the continuity reading treats it as superseded while claiming to preserve it. The post-conciliar renewal movements (organized/constrained) experience scaffold — they see the continuity reading as temporary, expecting that once post-conciliar practice stabilizes, the constant invocation of continuity will become unnecessary. The institutional magisterium increasingly experiences piton — the performance of continuity maintains institutional appearance but does not generate genuine conviction. The analytical observer at civilizational scale risks mountain — treating immutability of doctrine as a law of theological epistemology — but the structural data contradicts this: beneficiaries, victims, and enforcement mechanisms are all present.
 *
 * DIRECTIONALITY LOGIC:
 *   The continuity reading's directionality derives from the beneficiary-victim structure. Progressive reformers are beneficiaries with arbitrage options — they can invoke the continuity frame or set it aside depending on context; their d-value (0.10-0.15) produces negative effective extraction chi, reflecting their net benefit. Traditionalists are victims with constrained exit — they experience suppression of alternative readings and reframing of their preferred doctrinal positions; their d-value (0.70-0.75) produces elevated chi reflecting extraction. The pre-conciliar framework has no exit options (abstract, powerless) — d approaches 1.0, maximum chi. The organized renewal movements have constrained but real options; their d-value (0.55-0.60) produces moderate chi. The magisterium's arbitrage options and beneficiary status produce low d (0.15), but the institutional enforcement requirement raises suppression, maintaining moderate extractiveness despite low d. The analytical observer at civilizational scale (analytical/analytical) has canonical d (0.73), producing chi values in the rope-to-scaffold range, but the false summit detector identifies that the mountain classification cannot be sustained: beneficiaries and enforcement are present.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resolves the mandatrophy by defining the constraint as a hybrid coordination-extraction mechanism (tangled_rope) where genuine coordination of old and new norms is combined with asymmetric extraction favoring progressive reformers and suppressing traditionalist alternatives. The mandatrophy question — 'How can reforms be both continuous with prior doctrine and substantially new?' — is dissolved not by choosing between continuity and rupture but by recognizing that the continuity claim itself is the extraction mechanism: it allows beneficiaries to claim legitimacy for substantive changes while constraining traditionalist responses. The mountain perspective is a false summit: the immutability-of-doctrine principle naturalizes what is actually a contingent institutional arrangement (the magisterium's authority depends on maintaining narrative coherence, so it enforces the continuity frame even when hermeneutical evidence is ambiguous). The tangled_rope classification stands: the constraint is neither pure coordination (rope) nor pure extraction (snare) but both simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_ambiguity_resolution,
    'Can the 16 Vatican II documents be read coherently as expressing continuous development of doctrine, or do they contain logically incompatible theological positions that require either rupture reading or overdetermination reading to make sense of?',
    'Systematic comparison of specific doctrine pairs across pre-conciliar and conciliar texts: ecclesiology (ecclesia semper reformanda vs depositum fidei), religious liberty (Dignitatis Humanae vs prior papal teaching), ecumenism (Unitatis Redintegratio vs prior exclusivism claims), episcopal collegiality (Lumen Gentium vs prior ultramontane teaching). Identification of genuine logical continuity vs reframing/recontextualization vs unresolved contradiction.',
    'If coherent continuity demonstrated: continuity reading strengthened; rupture and overdetermination readings weakened. If contradictions irreducible: overdetermination reading becomes necessary; continuity reading appears as imposed hermeneutical frame rather than discovered reality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hermeneutical_ambiguity_resolution, conceptual, 'Whether Vatican II documents logically cohere under continuity reading or contain irreducible contradictions').

omega_variable(
    organic_development_criteria,
    'What hermeneutical criteria distinguish legitimate organic development of doctrine (as continuity reading claims) from revision or novelty in disguise?',
    'Apply Newman''s criteria (homogeneity, continuity of principles, power of assimilation, logical sequence, anticipation of developments, preservative addition, chronic vigor) to specific Vatican II reforms and track whether the criteria are applied consistently or selectively. Document instances where reformers invoke ''organic development'' and instances where they resist the label.',
    'If criteria applied consistently: continuity reading is systematic and defensible. If applied selectively (invoked for favored reforms, denied for traditionalist developments): the reading becomes circular and theater-dependent. High selectivity suggests theater ratio underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organic_development_criteria, conceptual, 'Consistency of ''organic development'' criteria across post-conciliar reforms').

omega_variable(
    beneficiary_narrative_alignment,
    'Do progressive reformers who most benefit from the continuity reading actually believe in it as a hermeneutical claim, or do they treat it as a necessary institutional fiction?',
    'Historical analysis of reformer rhetoric across different contexts: formal magisterial documents (continuity frame mandatory) vs private correspondence, theological journals, oral history interviews (continuity frame optional). Document instances of reformers describing their own work as ''development,'' ''reform,'' ''innovation,'' or ''rupture'' when institutional continuity mandate is absent.',
    'If beneficiaries genuinely convinced: the reading describes a live theological conviction. If beneficiaries treat continuity as institutional requirement rather than conviction: the reading is performative (theater-dependent), and extractiveness is driven by enforcement of narrative rather than by structural incentives. This directly raises theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_narrative_alignment, empirical, 'Whether beneficiaries genuinely hold or tactically maintain continuity reading').

omega_variable(
    institutional_enforcement_mechanism,
    'What enforces the continuity reading in post-conciliar Catholicism? Who loses standing or resources if they publicly embrace rupture or overdetermination readings?',
    'Document enforcement patterns: bishops who emphasize ''continuity'' receive appointments and resources; bishops who suggest rupture face resistance or marginalization. Theologians published in official channels must frame Vatican II as continuous; alternative framings appear only in independent presses or underground traditions. Identify the institutional costliness of each reading.',
    'If enforcement is light (merely conversational): suppression = 0.25. If enforcement is severe (institutional sanctions, career damage, access denial): suppression rises to 0.60+. High enforcement indicates the continuity reading is a snare from the institutional enforcer perspective, not a rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_enforcement_mechanism, empirical, 'Institutional enforcement mechanisms sustaining continuity reading').

omega_variable(
    rupture_reading_coexistence,
    'Is the rupture reading genuinely coexistent with the continuity reading (two parties in live dispute) or has the continuity reading foreclosed rupture as theologically illegitimate?',
    'Assess whether rupture-reading positions (traditionalist, integralist, anti-modernist critiques of Vatican II) are treated as live theological options within Catholic institutional framework or as excluded heresies. Document whether Catholic institutions engage rupture readings charitably or dismiss them as outside the pale.',
    'If coexistent: both readings remain live; continuity does not foreclose rupture. If continuity has foreclosed rupture: the relationship is not mere coexistence but active suppression, raising extractiveness and suggesting snare or tangled_rope from rupture perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_reading_coexistence, conceptual, 'Whether continuity reading coexists with or forecloses rupture reading').

omega_variable(
    false_summit_doctrinal_immutability,
    'Is the claim that doctrine is immutable and unchanging a genuine principle of Catholic theology or a naturalized institutional claim that benefits those controlling doctrinal interpretation?',
    'Historical study of pre-Vatican II teaching on doctrinal development (Newman, Denzinger, magisterial statements on doctrinal progress). If development was already acknowledged as legitimate, the continuity reading names a genuine theological principle. If development was initially resisted and only later accommodated, the ''immutability'' frame may be a false summit — a naturalized institutional position rather than a discovered truth.',
    'If immutability is genuine principle: mountain classification has merit; false summit trigger is false positive. If immutability is contingent institutional claim: the engine''s false summit detector correctly identifies naturalization; the constraint is tangled_rope (coordination + extraction), not mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_doctrinal_immutability, conceptual, 'Whether doctrinal immutability is genuine principle or naturalized institutional claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_cont_tr_t0, vatican_ii_authority__continuity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(vii_cont_tr_t10, vatican_ii_authority__continuity_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(vii_cont_tr_t20, vatican_ii_authority__continuity_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(vii_cont_be_t0, vatican_ii_authority__continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vii_cont_be_t10, vatican_ii_authority__continuity_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(vii_cont_be_t20, vatican_ii_authority__continuity_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vii_cont_su_t0, vatican_ii_authority__continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vii_cont_su_t10, vatican_ii_authority__continuity_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(vii_cont_su_t20, vatican_ii_authority__continuity_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel decomposes into three structurally distinct constraints, each with its own ε and beneficiary-victim structure. This constraint (continuity_reading, ε=0.38) represents the institutional Church's official position. The rupture_reading constraint (expected ε=0.50-0.60, beneficiaries=traditionalist agents, victims=modern pastoral movements) represents the traditionalist position. The composite_overdetermination_reading constraint (expected ε=0.65+, beneficiaries=neither [or both equally], victims=institutional narrative coherence) represents the post-conciliar critical position. All three affect one another: the continuity reading shapes the institutional space in which rupture and overdetermination readings operate; the persistence of rupture and overdetermination challenges the continuity reading's credibility and raises its theater ratio.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__continuity_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
