% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development (Continuity Reading)
 *   domain: ecclesiology/institutional/hermeneutics
 *
 * SUMMARY:
 *   The continuity reading of Vatican II claims the Council represents
 *   organic development within an unchanging doctrinal deposit; apparent
 *   novelties in the Council documents are explications of implicit prior
 *   teaching, not substantive reversals. This reading is ONE instantiation of
 *   a contested kernel—the kernel is 'what does Vatican II represent?' and
 *   this reading asserts 'development without rupture.' The reading is
 *   enforced through postconciliar magisterial teaching, seminary formation,
 *   and liturgical policy. It benefits the institutional magisterium
 *   (protecting claims to doctrinal infallibility) and the theological
 *   establishment (which has built its authority on the continuity frame). It
 *   burdens traditionalist orders and preconciliar liturgical communities,
 *   whose spiritual identity and liturgical practice are delegitimized as
 *   reactionary under this frame. The theater_ratio is high (0.71) because
 *   the constraint's persistence depends significantly on performative
 *   assertion that change is not change, on repeated reinterpretation of
 *   Council documents as continuity, and on enforcement against competing
 *   readings. The extractiveness is moderate (0.38) and oscillates: it peaks
 *   where the magisterium must exert maximal interpretive control
 *   (1970s–1980s) and moderates where the constraint has become normalized
 *   (2000s–2020s). The suppression rises to its maximum (0.67) in the
 *   mid-1980s when the traditionalist challenge was strongest, then
 *   stabilizes as the traditionalist alternative becomes increasingly
 *   marginalized.
 *
 * KEY AGENTS:
 *   - postconciliar magisterium: agenda-setter; enforces the continuity reading through teaching, liturgical policy, and seminarian formation
 *   - traditionalist orders (SSPX, etc.): identity-locked payers; maintain preconciliar forms as an act of resistance to the claimed rupture
 *   - preconciliar liturgical communities: trapped payers; attached to liturgical and spiritual forms now delegitimized as reactionary
 *   - progressive reform advocates: beneficiaries; protected by the continuity frame from accusations of doctrinal overreach
 *   - Vatican authority structure: agenda-setter and structural beneficiary; the continuity reading protects papal claims to unbroken authority
 *   - textual exegetes and historians: excluded voices; their findings about genuine Council shifts are reinterpreted through the continuity lens
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.38).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Organic Development (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'e3c25217-a118-45b6-a187-b25d6f610e2d').
narrative_ontology:cs_kernel_codification('e3c25217-a118-45b6-a187-b25d6f610e2d', fixed_text).
narrative_ontology:cs_authority_grounding('e3c25217-a118-45b6-a187-b25d6f610e2d', extraction).
narrative_ontology:cs_interpretation_layer_present('e3c25217-a118-45b6-a187-b25d6f610e2d').
narrative_ontology:cs_reading_relation('e3c25217-a118-45b6-a187-b25d6f610e2d', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('e3c25217-a118-45b6-a187-b25d6f610e2d', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('e3c25217-a118-45b6-a187-b25d6f610e2d', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('e3c25217-a118-45b6-a187-b25d6f610e2d', foundational, doctrinal_continuity_doctrine).
narrative_ontology:cs_axiom_status(doctrinal_continuity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('e3c25217-a118-45b6-a187-b25d6f610e2d', doctrinal_continuity_doctrine, deontological).
narrative_ontology:cs_axiom('e3c25217-a118-45b6-a187-b25d6f610e2d', foundational, hermeneutics_of_continuity_binding).
narrative_ontology:cs_axiom_status(hermeneutics_of_continuity_binding, holdable).
narrative_ontology:cs_axiom_grounding('e3c25217-a118-45b6-a187-b25d6f610e2d', hermeneutics_of_continuity_binding, conventional).
narrative_ontology:cs_reference_frame('e3c25217-a118-45b6-a187-b25d6f610e2d', preconciliar_magisterial_continuity).
narrative_ontology:cs_drift_state('e3c25217-a118-45b6-a187-b25d6f610e2d', contemporary_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e3c25217-a118-45b6-a187-b25d6f610e2d', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, institutional_continuity_doctrine).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_orders).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, preconciliar_liturgical_communities).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, latin_mass_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_advocates).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, doctrinal_hermeneutics_establishment).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, vatican_authority_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority (popes, bishops in union with Rome since 1965) administers the continuity narrative: Vatican II documents are reinterpreted as expressions of development, not rupture. Enforces this reading through catechesis, seminary formation, and liturgical policy. Controls the interpretive authority that defines what counts as legitimate post-conciliar practice. Any deviation that claims the Council authorized a break is redescribed as misreading or unauthorized development.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Communities (SSPX, Institute of St. John Crisostom, etc.) that read Vatican II as rupture and resist the continuity narrative. They maintain preconciliar liturgical and disciplinary forms as an act of fidelity to interrupted tradition. The continuity reading delegitimizes their position structurally—by making reform appear developmental, their resistance appears reactionary rather than preservationist. They bear the cost of institutional marginalization, pressure toward compliance, and canonical restriction. Exit requires abandoning their foundational identity claim (that they preserve what the Council broke).
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_orders, payer,
    organized, generational, identity_locked, global).

% Lay practitioners and some clergy attached to preconciliar forms (Latin Mass, preconciliar theology, preconciliar sacramental discipline) who experience the continuity framing as delegitimization of their spiritual formation. The reading that all change is development, not rupture, makes their liturgical preference appear nostalgic rather than normative. They have minimal leverage; access to preconciliar forms is controlled by the magisterium and restricted in most dioceses.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, preconciliar_liturgical_communities, payer,
    powerless, biographical, trapped, local).

% Bishops, theologians, and pastoral leaders who supported Vatican II reforms and its implementation. The continuity reading benefits them by framing their innovations as legitimate development of Council teaching, not as overreach. It protects them from the accusation that they have rewritten doctrine—they can claim they are merely explicating what was already implicit. They have substantial institutional power and exit options (academic careers, curial positions, episcopal influence).
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reform_advocates, beneficiary,
    powerful, generational, mobile, global).

% Theological faculties, pontifical academies, and interpretive authorities that have built their authority on the continuity reading. This framework has become foundational to how postconciliar theology is taught and legitimated. The reading provides them with a coherent interpretive grid: every Council document is read through a hermeneutics of continuity; ambiguities are resolved in favor of development, not rupture. Abandoning this would require dismantling decades of theological investment.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, doctrinal_hermeneutics_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Protestant, Orthodox, and Anglican churches engaged with the postconciliar Church. They observe that the continuity reading constrains what the Catholic Church can admit about the scope of Council reforms. If Vatican II were admitted as genuine rupture on ecclesiology, ecumenism, or liturgy, dialogue partners would have different leverage in conversations about convergence and divergence. The continuity frame limits how far the Church can move without appearing to contradict itself.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_dialogue_partners, observer,
    institutional, generational, constrained, global).

% Scholars (de Lubac, Ratzinger, Congar, O'Malley, Faggioli) who have analyzed Council documents with philological and historical precision, noting genuine shifts in language, emphasis, and pastoral approach. Some of these scholars would argue the continuity reading under-describes the scope and intentionality of conciliar change. They are excluded from setting the interpretive frame; instead, their findings are reinterpreted through the continuity lens or treated as preliminary to the magisterium's final word.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_textual_exegetes, excluded,
    moderate, biographical, mobile, global).

% The papal office and the teaching authority as such. The continuity reading protects the authority's self-understanding: it claims the magisterium cannot err on doctrine; therefore, Vatican II cannot have taught something truly new or rupture with prior teaching. The constraint enforces a hermeneutical rule that guards institutional authority by definition. Post-conciliar magisterial teaching is insulated from the charge that it reversed its predecessors; instead, every shift is framed as development or explication.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, vatican_authority_structure, agenda_setter,
    institutional, civilizational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, vatican_authority_structure, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__continuity_reading, post_conciliar_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent interpretive frame for Vatican II that prevents the Council from fracturing the Church into preconciliar and postconciliar factions. Without the continuity reading, Vatican II's genuine shifts in liturgy, ecclesiology, and pastoral approach would pit preconciliar communities against postconciliar reformers with no shared language or framework. The continuity reading solves this by offering a unified narrative: everyone is working within the same deposit, some change is real but all is development, the magisterium remains authoritative and unbroken. This prevents institutional schism.
% TRANSFER_FUNCTION: Transfers interpretive authority over Vatican II from those who would read the Council as allowing rupture (progressives claiming authorization for change, traditionalists claiming the Council was corrupted) toward the postconciliar magisterium and the theological establishment aligned with the continuity frame. It also transfers liturgical and spiritual legitimacy from preconciliar forms (now read as superseded developments) toward postconciliar forms (now read as authentic developments of the same deposit). The liturgy shift from Latin to vernacular, from priest-centered to participatory, is reframed as development, not rupture, which legitimates the shift and delegitimizes attempts to preserve preconciliar forms.
% ABSENT_VOICES: Scholars of textual history (de Lubac, O'Malley, Faggioli) who would document genuine shifts in Council documents. Traditionalist theologians who would argue Vatican II represents rupture and fidelity lies in recovering preconciliar teaching. Vatican II minority bishops (those who opposed key documents) whose interpretation of the Council's meaning might challenge the continuity narrative. Liberal Catholics who would argue the Council could authorize reform beyond its explicit text. These voices are excluded from the magisterium's interpretive monopoly; their claims are redescribed as misreadings rather than alternative legitimate readings.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared and the magisterium admitted Vatican II represented genuine doctrinal development or rupture on select issues, the grounds for papal authority would shift. Papal claims to unbroken doctrinal authority would require reformulation. Traditionalist schism would gain legitimacy; preconciliar forms would regain standing as expressions of an older, unbroken continuity that the magisterium had voluntarily abandoned. The postconciliar theological establishment would need to reorganize around a different hermeneutical frame. Ecumenical dialogue would shift: the Church would no longer claim absolute continuity with preconciliar positions. The institutional Church would rearrange itself around a new account of its own history.
% FOUNDING_PROBLEM: Vatican II produced numerous textual ambiguities, shifts in emphasis (religious freedom, relationship to other faiths and the modern world), and dramatic practical changes (especially liturgical). How could the magisterium claim it teaches unchanging doctrine while presiding over these changes? How could the Church prevent traditionalists from claiming the Council had corrupted the faith? How could postconciliar bishops justify their reforms as authoritative? The continuity reading solved these by asserting: all Council teaching is development within unchanging deposit; all changes are legitimate developments; the magisterium's authority remains unbroken.
% FOUNDING_PROBLEM_CORROBORATION: The postconciliar magisterium and theological establishment attest that the founding problem was real and urgent: without the continuity reading, the Council would have fractured the Church. Traditionalist bishops and scholars attest that the founding problem was manufactured by a reading that denies the Council's true novelty; the real crisis arose from the Council's actual rupture, not from the reading's application. Independent historical scholars (O'Malley, Faggioli, Komonchak) document genuine Council novelties in doctrine, practice, and approach—suggesting the founding problem was real (Council did produce substantial change) but the continuity reading systematically under-describes the scope of change to protect institutional authority claims. The current scholarly consensus is that Vatican II represents genuine development, with some elements involving doctrinal reversal (religious freedom was explicitly denied in preconciliar doctrine) and others representing prudential adaptation (liturgical reforms). This consensus exists outside the benefiting parties (the magisterium, the theological establishment built on the continuity frame) and suggests the continuity reading is a power-backed interpretation maintaining an institutional monopoly rather than a scholarly consensus finding.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits the structure of a tangled_rope: (1) coordination function—the continuity reading solves a real problem: how to maintain institutional coherence and papal authority while presiding over substantial practical change. Without a unifying frame, Vatican II would have fractured the Church. (2) Asymmetric extraction—beneficiaries (the magisterium, the theological establishment, progressive reformers) gain authority and protection from criticism; payers (traditionalists, preconciliar communities) lose liturgical legitimacy and exit options. Traditionalists are identity-locked: exiting the reading means abandoning the identity claim that they preserve unbroken tradition. (3) Active enforcement—the reading is maintained through constant reinterpretation, seminary formation that embeds the continuity frame, and restriction of access to preconciliar forms. Without active enforcement (teaching, control of liturgical access, marginalization of competing readings), traditionalist alternatives would gain credibility. Theater_ratio is high because much of the enforcement activity is performative: Council documents are reread repeatedly to extract continuity even where textual novelty is evident; ambiguities are resolved by fiat; competing scholarly readings are described as misinterpretations. The extractiveness trajectory is non-monotonic: rises as traditionalist resistance mounts and the magisterium must exert maximal control to maintain the frame (1970s–1980s), plateaus and moderates as the traditionalist alternative becomes increasingly isolated and younger generations inherit the continuity reading as background assumption (2000s–present). Suppression follows extractiveness: as the traditionalist threat recedes, less active suppression is required—the frame becomes naturalized.
 *
 * PERSPECTIVAL GAP:
 *   From the postconciliar magisterium's seat: the constraint is genuine coordination—it solved the real problem of postconciliar institutional unity and prevented the Church from fracturing. It enforces a hermeneutics of continuity that is faithful to the magisterium's nature. It is neither coercive nor extractive; it is the appropriate exercise of teaching authority. From the traditionalist seat: the constraint is pure extraction. It delegitimizes their spiritual identity, restricts their access to preferred liturgical forms, and redefines fidelity to tradition as acceptance of innovations. The continuity frame is itself a rhetorical strategy to prevent admission of the Council's true novelty. From the exegetical seat (excluded): the constraint obscures genuine textual evidence of doctrinal development and innovation; it prevents scholarly findings from entering magisterial reflection; it maintains an interpretive monopoly by dismissing historical analysis as misreading. The engine should compute these divergences from the structural data (beneficiary/victim declarations + exit options + power asymmetry); the per-seat classifications will reflect radically different interpretations of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The postconciliar magisterium and theological establishment sit near the beneficiary end of the directionality spectrum: they control the interpretive authority, frame competing claims as misreadings, and benefit from the constraint's persistence. Directionality for this seat: ~0.15–0.25 (low extraction, high subsidy). Traditionalist orders sit near the target end: they are identity-locked (cannot exit without abandoning their foundational identity), face institutional pressure and marginalization, and their preferred practices are actively restricted. Directionality for this seat: ~0.85–0.95 (high extraction, minimal exit). Preconciliar liturgical communities sit intermediate-to-target: they have somewhat more exit optionality than traditionalist orders (they can abandon preconciliar preference and accept postconciliar forms) but also face strong identity-based reasons not to exit. Directionality for this seat: ~0.70–0.80. Progressive reformers sit near beneficiary: they have substantial institutional power, mobile exit options (academic positions, curial roles), and the frame protects them from the charge of doctrinal overreach. Directionality: ~0.20–0.30.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits subtle mandatrophy: its founding problem was real and live (postconciliar implementation chaos, traditionalist resistance, textual ambiguities requiring integration). But over 60 years, the landscape has changed. (1) The founding problem has partially died: a new generation of Catholics has never known preconciliar practice; the 'crisis' of postconciliar reform is now historical rather than immediate. (2) Simultaneously, the constraint has become more theatrical (theater_ratio rises from 0.58 to 0.72 by t=24 then stabilizes): active enforcement of the continuity reading now defends not against genuine traditionalist organizational threat but against scholarship that documents genuine Council novelty. The suppression requirement has become maintenance of an interpretive monopoly rather than coordination of a fractured Church. (3) The constraint persists by inertia and institutional habit: seminary curricula embed the continuity frame; the theological establishment's authority depends on it; magisterial teaching has committed itself to it. But the exegetical evidence that Vatican II contained genuine shifts in doctrine and practice has only grown stronger; the constraint now functions partly as a truth-suppression mechanism rather than a coordination solution. The mandatrophy does not resolve cleanly: the constraint is not purely theatrical (it does coordinate real interests and protect real authority structures), nor is it a pure zombie (the founding problem has not entirely disappeared—traditionalist schism remains a live concern, however marginal). It sits in a state of partial irrelevance with continued active maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_novelty_vs_explication,
    'On specific doctrines (religious freedom, subsistit-in ecclesiology, relationship to modern world), do the Vatican II documents represent genuine reversals or logical explicitations of pre-conciliar doctrine?',
    'Comparative philological analysis of conciliar documents against pre-conciliar magisterial teaching, with attention to semantic shifts, new terminology, and deliberate Council votes on amendments. Natural experiment: if future generations of scholars converge on specific doctrinal reversals despite the continuity reading''s framing, that convergence would constitute empirical resolution favoring the rupture readings.',
    'If specific reversals are documented with textual precision, the mandatrophy reading becomes dominant—the constraint shifts from coordination to truth-suppression, and the architecture of postconciliar authority becomes unstable. If explication can be sustained across scholarly analysis, the continuity reading retains structural credibility and mandatrophy does not resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_novelty_vs_explication, empirical, 'Whether Council novelties are doctrinal reversals or legitimate developments.').

omega_variable(
    foundational_problem_resolution,
    'Has the founding problem (how to maintain authority claims while presiding over substantial change) actually been solved by the continuity reading, or has the reading merely postponed the crisis by establishing an interpretive monopoly?',
    'Empirical marker: does the constraint remain stable if magisterial teaching authority fragments (e.g., in response to scandals, credibility loss, or decentralization)? Can the continuity reading survive losing the monopoly on Council interpretation? If the interpretive monopoly breaks and competing readings gain credence, the founding problem resurfaces unresolved.',
    'If the constraint depends entirely on magisterial monopoly for its stability, it is a false solution—merely a power-backed interpretation with no independent legitimacy. If the continuity reading can survive competing interpretations, it has genuine problem-solving capacity. Current evidence suggests fragility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_problem_resolution, empirical, 'Whether the continuity reading solves its founding problem or merely suppresses its emergence.').

omega_variable(
    identity_lock_internalization,
    'For traditionalist communities, is their resistance to the continuity reading primarily structural (they lose material access to preconciliar forms) or internalized (they have internalized the identity-claim that they preserve tradition and experience the continuity reading as delegitimization of self)?',
    'Post-exit trajectory analysis: if traditionalists who leave their communities and are removed from suppression machinery retain resistance to the continuity reading and continue to identify with preconciliar tradition, suppression is partly internalized. If resistance largely collapses after exit, suppression is primarily structural.',
    'If suppression is internalized, the effective suppression is higher than the scalar measures suggest—the constraint carries suppressiveness with it even after removal from institutional enforcement. The extractiveness is thus under-measured by the base_properties scalar alone. If suppression is purely structural, the exit option becomes more genuine for those who can physically leave, moderating their directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization, empirical, 'Whether traditionalist suppression is structural or internalized.').

omega_variable(
    kernel_reading_coexistence_possibility,
    'Can multiple readings of the Vatican II kernel coexist within a single ecclesiastical framework, or does the continuity reading''s establishment as magisterial teaching functionally eliminate other readings as live options?',
    'Institutional policy observation: does the postconciliar magisterium permit organized theological or pastoral communities to instantiate the rupture_progressive or rupture_traditionalist readings, or is institutional space reserved only for the continuity reading? Specific markers: permission for distinct liturgical rites, tolerance of theologians publishing rupture-reading arguments, acceptance of traditionalist episcopal candidates.',
    'If readings truly coexist, the kernel has multiple live instantiations and the constraint is managing genuine pluralism. If the continuity reading enforces monopoly, coexistence is rhetorical only—the constraint is actually narrowing the interpretive space to one authorized reading. Current evidence (restriction of traditionalist bishops, suppression of progressive theology, consolidation of continuity frame in seminary curricula) suggests functional monopoly rather than coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_possibility, empirical, 'Whether the reading permits genuine coexistence with sibling readings or enforces interpretive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement(vati_tr_t8, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 8, 0.64).
narrative_ontology:measurement(vati_tr_t16, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 16, 0.68).
narrative_ontology:measurement(vati_tr_t24, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 24, 0.72).
narrative_ontology:measurement(vati_tr_t32, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 32, 0.73).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.71).
narrative_ontology:measurement(vati_tr_t48, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 48, 0.7).
narrative_ontology:measurement(vati_tr_t56, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 56, 0.71).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 60, 0.71).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(vati_be_t8, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(vati_be_t16, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(vati_be_t24, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement(vati_be_t32, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 32, 0.39).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(vati_be_t48, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 48, 0.37).
narrative_ontology:measurement(vati_be_t56, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 56, 0.38).
narrative_ontology:measurement(vati_be_t60, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vati_su_t8, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(vati_su_t16, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(vati_su_t24, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(vati_su_t32, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 32, 0.64).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(vati_su_t48, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 48, 0.61).
narrative_ontology:measurement(vati_su_t56, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 56, 0.62).
narrative_ontology:measurement(vati_su_t60, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_doctrinal_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, postconciliar_episcopal_authority).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_schism_institutional_status).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'vatican_ii_doctrinal_authority.' Sibling readings (rupture_progressive, rupture_traditionalist, composite_overdetermination) are separate constraint stories, each with its own epsilon, beneficiary/victim structure, and per-seat type classifications. The readings coexist (or claimed to coexist) as different parties' interpretations of the same underlying arrangement. The continuity reading forecloses the rupture readings (in any single framework, the Council cannot be both a development and a rupture); it coexists with the composite reading (a composite can be a development). Network links trace how each reading instantiates structural pressure on the others: the continuity reading's establishment as magisterial teaching constrains what the rupture readings can coherently claim; the empirical case for Council novelty (documented in scholarship) creates structural pressure on the continuity reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_doctrinal_authority__continuity_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
