% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II Magisterial Authority (Rupture Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   The Second Vatican Council (1962–1965) is contested in Catholic
 *   ecclesiology as either a fundamental rupture with pre-conciliar teaching
 *   or an organic development within unbroken tradition. This constraint
 *   story instantiates the RUPTURE READING: Vatican II texts encode a new
 *   ecclesiology fundamentally incompatible with prior magisterial teaching,
 *   particularly regarding religious freedom (Dignitatis Humanae), the
 *   relationship between the Church and the world (Gaudium et Spes),
 *   liturgical authority, and the locus of magisterial power (collegiality
 *   vs. papal supremacy). Under this reading, pre-conciliar positions—such as
 *   the claim that error has no rights (Mirari Vos, Quanta Cura)—are
 *   superseded. Liturgical experimentation becomes legitimate development
 *   rather than deviation. The rupture reading treats the Council as a
 *   genuine break in institutional self-understanding, authorized by the
 *   magisterium but not reducible to continuity with prior teaching. This
 *   reading has profound institutional consequences: it justifies radical
 *   implementation (the liturgical reforms, theological reorientation,
 *   missionary recalibration), but it also generates resistance from agents
 *   who experienced pre-conciliar Catholicism as normative and continue to
 *   see it as authoritative. The constraint exhibits tangled_rope structure:
 *   genuine coordination function (the Council solved acute institutional
 *   problems—liturgical accessibility, engagement with modernity, missionary
 *   effectiveness) coexists with asymmetric extraction—the suppression of
 *   pre-conciliar frameworks as obsolete, the institutional enforcement of
 *   the rupture narrative, and the redefinition of doctrinal coherence to
 *   accommodate apparent contradictions between Vatican II and prior
 *   teaching.
 *
 * KEY AGENTS:
 *   - Traditional Liturgical Community: Primary victim (powerless/trapped) — Latin Mass forbidden or restricted; pre-conciliar theology declared obsolete by the magisterium that authorized it
 *   - Pre-Conciliar Theologians: Secondary victim (moderate/constrained) — foundational premises (error has no rights, material sin in liturgy) superseded; authority to teach compromised
 *   - Reform-Implementing Clergy: Primary beneficiary (institutional/arbitrage) — Council authorizes radical innovation; liturgical experimentation, pastoral autonomy, doctrinal reinterpretation become legitimate
 *   - Post-Conciliar Theological Establishment: Organized beneficiary (organized/constrained) — gains institutional authority from Council but constrained by magisterial lock-in (cannot reverse or fundamentally question the Council)
 *   - Vatican Administrative Machinery: Institutional enforcer (institutional/arbitrage) — maintains rupture frame through ritual and enforcement; preserves many pre-conciliar structures (papal supremacy, clerical celibacy, male-only priesthood) while declaring rupture
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the rupture reading as factual history rather than contestable hermeneutical choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.52).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.68).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II Magisterial Authority (Rupture Reading)").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, 'c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae').
narrative_ontology:cs_kernel_codification('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', formalized).
narrative_ontology:cs_authority_grounding('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', extraction).
narrative_ontology:cs_interpretation_layer_present('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae').
narrative_ontology:cs_reading_relation('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', foundational, conciliar_texts_encode_coherent_new_ecclesiology).
narrative_ontology:cs_axiom_status(conciliar_texts_encode_coherent_new_ecclesiology, holdable).
narrative_ontology:cs_axiom_grounding('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', conciliar_texts_encode_coherent_new_ecclesiology, empirically_contingent).
narrative_ontology:cs_axiom('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', foundational, magisterial_contradiction_constitutes_supersession).
narrative_ontology:cs_axiom_status(magisterial_contradiction_constitutes_supersession, holdable).
narrative_ontology:cs_axiom_grounding('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', magisterial_contradiction_constitutes_supersession, deontological).
narrative_ontology:cs_reference_frame('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', unbroken_doctrinal_continuity_from_trent).
narrative_ontology:cs_drift_state('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', post_vatican_ii_institutional_reality, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c5b42dd0-3eb2-42f9-8225-3bb9f99ad1ae', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, reform_implementing_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, post_conciliar_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_doctrinal_coherence).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditional_liturgical_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL LITURGICAL COMMUNITY (SNARE) — Trapped within parishes implementing Council reforms; Latin Mass forbidden or restricted; pre-conciliar theology labeled obsolete. No structural exit: the magisterium that enforces the rupture reading is the same authority that previously authorized their practices. Maximum suppression of alternatives.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRECONCILIAR THEOLOGIAN (TANGLED ROPE) — Constrained by magisterial authority declaring their foundational premises (error has no rights; material sin in liturgy) superseded. Also benefits from the Council insofar as theological productivity increases through reinterpretation debates. Some agency (can argue continuity counter-narrative) but significant suppression of the foundation they worked from.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-IMPLEMENTING CLERGY (ROPE) — Net beneficiary. The rupture reading authorizes radical liturgical experimentation, pastoral innovation, and doctrinal reinterpretation. They experience the Council as liberation from pre-conciliar constraints. Coordination function: implementing new theology in parishes. Minimal extraction relative to benefit.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-CONCILIAR THEOLOGICAL ESTABLISHMENT (TANGLED ROPE) — Organized actors (Vatican theological commissions, episcopal conferences, theologian guilds) benefit from Council as source of new authority claims and institutional resources. But they face suppression: the rupture reading locks them into interpreting the Council as authoritative while denying they can reverse it. Active enforcement of the rupture frame prevents reversion to pre-conciliar foundations.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: VATICAN ADMINISTRATIVE MACHINERY (PITON) — The institutional church administration maintains the rupture reading through ritual and performance (anniversary celebrations, conciliar rhetoric in official documents) but actual implementation is spotty and contested. Theater_ratio high: declarations of rupture coexist with preservation of many pre-conciliar structures (papal supremacy language, clerical celibacy, male-only priesthood). The machinery enforces the rupture frame not because it is functionally coherent but through institutional inertia.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MAGISTERIAL AUTHORITY AS FIXED (MOUNTAIN) — From a universal civilizational view, the Church's magisterial authority is treated as a fixed, unchangeable institution — the Vatican is the source of authoritative interpretation and what it declares *is* doctrine by definition. Under this reading, the rupture is simply the magisterium's authoritative pronouncement; no further question can be asked. However, this perspective risks naturalizing what is actually a contestable hermeneutical claim — false summit.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__rupture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rupture reading generates extraction primarily through the enforcement of authoritative closure—the magisterium declares the new ecclesiology binding, and pre-conciliar frameworks lose institutional legitimacy. The beneficiaries (reform clergy, post-conciliar theologians) gain substantial authority, resources, and interpretive power. The victims (traditional communities, pre-conciliar theologians) bear the cost of institutional reorientation and loss of doctrinal coherence. However, the extraction is not maximal (not > 0.70) because the coordination function is genuine: the Council solved real institutional problems (accessibility, engagement with modernity), and the beneficiaries' dominance is not purely coercive—it derives from magisterial authority that reformed clergy accept as legitimate. Suppression (0.68): Moderate-high and rising. Initial suppression of pre-conciliar alternatives was immediate (0.55) but intensified over the 10-year interval as post-conciliar enforcement machinery developed (0.68). The rupture reading requires active suppression: if continuity reading were allowed equal institutional voice, the rupture would be exposed as contestable. Theater ratio (0.65): Moderate-high and rising. The rupture reading is sustained partly by performative institutional gestures—Council anniversaries, continuity rhetoric from popes, declarations of magisterial infallibility—while structural preservation of pre-conciliar elements (papal supremacy, hierarchical authority) remains intact. The apparent paradox (rupture rhetoric + structural continuity) suggests theatrical maintenance of the rupture frame.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival gap between beneficiary and victim perspectives. The reform-implementing clergy experience the Council as liberation (Rope)—coordination without extraction. The traditional community experiences it as suppression (Snare)—institutional authority enforcing new doctrine while forbidding prior practice. The Vatican administrative machinery sees its own contradiction (Piton)—declaring rupture while preserving pre-conciliar structures through inertia. The post-conciliar theological establishment experiences organized benefit with institutional constraint (Tangled Rope)—they gained authority from the Council but cannot reverse it. The preconciliar theologian experiences loss of foundation with some residual agency (Tangled Rope)—constrained by magisterial authority but able to argue counter-narrative. The analytical observer risks naturalizing the rupture as magisterial fait accompli (Mountain/false summit)—treating the magisterium's declarations as fixing reality rather than as contestable hermeneutical choices. The perspectival collapse between benefits (reform clergy, institutional church) and costs (traditional community, doctrinal coherence) reveals the extraction mechanism: institutional authority enforces the rupture reading to authorize structural changes that benefit some actors at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from agents' structural positions relative to the rupture reading. Reform-implementing clergy and post-conciliar theologians are declared beneficiaries: they benefit from the Council's authorization of innovation and reinterpretation. The rupture reading provides them legitimacy (low d → negative effective extraction in their perspective). Traditional communities and pre-conciliar frameworks are declared victims: they bear the cost of institutional reorientation and suppression (high d → high effective extraction in their perspective). The Vatican administrative machinery experiences mixed directionality: as enforcer of the magisterium, it is structurally beneficiary (arbitrage options), but as institutional entity that must preserve pre-conciliar elements (papal supremacy, hierarchy) while declaring them superseded, it experiences internal contradiction (mid-range d). The analytical observer's d is computed as canonical for the analytical power atom (≈ 0.73), reflecting that the analytical position is neither beneficiary nor victim but observer—they cannot perceive the constraint clearly from within any single institutional position; they need the full presheaf of perspectives to see what the rupture reading suppresses.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading generates mandatrophy at the meta-institutional level: the constraint exhibits genuine coordination function (solving acute institutional problems—liturgical participation, missionary engagement, doctrinal flexibility) alongside genuine extraction (suppression of alternatives, enforcement of authoritative closure, redefinition of doctrinal coherence to accommodate contradictions). The mandatrophy is resolved through perspectival multiplicity: from the beneficiary's perspective, the constraint is coordination (Rope—the Council solved real problems). From the victim's perspective, the constraint is extraction (Snare—institutional authority enforcing new doctrine while forbidding prior practice). From the analytical civilizational perspective, the constraint risks appearing as immutable law (Mountain—the magisterium is the source of authoritative doctrine), but this appearance dissolves when the beneficiary/victim structure is analyzed—the magisterium's authority is not a natural law but a contestable institutional arrangement. The rupture reading locks the magisterium into enforcing the new ecclesiology as binding, which is the source of suppression. If the continuity reading were institutionally authorized alongside the rupture reading, suppression would decline sharply (the constraint would become composite_overdetermination—institutional encoding of irresolvable hermeneutical ambiguity). The mandatrophy-resolution insight: the constraint is neither pure coordination nor pure extraction, but tangled_rope precisely because the magisterium uses coordination (solving real institutional problems) to authorize extraction (suppression of alternatives). The coordination function is genuine; the extraction is not incidental—it is structurally required to maintain the rupture reading as authoritative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_vs_authoritative_closure,
    'Do the Council texts themselves encode the rupture reading, or does the rupture reading project backward onto ambiguous conciliar formulations?',
    'Comparative hermeneutics: (1) texts read in isolation vs. (2) texts read in light of pre-conciliar magisterium vs. (3) texts read as intentional compromise formulations bridging incompatible positions. Examine preparatory documents (schemata rejections) and council floor debates for authorial intent.',
    'If texts encode rupture: rupture reading is correct and continuity reading is eisegesis. If texts are ambiguous: both rupture and continuity are equally defensible readings, and the constraint is actually composite_overdetermination. If texts are intentional bridges: the engine reclassifies as tangled_rope_institutionalized_ambiguity (a different constraint entirely).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_vs_authoritative_closure, empirical, 'Whether conciliar texts intrinsically encode the rupture or whether rupture is imposed by subsequent interpretation').

omega_variable(
    magisterial_self_contradiction_resolution,
    'When Vatican II declares religious freedom (Dignitatis Humanae) in apparent contradiction to prior magisterial teaching (e.g., Mirari Vos on error''s lack of rights), does the conciliar act supersede the prior teaching or reinterpret it?',
    'Formal analysis of contradiction structure: (a) Is DH truly incompatible with prior teaching or compatible under reinterpretation? (b) If incompatible, does Vatican II explicitly claim to supersede (rupture reading) or implicitly recontextualize (continuity reading)? (c) Can a single framework hold both formulations without contradiction?',
    'If DH is compatible under reinterpretation: continuity reading strengthened. If DH is incompatible and explicitly supersedes: rupture reading strengthened. If DH is incompatible but Vatican II does not explicitly claim to supersede: composite_overdetermination reading becomes dominant (the constraint is actually encoding two incompatible ecclesiologies).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magisterial_self_contradiction_resolution, conceptual, 'Resolution of apparent magisterial self-contradiction between Vatican II and pre-conciliar teaching').

omega_variable(
    implementation_fidelity_to_conciliar_intent,
    'Do post-conciliar reforms (liturgical experimentation, doctrinal reinterpretation, institutional restructuring) faithfully implement the Council''s actual decisions, or do they project the rupture reading onto the Council retroactively?',
    'Historical analysis comparing (1) conciliar texts and decisions, (2) documents issued by Paul VI and John Paul II interpreting the Council, and (3) actual implementation in parishes, seminaries, and theological faculties. Identify where implementation diverged from documented conciliar intent.',
    'If implementation faithful to conciliar decisions: rupture reading is validated by structural fidelity. If implementation diverges significantly: the rupture reading may be a post-conciliar construction imposed on an ambiguous or moderate Council. This feeds back to the textual_ambiguity omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_to_conciliar_intent, empirical, 'Fidelity of post-conciliar implementation to documented conciliar intent').

omega_variable(
    authority_grounding_shift_or_continuity,
    'Does the rupture reading claim that Vatican II changed the *source* or *grounding* of magisterial authority (e.g., from papal supremacy to collegial authority), or only its *application*?',
    'Analysis of Lumen Gentium (especially Chapter 3 on collegiality) and post-conciliar practice: (a) Did the Council formally relocate magisterial authority from Pope to College of Bishops? (b) Did subsequent popes (Paul VI, John Paul II) reassert papal supremacy or institutionalize collegiality? (c) Can both authority structures coexist, or are they fundamentally incompatible?',
    'If authority grounding shifted: radical rupture from pre-conciliar institutional structure. If only application shifted: moderate rupture. If both coexist uneasily: composite_overdetermination becomes structural fact (the constraint is an institutional encoding of irresolvable authority ambiguity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_shift_or_continuity, conceptual, 'Whether Vatican II shifted the source of magisterial authority or only its application').

omega_variable(
    natural_law_vs_institutional_suppression,
    'Is the suppression of pre-conciliar positions (0.68) a structural consequence of institutional authority (natural law of hierarchical authority), or an active enforcement choice made by post-conciliar leadership?',
    'Counterfactual analysis: Could the Vatican have authorized coexisting pre-conciliar and post-conciliar positions? If yes: suppression is active choice. If no: suppression is structural necessity of institutional hierarchy.',
    'If suppression is active choice: the rupture reading is contingent and potentially reversible. If suppression is structural: the rupture reading is locked into place by institutional logic. This bears on whether the constraint could be reclassified by a future papacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_suppression, conceptual, 'Whether suppression of pre-conciliar positions is structural or contingent on institutional choice').

omega_variable(
    kernel_reading_vs_factual_history,
    'Is this constraint a reading of a hermeneutical kernel (contested interpretation of what Vatican II means) or a factual historical claim (what Vatican II actually was)?',
    'Meta-theoretical: Distinguish between (a) the factual question ''What did the Council documents say?'' (empirical, resolvable by textual analysis) and (b) the interpretive question ''What does Vatican II mean for the Church''s identity?'' (hermeneutical, depends on framework). This constraint is a reading of the second question, not a claim about the first.',
    'If this is treated as factual history: empirical falsification of the rupture reading is possible (texts contradict the rupture claim). If this is treated as hermeneutical kernel: the rupture reading is one defensible interpretation among others; no empirical fact could falsify it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_factual_history, conceptual, 'Metahermeneutical status: is this constraint a reading of a hermeneutical kernel or a factual historical claim?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_rupture_theater_t0, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(vii_rupture_theater_t5, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 5, 0.58).
narrative_ontology:measurement(vii_rupture_theater_t10, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(vii_rupture_extract_t0, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vii_rupture_extract_t5, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(vii_rupture_extract_t10, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vii_rupture_suppress_t0, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vii_rupture_suppress_t5, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(vii_rupture_suppress_t10, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, dignitatis_humanae_religious_freedom_constraint).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_liturgical_reform_constraint).

% DUAL FORMULATION NOTE:
% The rupture reading is one of three competing readings of the vatican_ii_magisterial_authority kernel. Each reading is a separate constraint story with its own ε value, perspectives, and beneficiary/victim structure. The rupture reading (this story) has ε=0.52 (tangled_rope); the continuity reading has lower ε and different suppression profile; the composite reading has higher ε and different enforcement structure. All three affect downstream constraints in the Vatican II institutional ecosystem but in different ways—the rupture reading justifies radical implementation, the continuity reading justifies preservation of pre-conciliar elements, the composite reading justifies simultaneous holding of incompatible positions. The network linkage makes visible the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_magisterial_authority__rupture_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
