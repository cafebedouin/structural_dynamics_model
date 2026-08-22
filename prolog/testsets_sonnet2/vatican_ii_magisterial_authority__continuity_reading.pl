% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Hermeneutic of Continuity Reading of Vatican II Magisterial Authority
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story authors the continuity reading of the Vatican II magisterial
 *   authority kernel: the claim that the Council's texts constitute organic
 *   development within unbroken tradition, that 'spirit of Vatican II'
 *   appeals to a discontinuous new ecclesiology are unauthorized
 *   extrapolations, that the Latin-preservation mandate of Sacrosanctum
 *   Concilium §36 remains binding law rather than a superseded preference,
 *   and that Dignitatis Humanae's religious-freedom teaching is reconcilable
 *   with the Syllabus of Errors via thesis/hypothesis distinction or a
 *   doctrine-development framework. This is Benedict XVI's 2005 'hermeneutic
 *   of continuity vs. hermeneutic of rupture' address formalized as a
 *   constraint. The rupture reading and the composite/overdetermination
 *   reading are separate constraints (siblings in this kernel), not
 *   alternative measurements of this one — each has its own ε, its own
 *   beneficiary/victim structure, and its own classification, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - curial_continuity_faculty: agenda_setter (institutional/arbitrage) — administers the continuity standard via canonical mission and faculty discipline
 *   - traditionalist_leaning_bishops: beneficiary (powerful/constrained) — use continuity framing to slow-walk reform implementation
 *   - progressive_theologians_disciplined_for_rupture_readings: payer (moderate/constrained) — face sanction for teaching discontinuity readings
 *   - traditionalist_communities_denied_full_rite_latitude: payer (powerless/trapped) — experience vernacular displacement as the rupture the reading denies exists
 *   - laity_seeking_vernacular_and_reform_implementation: payer (powerless/constrained) — experience reform throttled by continuity-minded local implementation
 *   - conciliar_periti_and_historians: excluded (moderate/analytical) — hold documentary evidence of drafting compromise, denied adjudicative standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.42).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Hermeneutic of Continuity Reading of Vatican II Magisterial Authority").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, 'b0307e40-be57-496f-94e0-f6270fc069a4').
narrative_ontology:cs_kernel_codification('b0307e40-be57-496f-94e0-f6270fc069a4', fixed_text).
narrative_ontology:cs_authority_grounding('b0307e40-be57-496f-94e0-f6270fc069a4', lineage).
narrative_ontology:cs_interpretation_layer_present('b0307e40-be57-496f-94e0-f6270fc069a4').
narrative_ontology:cs_reading_relation('b0307e40-be57-496f-94e0-f6270fc069a4', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('b0307e40-be57-496f-94e0-f6270fc069a4', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b0307e40-be57-496f-94e0-f6270fc069a4', foundational, magisterial_indefectibility_precludes_doctrinal_contradiction).
narrative_ontology:cs_axiom_status(magisterial_indefectibility_precludes_doctrinal_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('b0307e40-be57-496f-94e0-f6270fc069a4', magisterial_indefectibility_precludes_doctrinal_contradiction, deontological).
narrative_ontology:cs_axiom('b0307e40-be57-496f-94e0-f6270fc069a4', foundational, dignitatis_humanae_reconcilable_with_syllabus_via_development).
narrative_ontology:cs_axiom_status(dignitatis_humanae_reconcilable_with_syllabus_via_development, holdable).
narrative_ontology:cs_axiom_grounding('b0307e40-be57-496f-94e0-f6270fc069a4', dignitatis_humanae_reconcilable_with_syllabus_via_development, conventional).
narrative_ontology:cs_axiom('b0307e40-be57-496f-94e0-f6270fc069a4', secondary, sc_36_latin_preservation_remains_binding_norm).
narrative_ontology:cs_axiom_status(sc_36_latin_preservation_remains_binding_norm, holdable).
narrative_ontology:cs_axiom_grounding('b0307e40-be57-496f-94e0-f6270fc069a4', sc_36_latin_preservation_remains_binding_norm, conventional).
narrative_ontology:cs_reference_frame('b0307e40-be57-496f-94e0-f6270fc069a4', unbroken_apostolic_magisterial_continuity).
narrative_ontology:cs_drift_state('b0307e40-be57-496f-94e0-f6270fc069a4', post_2005_hermeneutic_formalization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b0307e40-be57-496f-94e0-f6270fc069a4', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, curial_continuity_faculty).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, magisterial_teaching_office).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians_disciplined_for_rupture_readings).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities_denied_full_rite_latitude).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, laity_seeking_vernacular_and_reform_implementation).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, doctrinal_development_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, hermeneutic_of_continuity_doctrine).
narrative_ontology:constraint_vindicates(vatican_ii_magisterial_authority__continuity_reading, magisterial_self_consistency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupy teaching and disciplinary offices (CDF/DDF, pontifical universities, diocesan tribunals) that adjudicate which readings of conciliar texts count as authentic magisterium. They author commentaries, approve seminary curricula, and can decline canonical mission to theologians who teach discontinuity. Their institutional legitimacy depends on Vatican II being read as continuous with everything before it — a rupture reading would destabilize the claim that the magisterium never contradicts itself.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, curial_continuity_faculty, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Govern dioceses where they can slow-walk or minimize implementation of reforms (liturgical latitude, ecumenical initiatives) by appeal to continuity with prior discipline, citing SC §36 and pre-conciliar norms as still binding baselines. They benefit from a reading that treats aggressive reform implementation as an unauthorized 'spirit of the Council' overreach they are entitled to resist.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_leaning_bishops, beneficiary,
    powerful, generational, constrained, national).

% The abstract claim of an indefectible, self-consistent teaching authority is what the continuity reading exists to protect. It is not an actor that collects anything itself, but its coherence is the good the reading vindicates; listed for completeness, not as a rent-collecting agent.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, magisterial_teaching_office, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__continuity_reading, magisterial_teaching_office).

% Argue in print and in classrooms that Dignitatis Humanae cannot be squared with the Syllabus of Errors, or that Lumen Gentium's collegiality genuinely revises Pastor Aeternus's centralization, without resorting to thesis/hypothesis harmonization. Under the continuity reading's enforcement machinery they face withdrawal of canonical mission, non-renewal of teaching faculties, or formal warnings; their exit is to leave institutional Catholic academia, which forecloses their vocation rather than merely relocating it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, progressive_theologians_disciplined_for_rupture_readings, payer,
    moderate, biographical, constrained, national).

% Communities attached to the pre-1970 liturgy who read Sacrosanctum Concilium's Latin-preservation mandate (SC §36) as binding and experience its practical abrogation as itself a discontinuity the continuity reading cannot fully absorb. They are told the Council authorizes no rupture, yet the vernacular displacement they live under looks, from where they stand, exactly like one; their recourse is largely canonical petition with uncertain and slow-moving effect.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, traditionalist_communities_denied_full_rite_latitude, payer,
    powerless, generational, trapped, regional).

% Ordinary Catholics who experienced Vatican II as authorizing substantive change (ecumenism, religious freedom, liturgical vernacular, lay participation) find local implementation throttled where continuity-minded bishops treat reform as optional pastoral prudence rather than conciliar mandate. Their exit is disengagement from parish life or migration to more reform-implementing dioceses, neither of which is available everywhere.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, laity_seeking_vernacular_and_reform_implementation, payer,
    powerless, biographical, constrained, global).

% Historians of the Council's drafting process (including participants' own diaries and later testimony) who documented genuine doctrinal shifts and deliberate ambiguity in conciliar drafting are rarely given magisterial standing to adjudicate the hermeneutic question — their historical findings are treated as background scholarship, not as binding on the continuity claim, which is a theological and disciplinary determination, not a historical one.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, conciliar_periti_and_historians, excluded,
    moderate, generational, analytical, global).

% Study the reception history of Vatican II across dioceses and decades, comparing textual claims to disciplinary outcomes, without themselves holding magisterial office or being subject to its sanctions.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, academic_ecclesiologists_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretive frame that lets a global, multi-generational institution present its teaching as unbroken and self-consistent, allowing bishops, seminaries, and theologians worldwide to coordinate on what may be taught as authentic doctrine without each locality re-litigating the Council's meaning from scratch.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary leverage toward the curial and episcopal offices that administer the continuity standard, and away from theologians and communities whose readings of the same texts diverge from that standard — whether they read the Council as more radical (progressive theologians) or as itself already a rupture (traditionalist communities losing the old rite).
% ABSENT_VOICES: The conciliar periti and later historians who documented substantive drafting compromises and deliberate doctrinal ambiguity are not accorded a vote in the hermeneutic determination; their historical evidence is treated as informative but not authoritative, since the continuity claim is adjudicated theologically and disciplinarily, not historically.
% DISAPPEARANCE_RATIONALE: If the continuity reading's enforcement machinery (canonical mission review, faculty discipline, liturgical constraint) disappeared overnight, theologians teaching rupture or composite readings would face no institutional sanction, traditionalist communities could press full SC §36 Latin claims without being told they misread a text that mandates continuity, and diocesan variation in reform implementation would lose its 'this is merely prudential, not doctrinal' cover — both the progressive and traditionalist payer seats would immediately renegotiate their standing.
% FOUNDING_PROBLEM: The Council fathers and immediate post-conciliar magisterium needed a way to authorize substantial pastoral and doctrinal development (religious freedom, ecumenism, liturgical reform, collegiality) without conceding that an ecumenical council could contradict a prior one — since conceding that would undermine the doctrine of magisterial indefectibility itself.
% FOUNDING_PROBLEM_CORROBORATION: Curial documents (e.g., Benedict XVI's 2005 Christmas address articulating the hermeneutic of continuity) attest the problem as live and correctly resolved by continuity. Independent historians of the Council (e.g., the Bologna school and its critics, plus participant diaries) and canon lawyers outside the CDF/DDF chain attest that the drafting record shows genuine doctrinal renegotiation papered over by deliberately ambiguous language — corroboration for the founding-problem's necessity exists, but corroboration for the continuity reading's success in solving it without residue does not come from outside the benefiting curial and episcopal offices.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at moderate-high (0.42) rather than low, because the continuity reading does real coordination work (a single interpretive standard letting a billion-member institution avoid re-litigating doctrine locality by locality) while ALSO functioning as a disciplinary lever against theologians and communities whose textual readings diverge, in both directions (too radical a reading, or too traditional a reading of what was preserved). Suppression (0.55) reflects that persistence depends on active enforcement — canonical mission withdrawal, faculty non-renewal, liturgical discipline — not on the reading's self-evidence; a genuine natural-law-grade hermeneutic would not need comparable enforcement machinery. Theater ratio (0.3) is moderate: substantial genuine theological work is done under this frame (real doctrinal development scholarship), but a rising share of institutional energy over the interval (0.15 to 0.30) goes to policing the boundary of acceptable reading rather than doing first-order theology, particularly visible after 2005 when the hermeneutic itself became the explicit disciplinary standard.
 *
 * PERSPECTIVAL GAP:
 *   From the curial/episcopal agenda-setter seat, the continuity reading is Rope-like: genuine coordination that lets a global institution speak coherently across two millennia. From the payer seats on both wings — progressive theologians sanctioned for reading rupture, and traditionalist communities told their lived experience of liturgical discontinuity does not count as discontinuity — the same structure operates as Tangled-Rope-grade extraction: a coordination story with real teeth used to foreclose competing textual readings. The engine computes these divergently from the same structural data; this story does not adjudicate which seat is 'right,' only that the divergence is structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Curial continuity faculty and traditionalist-leaning bishops are declared beneficiaries because their institutional standing and disciplinary latitude depend on the continuity claim holding; their exit options (arbitrage, constrained-but-powerful) keep their derived directionality near the beneficiary end. Progressive theologians, traditionalist Latin-rite communities, and reform-seeking laity are declared victims because the same standard is what forecloses their competing readings from institutional legitimacy; their trapped/constrained exit options and lower power push derived directionality toward the target end. The magisterial teaching office itself is listed as a non-agent beneficiary (agent: false) because it is the abstract good being protected, not a rent-collecting actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — authorizing pastoral and doctrinal development without conceding an ecumenical council can contradict prior magisterium — was live in 1965 and arguably remains live: the institution still needs a non-contradiction doctrine to function as a teaching authority across centuries. This is why founding_problem_status is authored as contested rather than dead: unlike a pure zombie mandate, there is an ongoing structural need the reading answers. But the enforcement intensity has risen over sixty years (suppression 0.35 to 0.55) even as the acute post-conciliar crisis of interpretation has receded, which is the pattern this classification exists to flag — coordination function persisting alongside accumulating extraction, rather than either pure Rope or pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_reading_kernel_disagreement_location,
    'Where exactly does the continuity reading''s account diverge from the rupture and composite-overdetermination readings — is it located in the drafting history (what the periti intended), the textual content (what the documents actually say), or the reception history (how the teaching has been implemented)?',
    'Comparative textual-historical analysis cross-referencing conciliar drafting records (acta synodalia), the final promulgated texts, and fifty years of implementation record across dioceses; the disagreement would be resolved (or shown irresolvable) by locating which of the three loci the readings actually diverge on rather than assuming they diverge on all three uniformly.',
    'If the divergence is purely in reception/implementation, the continuity reading may be textually sound while its enforcement apparatus is doing separate extractive work; if the divergence is in the text itself (e.g., DH''s religious-freedom principle genuinely contradicts the Syllabus''s propositions at the level of stated doctrine, not just application), the continuity reading''s core reconciliation move (thesis/hypothesis, development-of-doctrine) is doing more load-bearing theological work than it can support, which would strengthen the rupture reading''s case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_reading_kernel_disagreement_location, conceptual, 'Locating where the continuity/rupture disagreement actually resides.').

omega_variable(
    sc_36_latin_mandate_binding_status,
    'Is Sacrosanctum Concilium §36''s Latin-preservation clause still binding law under the continuity reading, or has it been effectively superseded by subsequent liturgical legislation in a way the continuity reading must itself treat as a kind of discontinuity it does not acknowledge?',
    'Canonical analysis of whether post-conciliar liturgical instructions (e.g., Inter Oecumenici, the Novus Ordo promulgation) constitute authorized development of SC §36 or its de facto abrogation; input from canon lawyers outside the curial teaching offices administering the standard.',
    'If SC §36 was in fact set aside rather than developed, the continuity reading has an internal discontinuity of its own regarding liturgical law specifically, which would complicate its claim to zero rupture and lend some support to the traditionalist-community payer seat''s lived experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sc_36_latin_mandate_binding_status, empirical, 'Whether the Latin mandate was developed or abrogated under the continuity reading''s own terms.').

omega_variable(
    natural_vs_constructed_hermeneutic_standard,
    'Is the hermeneutic-of-continuity standard a genuine theological necessity following from the doctrine of magisterial indefectibility (in which case it functions closer to a structural constraint the institution cannot escape), or is it a constructed interpretive policy adopted for institutional-stability reasons that could in principle be revised (in which case its persistence is closer to administrative choice than doctrinal necessity)?',
    'Examination of whether prior councils'' relationships to their predecessors were adjudicated under an equivalent explicit continuity standard, or whether this level of formalized hermeneutic policing is itself a post-Vatican-II institutional innovation without precedent in the reception of earlier councils (e.g., Trent''s relationship to prior medieval teaching).',
    'If unprecedented, the continuity reading''s enforcement apparatus looks more like a constructed response to a specific 20th-century crisis of authority than an eternal feature of how the magisterium has always self-validated, which would support treating its extractive dimension as contingent rather than necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_hermeneutic_standard, conceptual, 'Whether the continuity standard is doctrinally necessary or a constructed post-conciliar institutional policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1990, 0.24).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1978, 0.33).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2005, 0.4).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2013, 0.38).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2005, 0.53).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2013, 0.5).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposing the natural-language concept 'what Vatican II means for magisterial continuity,' per the epsilon-invariance principle. The continuity reading (this file) authors low-to-moderate extraction and a tangled_rope claim; the rupture reading would author a different beneficiary/victim structure entirely (the rupture reading's beneficiaries are those advocating for further reform on the premise a break already occurred, and its victims would include institutional actors whose legitimacy depends on non-contradiction); the composite_overdetermination reading treats the ambiguity itself as the extractable resource, benefiting whichever faction can currently claim the compromise language for its own program. Each is authored with its own epsilon and its own stakeholders; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
