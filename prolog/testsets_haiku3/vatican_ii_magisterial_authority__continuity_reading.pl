% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: vatican_ii_magisterial_authority__continuity_reading
 *   human_readable: Vatican II Magisterial Authority—Continuity Reading
 *   domain: ecclesiology/institutional history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962–1965) issued ambiguous conciliar documents that could be
 *   read as authorizing either genuine rupture with pre-conciliar discipline
 *   or organic development within continuity. The immediate post-conciliar
 *   period (1965–1985) saw competing implementations: reformers pursued
 *   liturgical and doctrinal modernization under a 'spirit of Vatican II'
 *   reading; traditionalists and the Magisterium asserted that the texts,
 *   properly understood within the continuity of magisterial teaching, bound
 *   implementation to preserve core pre-conciliar doctrine. This constraint
 *   story instantiates the continuity reading: the authoritative claim that
 *   Vatican II texts, when interpreted in light of prior magisterial
 *   teaching, represent organic development, not rupture. The ruling enforces
 *   this reading by vetoing post-conciliar reforms that appeal to the
 *   'spirit' rather than the letter, especially regarding Latin liturgy and
 *   doctrinal scope. This is ONE READING of the contested kernel
 *   'vatican_ii_magisterial_authority'—the other readings (rupture_reading,
 *   composite_overdetermination_reading) instantiate structurally different
 *   constraints with different beneficiaries, victims, and ε values. The
 *   continuity reading is presented here as one committer-axis instantiation
 *   of that kernel.
 *
 * KEY AGENTS:
 *   - institutional_magisterium: Enforces the continuity reading via magisterial documents (Ecclesia Dei, Summorum Pontificum, Responsa ad dubia) that assert Vatican II preserves prior doctrine and that implementations must harmonize with pre-conciliar teaching.
 *   - latin_traditionalists: Benefit from the continuity frame because it grants their Latin preservation demand (based on SC §36) institutional legitimacy as fidelity to the Council's own text.
 *   - post_conciliar_reformers: Bear the cost—their pastoral programs are reframed as unauthorized 'spirit' readings that contradict the Council's true letter.
 *   - pre_conciliar_theologians: Cited as the reference point for what the continuity reading claims Vatican II preserves; they are analytical seats whose corpus is invoked to establish the benchmark.
 *   - rupture_reading_advocates: Excluded by institutional authority that denies their reading is what the texts actually say.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, 0.68).
domain_priors:suppression_score(vatican_ii_magisterial_authority__continuity_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__continuity_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__continuity_reading, "Vatican II Magisterial Authority—Continuity Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__continuity_reading, "ecclesiology/institutional history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__continuity_reading, '9c15e344-ffb6-4296-bab0-bd544be36f0f').
narrative_ontology:cs_kernel_codification('9c15e344-ffb6-4296-bab0-bd544be36f0f', fixed_text).
narrative_ontology:cs_authority_grounding('9c15e344-ffb6-4296-bab0-bd544be36f0f', lineage).
narrative_ontology:cs_interpretation_layer_present('9c15e344-ffb6-4296-bab0-bd544be36f0f').
narrative_ontology:cs_reading_relation('9c15e344-ffb6-4296-bab0-bd544be36f0f', vatican_ii_magisterial_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('9c15e344-ffb6-4296-bab0-bd544be36f0f', vatican_ii_magisterial_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('9c15e344-ffb6-4296-bab0-bd544be36f0f', foundational, magisterial_continuity_principle).
narrative_ontology:cs_axiom_status(magisterial_continuity_principle, holdable).
narrative_ontology:cs_axiom_grounding('9c15e344-ffb6-4296-bab0-bd544be36f0f', magisterial_continuity_principle, deontological).
narrative_ontology:cs_axiom('9c15e344-ffb6-4296-bab0-bd544be36f0f', foundational, pre_conciliar_doctrine_binding_reference).
narrative_ontology:cs_axiom_status(pre_conciliar_doctrine_binding_reference, holdable).
narrative_ontology:cs_axiom_grounding('9c15e344-ffb6-4296-bab0-bd544be36f0f', pre_conciliar_doctrine_binding_reference, conventional).
narrative_ontology:cs_reference_frame('9c15e344-ffb6-4296-bab0-bd544be36f0f', magisterial_tradition_unbroken_lineage).
narrative_ontology:cs_drift_state('9c15e344-ffb6-4296-bab0-bd544be36f0f', post_conciliar_reform_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c15e344-ffb6-4296-bab0-bd544be36f0f', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, institutional_magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, latin_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_reformers).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__continuity_reading, vernacular_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__continuity_reading, vernacular_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Vatican's teaching authority (Magisterium), particularly the Congregation for the Doctrine of the Faith and successive popes (Paul VI, John Paul II, Benedict XVI, Francis). They set and enforce the continuity interpretation by issuing clarifications (Ecclesia Dei 1986, Summorum Pontificum 2007), vetoing radical implementations, and maintaining that Vatican II texts, properly understood within magisterial tradition, preserve prior doctrine. They are trapped in defending this frame because institutional legitimacy depends on the Magisterium's authority to interpret the Council authoritatively. They collect interpretive veto power from this arrangement.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, institutional_magisterium, agenda_setter,
    institutional, civilizational, trapped, universal).

% Organized traditionalist communities (FSSP, Institute of Christ the King, Ecclesia Dei groups, independent traditionalist seminaries) and laypeople devoted to the Latin liturgy. They benefit from the continuity reading because it interprets the Council's Latin preservation mandate (Sacrosanctum Concilium §36) as binding rather than merely permissive. When the Magisterium asserts continuity, it grants their position hermeneutical legitimacy: they are not clinging to the past but faithfully implementing the Council itself. Their exit is constrained—they could split to fully independent communities (SSPX, sedevacantist groups) but that means formal schism and loss of institutional standing.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, latin_traditionalists, beneficiary,
    organized, generational, constrained, global).

% Bishops, theologians, and pastoral clergy who interpreted Vatican II as liberalizing—opening liturgy to the vernacular, engaging modern thought, pluralizing theological schools, embracing religious freedom and ecumenism not as developments but as ruptures from pre-conciliar restrictions. They bear the cost of the continuity constraint because each innovation they implement on pastoral or theological grounds can be reframed by the Magisterium as unauthorized departure from the Council's true (continuity-preserving) meaning. Their exit is identity-locked: leaving the priesthood or the Church's teaching community means abandoning their identity as Catholic clergy/theologians, yet staying means submitting to hermeneutical correction that denies their pastoral reasoning.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, post_conciliar_reformers, payer,
    organized, biographical, identity_locked, global).

% Parish priests, pastoral agents, and communities who embrace the Novus Ordo in the vernacular as the genuine Council-mandated reform. They are partly beneficiaries (the Council did authorize the vernacular) but partly payers (the continuity frame can reinterpret SC §36 to require Latin preservation and treat vernacular-only practice as spirit-reading excess). Their exit is constrained: they can practice traditionalist Latin liturgy only by accepting the risk that it is officially forbidden, or they can conform to vernacular norms and accept being told their pastoral choice is technically unauthorized excess.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, vernacular_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__continuity_reading, vernacular_advocates, beneficiary).

% The corpus of pre-conciliar magisterial teaching (papal encyclicals, conciliar decrees, dogmatic formulations from the Syllabus of Errors through Pius XII). Not a real agent but an cited reference point: the continuity reading appeals to this body to establish what Vatican II must preserve. The tradition itself cannot speak or defend itself; its meaning is interpreted and invoked by the Magisterium. The question whether Vatican II actually preserves this tradition or reinterprets it remains the core contest.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_magisterial_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_magisterial_authority__continuity_reading, pre_conciliar_magisterial_tradition).

% Theologians and bishops (Hans Küng, Edward Schillebeeckx in earlier periods, contemporary progressive scholars) who argue Vatican II represents a genuine break: new ecclesiology (People of God, collegiality), new approach to revelation and Scripture, new religious freedom doctrine incompatible with the Syllabus. They are structurally excluded from the continuity frame because accepting it requires abandoning their core claim that the Council broke with pre-conciliar restriction. Within the Church's institutional structure, the Magisterium can deny their reading by appealing to continuity; outside, they lose institutional standing to make their case. Trapped: they cannot maintain their reading within institutional Catholicism without resistance.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, rupture_reading_advocates, excluded,
    organized, biographical, trapped, global).

% Scholars (Vatican II historians, some canon lawyers) who argue the Council was deliberately ambiguous—its formulations were compromise language designed to accommodate incompatible ecclesiological visions held by different Fathers. The Council's genius was productive ambiguity, not continuity. This reading is excluded by the continuity frame because the frame asserts the texts have a stable meaning (continuity with prior doctrine) and uses that stability to adjudicate disputes. The composite reading threatens the frame's coherence. Constrained: they can publish scholarly work but lack institutional authority to define what the Council means.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__continuity_reading, composite_overdetermination_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__continuity_reading, institutional_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified hermeneutic standard for interpreting Vatican II conciliar texts: appeal to prior magisterial teaching as the reference frame. This enables the institutional Magisterium to coordinate post-conciliar implementations around a single benchmark, preventing divergent readings (rupture, overdetermination) from fragmenting the Church's self-understanding. The coordination problem is genuine: without a stable hermeneutic frame, bishops and theologians would implement the Council according to incompatible visions, risking institutional schism.
% TRANSFER_FUNCTION: Moves interpretive authority from the ambiguous conciliar texts themselves (which permit multiple readings) to the pre-conciliar magisterial deposit (which the continuity frame claims the texts must preserve). This transfer grants the Magisterium exclusive power to define what the Council actually meant, enabling it to reject implementations that appeal to 'the spirit of Vatican II' without textual warrant. Reformers and independent theologians bear the cost of constrained interpretation; the Magisterium collects the interpretive veto.
% ABSENT_VOICES: Conciliar Fathers who held rupture-reading intentions during the Council are silenced by the continuity frame, which asserts what the Council's actual votes produced (organic development) regardless of some Fathers' stated intentions. Post-conciliar reformers, who believed they were implementing the Council's true mandate, are structurally excluded from the hermeneutic conversation—their pastoral reasoning is reframed as unauthorized 'spirit' reading. Laity and religious communities depend on institutional permission to practice their understanding of the Council; their interpretations are absent from the authoritative machinery. Competing readings (rupture, composite) are excluded by the institutional enforcement of continuity as the canonical frame.
% DISAPPEARANCE_RATIONALE: If the continuity constraint disappeared—if the Magisterium conceded that Vatican II genuinely diverges from pre-conciliar teaching, or admitted that the texts are overdetermined composite rather than preserving prior doctrine—the post-conciliar settlement would reorganize fundamentally. The 'spirit of Vatican II' reading would gain legitimacy; reformers could justify vernacular liturgy and theological pluralism as fidelity to the Council's true break, not as unauthorized excess. Alternatively, if the Magisterium conceded overdetermination, the institutional solution would shift to acknowledging multiple legitimate readings rather than enforcing a single frame. The Church's self-understanding would stabilize into a different institutional settlement. The continuity constraint's disappearance would trigger reorganization because it controls how the Church adjudicates its own recent past.
% FOUNDING_PROBLEM: In the immediate post-conciliar period (1965–1985), the Church faced radical interpretive diversity: some Fathers and bishops read the Council as authorizing genuine rupture (vernacular liturgy as break from Latin tradition, religious freedom as new doctrine incompatible with Syllabus, reform as liberation from pre-conciliar rigidity); others read it as continuity (Latin preservation mandate as binding, religious freedom compatible with Syllabus via thesis/hypothesis distinction, reform as development within tradition). Without a stable hermeneutic frame, different national bishops' conferences implemented Vatican II radically differently—Netherlands highly reformist, Poland more conservative. The unified Church risked fragmenting into incompatible communities each claiming conciliar warrant. The founding problem was: how to maintain institutional cohesion when the Council's text permits incompatible readings?
% FOUNDING_PROBLEM_CORROBORATION: The institutional Magisterium (Paul VI, John Paul II, Benedict XVI) attests the founding problem remains live and requires continuity hermeneutics for its solution—they cite ongoing tensions between reform and tradition, the need for hermeneutical discipline, and the Syllabus-Vatican II compatibility question as evidence the problem persists. Post-conciliar historians and scholars outside the benefiting parties (John W. O'Malley, Gary Wills, Andrew Greeley) attest the founding problem's urgency has faded: the reform generation aged out, younger clergy were trained in continuity hermeneutics from the start, and institutional schism did not occur. They attribute the resolution to demographic turnover, not hermeneutical victory. No external corroboration supports the Magisterium's claim that continuity hermeneutics was necessary for the solution; the evidence suggests demographic and institutional factors sufficed.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__continuity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_magisterial_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) and rises monotonically from 1965 to 1985, then plateaus. The rise reflects the Magisterium's accumulating effort to suppress alternative readings and enforce hermeneutical discipline—each clarification (from Paul VI's cautions through John Paul II's Ecclesia Dei through Benedict XVI's Summorum Pontificum) tightens the constraint by making non-continuity readings institutionally costly. The initial lower value (0.42 in 1965, projected) reflects uncertainty: at the Council's close, the constraint was not yet crystallized. By 1975 (observed), the Magisterium had issued decisive documents establishing continuity as the official frame, and extraction rose sharply. Theater ratio also rises sharply from 1965–2000, then plateaus around 0.58: this reflects the increasing proportion of Magisterial activity devoted to hermeneutical policing (reinterpreting what 'religious freedom' truly means, why Latin must be retained, how pastoral aggiornamento fits continuity) rather than primary doctrinal teaching. The plateau after 2000 suggests theatrical maintenance has reached a stable level—the constraint persists not primarily through new doctrinal content but through interpretive enforcement. Suppression requirement rises from 1965–2000 and then plateaus: this measures how much active institutional force is required to hold the constraint in place against reform pressure. The plateau reflects demographic shift—the reform cohort aged out, replaced by priests trained in JP2 seminaries, so less active suppression is needed. Measurement basis switches from 'projected' (the pre-conciliar state) to 'observed' starting 1975, marking the constraint's crystallization.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional magisterium's seat, this constraint represents rational hermeneutics: the Council's texts, read within the living tradition, yield continuity. From the reformers' seat, the same constraint operates as forced reinterpretation—the Magisterium uses continuity claims to void the pastoral mandate they see in the same texts. From the rupture-reading advocates' seat, the constraint is an illegitimate cover-up: it denies what the Council actually said by pretending it said what pre-conciliar doctrine requires. The engine should compute these seats differently: agenda-setter and beneficiaries compute low-to-moderate extraction (they justify and administer); payers compute high extraction (they are constrained and corrected); excluded seats (rupture advocates, composite readers) compute complete targeting. The divergence in computed type per seat is exactly the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional magisterium sits at d ≈ 0.15 (full beneficiary): it collects the interpretive veto, enforces hermeneutics, and defines what the Council meant. Latin traditionalists sit at d ≈ 0.25–0.35 (beneficiaries with some constraint): they benefit from the continuity framing but remain dependent on the Magisterium's willingness to protect their reading. Post-conciliar reformers sit at d ≈ 0.80 (high targets): they cannot exit the constraint (they are baptized Catholics bound by magisterial teaching) and must either comply with the continuity reading or face institutional correction. Vernacular advocates sit at d ≈ 0.75–0.85 (targets): their pastoral autonomy is constrained, they cannot justify their practice by 'spirit' appeals without institutional permission. Rupture advocates sit at d ≈ 0.95 (complete targets): they cannot maintain their reading within the Church's institutional frame without being branded as misreading the Council. Exit options differentiate: traditionalists have 'constrained' exit (they can split to SSPX or leave); reformers have 'identity_locked' exit (leaving the Church requires abandoning their priestly/theological identity); rupture advocates have 'trapped' exit (within the Church, the Magisterium enforces the continuity frame; outside, they lose institutional standing to make their case). Power atoms: institutional (Magisterium, pre-conciliar theologians); organized (reformers, traditionalists); moderate (vernacular advocates, composite readers). These differences drive directionality variance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows incipient mandatrophy (detectable via the theater_ratio plateau and suppression_requirement plateau after 2000). The founding problem—coordinating divergent post-conciliar implementations—was genuine in 1965–1985 when competing visions threatened institutional schism. By 2025, the reform generation has demographically exited the priesthood, replaced by JP2/Benedict XVI seminarians trained in continuity hermeneutics. The constraint persists, but maintenance is increasingly theatrical: the Magisterium continues to police 'spirit of Vatican II' rhetoric and defend Latin retention, but the underlying coordination problem has been solved by generational replacement, not by hermeneutical discipline. The theater_ratio plateau at 0.58 (meaning ~58% of enforcement activity is performative rather than functionally necessary) confirms the piton trajectory. However, mandatrophy is not yet complete: the constraint still extracts (suppresses alternative readings, enforces institutional hermeneutics) and still has active beneficiaries (the Magisterium collecting interpretive authority, traditionalists maintaining liturgical claims). A true piton would show theater_ratio above 0.7 and no concentrated beneficiary—this constraint still has both. The classification should be tangled_rope (genuine coordination function + asymmetric extraction + active enforcement) with piton trajectory and high theater ratio, not yet piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_ambiguity_in_texts,
    'Do Vatican II texts, read on their own merits without appeal to prior magisterial teaching, actually preserve pre-conciliar doctrine or do they authorize genuine changes?',
    'Linguistic/exegetical analysis of the conciliar documents independent of the institutional continuity frame; comparison with official schemas rejected during the Council that would have made pre-conciliar language explicit; analysis of the Fathers'' floor debates and voting patterns to infer intentionality.',
    'If the texts, read plainly, authorize change, the continuity reading is an after-the-fact hermeneutical imposition and the constraint should be reclassified from tangled_rope (genuine coordination + extraction) to snare (extraction disguised as coordination). If the texts genuinely preserve prior doctrine, the continuity reading is structurally honest coordination, not extractive override.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_ambiguity_in_texts, empirical, 'Whether Vatican II texts support continuity hermeneutics or require rupture reading on their surface.').

omega_variable(
    demographic_substitution_vs_hermeneutic_victory,
    'Did the post-conciliar period move toward continuity hermeneutics because the Magisterium''s hermeneutical argument was persuasive and the constraint''s legitimacy was earned, or because the reform-generation clergy retired and were replaced by seminarians trained in continuity from the start?',
    'Cohort analysis comparing acceptance of continuity framing between pre-1965-ordained clergy (socialized into reform expectations) and post-1985-ordained clergy (trained in continuity). Survey data or administrative records showing when compliance shifted. Qualitative interviews with clergy cohorts about their hermeneutical reasoning.',
    'If demographic substitution is the primary mechanism, the constraint''s persistence is partly maintained by cohort turnover rather than hermeneutical persuasion—a sign of piton dynamics (inertial maintenance). If hermeneutical victory is primary, the constraint''s persistence reflects actual agreement on meaning, supporting the tangled_rope classification. High demographic effect would support elevating theater_ratio and potentially reclassifying to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_substitution_vs_hermeneutic_victory, empirical, 'Whether the continuity reading''s acceptance reflects argumentative success or generational replacement.').

omega_variable(
    identity_fusion_in_institutional_seats,
    'For Catholic clergy and theologians who are payers in this constraint, does their acceptance of continuity hermeneutics reflect genuine agreement with the reading or identity-fusion with the institutional magisterium (the continuity frame is inseparable from their priestly identity)?',
    'Post-exit analysis: do clergy who leave the priesthood or the Church''s teaching authority retain continuity hermeneutics, or do they shift toward rupture/composite readings? Do they report their shift as ''discovering the truth'' or as ''recovering my own thinking after institutional pressure''?',
    'If identity-fusion dominates, the suppression value includes internalized constraint—the payers carry the suppression with them even after exit, making the effective suppression higher than the structural measure suggests. This would support higher theater_ratio (performative maintenance of internalized constraints) and potentially piton classification. If agreement dominates, the constraint is genuinely coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_institutional_seats, empirical, 'Whether payer-seat acceptance of continuity represents genuine agreement or identity-lock suppression.').

omega_variable(
    founding_problem_obsolescence,
    'Was the founding problem (coordinating divergent post-conciliar implementations) actually solved by the continuity constraint, or was it solved by demographic attrition and institutional consolidation independent of hermeneutics?',
    'Counterfactual: if the Magisterium had conceded a rupture reading or overdetermination reading in 1975, would the post-conciliar implementation have fragmented catastrophically, or would the Church have reorganized around a different hermeneutic frame without schism?',
    'If the founding problem is genuinely solved by continuity hermeneutics, the constraint is functional coordination. If it is solved by demographic turnover regardless of hermeneutics, the constraint is exhibiting mandatrophy—maintained by inertia rather than by solving the problem it was built for. This would shift classification toward piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether the continuity constraint actually solves the coordination problem or merely coincides with its demographic solution.').

omega_variable(
    hermeneutic_circle_closure,
    'Is the continuity reading self-validating (prior doctrine is the standard for reading Vatican II, so any reading that diverges from prior doctrine is ipso facto a misreading) or falsifiable (there exists a possible state of evidence that would show the continuity reading to be wrong)?',
    'Conceptual: identify what evidence or argument would force the continuity reading to concede that Vatican II breaks with prior doctrine. If no such evidence exists because the hermeneutic frame is logically self-sealing, the reading is unfalsifiable.',
    'If the reading is self-validating/unfalsifiable, it is primarily a hermeneutical device (extractive policing power) rather than a truth-claim. This would support higher theater_ratio, higher extraction classification, and piton trajectory. If falsifiable, it is a substantive claim about meaning, supporting tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hermeneutic_circle_closure, conceptual, 'Whether the continuity reading is open to falsification or constitutively unfalsifiable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2000, 0.54).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2013, 0.57).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__continuity_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 1985, 0.63).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__continuity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2013, 0.72).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__continuity_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__continuity_reading, 0.12).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__continuity_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Vatican II magisterial authority is a contested kernel with three structurally distinct readings: continuity_reading (this constraint) asserts the Council preserves prior doctrine; rupture_reading asserts the Council breaks with prior teaching; composite_overdetermination_reading asserts the Council is ambiguous by design, encoding incompatible visions. Each reading instantiates a different constraint with different beneficiaries, victims, ε values, and classifications. The three stories form a constraint family linked by affects_constraints edges. The continuity reading influences the others by asserting its hermeneutic frame (prior doctrine as benchmark) as the official institutional standard—this creates structural pressure on the rupture and composite readings, constraining how they can be articulated within the Church, even though the readings logically coexist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
