% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Overdetermined Composite — Structural Ambiguity Reading
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   Vatican II (1962-1965) produced sixteen documents through a drafting
 *   process involving sharply divided theological schools within the
 *   episcopate and periti. This reading holds that specific passages in texts
 *   like Lumen Gentium (on collegiality vs. papal primacy), Dignitatis
 *   Humanae (on religious liberty vs. prior condemnations), and Nostra Aetate
 *   (on the status of non-Christian religions) contain language inserted by
 *   opposing factions during floor debate and never theologically reconciled,
 *   producing documents that are read coherently by continuity advocates,
 *   incoherently-and-erroneously by rupture advocates, and as genuinely
 *   internally contradictory by this reading. The magisterium's ongoing need
 *   to assert a single authoritative interpretation collides with this
 *   reading's claim that no such single interpretation is textually
 *   available.
 *
 * KEY AGENTS:
 *   - magisterial_interpretive_authority: institutional victim — must produce univocal doctrine from a text this reading holds cannot yield one
 *   - academic_conciliar_historians: beneficiary — archival work on drafting compromises gains interpretive priority
 *   - hermeneutic_pluralist_theologians: beneficiary — theological pluralism gains textual-genealogical support
 *   - parish_clergy_seeking_settled_teaching: powerless payer — bears the pastoral cost of unresolved ambiguity
 *   - lay_catechists: powerless payer — bears reputational risk teaching contested material
 *   - conciliar_periti_and_drafting_committees: historical agenda_setter — generated the compromise text structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II as Overdetermined Composite — Structural Ambiguity Reading").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'd530ec44-b72c-49c1-8108-049dc760c38c').
narrative_ontology:cs_kernel_codification('d530ec44-b72c-49c1-8108-049dc760c38c', fixed_text).
narrative_ontology:cs_authority_grounding('d530ec44-b72c-49c1-8108-049dc760c38c', extraction).
narrative_ontology:cs_interpretation_layer_present('d530ec44-b72c-49c1-8108-049dc760c38c').
narrative_ontology:cs_reading_relation('d530ec44-b72c-49c1-8108-049dc760c38c', vatican_ii_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('d530ec44-b72c-49c1-8108-049dc760c38c', vatican_ii_authority__rupture_reading, influences).
narrative_ontology:cs_axiom('d530ec44-b72c-49c1-8108-049dc760c38c', foundational, textual_contradiction_is_structural_not_hermeneutic).
narrative_ontology:cs_axiom_status(textual_contradiction_is_structural_not_hermeneutic, holdable).
narrative_ontology:cs_axiom_grounding('d530ec44-b72c-49c1-8108-049dc760c38c', textual_contradiction_is_structural_not_hermeneutic, empirically_contingent).
narrative_ontology:cs_axiom('d530ec44-b72c-49c1-8108-049dc760c38c', secondary, univocal_magisterial_interpretation_is_unavailable_in_principle).
narrative_ontology:cs_axiom_status(univocal_magisterial_interpretation_is_unavailable_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('d530ec44-b72c-49c1-8108-049dc760c38c', univocal_magisterial_interpretation_is_unavailable_in_principle, conventional).
narrative_ontology:cs_reference_frame('d530ec44-b72c-49c1-8108-049dc760c38c', conciliar_compromise_drafting_process).
narrative_ontology:cs_drift_state('d530ec44-b72c-49c1-8108-049dc760c38c', post_synod_on_synodality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d530ec44-b72c-49c1-8108-049dc760c38c', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_historians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_authority).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_settled_teaching).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, lay_catechists).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, factional_compromise_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, irreducible_textual_polysemy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must issue a single authoritative reading of the council's documents to govern doctrine, liturgy, and discipline worldwide. The composite-overdetermination reading holds that the texts themselves contain genuinely incompatible theological rationales stitched together by conciliar committees under time pressure and factional bargaining. Every attempt at univocal interpretation (whether continuity-framed or rupture-framed) is read by this account as papering over contradictions that cannot be resolved by hermeneutics alone, only suppressed by institutional fiat. The authority cannot simply admit the ambiguity is structural without undermining its own claim to definitively settle doctrine.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_interpretive_authority, payer,
    institutional, civilizational, trapped, global).

% Study the drafting history, committee votes, and floor debates of the council's sixteen documents. The composite reading vindicates decades of archival work showing that documents like Lumen Gentium and Gaudium et Spes contain sentences added by opposing factions as compromise language, never theologically reconciled. This reading gives their scholarship interpretive priority over both loyalist and traditionalist univocal readings, and it does not require them to resolve the contradictions — only to document them.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_historians, beneficiary,
    moderate, generational, mobile, global).

% Argue for multiple simultaneous legitimate readings of conciliar texts rather than a single official hermeneutic. The composite reading supports their position by supplying a textual-genealogical basis for pluralism: the ambiguity is not a defect to be interpreted away but a structural feature that any single hermeneutic (continuity or rupture) necessarily distorts. This elevates their theological method over the magisterium's insistence on hermeneutical singularity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians, beneficiary,
    moderate, generational, mobile, global).

% Must preach, catechize, and administer sacraments week to week using conciliar teaching on religious liberty, collegiality, ecumenism, and liturgy as settled ground. The composite-overdetermination account tells them the ground is not settled and cannot be settled — the documents encode live contradictions from the floor of the council itself. This leaves them exposed to contradictory diocesan guidance and to congregants who cite competing 'true' readings against them, with no higher resolution available except administrative silencing of the debate.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_settled_teaching, payer,
    powerless, biographical, constrained, local).

% Teach doctrine to converts and children using materials that assume one coherent post-conciliar teaching. When confronted with the composite reading's claim that the underlying texts are genuinely self-contradictory, they have no professional standing to adjudicate and must simply pick a side or avoid the contested material, bearing personal reputational risk when their choice is later challenged by clergy holding the opposite faction's reading.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, lay_catechists, payer,
    powerless, biographical, constrained, local).

% Both factions hold univocal readings (rupture and continuity respectively) and have organized movements, publications, and in some cases separate institutions around those readings. The composite reading treats both as partially correct about the presence of real contradiction but wrong to think their own side represents the 'true' council. Neither faction is invited to concede that its reading is only one determinate resolution of an irreducibly plural text — the composite reading's claim is structurally uncongenial to both and they are not represented as parties who accept it.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_and_progressive_factions, excluded,
    organized, generational, constrained, global).

% The theologians and bishops who drafted and voted on the sixteen documents in 1962-1965, operating under real time pressure, factional bargaining, and last-minute compromise amendments (modi). The composite reading holds that this drafting process itself is the origin of the structural ambiguity: incompatible theological schools (nouvelle theologie, neo-scholasticism, various national episcopal blocs) each secured textual concessions that were never harmonized into one coherent theological system. This agent-set is historical and no longer active, but its compromises are the generative structure every subsequent reading must contend with.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_periti_and_drafting_committees, agenda_setter,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The council itself solved a real coordination problem: producing a single set of texts that a global episcopate with sharply divided theological commitments (ressourcement reformers, neo-scholastic conservatives, missionary-church bishops, curial officials) could all vote to approve, avoiding schism at the moment of promulgation.
% TRANSFER_FUNCTION: The arrangement transfers interpretive authority away from any settled, checkable theological content and toward whichever institutional or scholarly body currently controls the reading of the ambiguous text — moving practical doctrinal certainty from parish-level clergy and laity (who need settled answers) to historians, theologians, and the magisterium (who can indefinitely defer resolution while each claims their reading is the correct one).
% ABSENT_VOICES: The original conciliar minority who lost specific textual battles (e.g., bishops who wanted stronger condemnation language on religious liberty, or those who wanted more decisive collegiality language) are not present to explain what they believe their compromise concessions actually meant; their intent is reconstructed from committee records rather than testified directly, and the composite reading's claim that the compromises are irreconcilable rather than merely under-specified cannot be checked against them.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination reading disappeared as an accepted account, the magisterium's continuity framing and traditionalist rupture framing would each claim uncontested field, and academic historians would lose the interpretive leverage the ambiguity thesis currently grants them — but whether the underlying pastoral and doctrinal conflicts of the post-conciliar Church would actually diminish is disputed: the composite reading claims the conflicts are generated by real textual contradiction and would persist regardless of which reading is institutionally endorsed, while both rival readings claim the conflicts are caused by misreading and would resolve if their own reading prevailed.
% FOUNDING_PROBLEM: The council was convened to update the Church's engagement with the modern world (aggiornamento) while preserving doctrinal continuity, requiring compromise texts acceptable to theologically opposed episcopal blocs; the composite reading holds that this compromise process, rather than any single theological program, is what actually generated the documents' final language.
% FOUNDING_PROBLEM_CORROBORATION: Independent conciliar historians outside both the magisterial and factional camps (drawing on published council diaries, the Acta Synodalia, and cross-referenced periti correspondence) corroborate that specific passages were the product of last-minute compromise amendments rather than unified theological drafting — this is documented apart from either the continuity or rupture camps' institutional interests, though those same historians disagree among themselves about whether the resulting ambiguity is fully irreconcilable or merely under-theorized.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58, moderate-high: the composite reading extracts institutional deference away from settled magisterial teaching and redirects interpretive authority toward historians and pluralist theologians, at real cost to clergy and catechists who need actionable answers. Suppression (0.62) reflects the magisterium's active institutional effort to foreclose the composite reading in official statements (e.g., repeated assertions of a 'hermeneutic of continuity') — suppression here is the raw structural fact of that foreclosure effort, not scaled by scope. Theater ratio (0.47) captures that a substantial share of ongoing synods, commissions, and interpretive documents function to perform resolution of the ambiguity rather than resolve it, since (on this reading) it is textually unresolvable. Accessibility collapse is only moderate (0.4) because the composite reading, unlike a mountain, does not foreclose alternative readings — continuity and rupture readings remain fully live and organized. Resistance is high (0.72): both the magisterium and the two rival factions actively resist the composite framing because it undercuts each of their claims to univocal authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (academic historians, pluralist theologians) sit near the low-d end: the composite reading is a resource their work produces and depends on, and they bear little institutional cost for holding it. Victims (magisterial authority, parish clergy, lay catechists) sit near the high-d end but for different structural reasons: the magisterium is targeted at the institutional level (its central legitimacy claim is contested) while parish clergy and catechists are targeted at the operational level (they must act daily without the settled ground the magisterium claims to provide). The magisterium's exit option is 'trapped' rather than merely 'constrained' because it cannot simply cede the interpretive question without undermining its own authority structure — unlike clergy, who can at least locally choose which faction's reading to preach, the magisterium cannot publicly adopt 'the text is genuinely contradictory' without conceding the premise its office depends on.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — producing conciliar texts that a divided episcopate could jointly ratify without schism in 1965 — was live and urgent at the time of drafting. Whether it remains live is exactly the contested question this reading raises: the composite reading holds the compromise-generated ambiguity was never resolved, only deferred, and that the post-conciliar conflicts of the following six decades (liturgy wars, disputes over religious liberty, ecumenism, collegiality vs. primacy) are the structural residue of that deferral rather than accidents of implementation or bad-faith misreading by either faction. This blocks a mandatrophy analysis that would simply say 'the founding problem is solved, the ambiguity is vestigial theater' — on this reading the ambiguity is not vestigial, it is load-bearing, and any single authoritative resolution (continuity or rupture) would require overriding actual textual content rather than merely restating settled doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreconcilability_vs_underdetermination,
    'Are the identified textual ambiguities genuinely irreconcilable (two incompatible theological claims asserted in the same authoritative text) or merely underdetermined (compatible claims stated with insufficient precision to rule out multiple readings)?',
    'Systematic formal analysis of the disputed passages (e.g., Lumen Gentium 22 on collegiality, Dignitatis Humanae''s relation to prior magisterial statements on religious liberty) against a strict logical-contradiction criterion, cross-checked against periti drafting notes to establish authorial intent at each amendment stage.',
    'If irreconcilable, the composite reading''s claim that no univocal interpretation is available is vindicated and the magisterium''s continuity claims are structurally false summits. If merely underdetermined, the ambiguity is resolvable in principle by further authoritative clarification, and the composite reading''s extraction claim against the magisterium weakens substantially — it would then be one contested interpretive strategy among several rather than a description of a structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreconcilability_vs_underdetermination, conceptual, 'Whether the conciliar textual ambiguities are strictly contradictory or merely underspecified.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (composite_overdetermination_reading) of the vatican_ii_authority kernel; the continuity_reading and rupture_reading are sibling constraints, not alternative measurements of this one. What would each sibling reading change structurally, and where precisely does the disagreement sit?',
    'No empirical resolution mechanism exists at the level of the kernel itself — the disagreement is located in whether the conciliar documents'' compromise language constitutes (a) organic development within one coherent theological trajectory (continuity_reading), (b) a genuine break introducing doctrinal error (rupture_reading), or (c) an unresolved composite of both trajectories simultaneously present in the text (this reading). Each reading would authorize different beneficiary/victim structures: continuity_reading benefits the magisterium and burdens no clear victim class; rupture_reading benefits traditionalist factions and burdens the post-conciliar magisterium''s legitimacy; this reading benefits historians/pluralists and burdens the magisterium''s claim to univocal authority.',
    'Adopting a different reading would not change ε for this story (each reading''s ε is authored independently per the ε-invariance principle) but would determine which constraint file governs a given real-world dispute — e.g., a Roman dicastery document asserting hermeneutic-of-continuity is best modeled against continuity_reading, not this file.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Documents the committer structure: this story is one of three sibling readings of a single contested kernel, disagreement located in the nature (organic/ruptural/compromise-composite) of the textual ambiguity.').

omega_variable(
    scholarly_beneficiary_extraction_ambiguity,
    'Do academic historians and pluralist theologians genuinely benefit in an extractive sense from the composite reading, or does their work simply describe a pre-existing textual fact without capturing any transferred value?',
    'Assess whether institutional positions, grant funding, and publication prestige accrue specifically to scholars advancing the irreconcilability thesis at a rate exceeding what would be expected from neutral historical description, versus scholars who reach continuity or rupture conclusions using comparable archival methods.',
    'If the beneficiary designation reflects genuine rent-capture (career advantage tied to maintaining rather than resolving ambiguity), the tangled_rope classification is well-supported. If scholars would gain equally from either resolving or maintaining the ambiguity, the beneficiary declaration may overstate extraction and the constraint sits closer to a rope with contested but non-extractive coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_beneficiary_extraction_ambiguity, empirical, 'Whether scholarly beneficiaries of the ambiguity thesis capture rent from its persistence or merely describe it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(vati_tr_t60, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 60, 0.47).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vati_be_t10, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(vati_be_t20, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(vati_be_t30, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(vati_be_t40, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(vati_be_t50, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(vati_be_t60, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vati_su_t10, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(vati_su_t20, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(vati_su_t30, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(vati_su_t40, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(vati_su_t50, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(vati_su_t60, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vatican_ii_authority kernel (composite_overdetermination_reading), alongside continuity_reading and rupture_reading. Each reading is authored as a separate, ε-invariant constraint with its own beneficiary/victim structure per the ε-invariance principle: continuity_reading places ε low (benign organic development, magisterium as coordinated beneficiary with minimal identified victim); rupture_reading places ε high with a different victim set (post-conciliar magisterium's legitimacy, doctrinal orthodoxy) and different beneficiaries (traditionalist institutions); this reading places ε at a moderate-high 0.58 with historians/pluralists as beneficiaries and both institutional authority and grassroots clergy/catechists as victims. All three readings are linked bidirectionally via affects_constraints since each reading's institutional and scholarly currency structurally affects the resource availability and legitimacy conditions of the other two — e.g. a Roman document reinforcing the continuity reading directly reduces the felt urgency and academic funding available to the composite reading's proponents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
