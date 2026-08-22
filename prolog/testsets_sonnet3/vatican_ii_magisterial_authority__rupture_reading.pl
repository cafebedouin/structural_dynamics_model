% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: vatican_ii_magisterial_authority__rupture_reading
 *   human_readable: Vatican II as Doctrinal Rupture — Progressive/Reformist Reading of Conciliar Authority
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Under the rupture reading, Vatican II's conciliar texts (especially
 *   Dignitatis Humanae on religious freedom, Nostra Aetate on other
 *   religions, and Sacrosanctum Concilium on liturgical reform) are read as
 *   superseding specific prior magisterial positions — most sharply the
 *   pre-conciliar doctrine that error possesses no rights and thus no claim
 *   to public toleration. Proponents treat this supersession as legitimate
 *   doctrinal progress authorized by a valid ecumenical council; the
 *   practical consequence has been sweeping liturgical and disciplinary
 *   reform implemented by post-conciliar episcopal authority, at direct cost
 *   to clergy, religious orders, and laity formed in and attached to
 *   pre-conciliar practice.
 *
 * KEY AGENTS:
 *   - progressive_episcopal_conferences: primary agenda-setter and beneficiary — implements the rupture reading through diocesan and conference-level authority
 *   - traditionalist_clergy and lay_faithful_attached_to_prior_liturgy: primary payers — bear the disciplinary and formational cost of the reading being institutionally enforced
 *   - academic_revisionist_theologians: beneficiary whose professional standing depends on the rupture framing being correct
 *   - vatican_curial_offices: observer/intermittent agenda-setter whose own oscillating rulings (1988, 2007, 2021) are themselves evidence of unresolved kernel contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__rupture_reading, 0.42).
domain_priors:suppression_score(vatican_ii_magisterial_authority__rupture_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__rupture_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__rupture_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__rupture_reading, "Vatican II as Doctrinal Rupture — Progressive/Reformist Reading of Conciliar Authority").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__rupture_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__rupture_reading, '50d9faa9-1fef-47eb-820a-14a04867c784').
narrative_ontology:cs_kernel_codification('50d9faa9-1fef-47eb-820a-14a04867c784', fixed_text).
narrative_ontology:cs_authority_grounding('50d9faa9-1fef-47eb-820a-14a04867c784', extraction).
narrative_ontology:cs_interpretation_layer_present('50d9faa9-1fef-47eb-820a-14a04867c784').
narrative_ontology:cs_reading_relation('50d9faa9-1fef-47eb-820a-14a04867c784', vatican_ii_magisterial_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('50d9faa9-1fef-47eb-820a-14a04867c784', vatican_ii_magisterial_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('50d9faa9-1fef-47eb-820a-14a04867c784', foundational, doctrinal_supersession_is_legitimate_progress).
narrative_ontology:cs_axiom_status(doctrinal_supersession_is_legitimate_progress, holdable).
narrative_ontology:cs_axiom_grounding('50d9faa9-1fef-47eb-820a-14a04867c784', doctrinal_supersession_is_legitimate_progress, conventional).
narrative_ontology:cs_axiom('50d9faa9-1fef-47eb-820a-14a04867c784', foundational, error_has_no_rights_doctrine_is_superseded).
narrative_ontology:cs_axiom_status(error_has_no_rights_doctrine_is_superseded, overridden).
narrative_ontology:cs_axiom_grounding('50d9faa9-1fef-47eb-820a-14a04867c784', error_has_no_rights_doctrine_is_superseded, empirically_contingent).
narrative_ontology:cs_reference_frame('50d9faa9-1fef-47eb-820a-14a04867c784', pre_conciliar_juridical_ecclesiology).
narrative_ontology:cs_drift_state('50d9faa9-1fef-47eb-820a-14a04867c784', post_traditionis_custodes_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('50d9faa9-1fef-47eb-820a-14a04867c784', '').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, liturgical_reform_institutes).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences_post_conciliar).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__rupture_reading, academic_revisionist_theologians).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_religious_orders).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, lay_faithful_attached_to_prior_liturgy).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__rupture_reading, sspx_and_affiliated_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads the conciliar texts as authorizing sweeping reinterpretation of liturgy, ecumenism, and religious liberty, and uses episcopal authority to implement vernacular liturgy, altered sacramental discipline, and revised catechesis. Controls seminary formation and diocesan policy, and characterizes resistance to implementation as disobedience to a valid council.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, progressive_episcopal_conferences, beneficiary).

% Academic and pastoral institutes built after the Council whose funding, publishing output, and institutional identity depend on the rupture reading being correct — that the old rite and its theology were genuinely superseded rather than merely supplemented. Collects grants, appointments, and influence from being the authoritative interpreters of the 'spirit of the Council.'
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, liturgical_reform_institutes, beneficiary,
    organized, biographical, arbitrage, global).

% Gained substantially more doctrinal and disciplinary discretion under a rupture reading of collegiality than they held under the pre-conciliar centralized model. Sets local implementation policy and benefits from reduced Roman oversight of liturgical and pastoral practice.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences_post_conciliar, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, national_bishops_conferences_post_conciliar, agenda_setter).

% Careers, chairs, and publication records are built on treating Dignitatis Humanae and Nostra Aetate as genuine doctrinal reversals rather than developments in continuity. Mobile within academia; not bound to any single diocese or institution.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, academic_revisionist_theologians, beneficiary,
    organized, biographical, mobile, global).

% Ordained under or formed by pre-conciliar norms; find their prior formation, liturgical practice, and doctrinal formulations declared superseded. Face canonical restriction, loss of faculties, or reassignment for continuing pre-conciliar liturgical practice without special permission. Exit means leaving active ministry or affiliating with irregular canonical status.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, traditionalist_clergy, payer,
    moderate, biographical, constrained, national).

% Orders whose charism, habit, rule, and communal life were built around pre-conciliar theology and discipline. Experience mandated renewal (aggiornamento) as institutional dissolution — many communities lost membership, identity coherence, or existence entirely under the rupture implementation. Cannot easily exit the institutional Church while preserving their founding charism unaltered.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, pre_conciliar_religious_orders, payer,
    moderate, generational, constrained, global).

% Ordinary Catholics whose sacramental and devotional life was formed by the pre-conciliar rite and catechism. Under the rupture reading, that formation is treated as obsolete; access to the prior liturgy is restricted or requires special dispensation. Exit options are limited to irregular chapels, sedevacantism, or acceptance of loss without institutional standing to object.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, lay_faithful_attached_to_prior_liturgy, payer,
    powerless, biographical, trapped, local).

% Formally organized resistance to the rupture reading's practical consequences; regarded by proponents of rupture as having chosen schism rather than accept doctrinal development. Bears canonical irregularity (historically including excommunication of bishops) as the direct cost of rejecting the rupture implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, sspx_and_affiliated_communities, payer,
    moderate, generational, trapped, global).

% Adjudicates disputes between rupture and continuity readings through documents like the 1988 motu proprio and later Summorum Pontificum / Traditionis Custodes. Its own position has oscillated, which is itself evidence for how contested the kernel is at the level that would need to resolve it.
narrative_ontology:constraint_stakeholder(vatican_ii_magisterial_authority__rupture_reading, vatican_curial_offices, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_magisterial_authority__rupture_reading, vatican_curial_offices, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_magisterial_authority__rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_magisterial_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the Council coordinates the Church's engagement with modernity — ecumenism, religious liberty, vernacular liturgy — by authorizing a genuine reinterpretation that lets a global institution function credibly in a pluralist, post-colonial world it could not function in under the prior framework.
% TRANSFER_FUNCTION: Moves interpretive and disciplinary authority from Rome-centered pre-conciliar magisterium and traditional religious orders toward national episcopal conferences, academic theological guilds, and liturgical reform bodies; moves standing and institutional legitimacy away from clergy, orders, and laity formed in pre-conciliar practice toward those who adopted the new implementation.
% ABSENT_VOICES: Traditionalist clergy and lay faithful who regard the conciliar texts as compatible with prior teaching are present in the debate but are not the ones who set diocesan or seminary policy; their continuity reading is treated by rupture-reading authorities as a minority position to be managed rather than a live alternative reading of the same texts.
% DISAPPEARANCE_RATIONALE: Proponents of the rupture reading hold that if the reading disappeared, decades of liturgical, ecumenical, and doctrinal development would be delegitimized and reversed, forcing renewed conflict over what the Council actually authorized. Opponents (continuity-reading holders) argue nothing structural would change because the underlying texts remain and only the interpretive gloss would vanish. The dispute over what would happen is itself part of the kernel contest.
% FOUNDING_PROBLEM: The Council was convened to address the Church's relationship to modernity, other Christian communities, other religions, and religious liberty in pluralist states, after centuries in which prior teaching (e.g., error has no rights, extra ecclesiam nulla salus in its narrower formulations) had become pastorally and diplomatically untenable in much of the world.
% FOUNDING_PROBLEM_CORROBORATION: Progressive episcopal conferences and academic revisionist theologians (benefiting parties) attest the founding problem required rupture-level reinterpretation. Independent historians of the Council (e.g., studies of the conciliar debates and minority reports) and traditionalist canon lawyers, both outside the benefiting coalition, attest that many council fathers explicitly denied intending doctrinal rupture, which is direct outside corroboration that the founding-problem-required-rupture claim is not self-evidently settled even among those present at the Council.
narrative_ontology:disappearance_verdict(vatican_ii_magisterial_authority__rupture_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_magisterial_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_magisterial_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_magisterial_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_magisterial_authority__rupture_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__rupture_reading_tests).
:- end_tests(vatican_ii_magisterial_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 rather than high: the rupture reading does solve a real coordination problem (the Church's credible functioning in a pluralist world) even as it redistributes standing away from pre-conciliar communities. Suppression is authored higher (0.55) and reflects the disciplinary machinery used against traditionalist practice (restricted faculties, canonical irregularity, the historical 1988 excommunications) — this is a raw structural property, not scaled by scope. Theater ratio is moderate (0.3): the reform program has real functional content (vernacular liturgy did expand comprehension and participation for many) alongside a documented layer of performative aggiornamento (renewal programs that dissolved orders without replacing their function) that inflates the theater component over time.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (progressive episcopal conferences), the arrangement reads as legitimate doctrinal development they are simply implementing. From the payer seats (traditionalist clergy, attached laity), the identical texts and disciplinary machinery read as an imposed rupture that cost them their formation and community without their consent to the reinterpretation. The engine computes these as different seat-level types from the same structural data; this divergence is exactly what a kernel-reading story is meant to expose, not resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive episcopal conferences, liturgical reform institutes, national conferences, and revisionist academics are declared beneficiaries because the rupture reading is the structural source of their institutional authority, funding, and professional standing (low d, near the beneficiary end). Traditionalist clergy, pre-conciliar orders, attached laity, and SSPX-affiliated communities are declared victims because the same reading, once implemented with enforcement, directly cost them canonical standing, community continuity, or liturgical access (high d, near the target end) — laity in particular sit near full-target given powerless/trapped positioning with no institutional recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) is deliberate: the rupture reading is authored as having a genuine coordination function (real pastoral and diplomatic problems the pre-conciliar framework could not solve) coexisting with asymmetric extraction (traditionalist communities bear disproportionate disciplinary cost while progressive institutions capture the resulting authority). Calling it a pure snare would deny the coordination function proponents sincerely claim; calling it a pure rope would erase the documented victims. The founding_problem/disappearance_verdict mismatch check applies here: founding_problem_status is authored contested rather than dead, so no zombie/capture flag should fire from that channel alone — the extraction is live-function-coupled, not vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_vs_continuity_textual_warrant,
    'Do the conciliar texts themselves (as opposed to subsequent implementation) actually assert doctrinal rupture, or is the rupture located in post-conciliar implementation and commentary rather than in the texts?',
    'Close textual-historical analysis of the conciliar debates (acta synodalia), the relatio for Dignitatis Humanae, and the explicit statements of council fathers (including minority reports) regarding intended continuity or discontinuity with prior magisterium.',
    'If the rupture is located in implementation rather than in the texts, this constraint''s claimed_type may better describe the implementation apparatus than the Council itself, and the composite_overdetermination_reading becomes the more defensible account of the texts proper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_vs_continuity_textual_warrant, conceptual, 'Whether doctrinal rupture is textually warranted or an artifact of post-conciliar implementation.').

omega_variable(
    kernel_committer_location,
    'This constraint is one reading (rupture_reading) of the contested kernel vatican_ii_magisterial_authority. The sibling readings are continuity_reading (organic development, no rupture with prior magisterium) and composite_overdetermination_reading (ambiguous compromise texts encoding incompatible visions). Where exactly do the three readings locate their disagreement?',
    'The disagreement is located specifically at: (1) whether Dignitatis Humanae''s religious-liberty teaching is compatible with prior teaching that error has no civil right to propagation, (2) whether the shift from a juridical to a communio ecclesiology is development or replacement, and (3) whether ambiguous compromise language in conciliar texts should be read univocally (rupture or continuity) or as genuinely overdetermined. A sibling reading would change the beneficiary/victim structure entirely: continuity_reading would deny that traditionalist communities are victims of anything but misimplementation, and composite_overdetermination_reading would spread beneficiary/victim status across factions depending on which textual strand is emphasized.',
    'Adopting the continuity_reading in place of this one would collapse most of the declared victim set (no supersession occurred, so no cost was imposed by doctrine itself, only by discretionary implementation) and could reclassify large parts of this constraint as scaffold or rope. Adopting composite_overdetermination_reading would fragment the single ε into multiple contested sub-claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_location, conceptual, 'Location of the rupture/continuity/composite disagreement within the kernel.').

omega_variable(
    religious_freedom_reversal_status,
    'Is Dignitatis Humanae''s teaching on religious freedom a genuine doctrinal reversal (as this reading holds) or a development that clarifies rather than contradicts prior teaching on error and toleration?',
    'Magisterial self-interpretation across subsequent pontificates (particularly whether later popes describe DH as continuous or as correction), combined with theological analysis of whether ''religious freedom'' in DH addresses the same formal object as the condemned propositions of Quanta Cura/Syllabus.',
    'If a later, sufficiently authoritative magisterial act definitively characterizes DH as continuous development, the rupture_reading''s foundational axiom becomes harder to sustain as the dominant institutional self-understanding, shifting relative weight toward continuity_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(religious_freedom_reversal_status, conceptual, 'Whether DH''s religious liberty teaching is a reversal or a clarification of prior doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__rupture_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1962, 0.1).
narrative_ontology:measurement(vati_tr_t1970, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 1985, 0.24).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2013, 0.29).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_magisterial_authority__rupture_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1962, 0.15).
narrative_ontology:measurement(vati_be_t1970, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2013, 0.41).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_magisterial_authority__rupture_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(vati_su_t1970, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_magisterial_authority__rupture_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_magisterial_authority__rupture_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__rupture_reading, vatican_ii_magisterial_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the single kernel vatican_ii_magisterial_authority. continuity_reading denies any rupture occurred and would show near-zero declared victims; composite_overdetermination_reading treats the texts as encoding multiple incompatible visions simultaneously and distributes beneficiary/victim status across textual strands rather than across a single rupture/continuity axis. Each reading carries its own stable ε per the ε-invariance principle; they are linked here for contamination/network analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
