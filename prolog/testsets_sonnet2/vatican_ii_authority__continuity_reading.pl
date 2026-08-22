% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Continuity (Hermeneutic of Continuity)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story authors ONLY the continuity reading of the Vatican II kernel —
 *   the account, articulated most authoritatively by the post-conciliar
 *   teaching office, that the council's sixteen documents represent organic
 *   development of an unchanging deposit of faith rather than a break with
 *   prior magisterial teaching. Under this reading, apparent tensions (e.g.
 *   religious liberty language against prior condemnations of religious
 *   indifferentism) are resolved through traditional hermeneutical tools —
 *   reading texts in their fullest context, distinguishing dogmatic content
 *   from disciplinary or pastoral expression, and giving weight to the
 *   council's own stated continuity with prior councils. The reading is
 *   authored as a coordination mechanism (rope): it lets a global institution
 *   proceed with reform without requiring every actor to re-litigate
 *   legitimacy, and it names no victim because its own internal claim is that
 *   development is cost-free when properly understood. This is deliberately
 *   narrow: the sibling readings (rupture_reading,
 *   composite_overdetermination_reading) author DIFFERENT epsilon values and
 *   DIFFERENT beneficiary/victim structures for the same historical event,
 *   because they read the same texts as accomplishing something structurally
 *   different. Per the epsilon-invariance principle, these are three separate
 *   constraints, linked by network.affects_constraints, not one constraint
 *   with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.18).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.32).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Doctrinal Continuity (Hermeneutic of Continuity)").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '489ecdd6-5856-497b-b323-e50ec32cc9f4').
narrative_ontology:cs_kernel_codification('489ecdd6-5856-497b-b323-e50ec32cc9f4', fixed_text).
narrative_ontology:cs_authority_grounding('489ecdd6-5856-497b-b323-e50ec32cc9f4', lineage).
narrative_ontology:cs_interpretation_layer_present('489ecdd6-5856-497b-b323-e50ec32cc9f4').
narrative_ontology:cs_reading_relation('489ecdd6-5856-497b-b323-e50ec32cc9f4', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('489ecdd6-5856-497b-b323-e50ec32cc9f4', vatican_ii_authority__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('489ecdd6-5856-497b-b323-e50ec32cc9f4', foundational, deposit_of_faith_cannot_materially_change).
narrative_ontology:cs_axiom_status(deposit_of_faith_cannot_materially_change, holdable).
narrative_ontology:cs_axiom_grounding('489ecdd6-5856-497b-b323-e50ec32cc9f4', deposit_of_faith_cannot_materially_change, theological).
narrative_ontology:cs_axiom('489ecdd6-5856-497b-b323-e50ec32cc9f4', foundational, traditional_hermeneutics_suffice_to_resolve_apparent_tension).
narrative_ontology:cs_axiom_status(traditional_hermeneutics_suffice_to_resolve_apparent_tension, holdable).
narrative_ontology:cs_axiom_grounding('489ecdd6-5856-497b-b323-e50ec32cc9f4', traditional_hermeneutics_suffice_to_resolve_apparent_tension, conventional).
narrative_ontology:cs_reference_frame('489ecdd6-5856-497b-b323-e50ec32cc9f4', pre_conciliar_magisterial_settlement).
narrative_ontology:cs_drift_state('489ecdd6-5856-497b-b323-e50ec32cc9f4', post_2005_hermeneutic_of_continuity_address, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('489ecdd6-5856-497b-b323-e50ec32cc9f4', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reform_clergy).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, conciliar_theologians).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, diocesan_bishops_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, vatican_curia_teaching_office).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, ordinary_catholic_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, ordinary_catholic_laity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, deposit_of_faith_doctrinal_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, conciliar_magisterial_authority).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, organic_development_of_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement liturgical, ecumenical, and pastoral reforms (vernacular liturgy, ecumenical dialogue, revised religious liberty teaching) by reading the sixteen conciliar documents as faithful development of prior doctrine. This reading legitimates changes they have already made and shields ongoing pastoral practice from charges of doctrinal rupture.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reform_clergy, beneficiary,
    organized, generational, constrained, global).

% Produce and defend the hermeneutical framework (associated with the 'hermeneutic of reform in continuity' articulated at the highest levels of the Church) that reads ambiguous or novel-sounding passages as continuous with prior magisterial teaching. Their scholarly and ecclesial standing depends on this framework holding; abandoning it would require conceding either rupture or unresolved contradiction, both professionally and spiritually costly.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, conciliar_theologians, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, conciliar_theologians, agenda_setter).

% Administers the ordinary magisterium's interpretive authority over the council's texts, issuing catechisms, encyclicals, and doctrinal clarifications that read Vatican II through a continuity lens. Its institutional legitimacy is bound to the claim that the deposit of faith is unchanging even as its living expression develops.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, vatican_curia_teaching_office, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Hold that specific conciliar passages (religious liberty, ecumenism, collegiality) are irreconcilable with prior binding teaching and that the continuity reading obscures a real break. They are not the constraint this story describes (their view is a sibling reading, rupture_reading), but from inside the continuity reading their objections are treated as failures of hermeneutical charity rather than live evidence of contradiction — they are heard but not credited within this reading's framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_clergy_and_laity, excluded,
    organized, generational, constrained, global).

% Receive vernacular liturgy, encouragement toward Scripture and ecumenical engagement, and a settled institutional account that resolves cognitive dissonance about apparent doctrinal change. Some also bear the cost of navigating parish-level disputes when local clergy split between continuity and rupture readings, but the continuity account itself imposes no direct extraction — it is offered as reassurance rather than collected as a toll.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, ordinary_catholic_laity, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, ordinary_catholic_laity, payer).

% Study the textual history, drafting committees, and reception of the sixteen documents. Some corroborate the continuity reading's textual claims; others (including scholars within the Church) document genuine discontinuities the continuity reading elides. They sit outside the beneficiary set and can attest to the historical record independently of institutional stake.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, academic_historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the Church's global hierarchy, clergy, and laity a single authoritative account of how the sixteen conciliar documents relate to two millennia of prior teaching, allowing pastoral, liturgical, and doctrinal decisions to proceed without every local actor re-litigating the council's legitimacy.
% TRANSFER_FUNCTION: Moves interpretive authority toward the magisterial teaching office and toward theologians and clergy who can articulate the continuity framework persuasively, and moves reassurance and settled practice toward the laity; it does not, on this reading, move material costs from any identifiable victim group — the reading's own claim is that no one pays for organic development.
% ABSENT_VOICES: Traditionalist clergy and laity who read specific passages as ruptures are present in the Church but are not credited within this reading's own hermeneutical apparatus as bearers of a live counter-claim; their objections register as pastoral friction to be managed rather than doctrinal evidence to be weighed. This is the seat occupied by the rupture_reading sibling constraint.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared overnight, the institutional teaching office would need an alternative account of the council's authority; progressive reform practices (vernacular liturgy, ecumenical structures) would lose their doctrinal legitimation and face renewed contest, while traditionalist critics would gain ground. Whether the 'world rearranges' or 'stays the same' is itself contested between the sibling readings — the continuity reading's own adherents hold that the underlying deposit of faith is unaffected either way (world_unchanged from inside the theology), while institutional practice built on the reading would visibly destabilize (world_rearranges from inside the institution). Marked contested because the verdict depends on which layer — dogmatic content or institutional legitimation — is being asked about.
% FOUNDING_PROBLEM: The Second Vatican Council (1962-1965) was convened to address the Church's relationship to modernity, other Christian communions, non-Christian religions, and the vernacular languages of the laity, following a period (arguably since Trent) in which the Church's institutional posture had become defensively fixed. The continuity reading was built to explain how substantial changes in posture (ecumenism, religious liberty, liturgical vernacularization, collegiality) could be issued by a body whose doctrinal authority depends on never contradicting itself.
% FOUNDING_PROBLEM_CORROBORATION: The teaching office itself (notably articulated in a landmark 2005 curial address distinguishing a 'hermeneutic of continuity' from a 'hermeneutic of discontinuity and rupture') attests the founding problem as live and resolved by continuity. Independent historical theologians outside the magisterial beneficiary set — including scholars documenting the drafting history of Dignitatis Humanae and Nostra Aetate — corroborate that real doctrinal movement occurred, but disagree among themselves and with the teaching office about whether that movement is properly described as continuous development or partial rupture; no fully independent corroborating source is described here as endorsing the continuity account without qualification.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at 2025) because, BY THIS READING'S OWN LIGHTS, the reform program does not extract from anyone — it is presented as the Church becoming more fully itself. Suppression is moderate (0.32) because the reading does exercise real interpretive foreclosure: passages read by others as rupture are, within this framework, treated as settled rather than open, and dissenting readings face real institutional friction (canonical discipline against some traditionalist positions, restricted liturgical permissions). Theater ratio is low and rises only slightly (0.10 to 0.15) reflecting that the coordination function (a working, livable interpretive settlement for the overwhelming majority of clergy and laity) remains substantially functional across sixty years, with a small accumulating theatrical component as the settlement becomes more routinely asserted than argued.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive reform clergy, conciliar theologians, and the curial teaching office are authored as beneficiaries: the continuity frame legitimates practices they have implemented or authority they exercise, giving them low d. Ordinary laity are also beneficiaries in the primary sense (reassurance, vernacular access) but carry a secondary payer role where local disputes impose real costs — this is why they carry a dual role rather than a pure beneficiary designation. No group is authored as a pure victim under this reading; that asymmetry (relative to the rupture_reading sibling, which would name traditionalists as victims of imposed doctrinal change) is the central structural fact distinguishing the two readings and is exactly why they must be separate constraint files.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does an authority claiming unchanging deposit of faith accommodate substantial institutional change) is authored as contested rather than resolved or dead — the continuity reading's own proponents hold it live and successfully addressed; outside historical scholarship corroborates real change occurred but does not uniformly agree the change was cost-free or fully continuous. This keeps the story from over-claiming resolution: the classification here (rope, low extraction) is what the continuity reading's own internal logic supports, not an adjudication of whether that logic is correct relative to the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_versus_rupture_locus,
    'Is the disagreement between continuity and rupture readings located in the texts themselves (genuinely ambiguous drafting), in the hermeneutical method applied to settled texts, or in prior doctrine''s own binding status (whether pre-conciliar condemnations were themselves reformable)?',
    'Close textual-historical analysis of conciliar drafting committee records (the acta synodalia) cross-referenced against the specific prior magisterial statements alleged to be in tension, distinguishing drafting compromise language from doctrinal assertion.',
    'If the disagreement is located in genuinely ambiguous drafting, composite_overdetermination_reading is best supported. If located in hermeneutical method alone, continuity_reading''s claim that traditional tools resolve the tension is strengthened. If located in the reformability of prior doctrine itself, rupture_reading''s claim gains force because no hermeneutic can rescue continuity if the premise itself changed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_versus_rupture_locus, conceptual, 'Where the continuity/rupture disagreement structurally sits: text, method, or premise.').

omega_variable(
    authority_grounds_own_verdict,
    'Can the teaching office that both authored the council''s texts and now adjudicates their continuity be treated as a neutral corroborating source for the continuity reading, or does its institutional stake in continuity (avoiding an admission that its own binding teaching changed) disqualify it as independent corroboration?',
    'Compare the teaching office''s continuity verdict against fully independent historical-critical scholarship (including scholarship from within the Church not tied to curial office) on specific contested passages, checking for convergence or divergence.',
    'If independent scholarship converges with the teaching office''s continuity account, the reading''s corroboration is strengthened beyond self-assertion. If independent scholarship diverges, the founding_problem_corroboration for this reading rests substantially on the benefiting party''s own testimony, weakening its epistemic standing relative to the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounds_own_verdict, conceptual, 'Whether the magisterium can corroborate its own continuity claim without circularity.').

omega_variable(
    traditionalist_dissent_credit,
    'Should sustained, organized traditionalist objection to specific conciliar passages be treated as evidence against the continuity reading''s completeness, or as exactly the kind of dissent a legitimate doctrinal development is expected to provoke?',
    'Track whether traditionalist objections concern disciplinary/pastoral matters (which continuity readings can absorb without strain) versus specific dogmatic content (which would require the continuity reading to show, not merely assert, reconciliation with prior binding statements).',
    'If objections are substantially dogmatic rather than disciplinary, the continuity reading''s suppression metric is understated and its accessibility_collapse overstated — real doctrinal alternatives may remain live rather than settled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_dissent_credit, empirical, 'Whether traditionalist dissent is disciplinary friction or unresolved dogmatic contradiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.11).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__continuity_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.17).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__continuity_reading, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(vatican_ii_authority__continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__continuity_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language claim 'Vatican II' per the epsilon-invariance principle. continuity_reading (this file) authors low epsilon (0.18), no victim, and rope classification from its own internal logic. rupture_reading authors substantially higher epsilon and names traditionalist clergy/laity as victims of an imposed doctrinal break. composite_overdetermination_reading authors an epsilon reflecting irreducible structural ambiguity itself, refusing to resolve into either a pure coordination or pure extraction story. The three are linked here and in each sibling's network.affects_constraints array; none is complete without acknowledging the other two as competing readings of the same historical event, not measurements of the same constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
