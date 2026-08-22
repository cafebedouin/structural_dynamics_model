% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II as Rupture: Council Invalid/Gravely Defective Reading
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the contested Vatican II
 *   authority kernel: the position that the Council's documents contain
 *   doctrinal errors or irreconcilable contradictions with prior binding
 *   teaching, rendering the Council invalid or gravely defective in whole or
 *   in part. This is the reading structurally instantiated by the SSPX and
 *   aligned traditionalist societies. It is authored here as ONE clean,
 *   ε-invariant constraint — not as an average across the kernel's readings
 *   and not hedged against the continuity or composite-overdetermination
 *   readings, which are separate constraint files. The 1988 spike in
 *   suppression_requirement models the Ecclesia Dei crisis (Archbishop
 *   Lefebvre's episcopal consecrations without papal mandate and the
 *   resulting excommunications), after which enforcement partially relaxed
 *   (indult communities, later Summorum Pontificum in 2007, then partial
 *   re-tightening under Traditionis Custodes in 2021 — reflected in the 2025
 *   uptick).
 *
 * KEY AGENTS:
 *   - post_conciliar_curial_administrators: institutional agenda-setter who benefits from the Council's authority being unquestioned
 *   - modernist_theological_faction: organized beneficiary whose academic and pastoral standing rests on conciliar legitimacy
 *   - traditional_catholic_laity: powerless payer bearing catechetical and sacramental discontinuity costs
 *   - traditionalist_clergy_orders: moderate-power payer facing canonical irregularity for holding this reading
 *   - doctrinal_stability_claimants: payer bearing the cost of pressing the non-contradiction claim within official structures
 *   - continuity_reading_magisterium: excluded from this reading's own frame as the party to be refuted
 *   - ecclesiastical_historians: analytical observer documenting textual and reception evidence independent of either camp
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Rupture: Council Invalid/Gravely Defective Reading").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'da56126d-1212-407b-88d1-04eaed7ea8a0').
narrative_ontology:cs_kernel_codification('da56126d-1212-407b-88d1-04eaed7ea8a0', formalized).
narrative_ontology:cs_authority_grounding('da56126d-1212-407b-88d1-04eaed7ea8a0', lineage).
narrative_ontology:cs_interpretation_layer_present('da56126d-1212-407b-88d1-04eaed7ea8a0').
narrative_ontology:cs_reading_relation('da56126d-1212-407b-88d1-04eaed7ea8a0', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('da56126d-1212-407b-88d1-04eaed7ea8a0', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('da56126d-1212-407b-88d1-04eaed7ea8a0', foundational, prior_dogmatic_formulations_are_irreformable).
narrative_ontology:cs_axiom_status(prior_dogmatic_formulations_are_irreformable, holdable).
narrative_ontology:cs_axiom_grounding('da56126d-1212-407b-88d1-04eaed7ea8a0', prior_dogmatic_formulations_are_irreformable, deontological).
narrative_ontology:cs_axiom('da56126d-1212-407b-88d1-04eaed7ea8a0', foundational, conciliar_documents_contradicting_prior_anathemas_are_invalid).
narrative_ontology:cs_axiom_status(conciliar_documents_contradicting_prior_anathemas_are_invalid, holdable).
narrative_ontology:cs_axiom_grounding('da56126d-1212-407b-88d1-04eaed7ea8a0', conciliar_documents_contradicting_prior_anathemas_are_invalid, conventional).
narrative_ontology:cs_reference_frame('da56126d-1212-407b-88d1-04eaed7ea8a0', pre_conciliar_tridentine_magisterium).
narrative_ontology:cs_drift_state('da56126d-1212-407b-88d1-04eaed7ea8a0', post_traditionis_custodes_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('da56126d-1212-407b-88d1-04eaed7ea8a0', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_theological_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_curial_administrators).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy_orders).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_stability_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the post-1965 magisterial apparatus, promulgate the Novus Ordo liturgy and revised catechetical materials, and require priests and bishops to operate within the conciliar framework as a condition of canonical standing. From the rupture reading's vantage, this faction consolidated authority precisely by declaring the break irreversible and treating dissent as schismatic rather than as legitimate theological objection.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_curial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, post_conciliar_curial_administrators, beneficiary).

% Academic theologians and diocesan reformers whose careers, publications, and institutional posts (seminary chairs, episcopal conferences, ecumenical bodies) depend on Vatican II's documents being read as authoritative development. They gained platform, funding, and doctrinal legitimacy from the conciliar turn toward ecumenism, religious liberty, and collegiality.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_theological_faction, beneficiary,
    organized, generational, mobile, global).

% Ordinary Catholics formed in pre-conciliar catechesis who experienced the liturgical and doctrinal changes as a rupture with what they were taught was unchangeable. Many report loss of catechetical continuity, confusion over binding teaching, and diminished access to the older liturgical forms; their recourse is largely limited to seeking out traditionalist parishes or societies, at real social and sacramental cost.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_laity, payer,
    powerless, biographical, constrained, global).

% Priests, religious orders, and societies (exemplified structurally by the SSPX) who hold the rupture reading and face canonical irregularity, suppressed faculties, or outright excommunication risk for refusing full assent to conciliar documents. Their exit options are narrow: operate in a canonically irregular status, seek Vatican-negotiated exceptions (personal ordinariates, indult communities), or remain formally outside communion.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy_orders, payer,
    moderate, generational, trapped, global).

% Theologians and canonists who hold that the Church's charism of indefectibility requires doctrinal non-contradiction across councils. They bear the cost of having to explain apparent discontinuities (religious liberty, ecumenism, collegiality) against prior anathemas, and are institutionally marginalized when they press the contradiction claim within official structures.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_stability_claimants, payer,
    moderate, civilizational, constrained, universal).

% The papal and curial voices (from Benedict XVI's 'hermeneutic of continuity' framing onward) who hold that no rupture occurred and who would directly contest this reading's premise. They are excluded from this constraint's own frame by definition — the rupture reading treats their continuity claim as the thing to be refuted, not a live alternative within this reading's own logic.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, continuity_reading_magisterium, excluded,
    institutional, civilizational, analytical, universal).

% Scholars who study conciliar texts, redaction history, and reception without institutional stake in either outcome. They document the textual ambiguities, drafting compromises, and reception patterns that different readings mobilize as evidence.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The rupture reading coordinates a traditionalist identity and practice around a shared diagnosis: that the Council's documents (especially on religious liberty, ecumenism, and collegiality) cannot be reconciled with prior binding teaching, and that fidelity to the pre-conciliar deposit requires resisting or rejecting the Council's authority in whole or in part.
% TRANSFER_FUNCTION: Moves doctrinal authority and institutional legitimacy away from the post-conciliar magisterium's ordinary teaching organs toward pre-1962 magisterial sources and toward the traditionalist societies/clergy who claim fidelity to them; correspondingly moves canonical standing, sacramental access, and institutional resources away from traditionalist clergy who act on the reading.
% ABSENT_VOICES: The continuity-reading magisterium (post-conciliar popes and their doctrinal congregations) is treated within this reading's own logic as the party to be refuted rather than heard as an equal interlocutor; the composite/overdetermination reading, which would deny that a single verdict is even available, is likewise not represented inside this reading's frame.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished as a live position, the SSPX and aligned traditionalist societies would lose their primary doctrinal justification for canonically irregular status, traditionalist Catholic identity formation would lose its organizing grievance, and pressure on Rome to negotiate personal ordinariates or indult arrangements would collapse. Conversely, if the reading became formally adjudicated as correct, the post-conciliar magisterium's authority claims for the intervening six decades would be structurally undermined.
% FOUNDING_PROBLEM: The reading was built to explain a perceived experiential and textual discontinuity: pre-conciliar Catholics who were taught specific propositions as unchangeable (e.g., on religious liberty, extra ecclesiam nulla salus, the nature of the Mass) encountered post-conciliar teaching and practice that appeared to contradict those propositions, and the reading supplies a framework — invalidity or grave defect — for holding the earlier teaching as authoritative over the later.
% FOUNDING_PROBLEM_CORROBORATION: Traditionalist clergy and lay commentators (the reading's own adherents) attest the discontinuity is real and unresolved. Independent corroboration comes partially from continuity-reading theologians themselves, who concede specific texts (Dignitatis Humanae on religious liberty, Nostra Aetate on other religions) required careful harmonization work precisely because the surface tension is real, even though they resolve it differently; academic historians of the Council (e.g., studies of the minority bloc's interventions and the final vote margins) corroborate that substantial contemporaneous opposition existed among bishops who are not traditionalist partisans, which supports the claim that the tension is not purely a product of post-conciliar polemics.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 for this reading's own referent — the standing post-conciliar arrangement as the rupture reading sees it: an arrangement that transferred doctrinal authority, institutional resources, and canonical legitimacy toward the modernist faction and curial administrators while imposing real costs (canonical irregularity, sacramental marginalization, catechetical rupture) on those who hold the traditional teaching as binding. This is NOT the extraction the reading would attribute to its own endorsed alternative (a restored pre-conciliar magisterium), which would register near zero by the reading's own lights — per the fixed ε-referent rule, ε describes the arrangement under contest, not the reading's preferred replacement. Suppression is high (0.72) because the mechanism by which this reading is kept from full institutional legitimacy is canonical (excommunication risk, suppressed faculties, loss of parish access) rather than merely rhetorical. Theater ratio (0.42) reflects that a meaningful share of post-conciliar enforcement activity is now maintaining institutional face (declaring the matter settled) rather than substantively engaging the doctrinal-contradiction claims on their textual merits.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (post-conciliar curial administrators), this reading appears as schismatic refusal to accept a validly exercised ecumenical council — a payer-side complaint dressed as doctrine. From the payer seats (traditionalist clergy and laity), the same structure appears as coerced acceptance of teaching that contradicts what they were bound to hold as unchangeable, enforced through canonical penalty rather than persuasion. The engine computes these as structurally different experiences of the same declared data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Modernist theological faction and curial administrators are declared beneficiaries: they hold institutional posts, publication platforms, and unquestioned doctrinal authority contingent on the Council's legitimacy being unchallenged — low d, benefit-side. Traditional Catholic laity, traditionalist clergy orders, and doctrinal stability claimants are declared victims: they bear catechetical rupture, canonical irregularity risk, and institutional marginalization for holding the reading — high d, target-side. Traditionalist clergy orders carry `trapped` exit specifically because canonical regularization requires accepting premises (full conciliar assent) that would dissolve the very identity the reading protects — this is not a mobile population that can simply relocate to another employer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification here (tangled_rope) resists two mislabelings. First, it resists reducing this reading to pure snare-on-tradition: the reading does perform a genuine coordination function for its adherents — organizing catechetical continuity, sacramental access to older forms, and doctrinal clarity claims — so it is not merely extractive cover. Second, it resists treating the post-conciliar arrangement it targets as simple, uncontested rope: the reading's own account requires active enforcement (canonical penalties, suppressed faculties) to hold, which is precisely what tangled_rope requires as a structural gate. Whether this reading is itself correct about the Council's validity is exactly what the omega variables below leave open — the classification describes the reading's structural shape, not its truth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    council_validity_vs_reception_crisis,
    'Are the documented tensions (religious liberty vs. prior anathemas on error having no rights, ecumenism vs. prior exclusivist formulations, collegiality vs. prior papal primacy emphasis) genuine doctrinal contradictions that invalidate or gravely compromise the Council''s authority, or are they resolvable developments whose apparent tension reflects incomplete reception and interpretation rather than defect in the Council itself?',
    'Would require either a definitive future magisterial adjudication treated as authoritative by all parties (unlikely to resolve the dispute for the reading''s own adherents, who contest the adjudicating body''s standing) or a neutral historical-theological methodology capable of distinguishing ''organic development'' from ''reversal'' independent of institutional interest — no such neutral arbiter currently commands assent across all three kernel readings.',
    'If the contradiction claim is validated by a framework the reading''s own adherents would accept, the post-conciliar magisterium''s authority for the intervening six decades is substantially undermined and the extraction/beneficiary structure reverses. If invalidated, this reading''s own coordination function (organizing traditionalist identity around a false diagnosis) becomes itself a form of extraction from its own adherents rather than a defense of doctrinal stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(council_validity_vs_reception_crisis, conceptual, 'Whether the rupture reading''s core premise (doctrinal contradiction) is empirically/theologically sustainable or a contested framing artifact.').

omega_variable(
    sspx_regularization_bargaining_position,
    'Is the SSPX''s canonically irregular status best modeled as principled resistance bearing real institutional cost, or as a bargaining position that has itself become a source of organizational identity and fundraising independent of resolution?',
    'Track SSPX institutional behavior across Rome''s periodic regularization overtures (1988, 2000s doctrinal talks, ongoing dialogue) — willingness to accept terms that would resolve the canonical irregularity while preserving liturgical and some doctrinal distinctives would support the principled-resistance reading; consistent goalpost-shifting would support the entrenched-identity reading.',
    'If entrenched identity, part of the measured suppression this reading experiences is self-selected rather than purely imposed, which would lower the effective victim-side directionality for traditionalist clergy orders specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sspx_regularization_bargaining_position, empirical, 'Whether traditionalist institutional irregularity reflects imposed suppression or partly self-maintained boundary.').

omega_variable(
    cs_framing_underdetermination_kernel_vs_reception,
    'Is the relevant kernel the Council''s documents themselves (textual authority), or the six-decade reception and implementation apparatus (liturgical reform, catechetical reform, episcopal appointments) built atop them? The rupture reading''s beneficiary/victim structure looks different depending on which framing is adopted: a text-level framing implicates the conciliar fathers and drafting commissions; a reception-level framing implicates the post-conciliar curial administrators who implemented specific reforms not strictly mandated by the texts (e.g., vernacular liturgy maximalism beyond what Sacrosanctum Concilium explicitly required).',
    'Compare the rupture reading''s own polemical literature: does it target the conciliar texts themselves as defective, or the ''spirit of the Council'' implementation apparatus as having exceeded textual mandate? Mixed targeting in the actual traditionalist literature suggests the ambiguity is real and unresolved within the reading itself.',
    'A text-level framing would make this a rope/snare question about the Council''s own authority; a reception-level framing would make this substantially a tangled_rope question about implementing institutions exceeding their textual mandate — the classification chosen here (tangled_rope) leans toward the reception-level framing but the text-level framing remains live within the reading''s own adherent literature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination_kernel_vs_reception, conceptual, 'Alternative framings of what the kernel actually IS (text vs. reception apparatus) that would shift this reading''s own internal classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__rupture_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_authority__rupture_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_authority__rupture_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_authority__rupture_reading, theater_ratio, 2012, 0.4).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__rupture_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__rupture_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_authority__rupture_reading, base_extractiveness, 1988, 0.58).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_authority__rupture_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_authority__rupture_reading, base_extractiveness, 2012, 0.65).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__rupture_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__rupture_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__rupture_reading, suppression_requirement, 1975, 0.55).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_authority__rupture_reading, suppression_requirement, 1988, 0.75).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_authority__rupture_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_authority__rupture_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__rupture_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__rupture_reading, 0.1).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the single natural-language label 'the Vatican II authority question' per the epsilon-invariance principle. continuity_reading authors near-zero extraction (organic development, no victim class). composite_overdetermination_reading authors structural ambiguity itself as the constraint (no single beneficiary/victim resolution possible). rupture_reading (this file) authors substantial tangled_rope extraction with a named beneficiary (modernist faction, post-conciliar administrators) and named victims (traditional Catholic laity, traditionalist clergy, doctrinal stability claimants). The three share the same kernel_id (vatican_ii_authority) but are NOT the same constraint — their epsilon values differ by a wide margin because they describe structurally distinct claims about the same contested text, exactly as the BGS conjecture decomposes into spectral universality and eigenvector thermalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
