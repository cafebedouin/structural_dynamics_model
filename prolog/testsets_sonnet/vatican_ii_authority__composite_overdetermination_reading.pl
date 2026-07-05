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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Overdetermined Doctrinal Composite (Structural Ambiguity Reading)
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II authority kernel: the claim that the council's documents are
 *   not a single interpretable event resolvable into either organic
 *   development (continuity_reading) or substantive break (rupture_reading),
 *   but an overdetermined stitching-together of theologically incompatible
 *   factional positions whose ambiguity is structural rather than a temporary
 *   interpretive gap awaiting magisterial clarification. This reading treats
 *   the fifty-plus years of post-conciliar hermeneutical conflict
 *   (Ratzinger's 'hermeneutic of continuity' versus progressive and
 *   traditionalist counter-readings) as evidence FOR the composite thesis
 *   rather than as a solvable exegetical puzzle. The claimed type
 *   (tangled_rope) and the authored metrics are independent facts: the
 *   council text did solve a genuine coordination problem (avoiding schism at
 *   the moment of promulgation), which is the rope component, but the
 *   mechanism by which it solved that problem — engineered ambiguity — now
 *   functions as an ongoing extraction on everyone who must act under
 *   unresolved doctrine while institutional authority claims the matter is
 *   settled.
 *
 * KEY AGENTS:
 *   - magisterial_univocal_interpreters: institutional authority whose legitimacy depends on denying structural ambiguity
 *   - academic_conciliar_scholars: beneficiaries of the composite reading via historical-critical vindication
 *   - hermeneutic_pluralist_theologians: partial beneficiaries who still bear institutional exposure
 *   - parish_clergy_seeking_settled_doctrine: powerless payers needing practical resolution that never arrives
 *   - lay_catholics_navigating_conflicting_teaching: powerless payers bearing the cost of unresolved doctrine
 *   - traditionalist_rupture_advocates: excluded voices whose all-or-nothing reading is treated as incomplete
 *   - conciliar_scholarship_field: analytical observer of the drafting record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II as Overdetermined Doctrinal Composite (Structural Ambiguity Reading)").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, 'e1db07ca-5357-48c7-8c86-47d56c2dd088').
narrative_ontology:cs_kernel_codification('e1db07ca-5357-48c7-8c86-47d56c2dd088', fixed_text).
narrative_ontology:cs_authority_grounding('e1db07ca-5357-48c7-8c86-47d56c2dd088', extraction).
narrative_ontology:cs_interpretation_layer_present('e1db07ca-5357-48c7-8c86-47d56c2dd088').
narrative_ontology:cs_reading_relation('e1db07ca-5357-48c7-8c86-47d56c2dd088', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1db07ca-5357-48c7-8c86-47d56c2dd088', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('e1db07ca-5357-48c7-8c86-47d56c2dd088', foundational, conciliar_ambiguity_is_structurally_irreducible).
narrative_ontology:cs_axiom_status(conciliar_ambiguity_is_structurally_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('e1db07ca-5357-48c7-8c86-47d56c2dd088', conciliar_ambiguity_is_structurally_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('e1db07ca-5357-48c7-8c86-47d56c2dd088', foundational, univocal_magisterial_interpretation_is_a_legitimation_claim_not_a_descriptive_fact).
narrative_ontology:cs_axiom_status(univocal_magisterial_interpretation_is_a_legitimation_claim_not_a_descriptive_fact, holdable).
narrative_ontology:cs_axiom_grounding('e1db07ca-5357-48c7-8c86-47d56c2dd088', univocal_magisterial_interpretation_is_a_legitimation_claim_not_a_descriptive_fact, conventional).
narrative_ontology:cs_reference_frame('e1db07ca-5357-48c7-8c86-47d56c2dd088', pre_conciliar_neo_scholastic_consensus).
narrative_ontology:cs_drift_state('e1db07ca-5357-48c7-8c86-47d56c2dd088', post_synod_2015_2025_polarization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e1db07ca-5357-48c7-8c86-47d56c2dd088', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, magisterial_univocal_interpreters).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_settled_doctrine).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, lay_catholics_navigating_conflicting_teaching).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, conciliar_texts_are_products_of_factional_compromise).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_conflict_is_structural_not_accidental).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops' conferences, curial offices, and papal teaching authority stake institutional legitimacy on the claim that the council's documents express a single coherent teaching continuous with (or authoritatively developing) prior tradition. The composite-overdetermination reading directly undercuts this claim by asserting the texts encode irreconcilable theological positions stitched together by compromise, which the magisterium cannot concede without admitting its own interpretive authority is adjudicating contradictions rather than declaring settled doctrine. They cannot exit this position; their authority is constituted by the claim of univocal interpretability.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, magisterial_univocal_interpreters, payer,
    institutional, civilizational, identity_locked, global).

% Historical-critical scholars of the council debates (Alberigo school and successors) benefit professionally and intellectually from a reading that treats the acta, drafting history, and floor debates as evidence of genuine factional conflict rather than as surface noise over an underlying unity. This reading generates research programs, sustains academic careers, and validates archival methodologies. They have mobile exit — they can pursue this reading in secular or ecumenical academic settings regardless of magisterial reception.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, academic_conciliar_scholars, beneficiary,
    organized, generational, mobile, global).

% Theologians who argue for legitimate plural readings of conciliar texts benefit from the composite reading's vindication of interpretive multiplicity, but they also pay a cost: this reading denies them the comfort of any single defensible hermeneutic (continuity OR rupture) and leaves them permanently exposed to charges of incoherence from both traditionalist and progressive critics. Their institutional standing (seminary posts, ecclesiastical mandates) constrains how openly they can assert structural contradiction rather than 'development' language.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, hermeneutic_pluralist_theologians, payer).

% Priests administering sacraments, liturgy, and pastoral guidance need practical, settled answers on questions the council left ambiguous (collegiality vs. papal primacy, religious liberty vs. prior condemnations, liturgical latitude). The composite reading tells them no resolution is coming because the ambiguity is structural, not a temporary interpretive lag — leaving them to improvise pastoral practice under permanent uncertainty with no institutional recourse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, parish_clergy_seeking_settled_doctrine, payer,
    powerless, biographical, trapped, local).

% Ordinary Catholics receive contradictory pastoral messages depending on diocese, parish, or theological formation of their clergy — traditionalist Latin Mass communities, progressive post-conciliar parishes, and everything between all claim conciliar warrant. The composite reading validates their confusion as tracking a real structural fact rather than their own misunderstanding, but offers no path to resolution; they bear the cost of a divided Church without the institutional standing to demand clarity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, lay_catholics_navigating_conflicting_teaching, payer,
    powerless, biographical, trapped, global).

% Groups holding the rupture reading (SSPX and sympathizers) would object that the composite reading is itself a evasive half-measure that refuses to name the break plainly, preferring academic complexity to theological judgment. They are structurally excluded from this reading's own framing because the composite reading treats their all-or-nothing rupture claim as one incomplete pole rather than as a candidate truth.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_rupture_advocates, excluded,
    organized, generational, constrained, global).

% The discipline of conciliar historiography and theological hermeneutics observes the drafting history, competing schemata, minority/majority position papers, and post-conciliar reception across sixty years, documenting where continuity language and rupture language were both deployed by different conciliar factions within the same documents to secure passage.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, conciliar_scholarship_field, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The council's own compromise language coordinated passage of documents across deeply divided factions (Roman curial conservatives, northern European reformist bishops, missionary-church bishops) by permitting each faction to read key passages (collegiality, religious liberty, ecumenism, liturgy) as vindicating its own prior position. This let the council conclude with near-unanimous votes instead of schism.
% TRANSFER_FUNCTION: The compromise-drafting move transfers the cost of resolving the underlying theological disagreement from the council fathers (who needed unanimity to close the council) onto every subsequent generation of interpreters, clergy, and laity, who must live with the resulting ambiguity without the authority to resolve it themselves.
% ABSENT_VOICES: The minority conservative bishops who lost floor votes but whose language was retained in qualifying clauses are formally present in the text but structurally muted — their preferred reading survives only as a dissenting substrate that later rupture advocates invoke. Lay Catholics affected by decades of liturgical and doctrinal instability were never conciliar participants at all.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination reading were simply declared false and abandoned, institutional authorities would experience relief (their univocal-interpretation claim would go uncontested) while historical-critical scholarship would have to suppress or reinterpret its own archival findings about factional drafting conflict. Whether the world 'rearranges' depends on which seat is asked: for the magisterium the ambiguity would appear resolved (world unchanged in practice, since pastoral conflict has independent causes); for scholars the suppression of the composite reading would materially change what counts as legitimate conciliar scholarship.
% FOUNDING_PROBLEM: The council needed to produce texts that a deeply divided body of bishops — some wanting minimal change, some wanting substantial reform, operating across incompatible theological schools (neo-scholastic and ressourcement) — could all vote to approve without triggering formal schism.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the council (outside both the magisterium and the reformist theological establishment that benefited from the reforms), drawing on the conciliar acta and private diaries of periti and bishops released decades later, corroborate that specific passages were drafted and redrafted explicitly to secure votes from opposing factions by preserving ambiguity — this is documented archival practice, not merely a scholarly inference asserted by interested parties.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) and suppression (0.62) are both moderate-to-substantial rather than extreme: the composite reading does not claim the council was a pure power grab, but that its coordination function (avoiding schism) was purchased by an extraction the council fathers could not have fully declared without failing to secure votes — namely, decades of downstream interpretive labor and pastoral instability imposed on people with no seat at the council. Theater ratio rises steadily (0.20 to 0.55) because increasing institutional energy over sixty years has gone into performing settled interpretation (synods, commissions, anniversary commemorations reasserting 'continuity') rather than resolving the underlying textual contradictions, which the composite reading holds are irresolvable by further commentary. Accessibility collapse is low (0.35) because multiple live readings persist in practice — this is precisely the composite reading's diagnostic claim, that no single interpretation has actually foreclosed the others despite institutional insistence. Resistance is high (0.72) because every named party except the analytical observer actively contests this reading: the magisterium resists it as delegitimizing, traditionalists resist it as too soft, and even some pluralist theologians resist its bluntness about irreconcilable contradiction versus their preferred 'development' language.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic conciliar scholars and hermeneutic pluralist theologians are coded as beneficiaries because the composite reading validates their historical-critical and pluralist methodologies and generates ongoing scholarly work; their exit options (mobile, constrained) reflect that scholars can pursue this reading in secular academic contexts even where ecclesiastical reception is hostile. Magisterial interpreters, parish clergy, and lay Catholics are coded as victims/payers because the composite reading directly undermines the univocal-interpretation claim the magisterium needs (institutional cost) and denies clergy and laity the practical resolution they need to act (existential/pastoral cost) — and unlike the scholars, clergy and laity have no alternative venue in which the ambiguity resolves itself; they are trapped inside a single institution whose doctrine is contested.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing conciliar unanimity across incompatible factions to avoid 1870-style schism — was genuinely live in 1962-65 and is honestly reported as still live in a diffuse sense (the Church's internal factional divisions persist), which blocks a clean mandatrophy verdict. But the founding_problem_status/disappearance_verdict pairing (live/contested) is itself the diagnostic finding of this reading: the coordination function that justified engineered ambiguity in 1965 does not justify the same ambiguity's indefinite continuation six decades later when the votes have long been cast and the schism-avoidance rationale no longer applies to ongoing interpretive disputes. The composite reading refuses to let magisterial authority claim the founding problem's resolution (settled doctrine) while simultaneously declining to resolve the substance — that gap between claimed resolution and actual structural ambiguity is what the tangled_rope classification is measuring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_versus_genuine_synthesis,
    'Were the ambiguous passages in the conciliar documents (e.g., Lumen Gentium''s treatment of collegiality and papal primacy, Dignitatis Humanae''s relation to prior magisterial condemnations of religious liberty) the product of genuine theological synthesis at a higher level of abstraction that the continuity and rupture readings both fail to see, or were they irreducible factional compromises papering over live contradiction, as the composite reading holds?',
    'Comparative analysis of conciliar drafting history (schema revisions, floor debate transcripts, private correspondence among periti) to determine whether textual ambiguity resolved toward a discoverable underlying coherence during redaction, or whether redaction consistently moved toward preserving maximal ambiguity to satisfy competing factions without resolving their disagreement.',
    'If the ambiguity resolves toward genuine synthesis under closer historical analysis, the composite reading''s core claim of irreducible contradiction fails and the constraint should be reclassified toward a rope or scaffold (transitional formulation later clarified) rather than tangled_rope. If the ambiguity is confirmed as unresolved factional compromise at every level of analysis, the tangled_rope classification with institutional extraction is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_versus_genuine_synthesis, empirical, 'Whether conciliar ambiguity reflects deeper synthesis or unresolved factional compromise.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the choice among the continuity, rupture, and composite readings itself resolvable by evidence internal to theology and history, or is it a genuinely under-determined framing choice where the same archival record supports all three readings depending on prior theological commitments (a view of doctrinal development already held before approaching the council texts)?',
    'No purely empirical resolution mechanism exists because the disagreement is partly about what would COUNT as doctrinal continuity or rupture, which is itself a theological rather than historical question. Partial resolution might come from tracking whether scholars who change their prior theological commitments (e.g., converts between traditionalist and progressive positions) also change their reading of the same conciliar evidence, which would suggest the framing choice drives the reading rather than the evidence driving the framing.',
    'If the readings are genuinely incommensurable (evidence-independent), this vindicates the composite reading''s meta-claim that structural ambiguity is irreducible — not merely under-researched but constitutively unresolvable by additional textual analysis. If one reading demonstrably tracks the evidence better independent of prior commitment, the kernel is less genuinely contested than this reading assumes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the continuity/rupture/composite reading choice is evidence-resolvable or framing-dependent.').

omega_variable(
    extraction_beneficiary_scope,
    'Does the composite reading''s designation of academic conciliar scholars as ''beneficiaries'' understate a further asymmetry — that Western, English/German/French-language academic theology captures most of the interpretive authority generated by this reading, while global-South clergy and laity who bear the pastoral costs of ambiguity have negligible voice in the scholarly discourse that validates the composite framing?',
    'Survey of authorship and citation patterns in conciliar historiography by region and language, cross-referenced against where post-conciliar pastoral conflict is most acute (mission territories with rapid liturgical and doctrinal shifts versus European academic centers producing the interpretive literature).',
    'If academic benefit from the composite reading is concentrated in wealthy Western institutions while pastoral costs fall disproportionately on under-resourced dioceses elsewhere, the beneficiary/victim asymmetry in this story is more severe (and more geographically patterned) than currently modeled, which would argue for adding regional stakeholder seats in a follow-on story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_scope, empirical, 'Whether the beneficiary group is itself internally stratified by geography and institutional wealth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(vati_tr_t1990, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.47).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.51).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement(vati_be_t1990, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.4).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1990, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the vatican_ii_authority kernel. continuity_reading and rupture_reading each claim the council produced a single coherent theological position (development-within-tradition versus substantive break, respectively); this composite_overdetermination_reading denies that a single coherent position exists to be classified along that spectrum, holding instead that the texts encode genuinely incompatible factional positions. All three stories share the same underlying conciliar documents and drafting history but assign different ε, different beneficiary/victim structures, and different classifications because they make incompatible claims about what kind of object the council's textual output actually is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
