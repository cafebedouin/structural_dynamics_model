% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Composite Overdetermination — Bundled Reform Package
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   Vatican II (1962-1965) promulgated sixteen documents across four
 *   sessions, subsequently received by the institutional Church, by national
 *   episcopates, and by the faithful as 'the Council' — a single unified
 *   reform event requiring single acceptance or rejection. This story treats
 *   that unification as itself a constraint: an administrative and
 *   hermeneutical bundling that extracts institutional leverage by making
 *   component-specific evaluation structurally unavailable. The bundling is
 *   not itself the theological content of any document; it is a second-order
 *   framing choice about how the documents are to be received, defended, and
 *   reversed, and this story evaluates that framing choice as a constraint
 *   independent of whether any individual component (say, Dignitatis Humanae)
 *   is itself well- or ill-grounded.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.55).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II as Composite Overdetermination — Bundled Reform Package").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '563b934b-0454-4e14-a558-e8f0dac618f9').
narrative_ontology:cs_kernel_codification('563b934b-0454-4e14-a558-e8f0dac618f9', fixed_text).
narrative_ontology:cs_authority_grounding('563b934b-0454-4e14-a558-e8f0dac618f9', lineage).
narrative_ontology:cs_interpretation_layer_present('563b934b-0454-4e14-a558-e8f0dac618f9').
narrative_ontology:cs_reading_relation('563b934b-0454-4e14-a558-e8f0dac618f9', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('563b934b-0454-4e14-a558-e8f0dac618f9', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('563b934b-0454-4e14-a558-e8f0dac618f9', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('563b934b-0454-4e14-a558-e8f0dac618f9', foundational, documents_are_heterogeneous_in_authority_and_continuity).
narrative_ontology:cs_axiom_status(documents_are_heterogeneous_in_authority_and_continuity, holdable).
narrative_ontology:cs_axiom_grounding('563b934b-0454-4e14-a558-e8f0dac618f9', documents_are_heterogeneous_in_authority_and_continuity, empirically_contingent).
narrative_ontology:cs_axiom('563b934b-0454-4e14-a558-e8f0dac618f9', foundational, single_epsilon_measurement_is_a_category_error).
narrative_ontology:cs_axiom_status(single_epsilon_measurement_is_a_category_error, holdable).
narrative_ontology:cs_axiom_grounding('563b934b-0454-4e14-a558-e8f0dac618f9', single_epsilon_measurement_is_a_category_error, conventional).
narrative_ontology:cs_reference_frame('563b934b-0454-4e14-a558-e8f0dac618f9', conciliar_texts_as_undifferentiated_single_magisterial_act).
narrative_ontology:cs_drift_state('563b934b-0454-4e14-a558-e8f0dac618f9', post_diamond_jubilee_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('563b934b-0454-4e14-a558-e8f0dac618f9', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_curial_administrators).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reform_establishment).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy_and_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, parishes_seeking_component_specific_reversal).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_of_dissenting_minority_reports).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_partners).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_texts_as_single_juridical_act).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, hermeneutic_of_the_council_as_one_event).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the promulgated documents as a single juridical corpus, issuing implementation norms (post-conciliar instructions, revised codes, liturgical books) that treat liturgical reform, ecumenical opening, collegiality, and religious-freedom teaching as one indivisible mandate. They benefit from the bundling because it forecloses component-by-component challenge: rejecting the liturgical reform can be framed as rejecting the whole Council, raising the cost of any selective reversal.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_curial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Gained substantially expanded regulatory discretion (vernacular liturgy, local adaptation, ecumenical dialogue structures) under the umbrella of a single 'conciliar reform' label. Their institutional authority is now bound up with defending the package as a whole, since disaggregating it would expose which specific grants of authority came from which component and invite Roman reclamation of specific powers.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, agenda_setter).

% Institutes, seminaries, and publishing operations built around the reformed liturgy benefit from the bundled framing: liturgical change (the most extractive and contested component measured on its own) is shielded from independent scrutiny by being presented as inseparable from the far less contested ecumenical and collegial components.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reform_establishment, beneficiary,
    organized, generational, constrained, global).

% Wish to accept some components (e.g., religious-freedom teaching as development) while rejecting or renegotiating others (e.g., the new liturgical rite) but are structurally denied that option because ecclesial authority treats the whole as one act of the magisterium. Their component-specific objections are recast as rejection of 'the Council' itself, which raises the reputational and canonical cost of dissent far above what a component-level disagreement would warrant.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy_and_laity, payer,
    moderate, biographical, constrained, global).

% Individual parish communities that would prefer, say, retaining older liturgical forms while fully embracing ecumenical outreach have no institutional channel to make that distinction — permission structures (indults, later motu proprio provisions) treat the liturgical question as a special exception to a default package rather than restoring an independent axis of choice.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, parishes_seeking_component_specific_reversal, payer,
    powerless, biographical, trapped, local).

% Minority-position theologians at the Council (and their intellectual successors) who documented that particular schemas passed by narrow margins or with substantial unresolved objections are marginalized by a historiography that treats the final documents as expressing a single coherent conciliar mind, erasing the component-level contestation their objections targeted.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_of_dissenting_minority_reports, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theologians_of_dissenting_minority_reports, excluded).

% Other Christian communions and interfaith partners benefit concretely from the ecumenical and religious-freedom components regardless of how the liturgical or ecclesiological components are eventually judged; their interest is served by the bundle continuing to be defended as a whole even though their actual stake is narrower than the full package.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_partners, beneficiary,
    moderate, generational, mobile, global).

% Analyze conciliar documents, floor debates (acta), and post-conciliar implementation records component by component, documenting that liturgical, ecclesiological, ecumenical, and religious-freedom changes proceeded on different votes, different theological warrants, and different degrees of continuity with prior teaching — the evidentiary basis for treating 'Vatican II' as a bundle rather than a unitary shift.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single conciliar event with unified promulgation solves a real coordination problem: it lets the Church update multiple institutional and doctrinal subsystems (liturgy, ecclesial governance, interfaith posture, religious liberty doctrine) in one coordinated act rather than through decades of piecemeal, potentially conflicting local reforms, giving the changes shared legitimacy and a single canonical reference point.
% TRANSFER_FUNCTION: The bundling transfers interpretive and reversal leverage from component-level actors (parishes, dissenting theologians, traditionalist minorities who might accept some changes and reject others) to whoever controls the framing of 'the Council' as a single hermeneutical object — principally curial administrators and national conferences who can invoke the whole package to defend any single contested part.
% ABSENT_VOICES: The Council minority (roughly 20% of bishops on the most contested schemas, e.g. religious freedom and collegiality) is formally represented in the acta but functionally absorbed into a post-hoc narrative of near-unanimity; traditionalist communities that split from the ordinary structure entirely (SSPX and similar) are excluded from the conversation about how to selectively receive the documents, since the only offered choice is full reception or full rupture.
% DISAPPEARANCE_RATIONALE: If the composite-package framing dissolved and each component were formally re-evaluated on its own authority, historical continuity, and reception record, the practical result would be substantial: some components (ecumenical dialogue structures, religious freedom teaching) would likely be reaffirmed easily on independent merits, while others (specific liturgical mandates) would become newly contestable on their own terms, and canonical mechanisms for selective reception/reversal would need to be built where none currently exist.
% FOUNDING_PROBLEM: The bundling problem was not intentionally designed as extraction — it arose because a single ecumenical council, meeting over four sessions, needed a single closing act (promulgation of sixteen documents by one pope) to conclude, and subsequent Church governance needed one settled reference point ('the Council teaches...') rather than sixteen independently contestable authorities, especially amid Cold War pressure for the Church to present unified reform to a modernizing world.
% FOUNDING_PROBLEM_CORROBORATION: Curial administrators and the liturgical reform establishment attest the bundle remains a live, necessary unity of magisterial teaching. Independent ecclesiastical historians outside any faction (e.g., scholars documenting the redaction history and voting margins of individual schemas) corroborate that the documents differ sharply in genre, authority, and contestedness — some are dogmatic constitutions with near-unanimous votes, others are pastoral declarations passed by narrower margins — which is external evidence the 'single mandate' framing is a packaging choice rather than an inherent feature of the documents themselves.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: real coordination value exists (a single conciliar reference point genuinely solved a real problem of doctrinal and administrative fragmentation across a global institution), but the bundling also transfers reversal-leverage away from component-level dissenters toward whoever controls 'the Council' as a unified symbol — that transfer is the extractive residue. Theater ratio rises over the interval (0.30 to 0.58) as the anniversary commemorations, jubilee documents, and repeated magisterial reaffirmations of 'the spirit and letter of the Council' increasingly perform unity that the underlying documentary record (differing vote margins, differing doctrinal weight across constitutions vs. declarations) does not straightforwardly support. Suppression (0.55) reflects that selective reception is canonically and socially costly — a parish or priest cannot straightforwardly accept the ecumenical component while re-litigating the liturgical component through ordinary channels — without that being a raw exercise of top-down power over the level of a single document.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat, the package looks like a coherent single mandate whose parts reinforce each other's legitimacy — challenging one invites challenging all, which is treated as evidence of the package's integrity rather than of its leverage function. From the payer seats, the same bundling looks like an artificial coupling that forecloses reasonable, theologically defensible distinctions (accepting ecumenism while contesting liturgical discontinuity, for instance) that the documents' own internal heterogeneity would otherwise support. The engine computing divergent seat-level types from this same structural data is expected and is the point of authoring the composite reading separately from its siblings.
 *
 * DIRECTIONALITY LOGIC:
 *   Curial administrators, national conferences, and the liturgical reform establishment sit near the beneficiary end: the bundle gives them defensive leverage (any single-component challenge can be recast as an attack on the whole Council) and expanded discretionary authority acquired under the bundle's umbrella. Traditionalist clergy/laity, parishes seeking selective reversal, and dissenting-minority theologians sit near the target end: they bear the cost of a hermeneutical structure that denies them the component-level distinctions their actual objections require. Ecumenical dialogue partners are genuine beneficiaries of specific components but are directionally distinct from the administrators — their benefit does not depend on the bundle persisting, only on specific components surviving, which is why they are listed separately rather than folded into the same directionality bucket.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (a fragmented, regionally uneven Church needing one settled post-conciliar reference point) was real in 1965 and is largely resolved by 2025 — canonical, liturgical, and ecumenical structures are now mature and stable on their own institutional footings, each capable of independent evaluation and revision (as later motu proprio provisions on the liturgy demonstrate is technically possible). The bundle's persistence past that resolution point, defended increasingly through commemorative theater rather than functional necessity, is consistent with mandatrophy: the coordination justification has substantially expired for at least some components even as the packaging that was built to solve it continues to be defended as an indivisible whole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_kernel_vs_four_kernels,
    'Is ''Vatican II'' properly one kernel with contested readings, or is the composite_overdetermination_reading correct that it should be decomposed into four (or more) independent kernels — one each for liturgy, ecumenism, ecclesiology, and religious freedom — each with its own continuity/rupture contest?',
    'Systematic component-by-component analysis of each document''s voting margin, doctrinal genre (dogmatic constitution vs. pastoral declaration), citation of prior magisterial teaching, and subsequent reception history; convergent or divergent patterns across components would support or undermine treating them as one kernel.',
    'If the components genuinely warrant separate kernels, this composite reading is itself a transitional diagnostic pointing toward decomposition into a constraint family (per the epsilon-invariance principle) rather than a stable terminal reading; if they are irreducibly linked by the single act of promulgation regardless of internal heterogeneity, the composite reading remains the correct terminal analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_kernel_vs_four_kernels, conceptual, 'Whether the kernel itself should be split into independent per-component kernels.').

omega_variable(
    bundling_intentional_or_emergent,
    'Was the packaging of disparate reforms into a single conciliar act a deliberate extraction strategy by curial and episcopal actors seeking bundled leverage, or an emergent administrative necessity of how ecumenical councils have always concluded (one promulgation, sixteen documents, one pope)?',
    'Historical analysis of conciliar procedural debates (was separate promulgation per document ever proposed and rejected, and on what grounds?) and comparison with prior councils'' promulgation practices.',
    'If deliberate, the extraction reading of the bundle is strengthened considerably; if emergent from procedural convention with no deliberate leverage-seeking, the extractiveness score should be revised downward toward pure coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bundling_intentional_or_emergent, empirical, 'Whether the bundling was strategic extraction or procedural necessity.').

omega_variable(
    reception_asymmetry_measurement,
    'Can the differential reception and contestation levels across the four named components (liturgical, ecumenical, ecclesiological, religious-freedom) be reliably measured independently, or does any attempt at separate measurement already presuppose the composite reading''s conclusion?',
    'Independent historiographical review by scholars outside all three factional readings (continuity, progressive-rupture, traditionalist-rupture), examining whether component-level vote margins and subsequent revision histories (e.g., the 1970 Missal revisions vs. the stability of Nostra Aetate) show measurably different trajectories.',
    'If components show clearly different trajectories, this substantiates treating epsilon as component-indexed rather than kernel-wide, reinforcing the decomposition case; if trajectories converge, the composite reading''s core claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_asymmetry_measurement, empirical, 'Whether independent measurement of component-level extraction is methodologically achievable without circularity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.48).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.52).
narrative_ontology:measurement(vati_tr_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2007, 0.5).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.55).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.34).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.37).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.39).
narrative_ontology:measurement(vati_be_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2007, 0.36).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.45).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.52).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.49).
narrative_ontology:measurement(vati_su_t2007, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2007, 0.53).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.51).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the vatican_ii_doctrinal_authority kernel. The composite_overdetermination_reading differs from its siblings not by disputing the direction of change (continuity vs. rupture) but by denying that a single direction-of-change claim is the correct unit of analysis at all. Where the other three readings each author a single epsilon for 'the Council' as a whole (differing in magnitude and valence), this reading's structural delta is that it holds epsilon should properly be component-indexed — the epsilon authored here (0.42) represents this reading's assessment of the bundling mechanism itself as a constraint, not an average or synthesis of the other three readings' epsilon values. Per the epsilon-invariance principle, if finer decomposition into four independent per-component constraints is later warranted, that would be a further family expansion, not a revision of this reading's own epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
