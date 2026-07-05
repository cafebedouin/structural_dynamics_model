% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Vatican II as Composite Overdetermined Reform (Bundled Heterogeneous Changes)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the composite-overdetermination reading of the
 *   Vatican II authority kernel: the claim that 'Vatican II' names not one
 *   structural shift but a bundle of independently-warranted reforms
 *   (liturgical, ecumenical, ecclesiological, political/religious-freedom)
 *   that share only their common promulgation event and label. Under this
 *   reading, the continuity-vs-rupture debate that structures the sibling
 *   readings is a category error — it presupposes a single ε for a single
 *   shift, when the sixteen documents in fact carry different redaction
 *   histories, different degrees of departure from prior magisterial
 *   teaching, and different beneficiary/victim structures. The bundling
 *   itself, not any individual component, is the extractive move measured
 *   here: it forecloses component-level acceptance or rejection and forces
 *   every actor into an undifferentiated for/against posture toward 'the
 *   Council' as a whole.
 *
 * KEY AGENTS:
 *   - curial_administrators: primary agenda-setters who administer the bundle selectively
 *   - national_bishops_conferences: institutional beneficiaries of the collegiality component
 *   - ecumenical_dialogue_officials: institutional beneficiaries of the ecumenism component
 *   - traditionalist_clergy: bear the cost of forced wholesale acceptance/rejection
 *   - lay_faithful_seeking_doctrinal_clarity: bear the cost of unresolvable ambiguity at the pew level
 *   - conciliar_periti_and_theologians: analytical observers who can document the decomposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.42).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.48).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II as Composite Overdetermined Reform (Bundled Heterogeneous Changes)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '5912b4a6-7a84-4625-89ec-17637e5b5876').
narrative_ontology:cs_kernel_codification('5912b4a6-7a84-4625-89ec-17637e5b5876', fixed_text).
narrative_ontology:cs_authority_grounding('5912b4a6-7a84-4625-89ec-17637e5b5876', lineage).
narrative_ontology:cs_interpretation_layer_present('5912b4a6-7a84-4625-89ec-17637e5b5876').
narrative_ontology:cs_reading_relation('5912b4a6-7a84-4625-89ec-17637e5b5876', vatican_ii_doctrinal_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('5912b4a6-7a84-4625-89ec-17637e5b5876', vatican_ii_doctrinal_authority__rupture_progressive_reading, influences).
narrative_ontology:cs_reading_relation('5912b4a6-7a84-4625-89ec-17637e5b5876', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, influences).
narrative_ontology:cs_axiom('5912b4a6-7a84-4625-89ec-17637e5b5876', foundational, conciliar_corpus_is_structurally_heterogeneous).
narrative_ontology:cs_axiom_status(conciliar_corpus_is_structurally_heterogeneous, holdable).
narrative_ontology:cs_axiom_grounding('5912b4a6-7a84-4625-89ec-17637e5b5876', conciliar_corpus_is_structurally_heterogeneous, empirically_contingent).
narrative_ontology:cs_axiom('5912b4a6-7a84-4625-89ec-17637e5b5876', foundational, single_verdict_framing_is_category_error).
narrative_ontology:cs_axiom_status(single_verdict_framing_is_category_error, holdable).
narrative_ontology:cs_axiom_grounding('5912b4a6-7a84-4625-89ec-17637e5b5876', single_verdict_framing_is_category_error, conventional).
narrative_ontology:cs_reference_frame('5912b4a6-7a84-4625-89ec-17637e5b5876', conciliar_corpus_as_composite_document).
narrative_ontology:cs_drift_state('5912b4a6-7a84-4625-89ec-17637e5b5876', post_synodal_era_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5912b4a6-7a84-4625-89ec-17637e5b5876', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, curial_administrators).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_officials).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_parishes_experiencing_liturgical_whiplash).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_texts_as_composite_document).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__composite_overdetermination_reading, aggiornamento_as_administrative_program).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the implementation of the sixteen conciliar documents, deciding case by case which passages govern liturgical practice, which govern ecumenical posture, and which govern internal governance. Because the documents bundle distinct reforms under a single conciliar label, administrators can invoke whichever component serves an immediate administrative need — liturgical latitude here, doctrinal firmness there — without ever being pinned to a single coherent theory of what 'the Council' requires.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, curial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Gained substantially expanded discretionary authority under the collegiality and inculturation components of the reform package, while remaining formally bound by the continuity language of the doctrinal components. This asymmetric expansion of local authority is a genuine structural gain that rides on the same 'unified Council' label as the liturgical and ecumenical shifts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, national_bishops_conferences, agenda_setter).

% A new institutional career track — interfaith and inter-Christian dialogue offices — was created almost entirely by the ecumenism component of the bundle. Their institutional existence depends on the bundled reading being defended as a single coherent 'spirit of Vatican II' rather than being decomposed into discrete, separately-justified changes.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_officials, beneficiary,
    institutional, generational, mobile, global).

% Bear the cost of having liturgical change, doctrinal reinterpretation, and ecclesiological restructuring presented as a single indivisible reform they must accept wholesale or be labeled schismatic. Cannot accept the ecumenical or liturgical components while rejecting others without being treated as rejecting 'the Council' as such — the bundling forecloses selective acceptance that a decomposed reading would permit.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_clergy, payer,
    moderate, biographical, constrained, national).

% Encounter homilies and catechesis that treat 'Vatican II' as a single settled fact, when in reality the liturgical reform, the religious-freedom declaration, and the collegiality shift each rest on very different textual and theological warrants. They have no institutional mechanism to ask which specific component is at issue when a pastoral instruction is justified by appeal to 'the Council.'
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, lay_faithful_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Experience concrete, high-visibility liturgical change (vernacular Mass, altar orientation, lay participation) as the primary lived content of 'Vatican II,' even though liturgical reform is only one of several distinct structural shifts bundled under that name — and the component with the least theological controversy but the most disruptive practical footprint.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, local_parishes_experiencing_liturgical_whiplash, payer,
    powerless, immediate, trapped, local).

% Scholars and theologians who examine the drafting history of individual documents and can show that Sacrosanctum Concilium, Nostra Aetate, Dignitatis Humanae, and Lumen Gentium had distinct redaction histories, distinct theological warrants, and distinct degrees of continuity with prior magisterial teaching — evidence for the decomposition claim that the bundled reading obscures.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, conciliar_periti_and_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single ecumenical council needed to issue its decisions as one coherent conciliar act (sixteen documents promulgated under one papal authority) rather than as sixteen independent, separately-ratified reforms, in order to preserve the theological unity and binding character of a general council.
% TRANSFER_FUNCTION: The bundling transfers interpretive discretion from the lay faithful and lower clergy (who cannot separately evaluate or contest individual components) to institutional actors positioned to administer 'the Council' as an undifferentiated whole — curial administrators, bishops' conferences, and ecumenical offices who each draw legitimacy from whichever component suits their function.
% ABSENT_VOICES: Individual theologians and lower clergy who might accept the ecumenical opening while rejecting particular liturgical changes, or vice versa, have no institutional channel for component-level dissent; the up-or-down 'for or against the Council' framing was set by the drafting and reception process itself, not negotiated with them.
% DISAPPEARANCE_RATIONALE: If the bundled 'Vatican II' framing dissolved into its constituent claims — liturgical reform, religious freedom doctrine, ecumenical posture, and collegial governance each argued and ratified on its own separate merits — the current binary continuity/rupture debate would collapse, traditionalist and progressive factions would fragment along component lines rather than a single axis, and institutional actors currently drawing authority from 'the Council' as an undifferentiated whole would need new, narrower warrants for each specific practice.
% FOUNDING_PROBLEM: The Council was convened to address a cluster of genuinely distinct problems that had accumulated independently: a liturgy widely seen as inaccessible, doctrinal isolation from other Christian communities and world religions, an overcentralized curial governance structure, and a Church perceived as poorly positioned relative to modern political and intellectual life. These problems did not share a common root cause.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of the Council (e.g. the Bologna school and its critics) attest from outside both progressive and traditionalist camps that the conciliar documents have documented, divergent redaction histories and were the product of distinct commission processes — evidence for treating them as structurally separate reforms rather than a single mandate, even though neither faction inside the Church has an institutional incentive to make that decomposition explicit.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) and suppression (0.48) are moderate, not high, because the bundling constraint does not extract resources or obedience directly — it extracts interpretive discretion, concentrating the power to say what 'Vatican II requires' in institutional actors who can selectively invoke whichever component serves them. Theater ratio is elevated and rising (0.25 to 0.55) because increasing institutional energy goes into defending or attacking 'the Council' as a unified symbolic object rather than adjudicating the substantially different textual and theological merits of its component parts — this is Goodhart drift where the symbol replaces the substance. Accessibility collapse is moderate (0.40): a decomposed reading remains available to scholars and to some factions, but has not displaced the bundled reading in ordinary ecclesial discourse. Resistance is substantial (0.60) because both progressive and traditionalist factions have institutional incentives to preserve the bundle — each faction needs 'the Council' to be one thing in order to be either fully for or fully against it.
 *
 * DIRECTIONALITY LOGIC:
 *   Curial administrators and the two institutional beneficiary groups sit near the beneficiary end of directionality: the bundle gives them discretionary latitude to invoke favorable components and downplay unfavorable ones, and their institutional roles were substantially created or expanded by specific components (collegiality, ecumenism) that ride on the bundle's unified legitimacy. Traditionalist clergy and ordinary lay faithful sit near the target end: they experience the bundle as a take-it-or-leave-it package that forecloses the selective acceptance a decomposed reading would allow, and their exit options are constrained or trapped respectively — a traditionalist priest cannot simply accept Nostra Aetate while contesting Sacrosanctum Concilium without being read as rejecting the Council wholesale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuine cluster of distinct, accumulated institutional and doctrinal problems circa 1962 — was, on this reading, real for each component but not uniformly so: the liturgical problem and the collegiality problem may be substantially resolved (making that portion of the mandate arguably dead), while the ecumenical and religious-freedom questions remain live and contested. The composite reading resists a single mandatrophy verdict for exactly the reason it resists a single ε: mandatrophy analysis, like extraction analysis, is a category error when applied to the bundle as a whole. Each component needs its own founding-problem audit, which the bundled framing structurally prevents by treating any component-level obsolescence claim as an attack on 'the Council' itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decomposability_of_the_conciliar_corpus,
    'Can the sixteen conciliar documents be non-arbitrarily separated into independently-assessable reform components, or does the Church''s own doctrine of conciliar authority (that a general council''s acts form one binding magisterial act) make the decomposition itself theologically illegitimate?',
    'Comparative analysis of the drafting commissions, voting records, and promulgation formulas for each document against the Church''s formal theory of conciliar authority; canonical and theological scholarship on whether differential reception of conciliar documents has precedent (e.g., partial reception of earlier councils).',
    'If decomposition is theologically illegitimate, this reading itself would be a heterodox innovation rather than a neutral analytical description, collapsing it toward the rupture_traditionalist framing it claims to transcend. If decomposition is legitimate, the sibling readings'' shared premise (a single degree-of-change fact) is the actual category error, vindicating this reading''s structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decomposability_of_the_conciliar_corpus, conceptual, 'Whether the conciliar corpus can be legitimately decomposed into independent components or must be treated as one indivisible magisterial act.').

omega_variable(
    beneficiary_capture_of_bundling,
    'Is the persistence of the bundled ''Vatican II'' framing driven primarily by genuine theological necessity (a council''s acts must be received as one) or by the institutional interests of actors (curial administrators, bishops'' conferences, ecumenical offices) who benefit from retaining selective interpretive discretion over an undifferentiated whole?',
    'Trace which institutional actors have historically invoked ''the spirit of the Council'' or ''conciliar teaching'' in disputes, and whether their invocations track specific textual warrants or function as general-purpose legitimating language; compare to historical cases of selective conciliar reception (e.g. Trent''s uneven implementation) for base-rate evidence.',
    'If institutional capture predominates, the bundle is better modeled as tangled_rope (as authored) with identifiable beneficiaries; if theological necessity predominates, the constraint moves toward mountain-adjacent (an irreducible feature of conciliar authority rather than a constructed extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_bundling, empirical, 'Whether the bundle''s persistence is institutionally self-serving or theologically required.').

omega_variable(
    component_specific_epsilon_measurement,
    'If each component (liturgy, religious freedom, ecumenism, collegiality) were split into its own constraint story with its own ε, would any single component alone justify a tangled_rope or snare classification, or would the aggregate extraction only appear at the level of the bundle itself?',
    'Author four decomposed sibling stories (per the ε-invariance principle this reading itself calls for) each with independently measured ε, beneficiaries, and victims; compare each component''s standalone classification to this composite story''s aggregate reading.',
    'If no individual component reaches tangled_rope on its own, the extraction identified here is an emergent property of bundling itself rather than of any doctrinal content — strengthening the case that the bundling, not any teaching, is the object of analysis. If one or more components independently qualify as extractive, the composite reading''s claim that ''the debate is a category error'' would need qualification for that component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_specific_epsilon_measurement, empirical, 'Whether extraction is a property of the bundle or would also appear in properly decomposed component-level stories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement_basis(vati_tr_t1962, observed).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement_basis(vati_tr_t1975, observed).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.45).
narrative_ontology:measurement_basis(vati_tr_t1988, observed).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(vati_tr_t2000, observed).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2013, 0.52).
narrative_ontology:measurement_basis(vati_tr_t2013, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.55).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.2).
narrative_ontology:measurement_basis(vati_be_t1962, observed).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement_basis(vati_be_t1975, observed).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.35).
narrative_ontology:measurement_basis(vati_be_t1988, observed).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement_basis(vati_be_t2000, observed).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2013, 0.4).
narrative_ontology:measurement_basis(vati_be_t2013, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.3).
narrative_ontology:measurement_basis(vati_su_t1962, observed).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement_basis(vati_su_t1975, observed).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.42).
narrative_ontology:measurement_basis(vati_su_t1988, observed).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(vati_su_t2000, observed).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2013, 0.46).
narrative_ontology:measurement_basis(vati_su_t2013, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.48).
narrative_ontology:measurement_basis(vati_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, rupture_traditionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the vatican_ii_doctrinal_authority kernel. continuity_reading, rupture_progressive_reading, and rupture_traditionalist_reading each assert a single degree-of-change verdict for 'the Council' as a whole (organic development, authorized ongoing rupture, and doctrinally erroneous rupture, respectively). This composite_overdetermination reading instead asserts that the single-verdict premise shared by all three siblings is itself the structural artifact requiring analysis: the conciliar corpus bundles components with independently different continuity/rupture profiles, and the perceived need to pick one verdict for the whole is generated by the bundling, not by any property of the doctrine. A full analytical treatment would further decompose into per-component constraint stories (e.g. sacrosanctum_concilium_liturgical_reform, dignitatis_humanae_religious_freedom, nostra_aetate_ecumenism, lumen_gentium_collegiality), each with its own ε — this story stands in for that decomposition at the kernel-reading level without yet authoring the four component stories individually.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
