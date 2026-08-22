% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II as Organic Development Within Unchanging Tradition (Hermeneutic of Continuity)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the Vatican II
 *   doctrinal authority kernel: the claim that the Council's teaching,
 *   including apparently novel positions on religious liberty, ecumenism, and
 *   collegiality, is an organic explication of what was already implicitly
 *   present in prior Tradition, not a rupture with it. On this reading, the
 *   visible discontinuities in liturgy and pastoral life are prudential and
 *   disciplinary adaptations, not doctrinal changes, and post-conciliar
 *   excesses (liturgical abuse, doctrinal laxity, institutional collapse in
 *   vocations and practice) are implementation failures that betray rather
 *   than fulfill conciliar intent. Doctrinal-content extraction is authored
 *   low (0.28) because, by this reading's own lights, nothing doctrinal was
 *   actually extracted or altered — texts were clarified, not contradicted.
 *   But the reading requires active magisterial enforcement to hold against
 *   two simultaneous rival readings (progressive and traditionalist), and it
 *   imposes real costs on communities on both flanks whose competing textual
 *   readings are foreclosed by administrative fiat rather than argued down on
 *   the merits.
 *
 * KEY AGENTS:
 *   - magisterial_curial_authority: sets and enforces the authoritative interpretation (institutional/arbitrage)
 *   - conciliar_bishops_and_successors: beneficiaries whose institutional legitimacy depends on conciliar validity (institutional/constrained)
 *   - continuity_theologians: beneficiaries whose scholarly and professional standing rides on the continuity thesis (organized/mobile)
 *   - traditionalist_communities_denied_full_recognition: payers whose rupture diagnosis is foreclosed (moderate/trapped)
 *   - progressive_reformers_whose_readings_are_disciplined: payers whose open-trajectory reading is disciplined (moderate/constrained)
 *   - laity_experiencing_doctrinal_whiplash: payers bearing the cognitive/communal cost of reconciling declared continuity with lived discontinuity (powerless/constrained)
 *   - ecclesiastical_historians: analytical observers of conciliar drafting history (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.28).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.42).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II as Organic Development Within Unchanging Tradition (Hermeneutic of Continuity)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'b09f8fe5-9006-4eec-b74a-9c7e3533ca98').
narrative_ontology:cs_kernel_codification('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', fixed_text).
narrative_ontology:cs_authority_grounding('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', lineage).
narrative_ontology:cs_interpretation_layer_present('b09f8fe5-9006-4eec-b74a-9c7e3533ca98').
narrative_ontology:cs_reading_relation('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', vatican_ii_doctrinal_authority__rupture_progressive_reading, forecloses).
narrative_ontology:cs_reading_relation('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, forecloses).
narrative_ontology:cs_axiom('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', foundational, doctrine_develops_without_rupture).
narrative_ontology:cs_axiom_status(doctrine_develops_without_rupture, holdable).
narrative_ontology:cs_axiom_grounding('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', doctrine_develops_without_rupture, theological).
narrative_ontology:cs_axiom('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', foundational, magisterium_alone_determines_authentic_reading).
narrative_ontology:cs_axiom_status(magisterium_alone_determines_authentic_reading, holdable).
narrative_ontology:cs_axiom_grounding('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', magisterium_alone_determines_authentic_reading, conventional).
narrative_ontology:cs_axiom('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', secondary, postconciliar_disorder_is_implementation_failure_not_intent).
narrative_ontology:cs_axiom_status(postconciliar_disorder_is_implementation_failure_not_intent, holdable).
narrative_ontology:cs_axiom_grounding('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', postconciliar_disorder_is_implementation_failure_not_intent, instrumental).
narrative_ontology:cs_reference_frame('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', unbroken_apostolic_doctrinal_transmission).
narrative_ontology:cs_drift_state('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', post_2005_hermeneutic_of_continuity_address, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b09f8fe5-9006-4eec-b74a-9c7e3533ca98', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterial_curial_authority).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, conciliar_bishops_and_successors).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, continuity_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities_denied_full_recognition).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_reformers_whose_readings_are_disciplined).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_doctrinal_whiplash).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, hermeneutic_of_reform_in_continuity).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, development_of_doctrine_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, magisterial_interpretive_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and exercises the authority to declare which readings of the Council's documents are authentic. Issues authoritative interpretive statements (e.g. Benedict XVI's 2005 Curia address framing 'hermeneutic of continuity' against 'hermeneutic of rupture'), disciplines both traditionalist and progressive deviations, and controls seminary formation and doctrinal offices that reproduce this reading across generations. Bears essentially no cost from maintaining this reading; it is the seat that both authors and administers it.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterial_curial_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Inherited episcopal legitimacy premised on the Council having been a valid, tradition-consistent exercise of collegial magisterium. A rupture reading (of either progressive or traditionalist flavor) would call into question the validity or authority of decisions bishops now administer day to day, so continuity language protects their standing office.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, conciliar_bishops_and_successors, beneficiary,
    institutional, generational, constrained, global).

% Academic and clerical specialists (ressourcement-adjacent and neo-scholastic alike) who have built careers, journals, and institutes on demonstrating textual continuity between conciliar and pre-conciliar teaching. Their scholarly output is validated precisely to the degree the continuity frame holds; they have professional exit into other theological subfields but identity and reputational capital are concentrated here.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, continuity_theologians, beneficiary,
    organized, biographical, mobile, global).

% Groups (SSPX and sympathizers, communities attached to the pre-1962 liturgy) who read the same documents as containing genuine ruptures on religious liberty, ecumenism, and collegiality. Under the continuity reading their objections are recast as failures to understand true continuity rather than as legitimate doctrinal disagreement, which forecloses canonical normalization on their own terms and keeps them in an irregular or semi-irregular status they did not choose. Exit means leaving the visible institution altogether, which for many is not a live option given their self-understanding as the Church's continuation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_communities_denied_full_recognition, payer,
    moderate, generational, trapped, national).

% Clergy, theologians, and lay reformers who read the Council as authorizing an open-ended trajectory ('the spirit of the Council') toward married clergy, women's ordination, doctrinal development on sexuality, or decentralized authority. The continuity reading is repeatedly invoked to discipline these claims as exceeding the text, restricting their room to argue for reform as conciliar fulfillment rather than conciliar betrayal. Exit means marginalization within, or departure from, institutional Catholicism.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_reformers_whose_readings_are_disciplined, payer,
    moderate, generational, constrained, global).

% Ordinary parishioners who experienced concrete, visible liturgical and pastoral change (vernacular Mass, altar orientation, ecumenical gestures, altered catechesis) presented simultaneously as 'nothing doctrinal changed.' They bear the confusion of reconciling lived discontinuity with official continuity language, with limited theological training to adjudicate the dispute themselves and little institutional voice in how it is resolved.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, laity_experiencing_doctrinal_whiplash, payer,
    powerless, biographical, constrained, local).

% The sibling reading holding the Council authorized an open trajectory beyond the text itself; named here as the excluded alternative framework, not as an actor — it is disciplined out of magisterial legitimacy by the continuity reading's interpretive supremacy claim.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading).

% The sibling reading holding the Council documents themselves contain doctrinal rupture or ambiguity enabling heterodoxy; named here as the excluded alternative framework — its diagnosis of textual rupture is rejected a priori by the continuity reading's premise that no true rupture is possible in valid magisterial acts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading, excluded,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading).

% Study conciliar drafting history, minority/majority vota, and textual genealogy independent of confessional commitment to any reading's correctness. Can document where documents were deliberately left ambiguous to secure passage, which bears on whether 'continuity' was drafted intent or is a retrospective interpretive imposition.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecclesiastical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institutional Church a single authoritative framework for reading its own twentieth-century self-transformation without conceding that the Magisterium's prior teaching was wrong or that its post-conciliar teaching is unauthorized — this is a genuine coordination problem for any institution claiming doctrinal inerrancy across a visible historical rupture.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimacy toward whichever curial and episcopal actors control the official reading, and away from communities and theologians (of both traditionalist and progressive orientation) whose competing textual readings are recast as misunderstandings rather than live alternatives. Moves psychological and communal cost onto laity who must reconcile lived discontinuity with declared continuity.
% ABSENT_VOICES: Both traditionalist canonists arguing textual rupture on religious liberty/ecumenism and progressive theologians arguing for an open conciliar trajectory would object that the continuity frame pre-empts their reading by definitional fiat; neither is present as an adjudicating party in how 'authentic' interpretation is fixed — that determination sits entirely with the same curial authority whose legitimacy the reading protects.
% DISAPPEARANCE_RATIONALE: If the continuity reading were abandoned tomorrow, the institutional Church's claim to unbroken doctrinal authority across the Council would be directly exposed to challenge from both flanks simultaneously — bishops, curial offices, and continuity theologians would say the visible Church's coherence unravels; traditionalist and progressive parties would say the honest state of contested interpretation the continuity frame currently suppresses would simply become visible, which they would regard as clarification rather than rearrangement.
% FOUNDING_PROBLEM: The Council produced texts with genuine drafting ambiguities and deliberately compromise language (to secure both conservative and reforming majorities), alongside visible, rapid, and disorienting changes in liturgy and pastoral practice; the Church needed a way to affirm the Council's full authority while explaining why so much visibly changed and why some conciliar language admits multiple competing readings.
% FOUNDING_PROBLEM_CORROBORATION: Curial and magisterial sources (notably Benedict XVI's 2005 address) attest the continuity reading resolves a live interpretive need. Independent ecclesiastical historians outside the magisterial apparatus — studying conciliar minutes, minority reports, and the documented ambiguity deliberately built into texts like Dignitatis Humanae and Nostra Aetate to secure passage — corroborate that genuine drafting ambiguity exists, but do not corroborate that 'continuity' was the drafters' univocal intent rather than a retrospective interpretive framework; several note the compromise language was ambiguous by design, which is a different claim than implicit-development-made-explicit.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction on doctrinal content proper is authored low and essentially flat (0.18 to 0.28 over sixty years) because the continuity reading's central claim is precisely that no doctrinal content was extracted or overturned — this is a low-ε constraint by its own criterion of measurement. Suppression and theater ratio, by contrast, are authored moderate and rising: suppression (0.30 to 0.42) tracks the accumulating apparatus of magisterial statements, disciplinary actions, and seminary formation needed to hold the continuity frame against two persistent rival readings that never went away and in some respects hardened (SSPX regularization talks stalling repeatedly; progressive synodal-path tensions). Theater ratio (0.20 to 0.38) tracks the growing gap between the confident declarative language of continuity documents (culminating in the 2005 hermeneutic-of-continuity address) and the underlying unresolved textual ambiguity that ecclesiastical historians continue to document in the conciliar drafting record — the more the continuity claim is restated as settled, the more of that restatement is performing settlement rather than resolving the underlying interpretive dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (curial authority, episcopal successors, continuity theologians) sit near the beneficiary end of directionality: they hold arbitrage or mobile exit, collect institutional legitimacy or professional standing from the reading's persistence, and bear essentially none of its foreclosure costs. Payers (traditionalist communities, progressive reformers, ordinary laity) sit toward the target end: their exit options range from constrained to trapped, they experience the reading as something imposed on their competing textual interpretation rather than argued with them, and the cost is concentrated on them precisely because they are the parties whose readings the continuity frame exists to override. Laity carry the lowest power and among the most constrained exit, bearing a diffuse psychological cost (doctrinal whiplash) that is real but harder to concentrate into an organized victim claim than the traditionalist or progressive communities' more articulate grievances.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading is not obviously mandatrophic: its founding problem (reconciling visible historical change with claimed doctrinal inerrancy) remains live so long as the institution claims an unbroken magisterium, which it still does. What prevents simple 'pure extraction' classification is the genuine coordination function — some framework is needed for any institution claiming continuous authority across a documented moment of visible change, and continuity language is not merely invented after the fact; drafters at the Council itself used continuity language. What prevents simple 'pure coordination' (rope) classification is that the reading requires ongoing active enforcement against two persistent, organized rival readings, and it forecloses those readings by authority rather than by winning the argument, at real and unequally distributed cost to the foreclosed parties. Tangled rope captures this: real coordination function, real asymmetric cost, real requirement of active enforcement to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafting_ambiguity_vs_implicit_development,
    'Was the interpretive openness in key conciliar texts (e.g. Dignitatis Humanae on religious liberty, Nostra Aetate on non-Christian religions, Lumen Gentium on collegiality) the product of deliberate compromise language needed to secure conflicting conciliar majorities, or is it genuinely resolvable as the surface expression of doctrine that was already implicitly present and merely awaited explication?',
    'Comparative analysis of conciliar vota, minority/majority interventions, and drafting-committee correspondence against the pre-conciliar theological sources the continuity reading claims as the implicit antecedent; historians of the Council (Alberigo, O''Malley, and critics of their historiography) disagree on this question using the same archival record.',
    'If drafting ambiguity was primarily strategic compromise rather than latent-doctrine explication, the continuity reading''s central mechanism (organic development) is a retrospective interpretive imposition rather than a description of what the Council actually did, which would raise its effective extraction closer to the rupture readings'' assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafting_ambiguity_vs_implicit_development, empirical, 'Whether conciliar ambiguity reflects strategic compromise or genuine implicit-doctrine explication.').

omega_variable(
    kernel_framing_authority_vs_text,
    'Is the operative kernel here the conciliar texts themselves, or the subsequent magisterial authority to declare an ''authentic'' reading of those texts — and does the continuity reading actually rest on the text or on the authority claim layered above it?',
    'Compare classification under a framing where the kernel is the conciliar text corpus alone (bracketing subsequent magisterial commentary) versus a framing where the kernel includes the post-conciliar magisterium''s interpretive statements (Benedict XVI 2005, subsequent CDF documents) as part of the operative kernel.',
    'Under the text-only framing, ε on doctrinal content might be genuinely near-zero (an honest description of textual continuity where it holds). Under the authority-inclusive framing, a substantial share of what is being defended is the magisterium''s own claim to definitive interpretive authority, which raises suppression and enforcement dependence, since the more contestable proposition is not ''the text is continuous'' but ''we alone say authoritatively what the text means.'' This story adopts the authority-inclusive framing (the reading includes the interpretive-supremacy claim) because that is how the continuity reading actually operates institutionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_authority_vs_text, conceptual, 'Whether the kernel is the conciliar text or the magisterial authority to interpret it.').

omega_variable(
    implementation_error_vs_conciliar_intent_boundary,
    'Where exactly is the line between ''faithful implementation of the Council''s true intent'' and ''implementation error/abuse'' — and who has standing to draw that line?',
    'Systematic comparison of specific post-conciliar changes (liturgical reform scope, catechetical content shifts, collapse in Mass attendance and vocations across the reading''s geographic range) against explicit conciliar text to determine which changes the documents actually mandate versus which were downstream implementation choices not required by the text.',
    'If the line is drawn entirely by the same magisterial authority whose legitimacy depends on the continuity reading holding, the ''implementation error'' category functions as an unfalsifiable escape valve that absorbs any counterevidence rather than a genuine empirical boundary — this would push the reading''s effective suppression and theater ratio higher than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_error_vs_conciliar_intent_boundary, conceptual, 'Whether the implementation-error/conciliar-intent boundary is principled or an unfalsifiable escape valve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1988, 0.32).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.36).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2013, 0.37).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1978, 0.22).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1988, 0.24).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2013, 0.27).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2025, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1978, 0.36).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2013, 0.41).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Four constraints form the vatican_ii_doctrinal_authority kernel family: continuity_reading (this story — low doctrinal ε, tangled_rope, magisterial authority as agenda_setter), rupture_progressive_reading (expected higher ε on institutional resistance to reform, likely rope or tangled_rope from the progressive seat, snare-adjacent from the traditionalist seat), rupture_traditionalist_reading (expected high ε on doctrinal betrayal, likely snare or tangled_rope with curial authority as an extractive suppressor of legitimate dissent), and composite_overdetermination_reading (treats the whole kernel as several independently-moving structural shifts rather than one interpretive dispute). Each reading shares the same underlying textual/historical record but authors a distinct ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint viewed from different angles but four structurally distinct constraints linked by common subject matter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
