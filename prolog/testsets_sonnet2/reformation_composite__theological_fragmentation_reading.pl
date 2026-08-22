% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Confessional Fragmentation as Doctrinal Necessity (Theological Reading of the Reformation)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the theological-fragmentation reading of the
 *   Reformation kernel: the claim that competing soteriological commitments
 *   (justification, sacramental theology, ecclesiological authority) are the
 *   primary causal engine generating structurally incompatible denominations,
 *   with confessional documents as the constraint artifacts and
 *   denominational leadership/theologians as the concentrated beneficiaries
 *   of the resulting boundary-drawing. This is one of three sibling readings
 *   of the same historical kernel (political_realignment_reading,
 *   technological_mediation_reading) and is authored as a self-contained,
 *   ε-invariant constraint — it does not describe or average over the sibling
 *   readings, it only asserts what the theological account itself claims and
 *   produces. The rising suppression trajectory (0.35 to 0.80 through the
 *   Wars of Religion era, then partial relaxation) tracks the historical
 *   hardening of confessional boundaries from the 1520s through the Peace of
 *   Westphalia (1648) and beyond, with theater ratio climbing as later
 *   doctrinal disputes increasingly served institutional identity-maintenance
 *   rather than live soteriological urgency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.58).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.71).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Confessional Fragmentation as Doctrinal Necessity (Theological Reading of the Reformation)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, 'f920e807-48a4-4a85-9c14-247fe5ac91a3').
narrative_ontology:cs_kernel_codification('f920e807-48a4-4a85-9c14-247fe5ac91a3', formalized).
narrative_ontology:cs_authority_grounding('f920e807-48a4-4a85-9c14-247fe5ac91a3', lineage).
narrative_ontology:cs_interpretation_layer_present('f920e807-48a4-4a85-9c14-247fe5ac91a3').
narrative_ontology:cs_reading_relation('f920e807-48a4-4a85-9c14-247fe5ac91a3', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('f920e807-48a4-4a85-9c14-247fe5ac91a3', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('f920e807-48a4-4a85-9c14-247fe5ac91a3', foundational, soteriological_disagreement_is_salvifically_decisive).
narrative_ontology:cs_axiom_status(soteriological_disagreement_is_salvifically_decisive, holdable).
narrative_ontology:cs_axiom_grounding('f920e807-48a4-4a85-9c14-247fe5ac91a3', soteriological_disagreement_is_salvifically_decisive, theological).
narrative_ontology:cs_axiom('f920e807-48a4-4a85-9c14-247fe5ac91a3', secondary, confessional_orthodoxy_is_necessary_condition_of_valid_communion).
narrative_ontology:cs_axiom_status(confessional_orthodoxy_is_necessary_condition_of_valid_communion, holdable).
narrative_ontology:cs_axiom_grounding('f920e807-48a4-4a85-9c14-247fe5ac91a3', confessional_orthodoxy_is_necessary_condition_of_valid_communion, conventional).
narrative_ontology:cs_reference_frame('f920e807-48a4-4a85-9c14-247fe5ac91a3', undivided_apostolic_church_doctrine).
narrative_ontology:cs_drift_state('f920e807-48a4-4a85-9c14-247fe5ac91a3', post_confessionalization_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('f920e807-48a4-4a85-9c14-247fe5ac91a3', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, territorial_state_churches).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religious_dissenters_within_confessions).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, mixed_confession_households).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, excommunicated_minorities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_believers_denied_cross_confessional_communion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, lay_believers_denied_cross_confessional_communion).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, confessional_orthodoxy_as_salvific_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, superintendents, and consistories draft and enforce confessional documents (Augsburg Confession, Formula of Concord, Canons of Dort, Tridentine decrees) that define who counts as orthodox within their jurisdiction. They administer excommunication, control ordination, and command loyalty and material support from congregations who accept the confession as a condition of communion. Their institutional survival depends on the confession being treated as doctrinally non-negotiable rather than as one contestable reading among several.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% University faculties and court theologians build careers, patronage, and intellectual authority on defending a specific soteriological position (justification by faith alone, transubstantiation vs. real presence vs. memorialism, predestination) as the decisive line between salvation and damnation. Their professional standing depends on the doctrinal disagreement remaining irreconcilable; a negotiated synthesis would dissolve their specialized authority.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theologians, beneficiary,
    organized, generational, mobile, continental).

% Cuius regio, eius religio arrangements let territorial rulers and their established churches collect tithes, control education and marriage law, and monopolize religious legitimacy within their borders by making confessional adherence a marker of political loyalty. The theological framing of the split lets them claim their arrangement is a matter of eternal truth rather than administrative convenience.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, territorial_state_churches, beneficiary,
    institutional, civilizational, arbitrage, national).

% Anabaptists, spiritualists, and internal reformers who hold soteriological positions that fit none of the consolidating confessions are branded heretics by multiple sides simultaneously. They face execution, exile, or forced conformity because the confessional boundaries drawn by leadership leave no space for their reading of scripture. Exit from one confession typically means persecution by the others, not tolerance.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religious_dissenters_within_confessions, payer,
    powerless, biographical, trapped, local).

% Families spanning Catholic and Protestant, or Lutheran and Reformed, lines face denial of communion, contested child baptism and marriage validity, and social ostracism because confessional documents treat cross-confessional practice as doctrinal contamination rather than as a private matter of conscience. Their exit option is confessional conversion, which itself carries severe social and legal costs.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, mixed_confession_households, payer,
    powerless, biographical, trapped, local).

% Congregants who dissent from a confession's official soteriology after it hardens are formally cut off from sacraments, inheritance protections tied to church membership, and often civic standing. Their bodily and material welfare is made contingent on accepting a specific theological formula they did not have a meaningful hand in drafting.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, excommunicated_minorities, payer,
    powerless, biographical, trapped, regional).

% Ordinary parishioners gain a clearer, more legible communal identity and pastoral structure from confessional consolidation, but pay for it through the closure of intermarriage, trade guild membership, and burial rights across confessional lines. Their spiritual life is genuinely shaped by real doctrinal conviction, which is why the coordination function is not pure pretense — but the boundary is enforced well past what personal conviction alone would require.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_believers_denied_cross_confessional_communion, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, lay_believers_denied_cross_confessional_communion, beneficiary).

% Figures like Melanchthon (in his more irenic moods), Bucer, and later Reformed-Lutheran unionists who sought doctrinal accommodation across the soteriological divides were marginalized by both consolidating orthodoxies. Their proposals for shared communion or minimal-doctrine confederation rarely reached the confession-drafting table, because both sides' institutional incentives ran toward sharpening rather than softening the boundary.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenically_minded_clergy, excluded,
    moderate, generational, constrained, continental).

% Examine confessional documents, disputation records, and correspondence to assess how much of the doctrinal disagreement was substantively irreconcilable versus how much tracked political and institutional incentives to harden boundaries. Their reconstructions inform whether the theological reading is treated as the primary causal account or as one lens among several.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, later_historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__theological_fragmentation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__theological_fragmentation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Confessional documents solve a genuine problem: they give scattered congregations, pastors, and territories a stable, transmissible statement of what they believe, enabling coordinated worship, catechesis, ordination standards, and mutual recognition across distance and generations — a real coordination function for those who hold the doctrine in good faith.
% TRANSFER_FUNCTION: Moves authority to interpret scripture and administer sacraments from a universal church hierarchy to competing confessional hierarchies, each claiming the same eternal warrant. Moves social, legal, and material standing from those who fall outside the winning formulation (dissenters, mixed-confession families, minorities) to those who administer and defend it (leadership, theologians, territorial churches), via exclusion from communion, marriage validity, inheritance, and civic recognition.
% ABSENT_VOICES: Anabaptists, spiritualists, and other radical reformers were excluded from the confession-drafting processes of both Catholic and magisterial Protestant authorities and were persecuted by all sides; irenic theologians seeking doctrinal minimalism were marginalized within their own confessions once consolidation accelerated. Their objection — that the soteriological disputes were being hardened into identity markers beyond what scripture itself required — went largely unheard in the documents that became authoritative.
% DISAPPEARANCE_RATIONALE: If the specific theological reading of the split (as opposed to the political or technological readings) were removed — i.e., if the doctrinal content of the disputes were genuinely negotiable rather than treated as salvifically decisive — confessional boundaries would lose their claimed metaphysical necessity, cross-confessional marriage and communion bans would lose their justification, and denominational leadership's authority to exclude and excommunicate on doctrinal grounds would collapse into a much thinner administrative function. The territorial and institutional structures might well persist for political reasons, but the theological fragmentation account is specifically about the claim that the doctrinal incompatibility itself is real and binding — remove that claim and the exclusionary machinery loses its warrant.
% FOUNDING_PROBLEM: Genuine, contested disagreement over how salvation is obtained (faith vs. faith-and-works, the nature of Christ's presence in the Eucharist, the locus of interpretive authority over scripture) that participants on multiple sides believed had eternal stakes and could not be resolved by mere accommodation.
% FOUNDING_PROBLEM_CORROBORATION: Confessional leadership and theologians on all sides attest the doctrinal disagreements are live and salvifically decisive — but this is testimony from the parties who benefit from the boundary's continuation. Outside corroboration is mixed: comparative historical theology (e.g., modern ecumenical dialogues, the Lutheran-Catholic Joint Declaration on Justification, 1999) suggests substantial portions of the original disputes rested on terminological and political factors as much as irreducible doctrinal incompatibility, while other portions (real presence vs. memorialism, predestination) remain genuinely unresolved even among sympathetic modern theologians outside any single confession's institutional interest.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.71) are authored at moderate-to-high levels because the theological reading, taken on its own terms, still requires an account of why doctrinal disagreement escalated into excommunication, exile, and religious war rather than remaining a matter of internal conscience or scholarly dispute — the escalation required active enforcement (confessional oaths, inquisitorial and consistorial discipline, exclusion from sacraments and civic life) that exceeds what genuine theological conviction alone would produce. Accessibility collapse (0.62) reflects that once a confession consolidated, alternate readings of the same scriptural material became practically inaccessible to ordinary believers, though not as totally as a mountain-type collapse. Resistance (0.74) captures sustained internal and external pushback — radical reformers, ecumenists, and eventually toleration movements — that a genuine natural-law constraint would not meet.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership, confessional theologians, and territorial state churches sit near the beneficiary end: they administer the boundary, derive authority and material support from its maintenance, and have mobile-to-arbitrage exit options (they can relocate, seek patronage elsewhere, or adapt doctrine incrementally without losing standing). Dissenters, mixed-confession households, and excommunicated minorities sit near the target end: trapped exit options, powerless structural position, and the confession's operation is what directly produces their exclusion from communion, marriage validity, and civic standing. Lay believers get a secondary beneficiary role because genuine coordination value (clear communal identity, pastoral structure) is real for them even as they pay costs at the boundary's edges — this dual role is why the constraint is tangled rather than a pure snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding theological problem (genuine disagreement about salvation and authority) was live and urgent in the 1520s-1540s; by the confessionalization era's later stages, much of the doctrinal machinery had ossified into identity-maintenance for institutions whose survival now depended on confessional distinctiveness rather than on the disagreement remaining actually unresolved (per the founding_problem_status: contested, with the Joint Declaration on Justification later suggesting some 'incompatibilities' were substantially reconcilable). Classifying this as tangled_rope rather than pure snare prevents mislabeling: the coordination function (stable shared doctrine enabling worship, catechesis, mutual recognition) is real and was never simply cover, which is why lay believers retain a genuine beneficiary interest even as the enforcement apparatus produced real victims among dissenters and cross-confessional families.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'Were the soteriological disputes themselves a sufficient cause of denominational fragmentation, or did they function primarily as post-hoc justification for political sovereignty assertions and territorial consolidation that would have occurred anyway?',
    'Comparative analysis of cases where doctrinal disputes existed without producing lasting institutional fragmentation (e.g., internal Catholic theological schools of thought that never schismed) versus cases where fragmentation tracked political boundaries more closely than doctrinal boundaries (e.g., the near-identical theology of some Reformed and some Anglican positions that nonetheless institutionalized separately along political lines).',
    'If political factors were doing most of the causal work, this reading''s claimed_type and extraction profile should be read as an overstatement of the theological account''s independent causal force — the political_realignment_reading would carry more of the true extraction, and this reading''s ε would need re-examination as partly an artifact of retrospective theological self-narration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Whether doctrinal disagreement was causally primary or instrumentalized by political actors.').

omega_variable(
    irreducible_vs_negotiable_doctrine,
    'Were the core soteriological disagreements (justification, real presence, predestination) genuinely logically incompatible positions, or were they substantially terminological/emphasis differences later hardened by institutional incentive into apparent incompatibility?',
    'Modern ecumenical theological scholarship (e.g., the 1999 Lutheran-Catholic Joint Declaration on Justification) as a natural experiment: where reconciliation proved possible after centuries, the original ''incompatibility'' was likely partly institutional rather than purely doctrinal; where it remains unresolved even among good-faith cross-confessional theologians, treat the doctrinal incompatibility as more genuinely irreducible.',
    'A finding of substantial negotiability would support classifying more of this constraint''s operation as false-summit-style extraction dressed as doctrinal necessity; a finding of genuine irreducibility on core points (e.g., real presence) would support treating a larger share of the boundary-maintenance as authentic coordination cost rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreducible_vs_negotiable_doctrine, empirical, 'Whether the theological disagreements were substantively irreconcilable or institutionally hardened.').

omega_variable(
    cs_framing_kernel_or_text,
    'Should the commitment-system kernel here be read as ''scripture as the fixed text under contested interpretation'' (fixed_text framing) or as ''the confessional document itself as the formalized kernel each denomination subsequently defends'' (formalized framing)? These produce different accounts of where the authority-grounding sits.',
    'Track whether disputes within a given confession''s later history return to scripture directly (supporting fixed_text) or are resolved/adjudicated primarily by reference to the confession''s own text and its authorized interpretive tradition (supporting formalized+lineage).',
    'Under fixed_text framing, sola scriptura movements would show more distributed/practice-grounded authority and less lineage-grounded interpretation; under formalized framing, denominational authority is more clearly extraction-grounded through confessional-text control. This story adopts the formalized+lineage framing because denominational leadership''s actual enforcement mechanism (excommunication, ordination gatekeeping) operates on the confession, not on unmediated scriptural interpretation — but the alternative framing would shift interpretation_layer_present and could route more authority toward a practice-grounded classification for radical/spiritualist wings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_or_text, conceptual, 'Alternative CS framings: scripture-as-kernel versus confession-as-kernel, and their differing authority-grounding implications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__theological_fragmentation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__theological_fragmentation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__theological_fragmentation_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(refo_tr_t65, reformation_composite__theological_fragmentation_reading, theater_ratio, 65, 0.4).
narrative_ontology:measurement(refo_tr_t90, reformation_composite__theological_fragmentation_reading, theater_ratio, 90, 0.44).
narrative_ontology:measurement(refo_tr_t110, reformation_composite__theological_fragmentation_reading, theater_ratio, 110, 0.4).
narrative_ontology:measurement(refo_tr_t130, reformation_composite__theological_fragmentation_reading, theater_ratio, 130, 0.42).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__theological_fragmentation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(refo_be_t20, reformation_composite__theological_fragmentation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(refo_be_t40, reformation_composite__theological_fragmentation_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(refo_be_t65, reformation_composite__theological_fragmentation_reading, base_extractiveness, 65, 0.6).
narrative_ontology:measurement(refo_be_t90, reformation_composite__theological_fragmentation_reading, base_extractiveness, 90, 0.57).
narrative_ontology:measurement(refo_be_t110, reformation_composite__theological_fragmentation_reading, base_extractiveness, 110, 0.5).
narrative_ontology:measurement(refo_be_t130, reformation_composite__theological_fragmentation_reading, base_extractiveness, 130, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__theological_fragmentation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(refo_su_t20, reformation_composite__theological_fragmentation_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(refo_su_t40, reformation_composite__theological_fragmentation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(refo_su_t65, reformation_composite__theological_fragmentation_reading, suppression_requirement, 65, 0.8).
narrative_ontology:measurement(refo_su_t90, reformation_composite__theological_fragmentation_reading, suppression_requirement, 90, 0.75).
narrative_ontology:measurement(refo_su_t110, reformation_composite__theological_fragmentation_reading, suppression_requirement, 110, 0.68).
narrative_ontology:measurement(refo_su_t130, reformation_composite__theological_fragmentation_reading, suppression_requirement, 130, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.08).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language label 'the Reformation' per the epsilon-invariance principle. The theological_fragmentation_reading treats doctrinal content as primary causal engine (epsilon = 0.58, tangled_rope). The political_realignment_reading treats sovereignty assertion as primary (expected to carry different, likely higher, extraction concentrated on subject populations of newly-consolidated confessional states). The technological_mediation_reading treats the printing press as the mechanism converting local dissent into continental movement (expected lower extraction, closer to rope/mountain — infrastructural enablement rather than direct extraction). All three are linked here; each authors its own epsilon and stakeholder structure rather than averaging across observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
