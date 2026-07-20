% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press and Reformation Mutual Shaping
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   The printing press and Reformation agency co-evolved during the early
 *   sixteenth century in a bidirectional causal lock-in: printers needed the
 *   controversial content that reformers supplied to open new markets, while
 *   reformers needed the speed and scale of print to outpace Catholic
 *   institutional response. This constraint story models that mutual shaping
 *   symbiosis as a historical constraint on media and religious agency. It is
 *   claimed as tangled_rope because the arrangement coordinated genuine
 *   collective action (creating a viable vernacular print market, solving the
 *   problem of dissent dissemination) while asymmetrically extracting from
 *   Catholic institutional monopoly and manuscript producers, and required
 *   active enforcement against censorship and guild restrictions to persist.
 *
 * KEY AGENTS:
 *   - reformist_printers: Primary agenda-setter (organized/constrained) â administered production, bore legal risk, captured economic surplus from controversial print.
 *   - reform_theologians: Primary beneficiary (moderate/constrained) â supplied content, gained unprecedented dissemination, but became dependent on print tempo.
 *   - vernacular_readers: Secondary beneficiary (powerless/constrained) â gained access to texts, formed a new demand constituency, but locked into the print-based information ecosystem.
 *   - catholic_hierarchy: Primary payer/victim (institutional/constrained) â lost information monopoly, bore cost of doctrinal challenge and institutional delegitimation.
 *   - manuscript_producers: Secondary payer/victim (moderate/trapped) â faced economic displacement as skills devalued by typographic reproduction.
 *   - catholic_princes: Excluded observer (institutional/constrained) â would have preferred media stability but lacked effective exit from cross-border print dynamics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.58).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.52).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, tangled_rope).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press and Reformation Mutual Shaping").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'ddd20ce0-4ae9-4b6b-843a-f72d49343859').
narrative_ontology:cs_kernel_codification('ddd20ce0-4ae9-4b6b-843a-f72d49343859', distributed).
narrative_ontology:cs_authority_grounding('ddd20ce0-4ae9-4b6b-843a-f72d49343859', expertise).
narrative_ontology:cs_interpretation_layer_present('ddd20ce0-4ae9-4b6b-843a-f72d49343859').
narrative_ontology:cs_reading_relation('ddd20ce0-4ae9-4b6b-843a-f72d49343859', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('ddd20ce0-4ae9-4b6b-843a-f72d49343859', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('ddd20ce0-4ae9-4b6b-843a-f72d49343859', foundational, technology_agency_bidirectional).
narrative_ontology:cs_axiom_status(technology_agency_bidirectional, holdable).
narrative_ontology:cs_axiom_grounding('ddd20ce0-4ae9-4b6b-843a-f72d49343859', technology_agency_bidirectional, empirically_contingent).
narrative_ontology:cs_axiom('ddd20ce0-4ae9-4b6b-843a-f72d49343859', foundational, media_transforms_through_practice).
narrative_ontology:cs_axiom_status(media_transforms_through_practice, holdable).
narrative_ontology:cs_axiom_grounding('ddd20ce0-4ae9-4b6b-843a-f72d49343859', media_transforms_through_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('ddd20ce0-4ae9-4b6b-843a-f72d49343859', medieval_media_agency_separation).
narrative_ontology:cs_drift_state('ddd20ce0-4ae9-4b6b-843a-f72d49343859', high_reformation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ddd20ce0-4ae9-4b6b-843a-f72d49343859', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformist_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reform_theologians).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated print shops and chose to publish controversial pamphlets, biblical translations, and polemics; developed new typefaces and formats for vernacular markets. Profited from high demand but faced legal peril, confiscation, and exile. Guild membership and capital investment in type and presses constrained exit to safer, lower-margin classical or liturgical work.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformist_printers, agenda_setter,
    organized, biographical, constrained, continental).

% Authored pamphlets, biblical translations, and catechisms that required rapid, wide dissemination to survive Catholic institutional response. Depended on printers for production speed and geographic reach. Once committed to print-based mobilization, returning to oral disputation or manuscript circulation meant surrendering the initiative and audience.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reform_theologians, beneficiary,
    moderate, biographical, constrained, continental).

% Gained access to religious and political texts in native languages at unprecedented scale and speed. Formed a dispersed demand constituency that shaped printer output. Alternativesârelying on priestly mediation or Latin manuscriptsâremained available but rapidly lost viability for timely, participatory religious discourse.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_readers, beneficiary,
    powerless, biographical, constrained, continental).

% Lost centuries-old information monopoly over scriptural interpretation and doctrinal gatekeeping. Faced heretical propagation at speeds manuscript systems could not match. Attempted censorship, indexing, and confiscation but could not suppress cross-border print networks. Bore the cost of institutional delegitimation and defensive mobilization.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_hierarchy, payer,
    institutional, generational, constrained, continental).

% Scribes, illuminators, and stationers faced progressive economic displacement as print replaced manuscript production for most religious, legal, and scholarly texts. Their craft skills were non-transferable to typographic production without substantial retraining and capital. Trapped in a collapsing market segment with sharply declining demand.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, manuscript_producers, payer,
    moderate, biographical, trapped, regional).

% Secular rulers who might have preferred confessional stability and media control found their territories penetrated by smuggled pamphlets and unauthorized Bibles. Their objections to disruptive print were overridden by political fragmentation, cross-border trade, and the difficulty of enforcing universal censorship. Structurally excluded from the mutual-shaping arrangement despite their power.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_princes, excluded,
    institutional, generational, constrained, continental).

narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and distribution of reformist ideas by aligning the economic interests of printers with the communicative needs of reformers, creating a viable market for vernacular religious pamphlets and Bibles where neither supply nor demand alone would have sufficed.
% TRANSFER_FUNCTION: Moves material resources (printing capital, labor, risk-bearing) and symbolic resources (legitimacy, audience attention) from Catholic manuscript institutions and displaced scribes to reformist printers, theologians, and the literate public; simultaneously transfers the costs of censorship evasion and legal jeopardy to printers and the costs of doctrinal polarization to the broader society.
% ABSENT_VOICES: Illiterate majorities and local parish clergy committed to liturgical continuity were largely excluded from the print-reform nexus; they would have argued for the sufficiency of oral transmission and sacramental mediation but lacked representation in the urban, literate print market.
% DISAPPEARANCE_RATIONALE: Without the symbiotic lock-in between reformers and printers, theological dissent would have remained localized and elite; the economic viability of vernacular controversial print would have collapsed or taken a different path; Catholic information monopoly would have persisted longer; and the early modern public sphere would not have emerged on the same trajectory.
% FOUNDING_PROBLEM: How to disseminate theological dissent and vernacular scripture widely enough to survive institutional suppression, without an existing communications infrastructure independent of the Catholic Church.
% FOUNDING_PROBLEM_CORROBORATION: Reformers like Luther and printers like Hans Lufft attested the need for rapid dissemination from within the benefiting parties. Catholic polemicists like Johannes Cochlaeus attested the problem from the opposing seat, confirming the scale of the threat. Modern historians outside the benefiting partiesâsuch as medievalists emphasizing the persistence of manuscript and oral channelsâcontest whether the problem genuinely required this specific print-mediated solution or whether reformers constructed the urgency.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-to-high: the symbiosis transferred wealth and authority from Catholic manuscript institutions to reformist print networks. Suppression (0.52) reflects active maintenance against Catholic censorship and the collapse of manuscript alternatives. Theater is low (0.25) because the coordination was functionally load-bearing, not performative. Accessibility collapse (0.68) captures how manuscript and oral alternatives rapidly lost viability for mass religious communication once the print-reform nexus was established. Resistance (0.62) reflects sustained Catholic counter-measures (Indices, inquisitions, confiscations). The measurement series show extraction and enforcement intensifying through the 1520s-30s, then plateauing as the arrangement became structurally entrenched by the mid-century.
 *
 * PERSPECTIVAL GAP:
 *   From the printer or reformer seat, the constraint appears as rope â a genuine coordination mechanism that solved the collective-action problem of dissent dissemination and created shared value. From the Catholic hierarchy or manuscript producer seat, it computes as snare â an extractive mechanism that destroyed a prior media equilibrium and transferred costs to them. The tangled_rope claim captures this structural divergence: the same arrangement is coordination for some and extraction for others, and its persistence required active enforcement against the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist printers and theologians are beneficiaries (d near 0.0) because the constraint subsidized their economic and mobilization goals. Vernacular readers are near-symmetric beneficiaries (d ~0.25) because they gained access but were also locked into the new information regime. The Catholic hierarchy and manuscript producers are victims (d near 1.0) because the constraint extracted their prior institutional and economic positions. No override is needed because the structural derivation from roles and exit options captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiaries and victims. A pure rope reading would ignore the displacement of manuscript producers and the Catholic Church's lost monopoly; a pure snare reading would ignore the genuine coordination problem solved (how to disseminate dissent without Church infrastructure). The founding problem remains contested â some historians argue it was live and solved by print, others that the problem was constructed by reformers. The contested status, combined with active enforcement and asymmetric extraction, locks the classification as tangled_rope rather than scaffold or rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_agency_boundary,
    'Is the mutual shaping dynamic an intrinsic feature of all media revolutions, or specific to the contingent political-religious context of sixteenth-century Europe?',
    'Comparative historical analysis of other media transitions (e.g., radio in the twentieth century, social media in the twenty-first) to test whether bidirectional co-evolution appears invariantly or only under specific institutional conditions.',
    'If invariant, the constraint''s type may generalize to other media revolutions as tangled_rope; if contingent, the classification remains tightly bound to early modern European conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_agency_boundary, conceptual, 'Whether mutual shaping is universal or contingent').

omega_variable(
    printer_motivation_ambiguity,
    'Were reformist printers primarily motivated by theological sympathy, market opportunity, or coercive pressure from local authorities and patrons?',
    'Archival analysis of printer account books, dedications, and correspondence to disaggregate theological, economic, and political motives.',
    'If market opportunity dominated, the coordination function was secondary to extraction and the constraint edges toward snare; if theological sympathy dominated, it edges toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_motivation_ambiguity, empirical, 'Printer motivation mix and its effect on type classification').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of Catholic manuscript alternatives achieved primarily through structural market displacement or through internalized doctrinal commitment to print?',
    'Post-Council-of-Trent trajectory analysis: if manuscript and liturgical alternatives rebounded when institutional support returned, suppression was structural; if they did not, internalized commitment to print had locked in the constraint.',
    'If internalized, effective suppression is higher than structural measures suggest and the constraint''s durability is stronger; if purely structural, the constraint might have been more reversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_mutual_tr_t0, press_reformation_causation__mutual_shaping, theater_ratio, 0, 0.1).
narrative_ontology:measurement(press_ref_mutual_tr_t5, press_reformation_causation__mutual_shaping, theater_ratio, 5, 0.12).
narrative_ontology:measurement(press_ref_mutual_tr_t10, press_reformation_causation__mutual_shaping, theater_ratio, 10, 0.15).
narrative_ontology:measurement(press_ref_mutual_tr_t15, press_reformation_causation__mutual_shaping, theater_ratio, 15, 0.18).
narrative_ontology:measurement(press_ref_mutual_tr_t20, press_reformation_causation__mutual_shaping, theater_ratio, 20, 0.2).
narrative_ontology:measurement(press_ref_mutual_tr_t25, press_reformation_causation__mutual_shaping, theater_ratio, 25, 0.22).
narrative_ontology:measurement(press_ref_mutual_tr_t30, press_reformation_causation__mutual_shaping, theater_ratio, 30, 0.24).
narrative_ontology:measurement(press_ref_mutual_tr_t35, press_reformation_causation__mutual_shaping, theater_ratio, 35, 0.25).
narrative_ontology:measurement(press_ref_mutual_tr_t40, press_reformation_causation__mutual_shaping, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(press_ref_mutual_be_t0, press_reformation_causation__mutual_shaping, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(press_ref_mutual_be_t5, press_reformation_causation__mutual_shaping, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(press_ref_mutual_be_t10, press_reformation_causation__mutual_shaping, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(press_ref_mutual_be_t15, press_reformation_causation__mutual_shaping, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(press_ref_mutual_be_t20, press_reformation_causation__mutual_shaping, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(press_ref_mutual_be_t25, press_reformation_causation__mutual_shaping, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(press_ref_mutual_be_t30, press_reformation_causation__mutual_shaping, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(press_ref_mutual_be_t35, press_reformation_causation__mutual_shaping, base_extractiveness, 35, 0.59).
narrative_ontology:measurement(press_ref_mutual_be_t40, press_reformation_causation__mutual_shaping, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(press_ref_mutual_su_t0, press_reformation_causation__mutual_shaping, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(press_ref_mutual_su_t5, press_reformation_causation__mutual_shaping, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(press_ref_mutual_su_t10, press_reformation_causation__mutual_shaping, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(press_ref_mutual_su_t15, press_reformation_causation__mutual_shaping, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(press_ref_mutual_su_t20, press_reformation_causation__mutual_shaping, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(press_ref_mutual_su_t25, press_reformation_causation__mutual_shaping, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(press_ref_mutual_su_t30, press_reformation_causation__mutual_shaping, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(press_ref_mutual_su_t35, press_reformation_causation__mutual_shaping, suppression_requirement, 35, 0.53).
narrative_ontology:measurement(press_ref_mutual_su_t40, press_reformation_causation__mutual_shaping, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the press_reformation_causation kernel, decomposed because the colloquial label 'the press caused the Reformation' conflates three structurally distinct claims. Each reading carries a distinct epsilon, beneficiary structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
