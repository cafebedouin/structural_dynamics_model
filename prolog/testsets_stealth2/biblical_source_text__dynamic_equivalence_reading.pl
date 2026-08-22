% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Regime in Bible Translation (Communicative Primacy Reading)
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel biblical_source_text:
 *   the dynamic_equivalence_reading, under which the source text is treated
 *   as a communicative act whose message must function in the target
 *   language, with structural fidelity subordinated to intelligibility and
 *   pastoral mission. The standing arrangement under contest (the epsilon
 *   referent) is the translation-governance regime built on this principle:
 *   agency-set standards, consultant review, translator certification, and
 *   committee-mediated rendering across hundreds of language projects. The
 *   sibling readings (formal_equivalence_reading,
 *   critical_reconstructive_reading) are separate constraint files with their
 *   own epsilon values, beneficiary/victim structures, and classifications;
 *   they are not averaged into this one. Under this reading's own lights,
 *   much of the cost the regime imposes is weighted as mission-necessary
 *   rather than gratuitous, which is why epsilon lands moderate rather than
 *   high even though identifiable parties clearly pay. The claimed type
 *   (tangled_rope) and the metrics were authored independently: the claim
 *   states the structure I believe true (real coordination plus real
 *   asymmetric extraction), the metrics describe the regime's actual
 *   operation, and the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - - translation_agency_standards_bodies: Agenda setter (institutional/arbitrage) — administers the standard, captures programmatic control and interpretive gatekeeping
 *   - - lay_target_language_readers: Primary beneficiary (powerless/constrained) — receives accessible text, sets nothing
 *   - - target_language_churches: Dual-positioned (moderate/constrained) — gains vernacular scripture, pays in interpretive dependency
 *   - - missionary_field_translators: Implementing payers (moderate/constrained) — surrender source-text intimacy to the methodology
 *   - - philological_scholars: Primary payer (organized/constrained) — lose the public text as a carrier of grammatical data
 *   - - confessional_doctrinal_traditions: Payer (organized/identity-fused) — doctrinal wording destabilized by re-rendering
 *   - - formal_equivalence_publishers: Excluded rival paradigm (organized/arbitrage) — outside the program pipeline
 *   - - translation_studies_analysts: Analytical observer (analytical/analytical) — sees the full trade-off structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.48).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.35).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Regime in Bible Translation (Communicative Primacy Reading)").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '719579b7-68b8-44f3-ac89-d6c462c0bbbd').
narrative_ontology:cs_kernel_codification('719579b7-68b8-44f3-ac89-d6c462c0bbbd', fixed_text).
narrative_ontology:cs_authority_grounding('719579b7-68b8-44f3-ac89-d6c462c0bbbd', expertise).
narrative_ontology:cs_interpretation_layer_present('719579b7-68b8-44f3-ac89-d6c462c0bbbd').
narrative_ontology:cs_reading_relation('719579b7-68b8-44f3-ac89-d6c462c0bbbd', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('719579b7-68b8-44f3-ac89-d6c462c0bbbd', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('719579b7-68b8-44f3-ac89-d6c462c0bbbd', foundational, intelligibility_outweighs_structural_fidelity).
narrative_ontology:cs_axiom_status(intelligibility_outweighs_structural_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('719579b7-68b8-44f3-ac89-d6c462c0bbbd', intelligibility_outweighs_structural_fidelity, instrumental).
narrative_ontology:cs_axiom('719579b7-68b8-44f3-ac89-d6c462c0bbbd', secondary, equivalent_effect_is_the_translation_standard).
narrative_ontology:cs_axiom_status(equivalent_effect_is_the_translation_standard, holdable).
narrative_ontology:cs_axiom_grounding('719579b7-68b8-44f3-ac89-d6c462c0bbbd', equivalent_effect_is_the_translation_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('719579b7-68b8-44f3-ac89-d6c462c0bbbd', source_text_as_communicative_act).
narrative_ontology:cs_drift_state('719579b7-68b8-44f3-ac89-d6c462c0bbbd', contemporary_paradigm_pluralism, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('719579b7-68b8-44f3-ac89-d6c462c0bbbd', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_target_language_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, target_language_churches).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, philological_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, confessional_doctrinal_traditions).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, missionary_field_translators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_field_translators).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, target_language_churches).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, nida_equivalent_effect_principle).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, receptor_response_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the translation principles every affiliated project must follow, trains and certifies translators, reviews drafts through consultant checkpoints, and controls publication funding. Grew out of the mid-century missionary effort to translate scripture into hundreds of languages; its methodology, staff, and donor base all depend on the communicative-effectiveness standard remaining the governing one. Exit would mean dismantling its own training and review apparatus.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_agency_standards_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive scripture in their own language in a form they can actually understand: idioms rendered naturally, difficult terms glossed for sense. In most of the languages served there are few or no alternative translations, so what the committees produce is largely what is available to read.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_target_language_readers, beneficiary,
    powerless, biographical, constrained, global).

% Gain a usable vernacular Bible around which worship and teaching organize. At the same time they become dependent on foreign-produced renderings: the reasoning behind a disputed passage sits in committee notes in another country, and local teachers cannot easily check a rendering against Hebrew or Greek grammar. They carry that dependency alongside the access they gain.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, target_language_churches, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, target_language_churches, payer).

% Do the drafting work under the standard: trained to ask first what the passage communicates, discouraged from preserving source phrasing when it reads oddly. Many entered the work out of attachment to the text's detail and experience the discipline as a loss; remaining in the mission network means working inside the methodology.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_field_translators, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, missionary_field_translators, beneficiary).

% Study the biblical languages for their own sake (morphology, lexicon, syntax) and teach through translations. As communicative renderings replace structural ones in pews and classrooms, the public text increasingly hides the grammatical data their work depends on, and students arrive with less exposure to it. Direct access to the original languages softens but does not remove the cost.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, philological_scholars, payer,
    organized, generational, constrained, continental).

% Anchor doctrinal formulation in specific wordings: covenant terms, justification language, prophetic verbs. When committees re-render those passages for naturalness, the verbal anchors move, and each revision forces renegotiation of teaching materials, hymns, and memory verses. Received wording is fused with the tradition's self-concept; some wings respond by treating older translations as effectively canonical.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, confessional_doctrinal_traditions, payer,
    organized, generational, identity_locked, global).

% Publish translations built on the opposite priority: preserve structure and let readers handle difficulty through teaching. They sell successfully in markets the mission programs do not serve, but they sit outside the translation-agency pipeline with no consultant review seats and no share of program funding, and agency training presents their approach as the problem the standard was invented to solve.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_publishers, excluded,
    organized, generational, arbitrage, global).

% Study translation theory and Bible-translation history from outside any program. They observe the trade-offs the standard makes, the disputes it generates, and the drift in its own methodology, without collecting from or paying into the arrangement.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, translation_agency_standards_bodies).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production of scriptural texts that communicate across radical linguistic and cultural distance: translator training, drafting conventions, consultant review, and publication are standardized once, centrally, so that hundreds of language projects proceed without each reinventing methodology.
% TRANSFER_FUNCTION: Moves interpretive authority from source-language-literate specialists to translation committees, and moves the text's communicative effect to lay readers while moving morphological and lexical data out of general circulation.
% ABSENT_VOICES: Target-language reader communities rarely hold seats on translation policy boards; philologists objecting to precision loss are heard as elite preference and overruled by mission metrics; formal-equivalence advocates stand outside the program's funding and training pipeline entirely.
% DISAPPEARANCE_RATIONALE: Hundreds of in-progress translation projects would lose their governing methodology overnight; consultant review, translator-training curricula, and publication pipelines would stall pending a replacement standard. Completed translations would persist on shelves, but the production system would reorganize around whatever successor regime emerged.
% FOUNDING_PROBLEM: Mid-twentieth-century missionary translation repeatedly produced structurally faithful renderings that target audiences could not understand: idioms, rhetorical forms, and kinship terms carried across literally obscured the message. The dynamic-equivalence program was built to solve the failure of form-first translation to communicate.
% FOUNDING_PROBLEM_CORROBORATION: Field comprehension testing and literacy research conducted outside the translation agencies corroborate that literal renderings frequently fail comprehension. Notably, formal-equivalence advocates themselves concede the communication problem while disputing the remedy, so the founding problem is attested from the opposing camp, not merely by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end): the regime delivers genuine communication at scale, but it systematically trades away morphological and lexical precision and concentrates interpretive judgment in committees whose reasoning is not visible in the product. Suppression is moderate-low (0.35) and mostly structural rather than coercive: enforcement runs through training, certification, consultant checkpoints, and funding gates, while formal translations, interlinears, and the original languages remain legally and commercially available, so alternatives persist outside the program. Theater_ratio (0.28) reflects a partly performative layer: reader-comprehension testing began as genuine engineering and has partially become a legitimating ritual that converts committee judgment into apparent receptor consensus. Accessibility_collapse (0.40) is well below mountain range because the alternatives do not fully collapse once the regime is understood; resistance (0.50) is substantial and documented: the gender-inclusive-language controversies, the 1997 Colorado Springs Guidelines coalition of scholars and confessional wings (an instance of coalition power among organized payers), liturgical mandates pushing back toward formal fidelity, and continuing scholarly critique. The measurement series run on one shared time grid (all three metrics at all seven points) so no metric's end-state is silently substituted into earlier rows. Boltzmann coordination_type is information_standard: the regime's primary function is encoding meaning reliably into target languages under a shared methodology, with correspondingly low inherent coordination cost.
 *
 * PERSPECTIVAL GAP:
 *   Three seats should compute materially different types from the same structure. From the agency seat the arrangement is a hard-won communicative technology it built, maintains, and legitimately administers. From the philologist seat it is the progressive devaluation of the only public artifact carrying the grammatical data the discipline runs on. From the confessional seat it is recurring instability in the verbal deposit that doctrine and memory ride on, and that seat's exit is identity_locked rather than merely constrained: received wording is fused with the tradition's self-concept, so switching renderings is experienced as a threat to identity, not a consumer choice. If that identity frame broke (if traditions came to hold wording instrumentally), the confessional seat's effective extraction would drop sharply and resistance would fall with it. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (lay_target_language_readers, target_language_churches) derive low directionality; declared victims (philological_scholars, confessional_doctrinal_traditions, missionary_field_translators) derive high directionality, amplified for the identity-locked confessional seat and damped for scholars by their partial exit into the original languages. Two overrides correct places where the derivation would err. First, institutional d=0.20: the standards bodies carry no beneficiary/victim declaration, so derivation would treat them as neutral administrators near symmetric, but the receipt surface establishes that the arrangement's gains (programmatic control, interpretive gatekeeping, donor-funded growth) demonstrably accrue to that seat, placing it near the beneficiary end. Second, powerless d=0.08: lay readers' constrained exit reflects thin translation markets in minority languages, not costs borne under the arrangement; without the pin, a trapped-exit profile could be misread as exploitation rather than subsidized access.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves what is real: the coordination achievement (first-ever accessible scripture in hundreds of languages) is genuine, and a snare verdict would erase the people the regime visibly serves. Refusing a rope verdict preserves what is also real: the same structure that coordinates extraction-free access concentrates interpretive authority and degrades the precision substrate, with named payers. The founding problem (form-first translation fails to communicate) remains live, attested by opponents as well as beneficiaries, so no mandatrophy declaration is warranted: the mandate has not outlived its function. The open risk runs the other direction: if the communication problem were ever solved by means that do not require sacrificing structure, the regime's persistence would become inertial, and the theater_ratio trajectory (rising steadily from 0.12 to 0.28) is the early indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel biblical_source_text (reading: dynamic_equivalence_reading). What would the formal_equivalence_reading change structurally if instantiated instead?',
    'Compile and classify the sibling stories and compare per-seat outputs: the formal reading inverts the beneficiary/victim structure (lay readers bear the comprehension burden through teaching; scholars and structure-dependent traditions become beneficiaries), while the critical_reconstructive_reading suspends both until the textual basis is settled.',
    'The epsilon referent (the translation-governance arrangement) stays fixed across readings, but its assessment flips: what this reading prices as mission-necessary cost, the formal reading prices as abdication of fidelity, and the victim set of this file becomes the beneficiary set of the sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    morphological_loss_cost_attribution,
    'Is the loss of morphological and lexical precision an extraction imposed on scholarship and confessional precision, or an unavoidable coordination cost of cross-lingual communication?',
    'Comprehension studies comparing precision-preserving renderings against communicative renderings in matched reader populations: if intelligibility holds when structure is preserved, the sacrifice is gratuitous and therefore extractive; if intelligibility genuinely fails, the loss is the price of the coordination itself.',
    'If the loss is gratuitous, epsilon rises toward the snare boundary and the coordination story loses its cover-function defense; if necessary, the measured extraction is substantially coordination cost and the tangled_rope verdict stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphological_loss_cost_attribution, empirical, 'Whether the precision the regime sacrifices is dispensable or constitutive of its communication function.').

omega_variable(
    committee_interpretive_concentration,
    'Does committee mediation of the text constitute concentrated interpretive authority extracted from reader communities, or a diffuse division of labor readers freely accept?',
    'Track doctrinal divergence rates and teacher dependence patterns across translation regimes: compare how target-language churches resolve disputed passages when their text is committee-rendered versus structurally transparent, and whether local exegetical capacity grows or atrophies under each.',
    'Strong concentration with atrophying local capacity supports drift toward snare (interpretive rents collected behind a communication front); diffuse mediation with growing local capacity supports a rope-leaning reading of the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committee_interpretive_concentration, conceptual, 'Whether the regime''s interpretive gatekeeping is a rent or a service.').

omega_variable(
    post_synthesis_persistence_basis,
    'After the optimal-equivalence synthesis softened the original doctrine, does the regime persist by demonstrated superiority or by institutional inertia (training pipelines, funding structures, sunk translation inventory)?',
    'Adoption patterns among new translation projects unaffiliated with legacy agencies: if unattached projects freely choose the standard, persistence tracks merit; if they choose it mainly where agency funding reaches, inertia dominates.',
    'Inertia-dominant persistence, combined with the already-rising theater_ratio, would signal piton drift: a formerly functional standard maintained theatrically by the institutions it built.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_synthesis_persistence_basis, empirical, 'Merit versus inertia as the basis of the regime''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.36).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, information_standard).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how the biblical source text governs translation' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. formal_equivalence_reading (structure primary) and this file (communication primary) carry different epsilon values and inverted beneficiary/victim structures; critical_reconstructive_reading (text recovery first) suspends the structure/meaning question entirely. They are linked because the dynamic regime's success alters the resource environment of its siblings: mass communicative translation shrinks the philological base that critical reconstruction depends on and competes with formal-equivalence publishing for the same reader markets. Upstream/downstream is contested between the siblings; the edges here record this reading's downstream pressure on both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, institutional, 0.2).
constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, powerless, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
