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
 *   human_readable: Dynamic Equivalence Regime for Scripture Translation
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'biblical_source_text': the dynamic-equivalence reading, under which a
 *   translation's legitimacy is measured by its communicative effect on its
 *   audience, and fidelity to source-language structure is a subordinate duty
 *   owed to intelligibility and pastoral mission. Under this regime,
 *   translation committees test drafts against reader comprehension, discard
 *   source constructions that impede understanding, and treat formal
 *   correspondence as expendable where the two conflict. The claim/metric gap
 *   is deliberate: the regime presents itself as pure service to the reader,
 *   while the authored metrics record a genuine coordination achievement
 *   carrying real asymmetric costs — morphological and syntactic precision is
 *   destroyed in the rendering, and the destruction lands on identifiable
 *   specialist populations. Sibling readings (formal-equivalence and
 *   critical-reconstructive) are separate constraint stories with their own
 *   epsilon values and seat structures; they are linked through the network
 *   surface, not averaged into this one.
 *
 * KEY AGENTS:
 *   - dynamic_equivalence_translation_agencies: agenda-setter (institutional/arbitrage) — sets and enforces the communicative-primacy standard, collects funding and authority
 *   - lay_vernacular_readers: primary beneficiary (moderate/mobile) — receives accessible text, can switch translations freely
 *   - first_translation_language_communities: beneficiary with trapped exposure (powerless/trapped) — receives first-ever access and inherits irreversible renderings
 *   - missionary_evangelism_movements: beneficiary (organized/constrained) — depends on immediate comprehensibility for field programs
 *   - word_study_scholars: primary target (organized/constrained) — bears the loss of morphological and syntactic precision
 *   - original_language_seminary_faculty: dual-positioned (organized/constrained) — bears remedial teaching costs while the gap sustains demand for their courses
 *   - confessional_dogmaticians: excluded voice (organized/identity_locked) — objects to doctrinal wording shifts from outside the deliberation
 *   - translation_theory_analysts: analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.48).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Regime for Scripture Translation").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '1a30b8a5-a19e-4c89-abb6-db347762d8fd').
narrative_ontology:cs_kernel_codification('1a30b8a5-a19e-4c89-abb6-db347762d8fd', fixed_text).
narrative_ontology:cs_authority_grounding('1a30b8a5-a19e-4c89-abb6-db347762d8fd', expertise).
narrative_ontology:cs_interpretation_layer_present('1a30b8a5-a19e-4c89-abb6-db347762d8fd').
narrative_ontology:cs_reading_relation('1a30b8a5-a19e-4c89-abb6-db347762d8fd', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('1a30b8a5-a19e-4c89-abb6-db347762d8fd', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('1a30b8a5-a19e-4c89-abb6-db347762d8fd', foundational, communicative_effect_constitutes_fidelity).
narrative_ontology:cs_axiom_status(communicative_effect_constitutes_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('1a30b8a5-a19e-4c89-abb6-db347762d8fd', communicative_effect_constitutes_fidelity, instrumental).
narrative_ontology:cs_axiom('1a30b8a5-a19e-4c89-abb6-db347762d8fd', secondary, source_form_subordinate_to_pastoral_access).
narrative_ontology:cs_axiom_status(source_form_subordinate_to_pastoral_access, holdable).
narrative_ontology:cs_axiom_grounding('1a30b8a5-a19e-4c89-abb6-db347762d8fd', source_form_subordinate_to_pastoral_access, deontological).
narrative_ontology:cs_reference_frame('1a30b8a5-a19e-4c89-abb6-db347762d8fd', scripture_as_communicative_instrument).
narrative_ontology:cs_drift_state('1a30b8a5-a19e-4c89-abb6-db347762d8fd', contemporary_paratext_rich_editions, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a30b8a5-a19e-4c89-abb6-db347762d8fd', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_vernacular_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, first_translation_language_communities).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_evangelism_movements).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, original_language_seminary_faculty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, original_language_seminary_faculty).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, first_translation_language_communities).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, nida_functional_equivalence_theory).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, audience_effect_criterion_of_translation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set translation policy for member projects worldwide and fund, staff, and review translations through consultant checkpoints that test drafts for audience comprehension rather than formal correspondence to source syntax. Collect project funding, consultant employment, and institutional authority tied to producing texts that whole populations can read. Because they operate many projects across many languages, they can shift methodology between projects without abandoning the enterprise.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, dynamic_equivalence_translation_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Read scripture in everyday contemporary language without source-language training, receiving a text they can understand on first encounter. In major languages they can switch among many competing translations at will, so their reliance on any one rendering is voluntary and reversible.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_vernacular_readers, beneficiary,
    moderate, biographical, mobile, global).

% Receive the first scripture ever rendered in their language, gaining access no prior generation had. The renderings chosen by the translation team become the community's received text for generations, including every interpretive decision embedded in them. No alternative translation exists in the language to compare against, and revising the text later requires restarting a process the community cannot itself fund or staff.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, first_translation_language_communities, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, first_translation_language_communities, payer).

% Deploy translations for rapid cross-cultural communication and depend on texts that hearers grasp immediately without instruction. Field programs, literacy campaigns, and oral strategies are built around particular translations, so switching frameworks mid-program carries real programmatic cost.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_evangelism_movements, beneficiary,
    organized, biographical, constrained, continental).

% Trace doctrinal word histories through morphological and syntactic detail that vernacular renderings flatten or discard. Must return to Hebrew and Greek sources for their own work and spend effort correcting popular readings that arise from simplified phrasings. Their discipline depends on the source languages, so they cannot simply adopt the vernacular-only posture the regime recommends.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_scholars, payer,
    organized, generational, constrained, global).

% Teach Hebrew and Greek partly because vernacular texts no longer carry the precision their exegesis requires, bearing heavier remedial instruction loads for students who arrived trusting the vernacular. At the same time, the widening gap between vernacular and source text sustains institutional demand for exactly the courses they teach.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, original_language_seminary_faculty, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, original_language_seminary_faculty, beneficiary).

% Hold doctrinal formulations tied to specific wordings and object when renderings shift contested terms, but sit outside translation-committee deliberation except where their denomination directly funds a project. Their objections travel through denominational channels rather than through the review process that shapes draft texts.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, confessional_dogmaticians, excluded,
    organized, generational, identity_locked, global).

% Study equivalence paradigms comparatively, publish assessments of how translation regimes distribute gains and losses across reader and specialist populations, and take testimony from every other seat without producing or pastorally consuming the texts themselves.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_theory_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, dynamic_equivalence_translation_agencies).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of transmitting scriptural content across languages to audiences with no source-language training: one shared decision framework lets dispersed translation teams produce texts that function natively in hundreds of languages, coordinating translator behavior around demonstrated audience comprehension instead of source-form correspondence.
% TRANSFER_FUNCTION: Moves interpretive authority from source-language specialists to translation committees and target-audience reception. Precision information embedded in source morphology and syntax is traded away in the rendering; the resulting accessibility and speed of dissemination flow to lay readers and mission contexts, while the lost precision falls on scholars and on future revisers of first-translation texts.
% ABSENT_VOICES: Confessional dogmaticians concerned with doctrinal wording are largely outside committee deliberation, as are source-text philologists whose precision interests the framework treats as a secondary concern. Future generations of first-translation language communities are absent by construction: the interpretive choices they will inherit are made before they exist.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, scripture access would collapse for the large majority of the world's Christians who read only through such translations; mission programs built on immediate comprehensibility would stall; and first-translation communities would have no received text at all rather than a contested one. Publishing, liturgy, and devotional life in vernacular Christianity would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Mid-twentieth-century mass evangelization and literacy expansion confronted a readability crisis: existing literal translations were impenetrable to newly literate audiences and impractical to produce at scale across thousands of languages, while mission urgency demanded texts hearers could understand on first contact.
% FOUNDING_PROBLEM_CORROBORATION: Mission agencies attest the problem remains live, citing the roughly half of world languages still lacking any scripture. Sociolinguistic scripture-access surveys corroborate that partial persistence. Translation-studies scholarship from outside the benefiting parties documents that the original readability crisis in major languages has substantially receded through rising literacy and improved formal translations, supporting the shifted-function reading. Corroboration exists on both sides and splits along the same lines as the underlying contest.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.48): the regime genuinely delivers what it promises, but the rendering act destroys precision information that identifiable populations need, and the destruction is structural — it recurs in every project the framework governs. Suppression is moderate-low (0.38): the regime does not coerce the broader translation marketplace, where formal-equivalence products remain widely available (hence accessibility_collapse at 0.45, alternatives persist); its suppressive force operates inside adopting institutions through consultant checkpoints and agency policy. Theater is low-moderate (0.28): readability testing is real work, but a growing share of public justification consists of accuracy claims and marketing that the specialist seats dispute. Resistance (0.55) is sustained and articulate: scholarly critique, literal-translation counter-publishing, and confessional complaints are persistent features of the landscape, not episodic noise. The measurement series run on one shared nine-point grid so every tracked metric is authored at every examined time point. The suppression_requirement series is authored deliberately because this story traces enforcement-capacity history: the ratchet rose for four decades as consultant infrastructure matured and agency policy hardened, then relaxed as translation pluralism normalized and formal-equivalence products stopped being treated as rivals — a hump, not a monotone, and the end-state scalar matches the relaxed terminal value.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat computes a coordination story it built and staffs: from inside the agency, the framework is the thing that made worldwide vernacular scripture possible. The specialist payer seats compute a different structure: from the word-study desk, the same framework is a machine for discarding precisely the information the discipline exists to preserve. Between two nominally identical beneficiary seats the engine should also diverge: lay readers in major languages hold mobile exit and experience pure subsidy, while first-translation communities hold trapped exit and carry generational exposure to renderings they cannot revise — same role label, materially different directionalities. The seminary-faculty seat is internally split, paying remedial costs while collecting enrollment demand, and should compute as neither cleanly targeted nor cleanly subsidized.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low-directionality seats: lay readers (voluntary consumption, mobile exit) sit nearest the beneficiary pole; missionary movements (genuine programmatic dependence, constrained exit) sit slightly higher; first-translation communities are the hard case. The structural derivation reads them as near-pure beneficiaries, which undersells their position: trapped exit plus inherited interpretive lock-in means the arrangement's choices bind them in ways no mobile reader experiences. A directionality override lifts the powerless atom to 0.30 to record that exposure without inverting the seat — they remain net beneficiaries. Victim declarations map the scholar seats toward the target pole: word-study scholars bear the precision loss directly with no substitute inside the vernacular economy; faculty bear it with a compensating offset captured by their secondary beneficiary role. The agency seat derives near-beneficiary despite administering the arrangement, which is accurate: it collects funding and authority and bears little of the cost it imposes.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what keeps both mislabels out. Reading the regime as pure coordination (rope) erases the documented asymmetric cost: precision is not merely redistributed, it is destroyed at the point of rendering, and named populations pay for the accessibility others gain. Reading it as pure extraction (snare) erases the founding achievement: for most of the world's languages this framework produced the only scripture that has ever existed, and the coordination function is load-bearing, not cover. The R5 interview reinforces the resolution: the founding problem is contested rather than dead, and the disappearance verdict is world_rearranges, so the dead-problem-plus-rearrangement mismatch flag does not fire — the regime is not a zombie mandate but a live arrangement whose justification is genuinely disputed. Mandatrophy-resolved is accordingly NOT declared: the mandate has not outlived its function; its function has grown a second, disputed face.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_seat_inversion,
    'This constraint is one reading of kernel biblical_source_text (reading: dynamic_equivalence_reading). What would the sibling readings change structurally if adopted as the governing frame?',
    'Compile the sibling stories (biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading) and compare computed seat classifications and epsilon over the same referent arrangement.',
    'Under the formal-equivalence sibling the seat structure inverts: scholars become beneficiaries of a precision-preserving regime and lay readers bear the cost of reader-formation duties, raising effective extraction on the lay seat. Under the critical-reconstructive sibling both translation regimes are suspended pending textual establishment, dissolving this story''s beneficiary/victim topology entirely. The disagreement is located in the criterion of translational fidelity, not in any measured quantity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_seat_inversion, conceptual, 'Committer-frame ambiguity: which reading of the source-text kernel governs, and how seat structure moves across readings.').

omega_variable(
    precision_loss_irreversibility,
    'Is the precision destroyed in first-translation renderings recoverable by later revision, or does it permanently foreclose interpretive options for communities with no comparative text?',
    'Track revision projects in languages that received first translations under the regime: measure whether revised editions restore source-form distinctions or reproduce the original committee''s interpretive choices.',
    'If recovery is practical, the trapped-community exposure is a temporary cost and the beneficiary reading stands unqualified; if recovery systematically fails, the trapped seat''s effective extraction approaches the target pole and the regime''s asymmetry deepens over generational time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precision_loss_irreversibility, empirical, 'Whether first-translation interpretive lock-in is permanent or revisable.').

omega_variable(
    translator_norm_internalization,
    'Is translator conformity to communicative primacy enforced by agency checkpoint policy (structural) or absorbed during translator training such that it persists after oversight relaxes (internalized)?',
    'Compare rendering choices of translators trained inside agency pipelines against comparably skilled translators trained outside them, under matched briefs with no consultant review.',
    'If internalized, the falling suppression_requirement series overstates the relaxation — the durable suppressive force travels in the translators, and the regime''s enforcement is more resilient than the policy record suggests; if structural, the measured decay is real and pluralism is genuinely eroding enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translator_norm_internalization, empirical, 'Structural versus internalized mechanism behind translator conformity to the communicative-primacy standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(bibl_tr_t70, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 70, 0.27).
narrative_ontology:measurement(bibl_tr_t80, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement(bibl_be_t70, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 70, 0.48).
narrative_ontology:measurement(bibl_be_t80, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 80, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(bibl_su_t70, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 70, 0.41).
narrative_ontology:measurement(bibl_su_t80, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 80, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'faithful Bible translation' decomposes into three structurally distinct regimes corresponding to the three readings of the biblical_source_text kernel. This story (dynamic_equivalence) holds moderate epsilon with lay-reader beneficiaries and scholar victims; the formal-equivalence sibling inverts the seat structure; the critical-reconstructive sibling suspends both translation regimes pending textual establishment. The upstream/downstream gradient runs from critical reconstruction (most epistemically conservative) through formal equivalence to dynamic equivalence (most dissemination-oriented); each upstream claim is cited by downstream regimes as warrant. Family members are linked exclusively through network.affects_constraints; no epsilon is shared or averaged across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, powerless, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
