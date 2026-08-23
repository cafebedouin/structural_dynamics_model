% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Translation Norm (Biblical Source Text)
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   This story instantiates the dynamic_equivalence_reading of the
 *   biblical_source_text kernel: the standing translation norm under which
 *   communicative effectiveness in the target language is primary and
 *   fidelity to source-language structure is subordinated to intelligibility
 *   and the pastoral mission. The arrangement arose from a documented
 *   mid-century failure — form-based, literal translation method was
 *   producing texts that missionaries found unintelligible or misleading in
 *   target cultures and lay readers could not understand — and was
 *   institutionalized from the 1960s onward through Bible society
 *   methodological mandates, committee infrastructure, and publisher
 *   standards until it became the mainstream default. It solves a real
 *   collective problem at real scale while concentrating interpretive
 *   authority in translation committees and devaluing the source-language
 *   precision on which scholarly word-study work depends. The claimed type
 *   and the authored metrics are independent facts: tangled_rope is claimed
 *   from the structural analysis (genuine coordination function plus
 *   asymmetric costs borne by identifiable parties under active enforcement);
 *   the metric values are authored as descriptively true of the norm's actual
 *   operation, without tuning toward any predicted engine output. The sibling
 *   readings are separate constraint files, not positions inside this one.
 *
 * KEY AGENTS:
 *   - bible_society_publishers: agenda-setting beneficiary (institutional/arbitrage) — sets the methodological mandate, owns the translations as products, accrues the institutional and economic gains
 *   - bible_translation_committees: agenda-setting beneficiary (institutional/arbitrage) — operationalizes the norm rendering by rendering, collects the interpretive authority it concentrates
 *   - lay_readers_without_source_languages: primary beneficiary with a diffuse secondary cost (moderate/constrained) — receives readable text, carries invisible pre-resolved ambiguity
 *   - missionary_translation_contexts: primary beneficiary (organized/mobile) — the norm is what makes translation across thousands of languages tractable
 *   - pastors_and_preachers: dual payer/beneficiary (moderate/constrained) — gains readable congregational texts, loses exegetical independence to the committee
 *   - exegetical_word_study_scholars: primary payer (moderate/identity_locked) — bears the precision loss; professional identity is fused with exactly what the norm subordinates
 *   - formal_equivalence_translators: excluded methodology constituency (moderate/constrained) — competes in the market but stands outside the committee conversation that sets mainstream method
 *   - textual_criticism_community: excluded upstream voice (moderate/constrained) — establishes the text being translated but is consulted for readings, not for methodology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.52).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Translation Norm (Biblical Source Text)").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, 'ea783ce4-e811-426d-927c-eeee089fc999').
narrative_ontology:cs_kernel_codification('ea783ce4-e811-426d-927c-eeee089fc999', fixed_text).
narrative_ontology:cs_authority_grounding('ea783ce4-e811-426d-927c-eeee089fc999', expertise).
narrative_ontology:cs_interpretation_layer_present('ea783ce4-e811-426d-927c-eeee089fc999').
narrative_ontology:cs_reading_relation('ea783ce4-e811-426d-927c-eeee089fc999', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea783ce4-e811-426d-927c-eeee089fc999', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('ea783ce4-e811-426d-927c-eeee089fc999', foundational, equivalent_effect_constitutes_fidelity).
narrative_ontology:cs_axiom_status(equivalent_effect_constitutes_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('ea783ce4-e811-426d-927c-eeee089fc999', equivalent_effect_constitutes_fidelity, instrumental).
narrative_ontology:cs_axiom('ea783ce4-e811-426d-927c-eeee089fc999', secondary, ambiguity_resolution_in_rendering).
narrative_ontology:cs_axiom_status(ambiguity_resolution_in_rendering, holdable).
narrative_ontology:cs_axiom_grounding('ea783ce4-e811-426d-927c-eeee089fc999', ambiguity_resolution_in_rendering, instrumental).
narrative_ontology:cs_reference_frame('ea783ce4-e811-426d-927c-eeee089fc999', source_text_as_communicative_event).
narrative_ontology:cs_drift_state('ea783ce4-e811-426d-927c-eeee089fc999', contemporary_post_equivalence_critique, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('ea783ce4-e811-426d-927c-eeee089fc999', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers_without_source_languages).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_translation_contexts).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, bible_society_publishers).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, exegetical_word_study_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, pastors_and_preachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, bible_translation_committees).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, pastors_and_preachers).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, lay_readers_without_source_languages).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, receptor_response_principle).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, functional_equivalence_methodology).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, translation_as_communication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund and direct translation projects across hundreds of languages, set the methodological mandate their translation teams work under, and own the resulting translations as products. The dynamic equivalence norm is their methodological standard: it makes translation scalable across languages and literacy levels and concentrates interpretive decisions in their committee process. Their revenue, institutional continuity, and donor funding base are tied to the norm's continuation. They could fund formal-equivalence projects instead, and do at the margin; abandoning the norm entirely would mean dismantling their own methodology infrastructure.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, bible_society_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Composed of translators, linguists, and denominational representatives, they decide every rendering: which ambiguities to resolve, which idioms to naturalize, which theological terms to preserve or flatten. Under the norm they are authorized to prioritize target-language effect over source structure, which makes their interpretive judgment the operative meaning for readers who will never check the Greek or Hebrew. They bear the labor of translation under the mandate and receive the interpretive authority the mandate concentrates in them; individual members come and go, the institution persists.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, bible_translation_committees, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, bible_translation_committees, beneficiary).

% Read scripture in translation as their primary or only access to the text. The norm gives them a readable, immediately comprehensible text without requiring Greek or Hebrew. The cost they carry is invisible: ambiguities in the source have been resolved for them by a committee, structural features that would support alternative readings are gone, and they generally cannot tell where translation ends and interpretation begins. Their exit is constrained — learning the source languages is a multi-year investment most will not make, and switching translations changes their reading less than they might expect, since most modern versions share the same methodological assumptions.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers_without_source_languages, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, lay_readers_without_source_languages, payer).

% Field translation programs working in thousands of languages for audiences that share neither the source languages nor the biblical cultural world. The norm is what makes their task tractable: meaning-based translation lets a team produce a functioning New Testament in years rather than generations, and gives them a testable criterion — receptor comprehension — they can check in the field. They are among the norm's strongest institutional supporters; their alternative would be slower, form-based methods with higher abandonment rates.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_translation_contexts, beneficiary,
    organized, generational, mobile, global).

% Preach and teach from translations to congregations. They gain texts their congregations can actually read. They pay in authority and precision: word studies preached from a dynamic-equivalence text often examine the committee's rendering rather than the source, interpretive options they might have surfaced are pre-resolved, and their exegetical role is partly displaced to a committee whose decisions they inherit without having made. Their exit is constrained — congregational readability needs bind them to the norm, and the language training that would restore independence is a seminary-level investment.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, pastors_and_preachers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, pastors_and_preachers, beneficiary).

% Academic exegetes, lexicographers, and philologists whose work depends on the source text's morphological and syntactic detail. The norm devalues their product in the markets that matter: the default texts their students and churches read no longer carry the structural information their analyses presuppose, and they must maintain Greek and Hebrew competence and interlinear apparatus at their own professional cost to reach the text at all. Their exit is identity-locked — their professional standing is constituted by exactly the source-language precision the norm subordinates; leaving it would dissolve the career, not escape the arrangement.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, exegetical_word_study_scholars, payer,
    moderate, biographical, identity_locked, continental).

% Translators and projects committed to preserving source-language structure — the essentially-literal tradition. They compete in the same market and argue their case in prefaces and reviews, but the committee conversation that sets mainstream methodology is structured around communicative effectiveness, and their approach enters it mainly as a foil. Their market share is real but niche, and their methodological argument is heard mostly where the norm's enforcement is weakest.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translators, excluded,
    moderate, biographical, constrained, global).

% Scholars who establish the Greek and Hebrew textual basis that translations translate from. They sit upstream of the entire enterprise and would object that a translation norm which embeds interpretive decisions invisibly compounds uncertainty: it renders a contested, reconstructed text into confident declarative prose, and the confidence does not track the evidence. They are structurally outside the committee conversation — consulted for readings, not for methodology — and their objections surface in footnotes rather than in the norm.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, textual_criticism_community, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, bible_society_publishers).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cross-language intelligibility problem at scale: how a text composed in dead languages, inside an alien cultural world, can function as live communication for audiences sharing neither the languages nor the world — one methodology, one quality-control apparatus, and one set of criteria (receptor comprehension) that thousands of translation projects can share.
% TRANSFER_FUNCTION: Moves interpretive authority and the source text's structural information from readers and from the scholarly apparatus that would equip them, into the translation committee's decisions; moves translation labor and funding from diffuse donor bases into society-controlled pipelines; moves professional status from source-language competence toward communicative expertise.
% ABSENT_VOICES: Formal-equivalence translators argue their case only at the margins of the committee conversation that sets mainstream method. Textual critics, who establish the very text being translated, are consulted for readings but not for methodology. Lay readers are present only through pastoral and market proxies, never as participants. The ancient source-language communities are two millennia absent and cannot object to how their ambiguities are resolved.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, translation projects would default to more form-based method; missionary translation timelines would lengthen and some projects would stall; lay reading would become harder and more dependent on teaching; scholarly word-study access would improve as precision renderings became the default; and the translation market would reorganize around formal correspondence, with the societies losing their methodological identity and much of their scalability advantage.
% FOUNDING_PROBLEM: Literal, form-based translation method was failing its audiences: missionaries found word-for-word renderings unintelligible or misleading in target cultures that lacked the biblical cultural world, and lay readers in the source-language communities' own successor cultures could not understand what they were reading. The dynamic equivalence program was built to solve the documented failure of formal correspondence to communicate.
% FOUNDING_PROBLEM_CORROBORATION: Mid-century missionary field reports and the recorded failures of literal method in non-Western contexts — documented in Bible society archives and in the founding movement's own published case work — attest the intelligibility failure, as does academic translation-studies literature, much of it critical of the norm and therefore structurally outside the beneficiary set. That the problem is still live is partly self-attested by the societies whose funding depends on the answer; the strongest external corroboration comes from translation-studies scholars and field linguists who are not employed by the benefiting publishers and who confirm the problem persists in every new language context while disputing whether this norm is still the best response to it.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.58 (end-state of the series): moderate, not high — the norm's costs are real (morphological and syntactic information leaves the translation; ambiguity is resolved invisibly; interpretive authority migrates from reader-community and scholar to committee) but the arrangement also delivers what it promises, and the reading's own lights acknowledge the precision losses as accepted costs of the mission rather than denying them. Suppression is authored as a raw structural property (0.52), unscaled by power or scope — only extractiveness is engine-scaled; 0.52 reflects alternatives that exist (essentially-literal translations persist) but are niche-ified and constrained inside society and denominational channels. Theater_ratio 0.33: the mission framing is mostly functional, with a growing share of institutional self-justification as the apparatus (committees, consultant systems, review bodies) maintains itself. Accessibility_collapse 0.45: alternatives partially collapse under the norm's framing (clarity as the criterion makes literal renderings look like failures) but do not vanish — interlinears, literal versions, and language training remain reachable. Resistance 0.55: sustained — the essentially-literal renaissance, word-study critiques of dynamic renderings, and the translation controversies of the 1990s-2000s. The measurement series run on one shared grid (t=0,10,20,30,40,50,60; roughly 1964-2024, anchored on the publication of the founding methodological work). The trajectory shape is deliberate: extraction accumulates during institutionalization (t0-t40) as the norm crowds alternatives; suppression_requirement peaks at t40 (controversy-era gatekeeping: denominational review bodies, publisher standards policing renderings) then partially relaxes (t50-t60) as the market segments and pluralism normalizes. Identity-lock note: the scholar seat's exit is identity_locked in the professional-identity sense — career path dependence, not external barrier; their standing is constituted by the source-language precision the norm subordinates, so exit would dissolve the career rather than escape the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the society and committee seats the arrangement is faithful service: a methodology that works, scaled globally, with the precision losses as honest prices of the mission. From the scholar seat the same structure operates as authority capture: the committee's rendering becomes the de facto meaning for millions who will never check the Greek or Hebrew, and the scholar's product is devalued in every market that matters. The pastor seat straddles: readable text gained, exegetical independence lost. The lay reader seat is distinctive in that the extraction is invisible from inside it — a reader cannot see the structural information that is not there, which is itself part of how the arrangement holds. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for lay readers, missionary contexts, and the societies; the victim declarations drive high directionality for scholars and pastors, with the scholars' identity_locked exit pushing them toward the full-target end and the pastors' genuine dual position (payer with secondary beneficiary role) placing them mid-high rather than maximal. Committees collect the interpretive authority but also bear the translation labor, placing them low-mid. No directionality_overrides are authored: the one case that would warrant a correction (the pastors' dual position) shares the moderate power atom with the scholars, whose high derived directionality is correct, and a per-atom override would misapply to both. The dual position is carried in the structural data (secondary_role, situation) instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two misreadings. The mission rhetoric would present the norm as pure coordination — a rope — and hide the identifiable parties whose work and authority the arrangement devalues; the anti-dynamic polemic would present it as pure extraction — a snare — and hide the real, documented intelligibility achievement that no serious account denies. The founding problem (cross-language intelligibility for audiences outside the source languages and cultures) is live: every new translation context re-raises it, so this is not a resolved-mandatrophy case and no sunset structure applies. The watch metric is theater_ratio: if the intelligibility problem were ever solved by other means — transparent plural rendering, universal source-language access, machine translation with visible alternatives — and the apparatus persisted on mission language alone, the arrangement would be drifting toward inertial maintenance, and the series authored here is what would show it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the dynamic_equivalence_reading of the biblical_source_text kernel. Would instantiating the formal_equivalence_reading or the critical_reconstructive_reading instead restructure the beneficiary/victim sets and the epsilon of the same standing arrangement?',
    'Comparative classification of the sibling story files (biblical_source_text__formal_equivalence_reading, biblical_source_text__critical_reconstructive_reading). Under formal equivalence the victim set relocates to lay readers denied structural precision and the beneficiary set to scholars; under critical reconstruction the contested element shifts to the textual basis itself, before either translation methodology operates.',
    'The disagreement between readings is located in the criterion of fidelity: effect-on-receptor (this reading) versus form-of-source (formal equivalence) versus stability-of-textual-basis (critical reconstruction). A different reading inverts who pays and who benefits; epsilon and classification are not comparable across readings unless this relocation is tracked. This file generates only this reading as a clean epsilon-invariant constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three live readings of the biblical_source_text kernel; sibling readings restructure the beneficiary/victim sets over the same referent.').

omega_variable(
    interpretive_authority_burden,
    'Is the committee''s resolution of source ambiguity a net cost to lay readers (they lose access to the text''s interpretive plurality and cannot see where rendering ends and interpretation begins) or a net benefit (an unguided reader of a formally ambiguous ancient text is worse off than one given a resolved rendering)?',
    'Reception and comprehension studies comparing reader outcomes across translation methodologies, including measures of whether readers can detect interpretive intervention and whether awareness of source-level ambiguity changes belief formation.',
    'If committee interpretation genuinely serves readers better than structural transparency would, part of the cost currently attributed to the norm inverts into subsidy and epsilon falls; if readers are systematically misled about the text''s own uncertainty, the true extraction is higher than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_burden, conceptual, 'Whether embedded interpretive authority helps or harms the readers it is exercised over.').

omega_variable(
    mission_framing_functionality,
    'How much of the pastoral-mission justification is functional (it genuinely drives rendering decisions that serve communication) versus institutional cover (it justifies scalability, market position, and methodological control)?',
    'Decision-tracing in translation committee records: do communicative-effectiveness criteria actually decide contested renderings, or do cost, timeline, and institutional criteria decide them under mission language?',
    'A high cover share would push theater_ratio above the authored 0.33 and move the arrangement toward inertial maintenance; a low share supports the genuine coordination-function reading and the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_framing_functionality, empirical, 'Functional versus cover share of the pastoral-mission justification.').

omega_variable(
    suppression_source_ambiguity,
    'Is the constrained position of formal-equivalence alternatives the result of reader preference (market competition) or institutional enforcement (society methodological mandates, denominational gatekeeping, publisher consolidation)?',
    'Market and institutional analysis: adoption rates of essentially-literal translations where distribution is not society-controlled, versus adoption inside society and denominational channels.',
    'If suppression is mostly market-driven, the authored suppression overstates coercion and the arrangement is closer to a contested coordination standard; if enforcement-driven, suppression is structural and the extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Market versus enforcement source of the constrained position of alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t50, observed).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement_basis(bibl_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(bibl_be_t40, observed).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(bibl_be_t50, observed).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(bibl_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(bibl_su_t40, observed).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(bibl_su_t50, observed).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(bibl_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% 'biblical_source_text' is a contested kernel, not a single constraint: each authorized reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification. This file is the dynamic_equivalence reading (effect-on-receptor primary; moderate extraction; beneficiaries are lay readers, missionary contexts, and the societies; victims are precision-dependent scholars). The formal_equivalence reading (source-structure primary) inverts the victim set toward lay readers; the critical_reconstructive_reading (textual-basis recovery primary) relocates the contest upstream of both translation methodologies. All three are linked through network.affects_constraints so drift and contamination can be traced across the family. Structural direction: the critical reconstructive project is empirically upstream (it produces the text that gets translated), this reading is institutionally downstream and dominant, and the formal equivalence reading competes laterally in the same market.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
