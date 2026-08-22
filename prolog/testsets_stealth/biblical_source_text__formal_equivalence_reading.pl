% ============================================================================
% CONSTRAINT STORY: biblical_source_text__formal_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__formal_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__formal_equivalence_reading
 *   human_readable: Formal-Equivalence Reading: Source-Structure Primacy with Community-Taught Intelligibility
 *   domain: religious/linguistic
 *
 * SUMMARY:
 *   This story authors ONE reading of the kernel biblical_source_text: the
 *   formal-equivalence regime, in which fidelity to the source languages'
 *   structure is the translator's primary obligation and intelligibility is a
 *   subordinate duty discharged by readers and communities through teaching.
 *   Under this arrangement, translations preserve word order, idiom,
 *   ambiguity, and intertextual echo where possible; hard passages arrive at
 *   the untrained reader in their difficult form, and the community's
 *   teaching office — credentialed in Hebrew and Greek — supplies
 *   understanding. The arrangement performs real coordination (it bounds
 *   translator discretion, preserves interpretive options for future
 *   generations, and anchors confessional stability to a stable text) while
 *   concentrating interpretive finality in a trained-and-ordained class whose
 *   authority the arrangement's own difficulty sustains. KEY AGENTS (by
 *   structural relationship): translation_oversight_committees
 *   (institutional/arbitrage) — sets and enforces the translation philosophy;
 *   ordained_parish_clergy (organized/identity_locked) — primary beneficiary,
 *   collects interpretive authority, having paid sunk training costs;
 *   seminary_language_faculties (institutional/constrained) — beneficiary via
 *   the language requirement; confessional_denominations
 *   (institutional/constrained) — beneficiary via doctrinal stability;
 *   lay_congregation_members (powerless/identity_locked) — primary target,
 *   bears the access cost; first_generation_untaught_believers
 *   (powerless/trapped) — acute target, meets the text without the teaching
 *   the arrangement presumes; non_credentialed_lay_teachers
 *   (moderate/constrained) — excluded voice; academic_translation_theorists
 *   (analytical/analytical) — observer. The claim and the metrics are
 *   independent authored facts: tangled_rope is claimed from the structural
 *   analysis (genuine coordination plus asymmetric burden plus active
 *   enforcement); the metrics describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - translation_oversight_committees: agenda-setter (institutional/arbitrage) — sets translation philosophy, enforces rendering consistency through committee review and style control, can adopt any philosophy at revision time
 *   - ordained_parish_clergy: primary beneficiary (organized/identity_locked) — collects interpretive finality; paid years of tuition and study into the structure; exit forfeits the vocation
 *   - seminary_language_faculties: beneficiary (institutional/constrained) — the language requirement sustains enrollment, departments, and careers
 *   - confessional_denominations: beneficiary (institutional/constrained) — doctrinal statements bind confession to stable renderings; switching endorsed translations risks schism
 *   - lay_congregation_members: primary target (powerless/identity_locked) — receive difficult passages pre-interpreted or defer to the trained office; questioning the received rendering reads as questioning the faith
 *   - first_generation_untaught_believers: acute target (powerless/trapped) — come to the text where the presumed teaching infrastructure does not exist
 *   - non_credentialed_lay_teachers: excluded voice (moderate/constrained) — willing and able to teach but gated by credentialing from adjudicating disputed renderings
 *   - academic_translation_theorists: analytical observer (analytical/analytical) — documents the trade-offs from outside the confessional economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, 0.7).
domain_priors:suppression_score(biblical_source_text__formal_equivalence_reading, 0.58).
domain_priors:theater_ratio(biblical_source_text__formal_equivalence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__formal_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__formal_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__formal_equivalence_reading, "Formal-Equivalence Reading: Source-Structure Primacy with Community-Taught Intelligibility").
narrative_ontology:topic_domain(biblical_source_text__formal_equivalence_reading, "religious/linguistic").

domain_priors:requires_active_enforcement(biblical_source_text__formal_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__formal_equivalence_reading, '4daf007c-8d35-4221-ad1f-f8caf9738bf1').
narrative_ontology:cs_kernel_codification('4daf007c-8d35-4221-ad1f-f8caf9738bf1', fixed_text).
narrative_ontology:cs_authority_grounding('4daf007c-8d35-4221-ad1f-f8caf9738bf1', lineage).
narrative_ontology:cs_interpretation_layer_present('4daf007c-8d35-4221-ad1f-f8caf9738bf1').
narrative_ontology:cs_reading_relation('4daf007c-8d35-4221-ad1f-f8caf9738bf1', biblical_source_text__dynamic_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4daf007c-8d35-4221-ad1f-f8caf9738bf1', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('4daf007c-8d35-4221-ad1f-f8caf9738bf1', foundational, source_structure_carries_revelation).
narrative_ontology:cs_axiom_status(source_structure_carries_revelation, holdable).
narrative_ontology:cs_axiom_grounding('4daf007c-8d35-4221-ad1f-f8caf9738bf1', source_structure_carries_revelation, theological).
narrative_ontology:cs_axiom('4daf007c-8d35-4221-ad1f-f8caf9738bf1', foundational, intelligibility_assigned_to_community_teaching).
narrative_ontology:cs_axiom_status(intelligibility_assigned_to_community_teaching, holdable).
narrative_ontology:cs_axiom_grounding('4daf007c-8d35-4221-ad1f-f8caf9738bf1', intelligibility_assigned_to_community_teaching, conventional).
narrative_ontology:cs_reference_frame('4daf007c-8d35-4221-ad1f-f8caf9738bf1', transmitted_source_form_norm).
narrative_ontology:cs_drift_state('4daf007c-8d35-4221-ad1f-f8caf9738bf1', contemporary_digital_access_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4daf007c-8d35-4221-ad1f-f8caf9738bf1', '').
narrative_ontology:cs_kernel_id(biblical_source_text__formal_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, ordained_parish_clergy).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, seminary_language_faculties).
narrative_ontology:constraint_beneficiary(biblical_source_text__formal_equivalence_reading, confessional_denominations).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, lay_congregation_members).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, first_generation_untaught_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(biblical_source_text__formal_equivalence_reading, ordained_parish_clergy).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, verbal_plenary_inspiration_doctrine).
narrative_ontology:constraint_vindicates(biblical_source_text__formal_equivalence_reading, translator_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Standing cross-denominational committees that set the translation philosophy, review every rendering against source-language style rules, issue revised editions, and license the text to publishers. They can adopt, modify, or abandon the philosophy at a revision boundary at low technical cost, which is what makes their seat arbitrage-grade; the philosophy binds the products, not the committee.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, translation_oversight_committees, agenda_setter,
    institutional, generational, arbitrage, global).

% Preach and teach from the translation; their counsel on hard passages rests on source-language training most hearers lack, so interpretive questions route to them by default. They paid for that position with years of graduate study and tuition, and their vocation, standing, and daily work are built around being the mediation the arrangement assigns. Leaving ministry would forfeit the career and the identity together.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, ordained_parish_clergy, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__formal_equivalence_reading, ordained_parish_clergy, payer).

% Teach Hebrew and Greek as degree requirements; the requirement fills classrooms, funds departments, and structures accreditation. Faculty careers are built on the language curriculum, and curricular change moves at institutional speed, so their position inside the arrangement is secure but not freely exitable.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, seminary_language_faculties, beneficiary,
    institutional, generational, constrained, national).

% Endorse particular translations in hymnals, lectionaries, and ordination exams, and write doctrinal formulations that quote specific renderings. The stable text anchors doctrinal continuity across generations; revising an endorsed translation has repeatedly triggered internal controversy and schism, so switching is possible only at high institutional risk.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, confessional_denominations, beneficiary,
    institutional, generational, constrained, continental).

% Attend, read, and are taught. Difficult passages arrive either pre-interpreted or requiring a question directed to someone trained; many have learned that doubting the received rendering is indistinguishable from doubting the faith. Switching congregations or translations carries social and familial cost, so most remain and defer.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, lay_congregation_members, payer,
    powerless, biographical, identity_locked, local).

% Come to the text in regions and seasons where the trained teaching office the arrangement presumes does not exist. They meet the translation's difficult forms with no mediator available, or with whatever mediator happens to be at hand, and cannot manufacture the education the arrangement prices access in.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, first_generation_untaught_believers, payer,
    powerless, immediate, trapped, global).

% Lead small groups and teach Scripture capably but without ordination or language credentials. The arrangement routes interpretive disputes to credentialed offices, so they teach under supervision and may not adjudicate contested renderings, however well equipped they are.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, non_credentialed_lay_teachers, excluded,
    moderate, biographical, constrained, local).

% Study translation philosophies comparatively, document the trade-offs between formal and dynamic programs, and publish the history of the debates. They hold no vote in confessional endorsement and occupy no pulpit; their seat sees the whole structure and pays none of its costs.
narrative_ontology:constraint_stakeholder(biblical_source_text__formal_equivalence_reading, academic_translation_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__formal_equivalence_reading, ordained_parish_clergy).
narrative_ontology:fixing_cost_class(biblical_source_text__formal_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the source languages' structures — word order, idiom, ambiguity, intertextual echo — in the target text, so renderings stay stable across generations, translators do not resolve interpretive questions on readers' behalf, and dispersed congregations hold one common fixed text to teach from.
% TRANSFER_FUNCTION: Moves interpretive labor and final authority from ordinary readers to the trained-and-ordained mediator class: the meaning of difficult passages transfers from the reader's own competence to the teacher's credential; concretely, tuition and years of study flow from aspiring readers to language faculties, and interpretive questions flow from pews to pulpits.
% ABSENT_VOICES: Non-credentialed laypeople who want direct access sit outside translation-committee deliberation entirely; historically, women and unlettered believers were barred from the very teaching office the arrangement makes the access path, so the objection that the arrangement prices people out of their own scripture was never voiced from a seated position — it survives mainly in the records of dissenting movements.
% DISAPPEARANCE_RATIONALE: Translation practice would reorganize around communicative effectiveness; the ordained mediator's authority premium would compress as renderings became self-explanatory; seminary language requirements would lose their rationale; confessions anchored to specific renderings would drift or fracture; and the common cross-generational text conservative communities coordinate on would dissolve into a shifting set of freer paraphrases.
% FOUNDING_PROBLEM: Translator discretion and doctrinal drift: every rendering choice smuggles interpretation, and loose paraphrase lets confession float free of the text. The arrangement was built to bind translation tightly to source form so that neither the individual translator nor the passing era decides what the text means.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: secular translation theorists and linguists — including the founders of the rival program — attest that translator discretion is a real and standing problem, and historians of the Reformation attest the drift problem the arrangement answered. What those same sources contest is whether binding structure remains the right cure now that literacy and study tools have spread; the problem's reality is corroborated while its status is disputed by the same witnesses.
narrative_ontology:disappearance_verdict(biblical_source_text__formal_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__formal_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__formal_equivalence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__formal_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__formal_equivalence_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__formal_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__formal_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__formal_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.70) because access to the text's meaning is priced in years of language study or deference to a credentialed mediator, and the price falls hardest on those least able to pay it — first-generation believers without teachers. Suppression (0.58) is authored as a raw structural property, unscaled by power or scope: rival translation philosophies are not banned, but within confessional communities choosing one carries liturgical and reputational sanction, and the enforcement machinery (denominational endorsements, ordination exams, 'essentially literal' branding) had to be built and maintained — the suppression_requirement series traces that build-up to a mid-interval peak before partial market segmentation relaxed it. Theater (0.40) grows across the interval as 'what does the original say?' becomes pulpit performance and translation marketing, while the underlying philology remains real work. Accessibility_collapse is moderate-low (0.45): the rival philosophy's products sit on the same shelves, so alternatives do not collapse once the arrangement is understood — exit is social, not physical. Resistance (0.55) reflects an organized counter-program running since mid-century plus recurring revision controversies inside conservative coalitions. All three tracked series share one nine-point grid; no metric is sampled on a private schedule.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. The agenda-setting committees hold arbitrage-grade exit — they can adopt any philosophy at the next revision — so the arrangement reads to them as stewardship of a trust. The ordained clergy seat is identity_locked twice over: professional identity fused with the mediation role (the vocation is being the one who knows what the original says) and doctrinal identity fused with the text's stability; from that seat the same structure computes as vocation and sacrifice, and the sunk decade of training raises their cost-bearing above what the beneficiary declaration alone implies. The lay seats — especially identity_locked members and trapped first-generation believers — compute the full weight of the arrangement. If the identity frame broke (mass lay fluency through study software or machine assistance), the clergy seat would migrate toward sunk-cost payer and the computed divergence would narrow from the beneficiary side.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (clergy, faculties, denominations) drive those seats toward the beneficiary end; victim declarations (lay members, untaught believers) drive them toward the target end, with identity_locked and trapped exits pushing them nearer full-target than mobile readers would sit. One override: the organized power atom (occupied only by ordained_parish_clergy) is raised from the near-floor value the beneficiary declaration would derive to 0.18, because the derivation cannot see the seat's sunk costs — years of tuition and study paid into the structure — which make clergy net beneficiaries with real cost-bearing rather than pure subsidized collectors. The receipt surface names the clergy seat because the durable gain — interpretive finality, the standing presumption that hard passages mean what the trained office says they mean — demonstrably accrues there; tuition captured by faculties is real but derivative on that authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem retains live force — rival-program renderings do resolve ambiguities, occasionally tendentially, and doctrinal drift remains a standing fear in confessional polities — so the mandate has not outlived its function and no mandatrophy flag is set; the R5 pairing (status contested x verdict world_rearranges) correctly declines the zombie signature. The tangled-rope claim does preventive work in both directions: a pure-extraction reading would erase the genuine coordination the arrangement performs (ambiguity preserved for future readers, translator discretion bounded, one stable text across generations), while a pure-coordination reading would hide the mediator premium and the education toll levied on readers who never chose the arrangement. The classification holds both truths in one structure: the community is coordinated through the same textuality by which some of its members are charged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the formal_equivalence_reading of the biblical_source_text kernel; which structural facts would differ if the sibling readings (dynamic_equivalence_reading, critical_reconstructive_reading) were instantiated instead?',
    'Each sibling is authored as its own constraint story with its own epsilon, beneficiary/victim structure, and claimed type; cross-file comparison locates the disagreement in classification space.',
    'Expected divergence: dynamic_equivalence lowers reader-side extraction and shifts beneficiaries toward mission agencies and publishers; critical_reconstructive raises uncertainty costs and shifts beneficiaries toward academic philology. The mapped divergence is the kernel contest made measurable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame membership: this constraint is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    structure_intelligibility_priority_axis,
    'Where exactly do the sibling readings disagree: in the priority ranking between source-form fidelity and target-audience intelligibility, or in who bears the cost of closing the gap between text and reader?',
    'Compare transfer_function and victim sets across the three sibling stories: if only the ranking differs, victim sets should coincide and only epsilon differs; if cost-bearing differs, victim sets differ.',
    'Determines whether the kernel contest is a single-axis preference dispute or a structural disagreement about cost allocation, changing which omega classes carry the residual disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structure_intelligibility_priority_axis, conceptual, 'Locates the disagreement axis among sibling readings of the same kernel.').

omega_variable(
    mediator_rent_vs_coordination_cost,
    'Is the burden concentrated on non-specialist readers genuine mediator rent, or the legitimate price of preserving unresolved ambiguity and bounding translator discretion?',
    'Compare communities with equivalent text access but different teaching density; if reader outcomes converge wherever teaching is dense regardless of translation philosophy, the burden is rent; if formal-equivalence communities uniquely retain interpretive options later vindicated by scholarship, part of the cost is coordination.',
    'If mostly rent, payer seats compute nearer pure extraction; if mostly coordination price, beneficiary seats compute nearer pure coordination — widening computed seat divergence in a diagnostic direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediator_rent_vs_coordination_cost, empirical, 'Separates mediator-rent burden from genuine ambiguity-preservation cost.').

omega_variable(
    access_barrier_structural_vs_internalized,
    'Is the non-specialist reader''s access barrier structural (source languages are genuinely hard; credentials gate the teaching office) or internalized (readers learn disqualification — ''I need an expert'' — that persists after technical barriers fall)?',
    'Post-digital-tool trajectory: free interlinears and study software lowered the technical barrier after roughly interval point 60; if lay engagement with original-language questions did not rise proportionally, a large internalized component exists.',
    'If internalized, effective suppression exceeds the structural measure and persists even if credentialing opens; payer-seat classifications stay burdensome longer than structural data alone predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_structural_vs_internalized, empirical, 'Composition of the reader-side access barrier: structural versus internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__formal_equivalence_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__formal_equivalence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__formal_equivalence_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__formal_equivalence_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(bibl_tr_t20, observed).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__formal_equivalence_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(bibl_tr_t30, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__formal_equivalence_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__formal_equivalence_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(bibl_tr_t50, observed).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__formal_equivalence_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(bibl_tr_t60, observed).
narrative_ontology:measurement(bibl_tr_t70, biblical_source_text__formal_equivalence_reading, theater_ratio, 70, 0.39).
narrative_ontology:measurement_basis(bibl_tr_t70, observed).
narrative_ontology:measurement(bibl_tr_t80, biblical_source_text__formal_equivalence_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(bibl_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__formal_equivalence_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__formal_equivalence_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__formal_equivalence_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(bibl_be_t20, observed).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__formal_equivalence_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(bibl_be_t30, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__formal_equivalence_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement_basis(bibl_be_t40, observed).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__formal_equivalence_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(bibl_be_t50, observed).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__formal_equivalence_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(bibl_be_t60, observed).
narrative_ontology:measurement(bibl_be_t70, biblical_source_text__formal_equivalence_reading, base_extractiveness, 70, 0.7).
narrative_ontology:measurement_basis(bibl_be_t70, observed).
narrative_ontology:measurement(bibl_be_t80, biblical_source_text__formal_equivalence_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement_basis(bibl_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__formal_equivalence_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__formal_equivalence_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__formal_equivalence_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(bibl_su_t20, observed).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__formal_equivalence_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(bibl_su_t30, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__formal_equivalence_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(bibl_su_t40, observed).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__formal_equivalence_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement_basis(bibl_su_t50, observed).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__formal_equivalence_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(bibl_su_t60, observed).
narrative_ontology:measurement(bibl_su_t70, biblical_source_text__formal_equivalence_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(bibl_su_t70, observed).
narrative_ontology:measurement(bibl_su_t80, biblical_source_text__formal_equivalence_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement_basis(bibl_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__formal_equivalence_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, dynamic_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__formal_equivalence_reading, critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'faithfulness to the biblical text' decomposes into three structurally distinct regimes per the epsilon-invariance principle — this file (formal_equivalence_reading: structure-primary, intelligibility delegated to community teaching), dynamic_equivalence_reading (effect-primary, intelligibility borne by the translator), and critical_reconstructive_reading (textual recovery prior to any privileging). Each carries its own epsilon, beneficiary/victim structure, and claimed type. Influence runs both ways across the family: the critical-reconstructive program supplies the printed edition this reading translates, while this reading's demand for 'the original' funds manuscript scholarship — and its published variants destabilize the singular-text premise this reading's reference frame asserts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__formal_equivalence_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
