% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Press-Caused-Reformation Settlement (Technological-Determinist Reading)
 *   domain: historiographical/intellectual
 *
 * SUMMARY:
 *   The claim that the printing press caused the Reformation by making
 *   censorship impossible and vernacular scripture inevitable operates less
 *   as a hypothesis than as an entrenched default: the account survey courses
 *   teach, documentaries dramatize, anniversaries commodify, and technology
 *   rhetoric cites as precedent. This story classifies that settlement as it
 *   operates. It retains a genuine coordination service (one compressible
 *   causal narrative for teaching and public memory) while transferring
 *   narrative authority and commercial value toward the determinist lineage
 *   and away from agency-centered and Catholic-adaptation scholarship, whose
 *   findings circulate below the headline. Claim and metrics are independent:
 *   the claimed type records the structure judged true of the settlement; the
 *   metrics record its observed operation; the engine computes per-seat
 *   classifications from the structural data. This file instantiates one
 *   reading of the press_reformation_causation kernel (see kernel_context and
 *   the reading omega); the strategic_deployment and mutual_shaping readings
 *   are separate constraints, not positions inside this one.
 *
 * KEY AGENTS:
 *   - - media_determinist_scholarship: Primary beneficiary (institutional/identity_locked) — canonical authority accrues to the lineage; exit means recanting foundational commitments
 *   - - popular_history_publishers: Commercial beneficiary (institutional/arbitrage) — amplifies whichever narrative sells
 *   - - technology_evangelists: Opportunist beneficiary (powerful/mobile) — borrows historical inevitability for present-day claims
 *   - - protestant_commemorative_sector: Heritage beneficiary (organized/constrained) — anniversary economies run on the story
 *   - - western_civ_curriculum_gatekeepers: Agenda setter (institutional/constrained) — administers reproduction through the survey canon
 *   - - revisionist_book_historians: Primary target (moderate/constrained) — archival corrections circulate below the headline
 *   - - catholic_adaptation_historians: Secondary target (moderate/constrained) — the futility premise pre-dismisses their subject
 *   - - survey_course_students: Captive audience (powerless/trapped) — absorbs the narrative as settled fact; incidentally gains a coherent story
 *   - - catholic_education_authorities: Excluded voice (organized/constrained) — would contest the futility framing; not consulted
 *   - - historiography_theorists: Analytical observer (analytical/analytical) — maps the meta-debate without collecting from it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.38).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.4).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.38).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, tangled_rope).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Press-Caused-Reformation Settlement (Technological-Determinist Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "historiographical/intellectual").

domain_priors:requires_active_enforcement(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'cb37c082-830a-45a8-b9ea-8d954899e7c2').
narrative_ontology:cs_kernel_codification('cb37c082-830a-45a8-b9ea-8d954899e7c2', distributed).
narrative_ontology:cs_authority_grounding('cb37c082-830a-45a8-b9ea-8d954899e7c2', distributed).
narrative_ontology:cs_reading_relation('cb37c082-830a-45a8-b9ea-8d954899e7c2', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('cb37c082-830a-45a8-b9ea-8d954899e7c2', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('cb37c082-830a-45a8-b9ea-8d954899e7c2', foundational, technological_capacity_determines_outcomes).
narrative_ontology:cs_axiom_status(technological_capacity_determines_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('cb37c082-830a-45a8-b9ea-8d954899e7c2', technological_capacity_determines_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('cb37c082-830a-45a8-b9ea-8d954899e7c2', foundational, vernacular_scripture_print_inevitable).
narrative_ontology:cs_axiom_status(vernacular_scripture_print_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('cb37c082-830a-45a8-b9ea-8d954899e7c2', vernacular_scripture_print_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('cb37c082-830a-45a8-b9ea-8d954899e7c2', secondary, church_resistance_futile_premise).
narrative_ontology:cs_axiom_status(church_resistance_futile_premise, holdable).
narrative_ontology:cs_axiom_grounding('cb37c082-830a-45a8-b9ea-8d954899e7c2', church_resistance_futile_premise, empirically_contingent).
narrative_ontology:cs_reference_frame('cb37c082-830a-45a8-b9ea-8d954899e7c2', print_capacity_exogenous_driver).
narrative_ontology:cs_drift_state('cb37c082-830a-45a8-b9ea-8d954899e7c2', contemporary_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cb37c082-830a-45a8-b9ea-8d954899e7c2', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, media_determinist_scholarship).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, popular_history_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, technology_evangelists).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_commemorative_sector).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, revisionist_book_historians).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_adaptation_historians).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, survey_course_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, survey_course_students).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, strong_press_causation_hypothesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_determinist_methodology).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, religious_authority_media_fragility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars, departments, and citation networks descended from the media-determinist lineage (Innis, McLuhan, and successors). Survey chapters, endowed lectures, and canonical reading lists route through their frameworks. Leaving the framework would mean recasting lifework built on its premises; seniority and canonization reward staying.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, media_determinist_scholarship, beneficiary,
    institutional, generational, identity_locked, global).

% Trade and textbook publishers who commission, edit, and market narrative history. A single-cause story compresses editing, sells across markets, and anchors series branding; they switch framing quickly when sales shift and carry no doctrinal commitment of their own.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, popular_history_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Industry speakers, authors, and executives who invoke the Gutenberg-to-Luther sequence as precedent that new media inevitably remake society. The historical parallel lends borrowed inevitability to product launches and policy arguments; they incur no cost when the scholarship underneath shifts.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, technology_evangelists, beneficiary,
    powerful, biographical, mobile, global).

% Museums, heritage sites, tourism boards, and anniversary organizers (including the 2017 Luther quincentenary) whose programming and revenue depend on the story of an unstoppable print-driven awakening. Their institutional identities are bound to the narrative's grandeur.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_commemorative_sector, beneficiary,
    organized, biographical, constrained, continental).

% Textbook editorial boards, examination bodies, and survey-course committees that select which causal account of the Reformation reaches classrooms. Simplicity and continuity favor renewing the existing account each cycle; changing it requires coordinated retraining and reprinting across systems.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, western_civ_curriculum_gatekeepers, agenda_setter,
    institutional, generational, constrained, national).

% Archival researchers of the early modern book trade who document printers' commercial calculations, failed editions, and market segmentation. Their findings appear in specialist monographs and journals while survey headlines retain the simpler account; moving subfields would forfeit accumulated expertise.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, revisionist_book_historians, payer,
    moderate, generational, constrained, global).

% Historians of the Catholic side who document effective counter-strategies: Catholic printing houses, targeted index enforcement that worked locally, and durable confessional consolidation in Italy, Spain, and Bavaria. The prevailing account treats Church resistance as futile in advance, so their positive findings read as footnotes.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_adaptation_historians, payer,
    moderate, generational, constrained, global).

% Undergraduates and general readers who encounter the account in required courses and popular media. They must reproduce it for examinations and carry its inevitability intuitions afterward; they also gain a usable, coherent story they would otherwise lack.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, survey_course_students, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, survey_course_students, beneficiary).

% Confessional school systems and catechetical bodies whose curricula touch the Reformation era. They would contest the futility-of-resistance framing and request treatment of Catholic print culture, but they are not represented on the secular curriculum and publishing bodies that set the survey account.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_education_authorities, excluded,
    organized, generational, constrained, continental).

% Philosophers and methodologists of history who study how causal accounts rise, persist, and fall in the discipline. They map the debate among the three explanatory schools without collecting from any of them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historiography_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, media_determinist_scholarship).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single compressible causal account connecting a communications technology to a religious transformation, solving a real pedagogical and mnemonic problem: survey teaching, journalism, and public memory need one teachable answer to why the Reformation spread where earlier reform movements (Wycliffe, Hus) did not.
% TRANSFER_FUNCTION: Moves narrative authority and attention from agency-centered and Catholic-adaptation scholarship toward the media-determinist lineage; moves commercial value (textbook adoptions, trade sales, keynote fees, heritage tourism) toward whoever retells the simple sequence; moves students' causal intuitions toward technological inevitability and away from contingency and strategy.
% ABSENT_VOICES: Confessional education authorities would contest the futility-of-resistance premise; economic historians of the print trade would object that printers' strategic choices vanish from the headline; the sixteenth-century printers themselves, whose decisions the account narrates away, cannot speak at all. Neither living group sits on the curriculum boards or documentary commissions that reproduce the account.
% DISAPPEARANCE_RATIONALE: Survey courses, documentaries, anniversary programming, and technology keynotes would lose their organizing account overnight; the revisionist and mutual-shaping literatures would move from specialist venues into general teaching; and the Gutenberg-to-internet analogy, which borrows this sequence as its founding precedent, would lose its anchor case.
% FOUNDING_PROBLEM: Explaining why the Reformation spread so far so fast despite ecclesiastical suppression that had contained Wycliffism and Hussitism: mid-twentieth-century quantitative bibliography (Febvre and Martin) measured print's multiplication of texts beyond confiscation capacity, and the determinist answer hardened into the settlement that print capacity itself caused the outcome.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: Febvre and Martin's bibliographic census (Annales social-economic history, not the determinist school) attests the material scale-up, and current revisionist monographs by book-trade and Catholic-adaptation historians attest that the underlying question is live while disputing the inevitability answer. No serious party denies the founding problem existed; what is disputed is whether this settlement solved it.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).
:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.38 from this reading's own lights: the determinist seat regards the core causal content as substantially veridical (print capacity did overwhelm confiscation logistics and did reprice vernacular publication), so the settlement is not experienced at this seat as theft; the residual extraction the seat can acknowledge is the strong form's overreach (single cause, erased strategy, pre-ruled futility) and the rents collected beyond the evidence by amplifiers. Suppression is 0.40 and structural-soft: canon placement, syllabus inertia, and publishing economics marginalize rivals without banning them; the revisionist literature is published and reachable, which caps suppression well below coercive levels. Theater is 0.40: anniversary cycles and keynote invocations repeat the sequence ritually, increasingly detached from the specialist literature, while survey teaching still performs a real function. Accessibility collapse is 0.30: rival readings remain fully articulable and available, so understanding the settlement does not close alternatives. Resistance is 0.60: four decades of revisionist book history and Catholic-adaptation scholarship constitute sustained organized pushback that has stopped the settlement's further hardening without displacing it. Identity-lock binds the primary beneficiary seat professionally: careers, canons, and lecture circuits are constituted by the framework, so exit equals recantation; if that fusion broke, the seat's classification would converge toward the mobile amplifier seats. The coordination_type declaration (identity_coordination) marks the settlement's real boundary-maintenance function for the mainstream narrative community; the known gaming risk of identity framing as extraction cover is handled by the conservative floor plus the declared victim seats. The three measurement series share one seven-point grid (interval 0-66, roughly 1958-2024); enforcement intensity rises through the canonization decades and then plateaus as revisionist pressure checks further growth, which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the determinist lineage the settlement is earned consensus describing a real mechanism; from the revisionist and Catholic-adaptation seats the same settlement operates as a ceiling on uptake, in which archival corrections are footnoted while the headline stays fixed; from the student seat it arrives as settled fact with no visible contest at all; from the publisher and evangelist seats it is inventory and raw material, held with no commitment and abandoned without cost. The engine computes these per-seat classifications from power, exit options, and declared position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit at the subsidy end: the determinist lineage (identity-locked, so its position stays pinned near the beneficiary end by fusion rather than mobility), publishers and evangelists (arbitrage-grade exit places them nearest the full-beneficiary end; they collect and can leave), and the commemorative sector (constrained but collecting). Declared victims sit at the target end: revisionist and Catholic-adaptation historians bear the credibility transfer with constrained exit, and students bear the flattened understanding with no exit from the course structure, damped slightly by their incidental benefit from having any coherent story. Curriculum gatekeepers administer without capturing: near-symmetric, their costs and benefits are institutional rather than personal. Scope is global for the anglophone and translated survey economy, which raises verification difficulty and scales effective extraction modestly upward for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against mislabeling as pure extraction: the settlement solves a real pedagogical coordination problem, so a pure-extraction verdict would erase the teaching function that gives it its grip. Against mislabeling as natural law: the inevitability rhetoric mimics a discovered regularity, but the Catholic print-culture counterexamples (high print penetration, no successful reformation in Italy, Spain, or France) and four decades of organized revisionism show a constructed, maintained, contested arrangement with identifiable collectors, which is why no mountain claim is authored and emerges_naturally stays false. The genealogy interview locates the residual vitality honestly: the founding problem (why the Reformation spread) is live, the settlement's monopoly answer is contested, and the arrangement persists because its reproduction machinery outpaces correction uptake. If a successor synthesis ever genuinely closed the founding problem, the settlement would decay toward theatrical repetition; the theater series' late-interval rise is the early signature of that path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (technological_determinism) of the press_reformation_causation kernel; the strategic_deployment and mutual_shaping readings are separate constraints. Would classifying the same settlement from those seats yield different types and epsilon values?',
    'Generate the sibling stories over the same referent and compare per-seat classifications; the strategic_deployment seat should raise epsilon (settlement as captured narrative serving deployers) and the mutual_shaping seat should reject unidirectionality outright.',
    'If the strategic_deployment reading dominates, the settlement looks like a captured narrative rather than a hybrid; if mutual_shaping dominates, the settlement''s unidirectionality fails and its coordination claim narrows to pedagogy alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one reading of a tri-polar kernel; sibling readings are separate files.').

omega_variable(
    discovered_regularity_vs_constructed_settlement,
    'Is the causal settlement a discovered regularity about print societies, or a constructed narrative serving identifiable interests?',
    'Systematic counterfactual comparison of print societies with and without successful reformation: Italian, Spanish, and French Catholic print cultures achieved high print penetration with no successful reformation.',
    'If the counterexamples weigh, the settlement is constructed and interest-serving rather than quasi-natural, raising effective extraction across all target seats and confirming the non-mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovered_regularity_vs_constructed_settlement, empirical, 'Whether the settlement''s inevitability core reflects a real regularity or constructed narrative maintenance.').

omega_variable(
    catholic_print_counterexample_handling,
    'Does the settlement absorb the Catholic print-culture counterexample ad hoc (those were not vernacular-Bible markets), or treat it as disconfirming?',
    'Test whether the settlement''s defenders specify falsifiable scope conditions ex ante; ad hoc scope-narrowing after each counterexample signals an unfalsifiable core.',
    'Ad hoc absorption would strengthen the extraction reading of the settlement''s persistence mechanism; genuine scope conditions would rehabilitate part of the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_print_counterexample_handling, empirical, 'How the settlement''s core handles its strongest disconfirming cases.').

omega_variable(
    epsilon_referent_decomposition,
    'This story''s epsilon is authored over the historiographical settlement (the doctrine''s entrenched status as default explanation); the historical communicative order itself (print capacity versus censorial capacity, 1450-1600) is a different arrangement. Are the two being kept apart?',
    'Author a separate flat story for the material print-versus-censorship arrangement with its own beneficiaries, victims, and metrics; link the two via network edges.',
    'Prevents epsilon conflation: the material arrangement''s extraction (likely low, a genuine capacity shift) must not contaminate the doctrine''s extraction, and vice versa.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_decomposition, conceptual, 'Referent discipline: the doctrine''s entrenchment and the historical capacity shift are distinct constraints.').

omega_variable(
    internet_analogy_spillover_boundary,
    'Part of the settlement''s contemporary operation runs through the Gutenberg-to-internet analogy in technology rhetoric; is that spillover inside this constraint or a separate downstream constraint?',
    'Decompose if the analogy''s epsilon diverges: it extracts present-day policy and product legitimacy from a historical precedent, with different stakeholders (policy audiences, investors) than the historiographical settlement.',
    'A separate story would carry the analogy''s own beneficiary and victim structure; this story keeps its network edges within the kernel family until the decomposition is authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internet_analogy_spillover_boundary, conceptual, 'Boundary of the settlement''s operation versus the downstream tech-analogy constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(pres_tr_t0, observed).
narrative_ontology:measurement(pres_tr_t11, press_reformation_causation__technological_determinism, theater_ratio, 11, 0.24).
narrative_ontology:measurement_basis(pres_tr_t11, observed).
narrative_ontology:measurement(pres_tr_t22, press_reformation_causation__technological_determinism, theater_ratio, 22, 0.28).
narrative_ontology:measurement_basis(pres_tr_t22, observed).
narrative_ontology:measurement(pres_tr_t33, press_reformation_causation__technological_determinism, theater_ratio, 33, 0.31).
narrative_ontology:measurement_basis(pres_tr_t33, observed).
narrative_ontology:measurement(pres_tr_t44, press_reformation_causation__technological_determinism, theater_ratio, 44, 0.37).
narrative_ontology:measurement_basis(pres_tr_t44, observed).
narrative_ontology:measurement(pres_tr_t55, press_reformation_causation__technological_determinism, theater_ratio, 55, 0.42).
narrative_ontology:measurement_basis(pres_tr_t55, observed).
narrative_ontology:measurement(pres_tr_t66, press_reformation_causation__technological_determinism, theater_ratio, 66, 0.4).
narrative_ontology:measurement_basis(pres_tr_t66, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(pres_be_t0, observed).
narrative_ontology:measurement(pres_be_t11, press_reformation_causation__technological_determinism, base_extractiveness, 11, 0.28).
narrative_ontology:measurement_basis(pres_be_t11, observed).
narrative_ontology:measurement(pres_be_t22, press_reformation_causation__technological_determinism, base_extractiveness, 22, 0.34).
narrative_ontology:measurement_basis(pres_be_t22, observed).
narrative_ontology:measurement(pres_be_t33, press_reformation_causation__technological_determinism, base_extractiveness, 33, 0.36).
narrative_ontology:measurement_basis(pres_be_t33, observed).
narrative_ontology:measurement(pres_be_t44, press_reformation_causation__technological_determinism, base_extractiveness, 44, 0.41).
narrative_ontology:measurement_basis(pres_be_t44, observed).
narrative_ontology:measurement(pres_be_t55, press_reformation_causation__technological_determinism, base_extractiveness, 55, 0.4).
narrative_ontology:measurement_basis(pres_be_t55, observed).
narrative_ontology:measurement(pres_be_t66, press_reformation_causation__technological_determinism, base_extractiveness, 66, 0.38).
narrative_ontology:measurement_basis(pres_be_t66, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__technological_determinism, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(pres_su_t0, observed).
narrative_ontology:measurement(pres_su_t11, press_reformation_causation__technological_determinism, suppression_requirement, 11, 0.26).
narrative_ontology:measurement_basis(pres_su_t11, observed).
narrative_ontology:measurement(pres_su_t22, press_reformation_causation__technological_determinism, suppression_requirement, 22, 0.32).
narrative_ontology:measurement_basis(pres_su_t22, observed).
narrative_ontology:measurement(pres_su_t33, press_reformation_causation__technological_determinism, suppression_requirement, 33, 0.35).
narrative_ontology:measurement_basis(pres_su_t33, observed).
narrative_ontology:measurement(pres_su_t44, press_reformation_causation__technological_determinism, suppression_requirement, 44, 0.38).
narrative_ontology:measurement_basis(pres_su_t44, observed).
narrative_ontology:measurement(pres_su_t55, press_reformation_causation__technological_determinism, suppression_requirement, 55, 0.39).
narrative_ontology:measurement_basis(pres_su_t55, observed).
narrative_ontology:measurement(pres_su_t66, press_reformation_causation__technological_determinism, suppression_requirement, 66, 0.4).
narrative_ontology:measurement_basis(pres_su_t66, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question of whether the press caused the Reformation decomposes into three structurally distinct readings of one kernel, each with its own epsilon, beneficiaries, and type. This file instantiates the technological_determinism reading (unidirectional capacity-to-outcome causation). The strategic_deployment reading (purposeful use of neutral capacity) and the mutual_shaping reading (co-evolution of technology and agency) are separate stories linked here. The upstream material arrangement (print capacity versus censorial capacity, 1450-1600) is a further distinct constraint deliberately not folded into this one (see the referent-decomposition omega). The Gutenberg-to-internet analogy circulating in technology rhetoric is recorded as candidate downstream spillover pending its own decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
