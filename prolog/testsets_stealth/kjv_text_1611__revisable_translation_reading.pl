% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: Revisable Translation Reading of the KJV — the Scholarly-Publishing Revision Apparatus
 *   domain: religious/textual_criticism/publishing
 *
 * SUMMARY:
 *   Under the revisable_translation_reading, the standing arrangement is the
 *   scholarly-publishing apparatus that continuously supersedes the 1611
 *   text: critical editions of the Hebrew and Greek are maintained by
 *   academic committees, denominations commission and adopt fresh
 *   translations, and publishing houses hold the copyrights through which
 *   every adoption is monetized. The arrangement performs genuine
 *   coordination — manuscript discoveries and linguistic advances really do
 *   reach English readers through it — while the same structure channels
 *   recurring revenue to the houses that control the licensed texts and
 *   displaces communities bound to the older wording. Interval mapping: t=0
 *   corresponds to approximately 1885 (completion of the English Revised
 *   Version, the first modern critical revision) and t=140 to approximately
 *   2025; measurement points are decade-scale approximations on that mapping.
 *   The claim and the metrics are independent authored facts: the type is
 *   claimed from the structure (coordination plus asymmetric extraction under
 *   active enforcement), the metrics from the arrangement's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - - academic_textual_critics: agenda-setting arbiter (institutional/mobile) — decides what the English text says; careers ride on continued revision
 *   - - bible_publishing_houses: primary beneficiary (powerful/arbitrage) — collects licensing and edition-cycle revenue from every adoption
 *   - - denominational_translation_boards: adopting beneficiary and co-agenda-setter (institutional/constrained) — funds committees, bears switch costs
 *   - - ordinary_laity: payer with incidental benefit (powerless/constrained) — absorbs repurchase and re-memorization costs, holds no committee seat
 *   - - traditionalist_kjv_congregations: resisting payer (organized/identity_locked) — bears marginalization costs; exit would dissolve community identity
 *   - - independent_translators: excluded competitor (moderate/trapped) — locked out of adoption channels by copyright and committee prestige
 *   - - religious_studies_scholars: analytical observer (analytical/analytical) — sees the full structure, takes no seat in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.62).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.34).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "Revisable Translation Reading of the KJV — the Scholarly-Publishing Revision Apparatus").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism/publishing").

domain_priors:requires_active_enforcement(kjv_text_1611__revisable_translation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, 'b5023063-a1f2-4f44-8925-57a577e77985').
narrative_ontology:cs_kernel_codification('b5023063-a1f2-4f44-8925-57a577e77985', fixed_text).
narrative_ontology:cs_authority_grounding('b5023063-a1f2-4f44-8925-57a577e77985', expertise).
narrative_ontology:cs_interpretation_layer_present('b5023063-a1f2-4f44-8925-57a577e77985').
narrative_ontology:cs_reading_relation('b5023063-a1f2-4f44-8925-57a577e77985', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('b5023063-a1f2-4f44-8925-57a577e77985', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('b5023063-a1f2-4f44-8925-57a577e77985', foundational, textual_authority_tracks_manuscript_evidence).
narrative_ontology:cs_axiom_status(textual_authority_tracks_manuscript_evidence, holdable).
narrative_ontology:cs_axiom_grounding('b5023063-a1f2-4f44-8925-57a577e77985', textual_authority_tracks_manuscript_evidence, empirically_contingent).
narrative_ontology:cs_axiom('b5023063-a1f2-4f44-8925-57a577e77985', foundational, revision_honors_scriptural_purpose).
narrative_ontology:cs_axiom_status(revision_honors_scriptural_purpose, holdable).
narrative_ontology:cs_axiom_grounding('b5023063-a1f2-4f44-8925-57a577e77985', revision_honors_scriptural_purpose, instrumental).
narrative_ontology:cs_reference_frame('b5023063-a1f2-4f44-8925-57a577e77985', evidence_governed_translation_lineage).
narrative_ontology:cs_drift_state('b5023063-a1f2-4f44-8925-57a577e77985', contemporary_commercial_revision_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b5023063-a1f2-4f44-8925-57a577e77985', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_textual_critics).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, bible_publishing_houses).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, denominational_translation_boards).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, ordinary_laity).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, traditionalist_kjv_congregations).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, independent_translators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, ordinary_laity).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, critical_text_method).
narrative_ontology:constraint_vindicates(kjv_text_1611__revisable_translation_reading, alexandrian_witness_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Edit the critical editions of the Hebrew Bible and Greek New Testament that modern translations follow, staff the translation committees, and publish the comparative studies that justify each textual decision. Careers advance through successive refinements of the text; departure paths exist into adjacent philology, ancient history, and general linguistics.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_textual_critics, agenda_setter,
    institutional, generational, mobile, global).

% Hold the copyrights on the major modern English translations, license every reprint, app, and church bulletin that reproduces the text, and plan edition cycles — study Bibles, updated texts, specialty editions — around each adoption wave. Revenue arrives with every institutional switch; the wider catalog can pivot toward other religious products if Bible lines weaken.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, bible_publishing_houses, beneficiary,
    powerful, biographical, arbitrage, global).

% Commission new translations, fund the committees, and vote the official version their hymnals, lectionaries, curricula, and pew racks will carry. Adoption confers cohesion and a common voice, but each switch costs money, provokes member complaints, and binds the denomination to defending its choice for decades; polity and member expectations limit their freedom to reverse course.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, denominational_translation_boards, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, denominational_translation_boards, agenda_setter).

% Read whatever version their congregation places in the pews and sells in the bookshop. When the denomination switches versions, they replace Bibles, relearn memorized wording, and follow curricula rewritten around the new text. They receive the improved wording at little direct charge — Bibles are gifted and streamed free — but hold no seat on any committee and register preferences mainly through attendance and giving.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, ordinary_laity, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, ordinary_laity, beneficiary).

% Keep the 1611 text as the marker of faithful worship, maintaining their own colleges, curriculum suppliers, and publication houses as mainstream institutions move on. Every newly adopted version further isolates them; their schools lose accreditation pathways, their ministers train in their own institutes, and abandoning the old text would dissolve the community boundaries the text draws.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, traditionalist_kjv_congregations, payer,
    organized, generational, identity_locked, continental).

% Produce careful modern-language renderings outside the major houses but find the adoption channels closed: denominations contract with established committees, retailers stock licensed editions, and building on copyrighted modern texts requires fees they cannot carry. Their work circulates in niches or enters the public domain and goes unseen.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, independent_translators, excluded,
    moderate, biographical, trapped, global).

% Trace how authority over the English Bible moved from episcopal commission to critical committee to publisher catalog, and who paid at each stage. They publish the histories and sociologies of the translation economy and take no side in which version a congregation carries.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, bible_publishing_houses).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single, evidence-updated English Bible: manuscript discoveries and philological advances are collated, adjudicated by scholarly committees, and propagated into standardized translations that churches, seminaries, and publishers all share, so that clergy, teachers, and readers work from one improving reference text.
% TRANSFER_FUNCTION: Moves money from congregations, denominations, and individual buyers — Bible purchases, pew-rack replacement, licensing fees, seminary textbooks — to the publishing houses; and moves interpretive authority over the biblical text from clerical tradition to academic committees.
% ABSENT_VOICES: Lay readers sit on no translation committee; traditionalist communities object but are handled as an anti-intellectual residue rather than consulted; majority-text advocates and independent translators stand outside the contracting channel; and Global South churches that receive English translations are scarcely represented among the arbiters.
% DISAPPEARANCE_RATIONALE: If the revision apparatus vanished overnight, denominations would freeze on whatever inherited text they carried, manuscript knowledge would stop propagating into English Bibles, publishers would lose the new-edition cycle that anchors their Bible lines, seminaries would lose the critical curriculum, and traditionalist communities would regain institutional ground — the whole English Bible economy would reorganize around static texts.
% FOUNDING_PROBLEM: The English translations available before 1611 rested on few and late manuscripts and on imperfect philology; the KJV was itself commissioned as a revision to repair this, and the modern apparatus continues the same problem — keeping the English Bible current with the best attainable Hebrew and Greek text as new witnesses surface and the language shifts.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the physical manuscript record (Sinaiticus, Vaticanus, the Dead Sea Scrolls, continuing papyrus publications) exists independently of any publisher; secular philologists and historians of English document the late-Byzantine limitations of the Greek text underlying the 1611 New Testament; and the 1611 translators' own preface concedes that even the best translation is improvable — testimony from inside the tradition but outside the modern beneficiary set.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the rent component is real (copyright licensing on every reproduction, edition cycles timed to adoption waves, pew-rack replacement funded by congregations) but bounded — the 1611 text is public domain, several modern translations stream free as loss leaders, and the coordination service delivered is substantive. Suppression is authored at 0.34 and is a raw structural property, unscaled by power or scope in the engine's arithmetic: consumer choice between translations is genuine, but copyright thickets close the production side and denominational adoption forecloses choice inside congregations. Theater ratio 0.36: the philological core (collation, apparatus, committee deliberation) is functional work, while a growing minority of activity is performative — marketing-driven 'updated' editions, preface rhetoric of unprecedented accuracy, specialty editions differing trivially. Accessibility collapse 0.35: alternatives persist robustly (public-domain KJV, interlinears, the original languages), so understanding the arrangement does not collapse exits. Resistance 0.45: the KJV-only movement, liturgical conservatism, and congregational backlash against version switches constitute sustained, organized friction. The temporal series runs on one shared eight-point grid with all three metrics authored at every point; base extractiveness climbs with publishing consolidation and the blockbuster-translation era, theater climbs as edition marketing proliferates, and suppression_requirement rises mildly as copyright and digital-licensing enforcement matured even as denominational coercion eased — the story does track enforcement-capacity change, which is why the series is authored. Adoption-controversy cycles (switch, backlash, accommodation) recur qualitatively but the tracked metrics drift monotonically, so no oscillating series is asserted. Coalition prospects for the victim set are poor: laity are diffuse, traditionalists are organized but identity-bound, and translators are structurally excluded — the heterogeneity blocks joint action.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the publishing seat the arrangement is ordinary commerce over a product it improves and licenses; from the laity seat it is intermittent churn paid for in replaced books and relearned verses; from the traditionalist seat — identity_locked, so exit is experienced as self-dissolution rather than relocation — the same arrangement registers as displacement of a sacred inheritance; from the scholarly seat it is the progressive triumph of evidence over tradition. The engine derives these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: publishing houses collect directly (d near 0.0), academic critics collect authority and career structure, denominational boards collect cohesion while also funding the machinery. Victims sit toward the target end: traditionalist congregations combine victim status with identity_locked exit, placing them near full-target despite organized power; excluded translators are targets of the exclusion itself. One override is authored: ordinary_laity carry the powerless atom and appear in the victims array, which would derive a near-full-target directionality, but they also consume the coordination good directly — the improved text reaches them at near-zero price through gifting and free streaming — so their true position is an indirect-beneficiary-modified target, corrected downward to d=0.72. The override is the documented indirect-beneficiary case: the derivation from victim declaration alone would overstate their extraction exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — English translations resting on inferior manuscripts and imperfect philology — remains live: new witnesses still surface, and the language keeps moving, so the arrangement's mandate has not outlived its function and no mandatrophy resolution is declared. The classification prevents symmetric mislabeling: reading the structure as pure coordination ignores the licensing rents and the displacement of traditionalist communities; reading it as pure extraction ignores the genuine epistemic delivery that no rival channel currently matches. The forward risk is drift, not decay: if the manuscript record stabilizes and revisions become purely marketing events, theater_ratio climbs past the functional core and the structure slides toward inertial performance — the mismatch consumer should find status=live paired with verdict=world_rearranges, a coherent pairing with no zombie flag expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'Is the revisable_translation_reading the correct characterization of the KJV''s normative status, or does the exclusive_inspiration_reading (or functional_equivalence_reading) hold?',
    'Hermeneutical and sociological adjudication: which reading actually governs institutional practice across seminaries, denominations, and publishers, and whether providential-preservation arguments survive contact with the manuscript record.',
    'Under the exclusive reading this entire apparatus reclassifies as corruption of a fixed text with maximal suppression; under the functional-equivalence reading extraction drops further as translations become interchangeable tools. The whole beneficiary/victim structure moves with the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'This story is one reading of kernel kjv_text_1611; sibling readings instantiate structurally different constraints.').

omega_variable(
    publishing_rent_vs_production_cost,
    'How much of modern-translation revenue covers genuine production cost — committee stipends, typesetting, distribution — versus copyright rent collected on adoption churn?',
    'Publisher cost-structure disclosure, or comparison against public-domain translation projects (World English Bible, ASV derivatives) delivering comparable text quality without licensing revenue.',
    'A wide rent margin deepens the extraction component and supports the tangled_rope-to-snare drift reading; a narrow margin supports treating most measured extraction as coordination cost and the type as closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publishing_rent_vs_production_cost, empirical, 'Whether publishing gains reflect service cost or monopoly rent on the revision cycle.').

omega_variable(
    arbiter_legitimacy_vs_committee_politics,
    'Do translation-committee textual decisions track manuscript evidence, or denominational and marketing sensitivities (as in the gender-inclusive-language episodes)?',
    'Longitudinal comparison of committee decisions against blind text-critical assessment of the same passages, controlling for evidence strength.',
    'If politics dominates decisions, the arbiter layer is substantially theatrical: theater_ratio rises, the scholarly seat''s authority claim weakens, and effective extraction on the payer seats increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbiter_legitimacy_vs_committee_politics, empirical, 'Whether the scholarly arbiter layer functions as claimed or as cover.').

omega_variable(
    laity_choice_sovereignty,
    'Is translation selection genuine consumer choice, or is it foreclosed for laypeople by denominational adoption, seminary pipelines, and pew-rack supply?',
    'Trace actual version-selection events in congregations and denominations: who initiated, what alternatives were visible, what switching cost individual members faced.',
    'If choice is effectively foreclosed at the congregational level, suppression is materially higher than the authored 0.34 and the laity seat''s directionality correction toward 0.72 is overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(laity_choice_sovereignty, empirical, 'Whether the low-suppression profile of this reading survives contact with actual selection processes.').

omega_variable(
    traditionalist_identity_fusion,
    'Is traditionalist attachment to the 1611 text doctrinal conviction that could revise under evidence, or identity fusion that makes exit unthinkable regardless of argument?',
    'Post-departure trajectories of individuals and congregations that left KJV-only environments: whether former members adopt critical-text positions or migrate to equivalent identity-marked alternatives.',
    'If fusion dominates, the traditionalist seat''s identity_locked exit and near-full-target directionality are confirmed and its resistance is internally reinforced; if conviction dominates, exit is merely constrained and its effective extraction falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(traditionalist_identity_fusion, empirical, 'Structural versus internalized binding of the traditionalist seat to the older text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_rev_tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(kjv_rev_tr_t0, observed).
narrative_ontology:measurement(kjv_rev_tr_t20, kjv_text_1611__revisable_translation_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(kjv_rev_tr_t20, observed).
narrative_ontology:measurement(kjv_rev_tr_t40, kjv_text_1611__revisable_translation_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement_basis(kjv_rev_tr_t40, observed).
narrative_ontology:measurement(kjv_rev_tr_t60, kjv_text_1611__revisable_translation_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(kjv_rev_tr_t60, observed).
narrative_ontology:measurement(kjv_rev_tr_t80, kjv_text_1611__revisable_translation_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(kjv_rev_tr_t80, observed).
narrative_ontology:measurement(kjv_rev_tr_t100, kjv_text_1611__revisable_translation_reading, theater_ratio, 100, 0.31).
narrative_ontology:measurement_basis(kjv_rev_tr_t100, observed).
narrative_ontology:measurement(kjv_rev_tr_t120, kjv_text_1611__revisable_translation_reading, theater_ratio, 120, 0.34).
narrative_ontology:measurement_basis(kjv_rev_tr_t120, observed).
narrative_ontology:measurement(kjv_rev_tr_t140, kjv_text_1611__revisable_translation_reading, theater_ratio, 140, 0.36).
narrative_ontology:measurement_basis(kjv_rev_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(kjv_rev_be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(kjv_rev_be_t0, observed).
narrative_ontology:measurement(kjv_rev_be_t20, kjv_text_1611__revisable_translation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(kjv_rev_be_t20, observed).
narrative_ontology:measurement(kjv_rev_be_t40, kjv_text_1611__revisable_translation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement_basis(kjv_rev_be_t40, observed).
narrative_ontology:measurement(kjv_rev_be_t60, kjv_text_1611__revisable_translation_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(kjv_rev_be_t60, observed).
narrative_ontology:measurement(kjv_rev_be_t80, kjv_text_1611__revisable_translation_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(kjv_rev_be_t80, observed).
narrative_ontology:measurement(kjv_rev_be_t100, kjv_text_1611__revisable_translation_reading, base_extractiveness, 100, 0.59).
narrative_ontology:measurement_basis(kjv_rev_be_t100, observed).
narrative_ontology:measurement(kjv_rev_be_t120, kjv_text_1611__revisable_translation_reading, base_extractiveness, 120, 0.61).
narrative_ontology:measurement_basis(kjv_rev_be_t120, observed).
narrative_ontology:measurement(kjv_rev_be_t140, kjv_text_1611__revisable_translation_reading, base_extractiveness, 140, 0.62).
narrative_ontology:measurement_basis(kjv_rev_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv_rev_su_t0, kjv_text_1611__revisable_translation_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(kjv_rev_su_t0, observed).
narrative_ontology:measurement(kjv_rev_su_t20, kjv_text_1611__revisable_translation_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(kjv_rev_su_t20, observed).
narrative_ontology:measurement(kjv_rev_su_t40, kjv_text_1611__revisable_translation_reading, suppression_requirement, 40, 0.23).
narrative_ontology:measurement_basis(kjv_rev_su_t40, observed).
narrative_ontology:measurement(kjv_rev_su_t60, kjv_text_1611__revisable_translation_reading, suppression_requirement, 60, 0.26).
narrative_ontology:measurement_basis(kjv_rev_su_t60, observed).
narrative_ontology:measurement(kjv_rev_su_t80, kjv_text_1611__revisable_translation_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement_basis(kjv_rev_su_t80, observed).
narrative_ontology:measurement(kjv_rev_su_t100, kjv_text_1611__revisable_translation_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement_basis(kjv_rev_su_t100, observed).
narrative_ontology:measurement(kjv_rev_su_t120, kjv_text_1611__revisable_translation_reading, suppression_requirement, 120, 0.32).
narrative_ontology:measurement_basis(kjv_rev_su_t120, observed).
narrative_ontology:measurement(kjv_rev_su_t140, kjv_text_1611__revisable_translation_reading, suppression_requirement, 140, 0.34).
narrative_ontology:measurement_basis(kjv_rev_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of kernel kjv_text_1611 per the epsilon-invariance principle: the colloquial label 'the KJV's authority' covers three structurally distinct arrangements. This file authors the revisable_translation_reading, whose standing arrangement is the scholarly-publishing revision apparatus (epsilon 0.62, extraction concentrated in publishing control of modern translations). The exclusive_inspiration_reading authors an enforced-text monopoly (high suppression, different victim set); the functional_equivalence_reading authors a pluralist matching arrangement (low extraction, coordination-dominant). The epsilon values differ because each reading constitutes a different arrangement, not because one constraint is measured differently. Upstream-downstream structure: this reading's revision pipeline supplies the modern translations that the functional-equivalence reading arranges, and its evidential premises are what the exclusive reading's fixed-text premise contradicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__revisable_translation_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
