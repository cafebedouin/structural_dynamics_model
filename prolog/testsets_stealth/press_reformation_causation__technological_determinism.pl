% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Press-Caused-Reformation Technological Determinism Narrative
 *   domain: historiography/history_of_technology/religious_history
 *
 * SUMMARY:
 *   The colloquial claim 'the printing press caused the Reformation' is a
 *   contested kernel with three live readings; this story instantiates only
 *   the technological_determinism reading — that print capacity was the
 *   upstream, effectively unopposed cause, making censorship impossible and
 *   vernacular scripture inevitable, with reformers as downstream
 *   beneficiaries of exogenous capacity and institutional resistance futile.
 *   The constraint this story describes is the determinist narrative as an
 *   operating arrangement in the historiographical field: it presents itself
 *   as a discovered media-historical law (inevitability language, futility of
 *   resistance — mountain-shaped self-presentation), while in operation it is
 *   a maintained interpretive arrangement with identifiable beneficiaries and
 *   costs. This is deliberate FSM authoring: the narrative is claimed as
 *   mountain (its own framing) with beneficiaries declared and omegas
 *   documenting the natural-law-versus-constructed ambiguity, so the engine
 *   can evaluate the false-summit signature. The epsilon referent is the
 *   standing arrangement — the determinist narrative's dominance of the
 *   Reformation's causal account — assessed as it actually operates; it is
 *   neither the sixteenth-century print regime itself (a separate constraint)
 *   nor the revisionist alternative this narrative suppresses. The sibling
 *   readings, strategic_deployment and mutual_shaping, are separate
 *   constraints linked in the network section. KEY AGENTS (by structural
 *   relationship): - grand_narrative_historians: Primary beneficiary
 *   (powerful/identity_locked) — career capital, citation networks, and
 *   synthesis authority accrue to holders of the causal spine -
 *   survey_gatekeepers: Agenda setter (institutional/constrained) —
 *   administer the narrative through editions, syllabi, and review
 *   gatekeeping; also collect authority from its stability -
 *   textbook_publishers: Secondary beneficiary (institutional/arbitrage) —
 *   collect revenue from clean, stable causal narratives -
 *   technological_determinist_media_theorists: Secondary beneficiary
 *   (organized/identity_locked) — the press-to-Reformation case is the
 *   load-bearing template for modern media-determinism claims -
 *   revisionist_reformation_historians: Primary target/payer
 *   (organized/constrained) — bear marginalization costs; must answer the
 *   default narrative before their findings are heard -
 *   censorship_historians: Excluded voice (moderate/constrained) — hold the
 *   archival evidence that most directly tests the censorship-impossibility
 *   premise but sit outside the causal debate - survey_course_students: Payer
 *   (powerless/mobile) — carry the flattened causal model; exit entirely when
 *   the course ends - historiography_analysts: Analytical observer
 *   (analytical/analytical) — study the debate itself with no stake in the
 *   outcome
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.6).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.6).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.6).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Press-Caused-Reformation Technological Determinism Narrative").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "historiography/history_of_technology/religious_history").

domain_priors:requires_active_enforcement(press_reformation_causation__technological_determinism).
domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e').
narrative_ontology:cs_kernel_codification('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', distributed).
narrative_ontology:cs_authority_grounding('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', lineage).
narrative_ontology:cs_interpretation_layer_present('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e').
narrative_ontology:cs_reading_relation('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', foundational, print_made_censorship_structurally_impossible).
narrative_ontology:cs_axiom_status(print_made_censorship_structurally_impossible, holdable).
narrative_ontology:cs_axiom_grounding('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', print_made_censorship_structurally_impossible, empirically_contingent).
narrative_ontology:cs_axiom('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', foundational, technological_capacity_trumps_institutional_resistance).
narrative_ontology:cs_axiom_status(technological_capacity_trumps_institutional_resistance, holdable).
narrative_ontology:cs_axiom_grounding('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', technological_capacity_trumps_institutional_resistance, empirically_contingent).
narrative_ontology:cs_reference_frame('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', print_capacity_as_upstream_fixed_cause).
narrative_ontology:cs_drift_state('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', post_revisionist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b00a4ae3-7507-4a1e-bd7e-cd266cf7ad5e', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, grand_narrative_historians).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, survey_gatekeepers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, textbook_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, technological_determinist_media_theorists).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, revisionist_reformation_historians).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, survey_course_students).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_determinism_template).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, print_revolution_synthesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars whose major syntheses and survey volumes anchor the print-caused-Reformation account. Citation networks, named lectureships, and synthesis-writing opportunities flow to those who extend and defend the causal spine. Their professional self-concept is built around having established the print-revolution account; revising it would mean recasting their life's work, so they defend the narrative's core claims even while acknowledging qualifications at the margins.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, grand_narrative_historians, beneficiary,
    powerful, biographical, identity_locked, continental).

% Survey-textbook authors, curriculum committees, and editors at flagship journals decide which causal account the field's default teaching narrative carries. They commission and approve editions, set syllabus norms, and referee which revisionist findings get footnoted versus incorporated into the spine. Their authority depends on the narrative remaining stable enough to teach; each revisionist wave imposes update labor they must absorb.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, survey_gatekeepers, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__technological_determinism, survey_gatekeepers, beneficiary).

% Publishers of world-history and Western-civilization surveys sell a causal account that fits a chapter and a memorable mechanism. Clean inevitability narratives are cheaper to produce, easier to market, and more stable across editions than contested multi-causal accounts. If the market shifted, they would repackage whatever account sells; their stake is revenue, not the specific claim.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, textbook_publishers, beneficiary,
    institutional, immediate, arbitrage, global).

% Media theorists in the McLuhan lineage use the press-to-Reformation case as the founding template for claims that communication technologies determine social and religious outcomes; the same template gets extended to broadcast media and the internet. The historical case's authority is load-bearing for their theoretical program, and a successful challenge to the template would unsettle the modern analogies built on it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, technological_determinist_media_theorists, beneficiary,
    organized, biographical, identity_locked, global).

% Scholars centered on printer strategies, urban politics, censorship records, and reading practices whose findings complicate or contradict the inevitability account. They publish in specialist venues but must fight for space in surveys and syllabi, answer the default narrative before their own findings can be heard, and watch their work get absorbed as footnotes rather than spine revisions. Leaving would mean abandoning Reformation studies for periods where the default narrative does not bind their material.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, revisionist_reformation_historians, payer,
    organized, biographical, constrained, continental).

% Archival historians of imperial mandates, urban censorship boards, the Index, and printer self-censorship hold the evidence that bears most directly on whether censorship under print was actually impossible. Their work sits outside the causal debate's mainstream, cited occasionally but rarely invited to shape the spine. They would insist the efficacy question be settled empirically before any inevitability claim is taught as fact.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, censorship_historians, excluded,
    moderate, biographical, constrained, continental).

% Undergraduates encounter the print-caused-Reformation story as settled fact in surveys and popular media. They carry away a flattened causal model in which technology decides outcomes and human choices are downstream; the contingency structure of the actual history is invisible to them. Their cost is paid in misunderstanding, and they exit the situation entirely when the course ends, with no stake in defending or fixing anything.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, survey_course_students, payer,
    powerless, immediate, mobile, global).

% Historians of historiography and media-studies scholars who study the debate itself: how the causal account rose, what work it does for the field, and how revisionist challenges are absorbed or deflected. They take testimony from every seat and hold no stake in which account wins.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, historiography_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__technological_determinism, grand_narrative_historians).
narrative_ontology:fixing_cost_class(press_reformation_causation__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single memorable causal mechanism that organizes how the Reformation is taught and popularly understood: print capacity explains why Luther survived when Hus and Wycliffe did not. The narrative gives survey courses a shared spine, gives textbooks a chapter structure, and gives the public a one-sentence answer to a genuinely multi-causal historical question.
% TRANSFER_FUNCTION: Moves interpretive authority and career capital from agency-centered and censorship-focused historians to the scholars and gatekeepers who hold the causal spine; moves a simplified causal certainty to students and general readers in exchange for attention and textbook revenue; and shifts the burden of proof onto dissenters, who must disprove inevitability rather than merely offer a rival account.
% ABSENT_VOICES: Censorship historians hold the archival evidence that most directly tests the censorship-impossibility premise but sit outside the causal debate's mainstream; mutual-shaping and strategic-deployment scholars are present in the specialist literature but marginalized in surveys; the sixteenth-century actors whose choices the narrative renders downstream — Luther, the printers, the imperial estates — cannot object to being written out of the causal story.
% DISAPPEARANCE_RATIONALE: If the determinist narrative vanished overnight, survey courses would reorganize around contested multi-causal accounts, textbook chapters would need restructuring, media theorists would lose the founding template for technological-determinism claims, and the revisionist literature would move from footnote to spine. Popular discourse would lose its one-sentence answer about why the Reformation succeeded.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century historiography needed a mechanism to explain why Luther's protest survived state repression when earlier movements — Wycliffe's, Hus's — were contained or crushed. Print capacity offered the clean answer: reproduction at scale outran any censor's reach.
% FOUNDING_PROBLEM_CORROBORATION: Febvre and Martin's L'Apparition du livre (1958) and Eisenstein's The Printing Press as an Agent of Change (1979) attest the founding problem from outside the current beneficiary coalition — Eisenstein herself framed print as an agent of change rather than a sole cause. The agency-centered and censorship-efficacy literature of the 2000s and 2010s corroborates that the underlying problem is live while disputing this reading's specific mechanism. No corroborating source outside the benefiting parties attests that the problem is solved by unopposed technological causation.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.60) because the narrative collects interpretive territory: research agendas are channeled, revisionist findings are absorbed as footnotes rather than spine revisions, and a causal certainty the evidence only partly supports is sold as settled in surveys and popular media. Suppression (0.60) is predominantly structural — survey economics, syllabus gatekeeping, review norms — with a smaller internalized component: graduate training socializes junior scholars into the spine before they encounter the revisionist literature. Theater (0.44) has risen steadily: as the empirical debate matured, a growing share of the narrative's activity became performative — ritual inevitability assertions, anniversary framings, template extensions to modern media — relative to its functional core of genuine print-effects analysis. Accessibility collapse is moderate (0.45): alternatives persist, since both sibling readings remain live and revisionists publish continuously; the narrative constrains the default account without collapsing the alternatives. Resistance is substantial (0.62): an organized revisionist literature has contested the spine for two decades. The measurement series run on one shared eight-point grid (t=0 to t=65, roughly 1960 to 2025): extractiveness peaks around t=40 (peak textbook dominance, circa 2000) and erodes slightly under revisionist pressure, while the suppression requirement keeps rising — the enforcement apparatus hardens as the narrative must be actively defended. Suppression is authored as a raw structural property; the engine scales extractiveness by directionality and scope. The dominant coordination function is identity coordination: what every trained Reformation historian shares, and the boundary the McLuhan-lineage maintains — and the identity framing also does cover-story work ('this is just what the evidence shows'), which is why the conservative floor for this type is appropriate rather than a raised one. The claimed type is mountain as FSM authoring: the narrative's own framing asserts natural-law inevitability, and the claim is left unreconciled with the metrics by design.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and payer seats compute differently from the same structure. From the grand-narrative historian's seat the narrative is the field's hard-won synthesis — the mechanism that finally explained why Luther survived when Hus did not; its stability is an achievement, and challenges are noise. From the revisionist seat the same structure operates as enforced orthodoxy: their findings must defeat a default before they can be heard, and absorption-as-footnote is a career tax. From the gatekeeper seat the narrative is teachable stability — every revisionist wave is update labor to be absorbed. Students experience it as settled fact with no awareness of the contest. Same-level divergence is sharp: grand-narrative and revisionist historians hold similar formal standing (senior scholars with publication records), but constraint-specific factors differentiate them — exit options (identity_locked versus constrained) and their positions in the accrual structure. The engine computes per-seat classifications from the structural data; the authored mountain claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (grand-narrative historians, survey gatekeepers, textbook publishers, media-determinism theorists) sit near the beneficiary end: the narrative subsidizes them with career capital, authority, revenue, and a load-bearing theoretical template. Victims (revisionist historians, students) sit near the target end: they pay in marginalization, update labor, and flattened understanding. Two identity-lock dynamics shape the structure without changing the beneficiaries' d: grand-narrative historians are professionally fused (their syntheses are their life's work — recanting means recasting a career), and media theorists are ideologically fused (the template licenses their modern claims). This produces the unusual feature that enforcement intensity comes partly from beneficiaries themselves, not only from agenda-setters: identity-fused beneficiaries defend the narrative as self-defense. If that identity frame broke — a leading synthesis author recanting — enforcement would shift entirely to gatekeepers and publishers, and the narrative's persistence would depend on inertia rather than defense. Publishers are the exception: arbitrage exit and a revenue stake only; they would switch accounts the moment the market moved, which is why the narrative's persistence cannot be attributed to them alone. Students are victims with mobile exit and no stake, which is why their cost never converts into pressure. Censorship historians are excluded rather than targeted: the debate's gatekeeping keeps their evidence peripheral, which is structurally distinct from bearing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — explaining why Luther's protest survived repression when Wycliffe's and Hus's did not — is live, but the parties dispute whether this reading's mechanism solves it: censorship-efficacy and agency-centered research contest the decisive-variable claim. The classification prevents two mislabels. Reading the narrative as the mountain it claims to be would launder its maintenance costs and beneficiary structure as natural law — exactly the false-summit failure the FSM signature exists to catch. Reading it as pure extraction would erase its genuine coordination function: a shared, teachable baseline that solved a real problem of causal pedagogy. The hybrid structure underneath (coordination plus asymmetric extraction under active enforcement) is what the engine should find once the mountain claim is tested. founding_problem_status is contested and disappearance is world_rearranges, so no dead-mandate zombie flag fires; the theater_ratio drift series is the monitored signal — if theater keeps rising while extractiveness erodes, the arrangement is transitioning toward theatrical maintenance of a spine nobody defends on the merits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_arrangement,
    'Is the press-caused-Reformation causal claim a discovered regularity of media history, or a constructed historiographical arrangement whose inevitability framing benefits identifiable agents?',
    'Comparative historiography: test whether the causal spine persists in scholarly traditions and teaching markets that do not reward clean inevitability narratives, and whether the claim survives adversarial review once censorship-efficacy evidence is admitted at spine level rather than footnote level.',
    'If constructed, the mountain claim is a false summit: the engine''s FSM signature should reclassify toward tangled_rope, with the beneficiary coalition''s maintenance costs counted as extraction. If a discovered regularity, the inevitability framing stands and the narrative''s extraction is mostly the price of a correct simplification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_arrangement, conceptual, 'Natural law versus constructed arrangement ambiguity (FSM documentation).').

omega_variable(
    censorship_efficacy_question,
    'Was censorship under print actually structurally impossible, or partially effective through imperial mandates, urban censorship boards, the Index, and printer self-censorship?',
    'Archival studies of enforcement outcomes: confiscation rates, prosecution records, printer compliance, and the geographic distribution of suppressed versus surviving editions.',
    'If censorship was partially effective, the impossibility premise fails empirically, this reading''s foundational axiom is overridden by evidence, and the narrative''s persistence becomes harder to justify as description — raising effective extraction and pushing classification away from mountain entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(censorship_efficacy_question, empirical, 'Empirical status of the censorship-impossibility premise.').

omega_variable(
    counterfactual_contingency_question,
    'Would the Reformation have failed or taken a materially different form under manuscript-scale reproduction, or without Luther''s specific choices and political protection?',
    'Systematic comparison of pre-print reform movements (Wycliffe, Hus) against print-era spread, controlling for political protection, urban governance, and theological content, plus counterfactual modeling of circulation capacity.',
    'If contingency was decisive, the inevitability claim collapses and the strategic_deployment and mutual_shaping readings gain the evidentiary high ground; if capacity was decisive, this reading''s spine holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_contingency_question, empirical, 'Counterfactual status of the inevitability claim.').

omega_variable(
    sibling_reading_underdetermination,
    'This constraint instantiates only the technological_determinism reading of the press_reformation_causation kernel; would the strategic_deployment or mutual_shaping siblings assign a different epsilon, beneficiary/victim structure, or type to the same historical material?',
    'Generate the sibling stories as separate constraints and compare per-seat classifications and epsilon over the shared referent (the standing causal narrative''s dominance). The disagreement is located in the locus of causation — capacity versus agency — and in the status of the resistance-futile claim.',
    'The siblings locate agency in printers and reformers (deployment) or in bidirectional co-evolution (mutual shaping), which would dissolve the inevitability framing, redistribute beneficiary status toward printer-capital and reformer-agency scholarship, and lower the narrative''s extractiveness by removing the futility claim that does much of its enforcement work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_underdetermination, conceptual, 'Committer structure: kernel, reading, and expected sibling delta.').

omega_variable(
    textbook_layer_absorption,
    'Does the survey/textbook interpretive layer absorb revisionist findings as footnotes without surfacing kernel revision, and does that absorption sustain the narrative''s extraction?',
    'Longitudinal content analysis of survey editions: where revisionist findings appear (footnote versus spine), whether causal language shifts (made-inevitable versus enabled), and how quickly gatekeepers incorporate censorship-efficacy results.',
    'If absorption is real, the narrative is a maintained arrangement rather than an eroding one — theater_ratio should keep rising and the maintenance cost should count as enforcement. If the spine is actually updating, the narrative is transitioning and extraction should fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textbook_layer_absorption, empirical, 'Whether the interpretive buffer absorbs drift without revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(pres_tr_t0, observed).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causation__technological_determinism, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(pres_tr_t10, observed).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__technological_determinism, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(pres_tr_t20, observed).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causation__technological_determinism, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(pres_tr_t30, observed).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.37).
narrative_ontology:measurement_basis(pres_tr_t40, observed).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causation__technological_determinism, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(pres_tr_t50, observed).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causation__technological_determinism, theater_ratio, 60, 0.43).
narrative_ontology:measurement_basis(pres_tr_t60, observed).
narrative_ontology:measurement(pres_tr_t65, press_reformation_causation__technological_determinism, theater_ratio, 65, 0.44).
narrative_ontology:measurement_basis(pres_tr_t65, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(pres_be_t0, observed).
narrative_ontology:measurement(pres_be_t10, press_reformation_causation__technological_determinism, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(pres_be_t10, observed).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__technological_determinism, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(pres_be_t20, observed).
narrative_ontology:measurement(pres_be_t30, press_reformation_causation__technological_determinism, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(pres_be_t30, observed).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(pres_be_t40, observed).
narrative_ontology:measurement(pres_be_t50, press_reformation_causation__technological_determinism, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(pres_be_t50, observed).
narrative_ontology:measurement(pres_be_t60, press_reformation_causation__technological_determinism, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(pres_be_t60, observed).
narrative_ontology:measurement(pres_be_t65, press_reformation_causation__technological_determinism, base_extractiveness, 65, 0.6).
narrative_ontology:measurement_basis(pres_be_t65, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__technological_determinism, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(pres_su_t0, observed).
narrative_ontology:measurement(pres_su_t10, press_reformation_causation__technological_determinism, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(pres_su_t10, observed).
narrative_ontology:measurement(pres_su_t20, press_reformation_causation__technological_determinism, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(pres_su_t20, observed).
narrative_ontology:measurement(pres_su_t30, press_reformation_causation__technological_determinism, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(pres_su_t30, observed).
narrative_ontology:measurement(pres_su_t40, press_reformation_causation__technological_determinism, suppression_requirement, 40, 0.55).
narrative_ontology:measurement_basis(pres_su_t40, observed).
narrative_ontology:measurement(pres_su_t50, press_reformation_causation__technological_determinism, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(pres_su_t50, observed).
narrative_ontology:measurement(pres_su_t60, press_reformation_causation__technological_determinism, suppression_requirement, 60, 0.6).
narrative_ontology:measurement_basis(pres_su_t60, observed).
narrative_ontology:measurement(pres_su_t65, press_reformation_causation__technological_determinism, suppression_requirement, 65, 0.6).
narrative_ontology:measurement_basis(pres_su_t65, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the press caused the Reformation' decomposes, per the epsilon-invariance principle, into at least three structurally distinct readings — this file (technological_determinism), strategic_deployment, and mutual_shaping — each with its own epsilon, beneficiary/victim structure, and type. The colloquial label also mixes a sixteenth-century material question (how effective print-era censorship was as a period constraint on the Church) with a modern historiographical question (what work the causal narrative does in the field today); this story authors only the latter, as instantiated by the determinism reading. In citation practice this reading is the upstream spine that textbooks transmit, and it structurally shapes the conditions under which the sibling readings compete (they must defeat the default before being heard) — while its own core premises foreclose both siblings within a single causal framework. Sibling stories should link back via their own network.affects_constraints entries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
