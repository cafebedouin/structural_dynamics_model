% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Discontinuity Reading of the Correct-Latin Standard: Textual-Reoccupation Orthodoxy
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the discontinuity_reading of the
 *   correct_latin_kernel: the claim that Classical and Medieval Latin are
 *   distinct systems, that native transmission of classical Latinity broke
 *   irrecoverably, and that reconstruction therefore proceeded by symbolic
 *   reoccupation from texts — recovering lost structure from written
 *   monuments rather than continuing a living practice. Under this reading
 *   the standing arrangement treats medieval forms as corruptions, installs
 *   the classical corpus as the sole measure of correctness, and makes
 *   textual expertise the gate through which legitimate Latin passes. The
 *   arrangement has a real coordination function (a shared supraregional
 *   learned language, reliable access to antiquity) AND an asymmetric cost
 *   structure (decades of acquisition labor imposed on learners,
 *   retrospective degradation of a millennium of medieval writing, editorial
 *   authority concentrated in a credentialed class). The claim/metric gap is
 *   deliberate: the reading rhetorically presents itself as a discovered fact
 *   about how things are, while the authored metrics describe a constructed,
 *   actively enforced, moderately extractive arrangement — the engine
 *   computes the divergence; nothing here reconciles the claim to the
 *   metrics.
 *
 * KEY AGENTS:
 *   - - philological_expert_class: agenda-setting beneficiary (institutional / identity_locked) — defines and certifies correctness; careers fused with the standard
 *   - - ecclesiastical_administrators: early-phase agenda setter (institutional / constrained) — commissioned the correction of law, liturgy, and doctrine
 *   - - classical_text_editors: beneficiary (organized / mobile) — collect editorial authority over the inherited corpus
 *   - - elite_latin_schools: beneficiary (institutional / identity_locked) — prestige and demand flow from the mandatory classical curriculum
 *   - - latin_students_across_generations: primary payer (powerless / constrained) — bear the acquisition labor; a minority converts into beneficiaries
 *   - - medieval_scholastic_writers: payer (powerless / trapped) — retrospectively graded against a standard they did not share
 *   - - monastic_scriptoria: payer (moderate / trapped) — transmission decisions overwritten by classical emendation
 *   - - vernacular_intellectuals: excluded voice (organized / mobile) — built rival prestige economies outside the correctness conversation
 *   - - modern_medievalist_rehabilitators: analytical observer (moderate / analytical) — documents scholastic Latin as a functional register
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.46).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.4).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Discontinuity Reading of the Correct-Latin Standard: Textual-Reoccupation Orthodoxy").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, 'cf1058e9-8dcc-497b-8c41-92a8a0a0fca4').
narrative_ontology:cs_kernel_codification('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', fixed_text).
narrative_ontology:cs_authority_grounding('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', lineage).
narrative_ontology:cs_interpretation_layer_present('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4').
narrative_ontology:cs_reading_relation('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', correct_latin_kernel__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', foundational, native_transmission_irrecoverably_broken).
narrative_ontology:cs_axiom_status(native_transmission_irrecoverably_broken, holdable).
narrative_ontology:cs_axiom_grounding('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', native_transmission_irrecoverably_broken, empirically_contingent).
narrative_ontology:cs_axiom('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', foundational, reconstruction_is_symbolic_reoccupation).
narrative_ontology:cs_axiom_status(reconstruction_is_symbolic_reoccupation, holdable).
narrative_ontology:cs_axiom_grounding('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', reconstruction_is_symbolic_reoccupation, instrumental).
narrative_ontology:cs_reference_frame('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', classical_corpus_normative_frame).
narrative_ontology:cs_drift_state('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', contemporary_mass_higher_education, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cf1058e9-8dcc-497b-8c41-92a8a0a0fca4', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, philological_expert_class).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_text_editors).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, elite_latin_schools).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, latin_students_across_generations).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_scholastic_writers).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, monastic_scriptoria).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, ecclesiastical_administrators).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, carolingian_correctio_program).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, ad_fontes_return_principle).
narrative_ontology:constraint_vindicates(correct_latin_kernel__discontinuity_reading, classical_canon_normativity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines correct Latin by measurement against the classical corpus, trains and certifies the people who will teach and edit it, examines candidates, and decides which readings stand in printed editions. Members' livelihoods and self-conceptions are bound up with the standard they administer; leaving it means leaving the profession. Authority, posts, and deference flow in; the unending labor of defending textual decisions flows out.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, philological_expert_class, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, philological_expert_class, beneficiary).

% From the late eighth century onward commissioned the correction of service books, legal codes, and doctrinal texts, because garbled copies threatened uniform worship and cross-kingdom governance. Funded the schools and copy shops, gained reliable texts and a continent-spanning administrative language, and paid the bill for the correction campaigns.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, ecclesiastical_administrators, agenda_setter,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, ecclesiastical_administrators, beneficiary).

% Make their living collating manuscripts and producing editions that restore classical wording. The recovery mandate is their job market; the skills port reasonably well to other old-text fields, so departure is feasible though costly.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_text_editors, beneficiary,
    organized, biographical, mobile, continental).

% Institutions whose curricula, fees, and reputations rest on transmitting the classical standard. The curriculum is what the institution is; abandoning it would dissolve the institution. Guaranteed demand flows in from advancement that runs through Latin; the cost of staffing ever harder material flows out.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, elite_latin_schools, beneficiary,
    institutional, generational, identity_locked, continental).

% Children and young adults who spent most of a decade acquiring a language nobody spoke at home, because advancement in church, university, law, and diplomacy ran through it. Most bore the cost and went on to other work; a minority converted the investment into membership among the certified.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, latin_students_across_generations, payer,
    powerless, biographical, constrained, continental).

% Authors working roughly 800 to 1400 in the technical Latin of their own day — dialectic, theology, administration. Later grading measures their prose against Cicero and finds it wanting; their reputations hang on a comparison they never entered and cannot contest.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_scholastic_writers, payer,
    powerless, biographical, trapped, continental).

% Copy shops whose selection, spelling, and layout decisions produced the surviving witnesses to ancient texts. Cleaned-up corrected editions overwrite those decisions, and the evidence of how texts actually traveled disappears with each tidied print run.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, monastic_scriptoria, payer,
    moderate, generational, trapped, regional).

% Poets and scholars who held that serious writing belonged in living tongues — Tuscan, French, Castilian — and built rival audiences and patronage outside the Latin conversation. They were never seated in the councils where the standard was set.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, vernacular_intellectuals, excluded,
    organized, generational, mobile, national).

% Scholars of the past century who documented scholastic Latin as a working technical language fitted to its tasks and who edit medieval texts on medieval terms. They observe, publish, and argue; they hold no lever over the classical standard itself.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, modern_medievalist_rehabilitators, observer,
    moderate, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, philological_expert_class).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single supraregional learned language and a stable bridge to ancient texts: scholars in different kingdoms and centuries read the same authorities; law, liturgy, and scholarship circulate without fragmenting into local dialects; the classical corpus stays accessible despite broken native transmission.
% TRANSFER_FUNCTION: Moves years of learner labor and credential-deference from students and writers to the credentialed textual elite; moves authority over all previously written Latin, including the medieval corpus, to whoever holds classical competence; concentrates editorial control of the inherited texts.
% ABSENT_VOICES: Vernacular intellectuals and defenders of medieval Latinity sit outside the standard-setting conversation; the medieval authors themselves cannot appear, and the scribes whose decisions shaped transmitted texts left no vote. The apparent unanimity that medieval forms are failures partly reflects who was in the room when the grading was fixed.
% DISAPPEARANCE_RATIONALE: If the discontinuous-recovery standard vanished overnight, European learned life would visibly reorganize: teaching would shift to living-language or vernacular pedagogies, editorial authority over medieval texts would pass to different hands, and the classical corpus would either be kept by a small self-selecting community or lose its infrastructural role. Schools, chanceries, and republics of letters arranged themselves around this standard for twelve centuries.
% FOUNDING_PROBLEM: After Roman schooling collapsed and the Romance vernaculars diverged, nobody acquired classical Latin natively anymore; copied texts accumulated errors at every generation; law, liturgy, and doctrine still needed a shared authoritative language whose living transmission had already broken.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Romance-language linguistics documents the divergence that proves native transmission ended; manuscript transmission studies independently quantify how copying errors accumulate; historians of education separately attest the loss of the late-antique school infrastructure. The beneficiaries themselves contribute little beyond restating their own mandate.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).
:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.46 in the current period because the arrangement's costs remain real but its reach has contracted: within the surviving domains (graduate philology, textual criticism, elite curricula) the price of admission is still years of labor for an idiom no one speaks natively, while the delivered good — reliable contact with the classical corpus — is genuine and large. The series traces a hump, not a line: modest extraction during the Carolingian repair phase, accumulation through the humanist peak (Ciceronian purism, contempt for 'barbarous' scholastic prose, printing amplifying editorial leverage), a secondary plateau under nineteenth-century state-examined classical curricula, then contraction as Latin requirements fell and medieval Latin was rehabilitated. Suppression is a raw structural property of the arrangement and is deliberately NOT scaled by power or scope here — scaling is the engine's business. Enforcement capacity rose from capitulary mandates through university examination and manuscript emendation to its most complete machinery in the nineteenth-century gymnasium/lycee system, then decayed sharply after 1950; that rise-and-fall is why suppression_requirement is tracked on the shared grid rather than left static. Theater peaks in the Ciceronian display culture of 1450–1550 (imitation exercises, purity quarrels, ceremonial prose) at 0.45, settling to 0.28 today where the surviving performance layer is mottoes, diplomas, and anniversary orations around a still-functional scholarly core. Accessibility_collapse is low-moderate (0.35) because alternatives never closed: vernaculars rose, hybrid registers persisted in practice, and understanding the arrangement does not compel participation in it. Resistance is correspondingly substantial (0.55): anti-Ciceronian humanists, the vernacular turn, twentieth-century rehabilitation of scholastic Latin, and curriculum rebellions all pressed on the standard continuously. Suppression in this arrangement is predominantly structural (examination gates, editorial control, career dependency) with an internalized residue — trained shame at non-classical style that outlives the gates — noted here and left to the omega layer rather than split into a fabricated scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the philological expert seat, the arrangement is a rescue operation: someone saved the texts, and the price was the price of craftsmanship. From the student seat, the same structure is a decade-long toll over a dead language, arbitrated by people who profit from the arbitration. From the medieval-writer seat it arrives entirely posthumously — a grading applied to a fixed corpus with no recourse. The vernacular seat never entered the conversation at all, which means the consensus that 'corruption' was the right frame was manufactured partly by absence. A further gap: because the reading is stated as a fact about linguistic history ('these are distinct systems'), observers are invited to mistake the arrangement built on it for a necessity — a mountain-shaped misread the per-seat computations expose, since no seat that pays experiences the standard as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The expert class declares as beneficiary and agenda setter: it collects status, posts, and editorial authority, and its identity_locked exit pins it near the beneficiary pole (d near 0.0 amplified by fusion of profession with standard). Ecclesiastical administrators are dual-positioned — early agenda setters who also collected reliable texts — sitting nearer the beneficiary end than the derivation from their payer-side funding costs alone would suggest. Editors and schools are straightforward beneficiaries (low d). Students are the mass payer seat: powerless, constrained exit, d near the full-target end, with the caveat that the arrangement converts a minority of payers into beneficiaries over a career, softening but not reversing their net position. Medieval scholastic writers and monastic scriptoria are maximally trapped targets — the grading and the emendation apply to completed work, d pinned near 1.0. Vernacular intellectuals are excluded rather than coordinated: the derivation assigns them no beneficiary relief, and their mobile exit (building rival prestige economies) explains why resistance took the form of secession rather than siege. Coalition note: the diffuse payer classes (students, scriptoria) never assembled coalition infrastructure; the effective counter-pressure historically came from the one organized, mobile excluded seat — the vernacular movement — which exited rather than negotiated.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is genuinely live: classical Latin is never again acquired natively, so the recovery problem the arrangement was built for persists in perpetuity. That keeps this from being a simple zombie — the core function (critical recovery of texts, maintenance of a precise reference language) still performs. What HAS atrophied is the strong-form enforcement arm: the corruption-grading of medieval Latin and the exclusive-mediation claim have lost their reach (Latin requirements collapsed, medieval texts are increasingly edited on medieval terms, digital corpora weaken gatekeeping by widening raw access). Ceremonial residues (mottoes, diploma language, anniversary orations) are candidates for inertial theater at the fringe while the philological core stays live. Keeping beneficiary declarations (real services delivered) alongside victim declarations (real costs imposed) prevents both failure modes at once: it stops the arrangement from being mislabeled as pure coordination (which would hide the students and the graded medieval corpus), and stops it from being mislabeled as pure predation (which would erase the rescued corpus and the cross-century communication the standard actually delivers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (discontinuity_reading) of the correct_latin_kernel; the siblings continuity_reading and hybrid_reading instantiate different constraints with different epsilon values and beneficiary structures. Where exactly is the disagreement located?',
    'Philological adjudication of transmission data: documentary evidence of native competence loss, rate of morphological drift in insular and continental Latin, and whether medieval usage constitutes failed maintenance or successful adaptation. Each reading resolves the kernel differently on the same evidence.',
    'If the continuity_reading is structurally right, the corruption-grading loses its warrant, the expert-gatekeeping extraction collapses toward ordinary coordination cost, and the arrangement reclassifies toward rope. If the discontinuity_reading holds, the current victim declarations and the expert-mediated structure stand. The hybrid_reading splits the difference and redistributes extraction onto the syntax/lexicon recovery layers only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-contest structure of the correct-Latin kernel: which reading''s structural delta governs the arrangement.').

omega_variable(
    thesis_arrangement_decomposition,
    'Does epsilon here measure the empirical thesis (Classical and Medieval Latin are distinct systems — a descriptive claim carrying near-zero extraction) or the institutional arrangement built on the thesis (textual-expert gatekeeping, corruption-grading, curriculum enforcement)?',
    'Decomposition per the epsilon-invariance rule: author the bare thesis as its own story (likely certifying near the mountain end, with no parties) and this arrangement as a separate story, linked via affects_constraints. The present file authors epsilon for the standing arrangement under contest.',
    'If the two are conflated, the thesis''s empirical solidity lends unwarranted necessity to the arrangement (false-summit pressure); if separated, each gets a stable epsilon and the arrangement''s constructed, enforced character becomes measurable on its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thesis_arrangement_decomposition, conceptual, 'Separability of the descriptive discontinuity thesis from the enforcement arrangement erected on it.').

omega_variable(
    corruption_vs_adaptation,
    'Were medieval forms corruptions — failures to maintain a inherited standard — or a functional technical adaptation, a new register fitted to theology, logic, and administration?',
    'Functional analysis of scholastic Latin''s communicative adequacy for its own tasks, comparative efficiency against classical idioms performing equivalent work, and the twentieth-century rehabilitation literature on medieval Latin as an autonomous technical language.',
    'If adaptation, the victim declaration for medieval_scholastic_writers weakens substantially — they were not damaged relative to their own purposes — and extraction concentrates on the student seat, pulling the classification toward rope with a narrower victim set. If corruption, the retrospective grading stands as a real imposed cost and the tangled structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corruption_vs_adaptation, empirical, 'Status of medieval Latin usage: degeneration versus register formation.').

omega_variable(
    recovery_route_necessity,
    'Was symbolic reoccupation from texts the only available route after the transmission break, or did viable continuity-preserving alternatives exist that the arrangement suppressed?',
    'Comparative history of transmission centers: the insular houses that preserved older competence longer, regions maintaining stronger bilingual schooling, and counterfactual trajectories where sustained native-adjacent instruction survived. Where alternative routes demonstrably existed and were displaced, suppression carried choice-making weight.',
    'If alternatives existed and were crowded out, the arrangement''s suppression carries less justification, effective extraction rises, and the profile leans toward snare. If the textual route was genuinely the only viable one, a larger share of the measured suppression is necessary coordination cost and the rope component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_route_necessity, empirical, 'Whether the textual-only recovery mandate displaced feasible alternatives.').

omega_variable(
    ceremonial_residue_drift,
    'Are the surviving non-academic uses of classical Latin (mottoes, diploma language, anniversary orations, institutional insignia) inertial theater around an atrophied function — piton-drift pressure at the fringe — or do they still perform coordination work?',
    'Observation of what the ceremonial uses actually coordinate: if they sustain institutional identity and boundary-marking that members act on, function persists; if they are recited without comprehension or consequence, the residue is performance.',
    'If the residue is inertial, the arrangement''s long-run trajectory bends toward piton at the ceremonial fringe even while the scholarly core stays live, and the theater_ratio series understates future drift. If functional, the current flat-tail reading of the series stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ceremonial_residue_drift, empirical, 'Functional status of the ceremonial Latin residue surrounding the standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 780, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clk_disc_tr_t780, correct_latin_kernel__discontinuity_reading, theater_ratio, 780, 0.15).
narrative_ontology:measurement(clk_disc_tr_t900, correct_latin_kernel__discontinuity_reading, theater_ratio, 900, 0.18).
narrative_ontology:measurement(clk_disc_tr_t1200, correct_latin_kernel__discontinuity_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement(clk_disc_tr_t1450, correct_latin_kernel__discontinuity_reading, theater_ratio, 1450, 0.4).
narrative_ontology:measurement(clk_disc_tr_t1550, correct_latin_kernel__discontinuity_reading, theater_ratio, 1550, 0.45).
narrative_ontology:measurement(clk_disc_tr_t1850, correct_latin_kernel__discontinuity_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(clk_disc_tr_t1950, correct_latin_kernel__discontinuity_reading, theater_ratio, 1950, 0.31).
narrative_ontology:measurement(clk_disc_tr_t2000, correct_latin_kernel__discontinuity_reading, theater_ratio, 2000, 0.28).

% Extraction over time
narrative_ontology:measurement(clk_disc_be_t780, correct_latin_kernel__discontinuity_reading, base_extractiveness, 780, 0.3).
narrative_ontology:measurement(clk_disc_be_t900, correct_latin_kernel__discontinuity_reading, base_extractiveness, 900, 0.38).
narrative_ontology:measurement(clk_disc_be_t1200, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1200, 0.42).
narrative_ontology:measurement(clk_disc_be_t1450, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1450, 0.58).
narrative_ontology:measurement(clk_disc_be_t1550, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1550, 0.6).
narrative_ontology:measurement(clk_disc_be_t1850, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1850, 0.56).
narrative_ontology:measurement(clk_disc_be_t1950, correct_latin_kernel__discontinuity_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(clk_disc_be_t2000, correct_latin_kernel__discontinuity_reading, base_extractiveness, 2000, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(clk_disc_su_t780, correct_latin_kernel__discontinuity_reading, suppression_requirement, 780, 0.35).
narrative_ontology:measurement(clk_disc_su_t900, correct_latin_kernel__discontinuity_reading, suppression_requirement, 900, 0.45).
narrative_ontology:measurement(clk_disc_su_t1200, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement(clk_disc_su_t1450, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1450, 0.68).
narrative_ontology:measurement(clk_disc_su_t1550, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement(clk_disc_su_t1850, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1850, 0.75).
narrative_ontology:measurement(clk_disc_su_t1950, correct_latin_kernel__discontinuity_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(clk_disc_su_t2000, correct_latin_kernel__discontinuity_reading, suppression_requirement, 2000, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, information_standard).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' covers structurally distinct claims and decomposes into a three-member kernel family. This file is the discontinuity_reading: transmission broke, recovery is textual, medieval forms are graded as corruption — epsilon is authored for that arrangement (moderate extraction, expert-mediated). The continuity_reading (separate story) treats the same history as unbroken evolution with internal correction, which dissolves most of the victim set and drops epsilon; the hybrid_reading (separate story) accepts textual recovery for syntax and lexicon only, redistributing extraction onto those layers. Upstream/downstream structure: the discontinuity_reading's recovery mandate built the editorial and pedagogical economy that the other two readings must answer to, so this story influences both siblings without resolving the kernel contest. Linkage here is family bookkeeping, not averaging: each member keeps its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
