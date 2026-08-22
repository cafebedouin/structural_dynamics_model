% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined-Composite Account of Dueling's Decline
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   This story instantiates the overdetermined_composite_reading of the
 *   dueling_disappearance_mechanism kernel: the doctrine that dueling's
 *   decline was caused by multiple independent sufficient conditions — legal
 *   prohibition, institutional modernization, cultural shift, and Civil War
 *   trauma — acting simultaneously, such that no single-cause account can be
 *   complete. The constraint governed by this story is the doctrine AS IT
 *   OPERATES in the scholarly field: as the canonical account circulated
 *   through peer review, survey textbooks, and qualifying-exam canons from
 *   roughly 1960 to 2020 (interval units are years since 1960). The epsilon
 *   referent is the standing arrangement under contest — the composite
 *   doctrine's own operation in the field — assessed by this reading's own
 *   lights; it is NOT the rival arrangements the sibling readings endorse.
 *   The non-separability the doctrine asserts concerns the HISTORICAL causal
 *   pathways; the doctrine's own operation in the field is a single, stably
 *   measurable arrangement, which is what permits one clean epsilon here
 *   despite the expected-delta note that pathway epsilon is non-measurable.
 *   Claim and metrics are authored independently: the tangled_rope claim
 *   reflects the structure I believe true (a genuine coordination function
 *   joined to asymmetric costs borne by identifiable seats, held up by active
 *   enforcement), while the metric values describe the doctrine's actual
 *   operation without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - - journal_editors_and_textbook_boards: agenda-setting seat (institutional/mobile) — administers the review processes and survey-textbook lines through which the composite account circulates
 *   - - social_history_synthesizers: primary beneficiary (organized/identity_locked) — holds the integrative center the doctrine made professionally central
 *   - - civil_war_era_historians: secondary beneficiary (organized/constrained) — collects explanatory billing for the wartime-trauma strand
 *   - - legal_prohibition_historians: secondary beneficiary (moderate/mobile) — collects billing for the statute strand
 *   - - single_mechanism_theorists: primary target (moderate/constrained) — bears the demotion of unitary accounts to 'one strand among several'
 *   - - causal_decomposition_methodologists: secondary target (moderate/arbitrage) — blocked from estimating the strands separately, but ports methods to other domains
 *   - - graduate_students_in_historical_sociology: captive target (powerless/trapped) — must reproduce the composite before credentialing
 *   - - comparative_anthropologists_of_honor: excluded voice (organized/mobile) — methodological objection never enters the review loop
 *   - - philosophers_of_history: analytical observer — assesses what 'independently sufficient' can mean for singular historical events
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.54).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined-Composite Account of Dueling's Decline").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'ccca72fe-c43e-4513-b6f4-61a4bac7e703').
narrative_ontology:cs_kernel_codification('ccca72fe-c43e-4513-b6f4-61a4bac7e703', distributed).
narrative_ontology:cs_authority_grounding('ccca72fe-c43e-4513-b6f4-61a4bac7e703', expertise).
narrative_ontology:cs_interpretation_layer_present('ccca72fe-c43e-4513-b6f4-61a4bac7e703').
narrative_ontology:cs_reading_relation('ccca72fe-c43e-4513-b6f4-61a4bac7e703', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('ccca72fe-c43e-4513-b6f4-61a4bac7e703', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('ccca72fe-c43e-4513-b6f4-61a4bac7e703', foundational, multiple_independent_sufficient_conditions).
narrative_ontology:cs_axiom_status(multiple_independent_sufficient_conditions, holdable).
narrative_ontology:cs_axiom_grounding('ccca72fe-c43e-4513-b6f4-61a4bac7e703', multiple_independent_sufficient_conditions, empirically_contingent).
narrative_ontology:cs_axiom('ccca72fe-c43e-4513-b6f4-61a4bac7e703', foundational, causal_pathways_non_separable).
narrative_ontology:cs_axiom_status(causal_pathways_non_separable, holdable).
narrative_ontology:cs_axiom_grounding('ccca72fe-c43e-4513-b6f4-61a4bac7e703', causal_pathways_non_separable, empirically_contingent).
narrative_ontology:cs_reference_frame('ccca72fe-c43e-4513-b6f4-61a4bac7e703', multi_causal_sufficiency_framework).
narrative_ontology:cs_drift_state('ccca72fe-c43e-4513-b6f4-61a4bac7e703', contemporary_causal_inference_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ccca72fe-c43e-4513-b6f4-61a4bac7e703', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, social_history_synthesizers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_war_era_historians).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_historians).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, single_mechanism_theorists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, causal_decomposition_methodologists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, graduate_students_in_historical_sociology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, journal_editors_and_textbook_boards).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, multi_causal_explanation_doctrine).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, singular_event_overdetermination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the peer-review processes and survey-course textbook lines through which the composite account circulates. They accept manuscripts that situate a finding within the multi-factor frame and return single-cause manuscripts with requests to 'situate the contribution among the known factors.' Their survey textbooks license the four-item list to classrooms, and revised editions refresh it on schedule. If the composite account lost canonical standing, their product lines would need restructuring around whichever account won.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, journal_editors_and_textbook_boards, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, journal_editors_and_textbook_boards, beneficiary).

% Write the integrative monographs and review essays that hold the four strands together — the genre the composite account made professionally central. Their standing rests on being the scholars who can speak across legal, military, and cultural subfields at once; a return to rival single-cause camps would strand that cross-subfield capital. Leaving the frame would mean rebuilding a career around one narrow mechanism, which their professional self-concept as big-picture scholars does not accommodate.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, social_history_synthesizers, beneficiary,
    organized, generational, identity_locked, continental).

% Study the American 1860s. The composite account bills wartime trauma as an independently sufficient cause of dueling's decline, which raises their subfield's explanatory billing in a story otherwise centered on European modernization. They did not build the composite account and mostly cite it in passing; their stake is the billing their archives receive, not the architecture holding it up.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_war_era_historians, beneficiary,
    organized, biographical, constrained, national).

% Document anti-dueling statutes and their uneven enforcement. The composite account credits legislation as independently sufficient, which flatters a record of laws contemporaries often ignored; under an account where custom simply outlived statute, their archival material would carry less causal weight. Their documentary skills port readily to other legal-history topics.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_historians, beneficiary,
    moderate, biographical, mobile, continental).

% Advance unitary explanations — that dignity-culture displacement alone remade the honor economy, or that courts and credit markets alone outcompeted the duel. Under the composite regime their manuscripts come back asking them to demote their mechanism to 'one strand among several,' and the burden falls on them to prove their cause sufficed alone — a proof no single-archive study can deliver. Continuing means accepting the demotion; full exit means abandoning the topic they specialize in.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, single_mechanism_theorists, payer,
    moderate, biographical, constrained, continental).

% Bring quantitative counterfactual and decomposition methods to historical causation and want the four strands estimated separately. The composite account answers that the pathways are non-separable — an assertion, not a demonstration — which closes off the application before it starts. Their methods port readily to other historical questions, so the block costs them a case study rather than a career.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, causal_decomposition_methodologists, payer,
    moderate, biographical, arbitrage, global).

% Must reproduce the four-factor composite on qualifying exams and in dissertation frames regardless of what their own archival findings suggest; challenging it before entering the job market carries visible risk. Their position clears only with the credential, after which dissent becomes cheaper — until then they bear the frame's training costs without any say in it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, graduate_students_in_historical_sociology, payer,
    powerless, immediate, trapped, continental).

% Compare honor cultures across societies and would object that the composite list mixes kinds of things — a statute, a bureaucracy, a value system, a war — as if they traded in a common causal currency, and that counting 'more factors' is being treated as 'more complete.' Their methodological objection rarely enters the review loop because the composite frame defines methodological seriousness as breadth, and they publish in venues the combatants rarely read.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, comparative_anthropologists_of_honor, excluded,
    organized, generational, mobile, global).

% Assess what 'independently sufficient' and 'overdetermined' can mean for singular historical events, and note that the composite account asserts rather than demonstrates both. They hold no stake in which account wins and publish where the combatants rarely look.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, philosophers_of_history, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, social_history_synthesizers).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After a century of dueling-decline studies produced mutually exclusive single-cause narratives, the field needed a framework letting legal historians, Civil War historians, cultural historians, and modernization theorists publish into a shared account without adjudicating between them. The composite doctrine coordinates by allocating each subfield a sufficient-condition slot in one story.
% TRANSFER_FUNCTION: Moves interpretive authority and citational priority from advocates of unitary mechanisms to the integrative center; moves graduate training toward breadth over causal adjudication; and moves the burden of proof onto anyone claiming a single cause, who must now show their mechanism sufficed alone — a bar the composite itself never has to meet for any individual strand.
% ABSENT_VOICES: Comparative anthropologists of honor would object that the four strands mix levels of analysis without a common causal currency, and that factor-counting is substituting for completeness; philosophers of history would press that 'independent sufficiency' and 'non-separability' are asserted, not shown; and nobody in the room speaks for parsimony as an epistemic value, or asks the counterfactual question — would dueling have died without strand X? — which the frame renders unaskable.
% DISAPPEARANCE_RATIONALE: The governed world here is the interpretive field, and it would rearrange: rival single-cause accounts would compete openly again instead of presenting as strands, survey textbooks would have to choose or explicitly present contested accounts, the burden-of-proof asymmetry would dissolve, and decomposition studies of the four mechanisms would proceed. The historical fact of dueling's decline is of course untouched — the doctrine explains it, it does not sustain it.
% FOUNDING_PROBLEM: Mid-twentieth-century dueling scholarship had fractured into mutually exclusive monocausal narratives — law-only, modernization-only, culture-only — each regionally parochial (the American story centered on the Civil War, the European on bureaucratic state formation), and findings could not accumulate across the camps.
% FOUNDING_PROBLEM_CORROBORATION: Review essays in the history of the human sciences attest the original stalemate was real. That the reconciliation remains incomplete is attested from outside the beneficiary set by the continued appearance of dedicated single-mechanism monographs and by methodological critiques of overdetermination claims in philosophy-of-history venues; no disinterested party attests the founding problem is fully solved, and the voices calling it settled are the doctrine's own beneficiaries.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.54: the doctrine performs real synthetic work (it ended a genuinely sterile monocausal stalemate and integrates evidence legal, military, and cultural historians each possess), but it also runs a burden-of-proof asymmetry — any scholar claiming THE cause must prove their mechanism sufficed alone, a bar no single-archive study can clear, while the composite needs only per-strand plausibility — and it channels graduate training toward breadth over adjudication. Suppression is 0.42 and is authored as a raw structural property, unscaled by power or scope: enforcement runs through review norms, exam canons, and the 'situate your contribution among the known factors' referee letter, not through exclusion from the profession. Theater_ratio is 0.38 and rising: the four-item litany (law, modernization, culture, war) increasingly functions as a recited formula — a shibboleth of methodological seriousness — rather than a performed analysis, a Goodhart-style drift from the integration work the doctrine originally did. Accessibility_collapse is low (0.3) because single-cause alternatives remain fully publishable; nothing about the composite forecloses them, it merely demotes them. Resistance is moderate (0.5): revisionist single-mechanism monographs and the quantitative causal-inference critique keep arriving. The measurement series run on one shared seven-point grid (every tracked metric authored at every time point); trajectories are monotonic, not cyclical — the doctrine hardened rather than oscillated — so no intermittent-reinforcement analysis applies. Base_properties values equal the interval-end measurements by construction of the grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute materially different types from the same structural data. From the synthesizer seat the doctrine is generous pluralism — the framework that let four subfields stop fighting; from the single-mechanism theorist's seat it is a closure machine that converts their central claim into a footnote; from the graduate student's seat it is exam furniture to be reproduced without conviction; from the methodologist's seat it is an untestable assertion (non-separability) blocking a research program; from the editor's seat it is curricular stability. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the synthesizer, Civil War, and legal-history seats toward the subsidized end; victim declarations drive the three payer seats toward the target end. Exit options modulate within that: graduate students are trapped (credential-gated), placing them nearest the full-target position despite their powerlessness; the methodologists' arbitrage-grade exit (portable methods) damps their effective burden well below what their program-block alone would suggest; single-mechanism theorists are constrained — they can reframe as 'one strand' (submission) or abandon the topic, but not freely continue. On the beneficiary side, the synthesizers' identity lock cuts the other way: their professional self-concept ('the scholar who sees the whole picture') is constituted by the integrative genre, so they cannot cash out their position even if the doctrine's returns decay — deepening commitment without changing direction. The editors sit near the administrative middle: they move the doctrine's traffic but capture little of what the payer seats lose.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is precise: the doctrine was built to end a mid-century stalemate of mutually exclusive monocausal narratives, and that founding problem is at best contested-live — the stalemate recurs whenever a new single-mechanism account appears, but the reconciliation may have been achieved decades ago, leaving the doctrine over-serving as a jurisdictional truce. The founding_problem_status x disappearance_verdict pair (contested x world_rearranges) is exactly the configuration the mismatch consumer flags for capture/zombie inspection, cross-checked against the rising theater_ratio series. Classifying as tangled_rope rather than snare preserves the doctrine's genuine coordination credit — it did integrate real evidence and end sterile warfare, and its victims are demoted rather than destroyed; classifying as anything purer than tangled_rope would erase the asymmetric costs (burden-of-proof asymmetry, strand-demotion, blocked decomposition) that identifiable seats demonstrably bear. If the independence omega resolves against the doctrine, expect drift toward a single-mechanism constraint with the composite surviving as theatrical shell — a piton-shaped endpoint the theater series is already trending toward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_dominance_uncertainty,
    'Which of the four conditions did most of the causal work, and does the composite''s equal billing misstate their relative weights?',
    'Comparative regional analysis where conditions varied independently — jurisdictions with prohibition statutes but no war trauma, honor cultures whose institutions modernized without statutory bans — to rank the strands'' marginal contributions.',
    'If one strand dominated, the composite over-credits the minor ones, the victim set shifts toward the dominant mechanism''s rivals, and the doctrine drifts toward a single-mechanism account wearing pluralist dress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_dominance_uncertainty, empirical, 'Relative causal weight of the four strands is unknown; the severity and composition of the victim set depends on it.').

omega_variable(
    independence_vs_interaction,
    'Were the four conditions causally independent, or did they interact — statutes enforced only because honor culture was already eroding, courts trusted only because the duel had already lost prestige?',
    'Process-tracing of enforcement and adoption records timed against independent measures of honor-culture vitality; if each condition''s effect proves conditional on the others, independence fails.',
    'Demonstrated interaction would void the foundational axiom of independent sufficiency, collapsing the composite toward whichever interaction sequence the evidence supports and converting this story''s structure into a single-mechanism constraint with the composite as residual shell.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_vs_interaction, empirical, 'Whether the strands were genuinely independent sufficient conditions or sequentially entangled.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the dueling_disappearance_mechanism kernel best framed as a substantive causal question about a historical event, or as a methodological settlement about how the discipline handles multi-causal episodes generally?',
    'Test whether the four-slot composite template generalizes: if the same structure is routinely applied to unrelated declines (vigilantism, vendetta, blood feud) as a disciplinary habit, the kernel is methodological; if it stays dueling-specific, it is substantive.',
    'Under the methodological framing this reading is a jurisdictional truce whose costs fall on methodological dissenters; under the substantive framing it is a causal hypothesis whose costs fall on rival historians — different victim sets and different epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Two coherent framings of the kernel yield different structural classifications for this reading.').

omega_variable(
    partisan_epsilon_gap,
    'Does the epsilon authored here from the composite reading''s own seat understate the burden the doctrine imposes as experienced from the single-mechanism and methodologist seats?',
    'Cross-reading authorship: have the contraction and institutional-displacement readings author epsilon for this doctrine''s operation and compare against the exit-modulation-corrected value authored here.',
    'If the partisans'' experience governs, effective extraction at the payer seats rises toward the range where the doctrine computes closer to pure extraction there; the per-seat divergence the engine computes would widen accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_epsilon_gap, conceptual, 'Reading-indexed epsilon for the composite doctrine authored from the composite seat may sit below the value partisans would author.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(duel_tr_t0, observed).
narrative_ontology:measurement(duel_tr_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(duel_tr_t10, observed).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(duel_tr_t20, observed).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(duel_tr_t30, observed).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(duel_tr_t40, observed).
narrative_ontology:measurement(duel_tr_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 50, 0.34).
narrative_ontology:measurement_basis(duel_tr_t50, observed).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(duel_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(duel_be_t0, observed).
narrative_ontology:measurement(duel_be_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(duel_be_t10, observed).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(duel_be_t20, observed).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(duel_be_t30, observed).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(duel_be_t40, observed).
narrative_ontology:measurement(duel_be_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(duel_be_t50, observed).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(duel_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(duel_su_t0, observed).
narrative_ontology:measurement(duel_su_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement_basis(duel_su_t10, observed).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(duel_su_t20, observed).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 30, 0.39).
narrative_ontology:measurement_basis(duel_su_t30, observed).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement_basis(duel_su_t40, observed).
narrative_ontology:measurement(duel_su_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 50, 0.42).
narrative_ontology:measurement_basis(duel_su_t50, observed).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(duel_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'why did dueling die?' decomposes per the epsilon-invariance principle into three structurally distinct constraint stories: the contraction reading (cultural-axiom displacement), the institutional-displacement reading (institutional substitution), and this overdetermined-composite reading (joint sufficiency with non-separable pathways). Each carries its own epsilon, beneficiary/victim structure, and claimed type; forcing them into one story would require an observable-dependent epsilon, which is the signature of a mislabeled family. This reading sits downstream of both siblings — it cites their mechanisms as sufficient strands — so erosion of either sibling's evidentiary base pressures this story's foundational axioms directly, and contamination propagation should route sibling purity loss into this story's non-separability claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
