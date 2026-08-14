% ============================================================================
% CONSTRAINT STORY: technician_intent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technician_intent_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: technician_intent_reading
 *   human_readable: Mental-State Anchor Requirement for Algorithmic Harm Attribution
 *   domain: law_and_technology/products_liability
 *
 * SUMMARY:
 *   This constraint is the technician-intent reading of a contested kernel
 *   about how algorithmic harm gets attributed to a firm. It treats
 *   attribution as requiring — or being strongest when grounded in — a
 *   demonstrable mental state: a person who held a theory of the harm
 *   mechanism and documented it, following the evidentiary template
 *   established in tobacco and Purdue litigation where internal memos did the
 *   attributional work. Applied to algorithmic systems whose harms are
 *   frequently emergent (arising from training dynamics, feedback loops, and
 *   optimization targets rather than any individually authored theory), this
 *   reading forecloses liability for exactly the harm class the underlying
 *   essay is centrally about: the 'fishing-lure' structure, where the
 *   system's harmful behavior was never scripted by anyone but emerged from
 *   the interaction of design choices and scale. As courts increasingly apply
 *   this standard to algorithmic cases, firms have adapted document-retention
 *   and internal-communication practices specifically to avoid creating the
 *   kind of memo this reading requires, converting an evidentiary standard
 *   into a design constraint on corporate recordkeeping and a durable shield
 *   against emergent-harm claims.
 *
 * KEY AGENTS:
 *   - platform_operators: primary beneficiary (institutional/arbitrage) — shielded by the evidentiary gate
 *   - emergent_harm_plaintiffs: primary target (powerless/trapped) — harm is real but unattributable under this reading
 *   - corporate_defense_counsel: agenda-setter (powerful/arbitrage) — actively shapes discovery and recordkeeping to preserve the gate
 *   - content_moderation_researchers: excluded analytical voice — produces emergent-causation evidence the reading structurally discounts
 *   - trial_courts_applying_reading: institutional agenda-setter — extends a tobacco/Purdue-shaped template to structurally dissimilar cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technician_intent_reading, 0.68).
domain_priors:suppression_score(technician_intent_reading, 0.71).
domain_priors:theater_ratio(technician_intent_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technician_intent_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technician_intent_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(technician_intent_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technician_intent_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(technician_intent_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technician_intent_reading, tangled_rope).
narrative_ontology:human_readable(technician_intent_reading, "Mental-State Anchor Requirement for Algorithmic Harm Attribution").
narrative_ontology:topic_domain(technician_intent_reading, "law_and_technology/products_liability").

domain_priors:requires_active_enforcement(technician_intent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technician_intent_reading, '20562d66-7544-4acf-8554-eaa3d0df0ac7').
narrative_ontology:cs_kernel_codification('20562d66-7544-4acf-8554-eaa3d0df0ac7', distributed).
narrative_ontology:cs_authority_grounding('20562d66-7544-4acf-8554-eaa3d0df0ac7', practice).
narrative_ontology:cs_interpretation_layer_present('20562d66-7544-4acf-8554-eaa3d0df0ac7').
narrative_ontology:cs_reading_relation('20562d66-7544-4acf-8554-eaa3d0df0ac7', technician_intent_reading__products_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('20562d66-7544-4acf-8554-eaa3d0df0ac7', technician_intent_reading__expressive_attribution_reading, coexists_with).
narrative_ontology:cs_reading_relation('20562d66-7544-4acf-8554-eaa3d0df0ac7', technician_intent_reading__conduct_regulation_reading, influences).
narrative_ontology:cs_axiom('20562d66-7544-4acf-8554-eaa3d0df0ac7', foundational, attribution_requires_documented_mental_state).
narrative_ontology:cs_axiom_status(attribution_requires_documented_mental_state, holdable).
narrative_ontology:cs_axiom_grounding('20562d66-7544-4acf-8554-eaa3d0df0ac7', attribution_requires_documented_mental_state, conventional).
narrative_ontology:cs_axiom('20562d66-7544-4acf-8554-eaa3d0df0ac7', secondary, undocumented_harm_mechanism_defeats_culpability_finding).
narrative_ontology:cs_axiom_status(undocumented_harm_mechanism_defeats_culpability_finding, holdable).
narrative_ontology:cs_axiom_grounding('20562d66-7544-4acf-8554-eaa3d0df0ac7', undocumented_harm_mechanism_defeats_culpability_finding, instrumental).
narrative_ontology:cs_reference_frame('20562d66-7544-4acf-8554-eaa3d0df0ac7', tobacco_purdue_documentary_evidence_template).
narrative_ontology:cs_drift_state('20562d66-7544-4acf-8554-eaa3d0df0ac7', contemporary_algorithmic_harm_litigation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('20562d66-7544-4acf-8554-eaa3d0df0ac7', '').
narrative_ontology:cs_kernel_id(technician_intent_reading, algorithmic_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technician_intent_reading, platform_operators).
narrative_ontology:constraint_beneficiary(technician_intent_reading, algorithm_design_engineers).
narrative_ontology:constraint_beneficiary(technician_intent_reading, corporate_defense_counsel).
narrative_ontology:constraint_victim(technician_intent_reading, emergent_harm_plaintiffs).
narrative_ontology:constraint_victim(technician_intent_reading, algorithmically_injured_minors).
narrative_ontology:constraint_victim(technician_intent_reading, content_moderation_researchers).
narrative_ontology:constraint_vindicates(technician_intent_reading, mens_rea_centrality_doctrine).
narrative_ontology:constraint_vindicates(technician_intent_reading, documentary_evidence_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy recommendation and ranking systems whose harmful effects emerge from training dynamics, feedback loops, and aggregate user behavior rather than from any single engineer's documented intent. Because this reading requires a demonstrable mental state — a person who held a theory of the harm mechanism and wrote it down — the absence of a 'memo' insulates the operator from liability even where the harm is real, foreseeable in the aggregate, and traceable to design choices. The operator's litigation strategy actively avoids creating the kind of internal documentation that would satisfy this reading's evidentiary anchor.
narrative_ontology:constraint_stakeholder(technician_intent_reading, platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Build the systems whose emergent behavior produces harm but, under this reading, are shielded unless discovery uncovers a document showing they understood and intended (or recklessly disregarded a documented theory of) the specific harm mechanism. They can testify credibly that they did not anticipate the emergent path, because in most cases they genuinely did not model it explicitly — the harm arose from optimization dynamics, not authored intent.
narrative_ontology:constraint_stakeholder(technician_intent_reading, algorithm_design_engineers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(technician_intent_reading, algorithm_design_engineers, agenda_setter).

% Litigate by demanding plaintiffs produce the 'memo' — the tobacco/Purdue-style smoking-gun document — as the price of admission for attribution. They shape discovery strategy, document retention policy, and internal communication norms specifically to minimize the creation of documents that would satisfy this reading's threshold, converting the evidentiary standard into a design constraint on corporate recordkeeping itself.
narrative_ontology:constraint_stakeholder(technician_intent_reading, corporate_defense_counsel, agenda_setter,
    powerful, biographical, arbitrage, national).

% Suffer concrete, sometimes severe harm traceable to an algorithmic system's design and deployment, but cannot obtain relief under this reading because no individual document evidencing a theory-of-harm-mechanism can be located — the harm emerged from training data, feedback loops, and scale effects that no single person authored or foresaw in writing. Their harm is real; their legal claim dies at the evidentiary gate this reading erects.
narrative_ontology:constraint_stakeholder(technician_intent_reading, emergent_harm_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Represent the paradigm emergent-path victim class — harmed by recommendation systems optimizing for engagement in ways no single engineer scripted or documented as intent to harm minors specifically. Their guardians pursue litigation modeled on tobacco-style discovery, searching for an internal memo that, structurally, is unlikely to exist because the harm mechanism was never a discrete authored theory but an emergent property of the optimization target.
narrative_ontology:constraint_stakeholder(technician_intent_reading, algorithmically_injured_minors, payer,
    powerless, generational, trapped, national).

% Produce technical analysis demonstrating that algorithmic harms are frequently emergent and mechanism-diffuse — arising from architecture and incentive structure rather than any documented individual intent. Their expert testimony on emergent causation is structurally devalued by this reading's insistence on a documentary mental-state anchor; courts applying this reading treat their systemic analysis as insufficient without the memo, regardless of its technical rigor.
narrative_ontology:constraint_stakeholder(technician_intent_reading, content_moderation_researchers, excluded,
    moderate, biographical, constrained, national).

% Apply this reading as the operative attribution standard, ruling that absent a document showing a person held and recorded a theory of the harm mechanism, attribution to the firm cannot be sustained (or survives only on a weaker theory). Courts administering this standard are themselves bound by precedent modeled on tobacco/Purdue litigation, where a documentary trail existed and did the attributional work; they extend that model to structurally dissimilar emergent-harm cases.
narrative_ontology:constraint_stakeholder(technician_intent_reading, trial_courts_applying_reading, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technician_intent_reading, platform_operators).
narrative_ontology:fixing_cost_class(technician_intent_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a tractable, administrable standard for courts to distinguish deliberate corporate wrongdoing (where a paper trail shows a person understood and chose to proceed despite known harm) from good-faith engineering error or genuinely unforeseeable emergent behavior — preventing liability from attaching to every unfortunate algorithmic outcome regardless of culpability.
% TRANSFER_FUNCTION: Moves the cost of undocumented, emergent, mechanism-diffuse algorithmic harm from the firms that designed and deployed the systems onto the injured parties, by making the existence of a specific internal document the threshold condition for any transfer of liability back to the firm.
% ABSENT_VOICES: Content moderation researchers and computational social scientists who can demonstrate emergent causation without a documentary anchor are excluded from meaningfully shaping the attribution standard; plaintiffs' technical experts are heard but structurally discounted because this reading was built for a different evidentiary shape (single-actor, documented intent) than the one their evidence describes (distributed, emergent, undocumented).
% DISAPPEARANCE_RATIONALE: If this reading disappeared and attribution shifted to a conduct-regulation or products-liability standard not anchored in documentary mental state, platform operators would face liability exposure for a much larger class of emergent harms; document retention and internal communication practices would no longer function as a liability shield; plaintiffs currently barred at the evidentiary gate would gain standing to proceed to causation and damages analysis.
% FOUNDING_PROBLEM: Courts needed a way to distinguish culpable corporate concealment of known harm (tobacco industry's documented internal knowledge of addiction and cancer risk, Purdue's documented internal knowledge of opioid abuse potential) from ordinary commercial risk-taking, so that liability would track actual wrongdoing rather than mere causation of harm.
% FOUNDING_PROBLEM_CORROBORATION: Defense-side litigators and courts applying the standard attest the mental-state anchor remains necessary to prevent strict-liability-by-hindsight for good-faith engineering choices. Plaintiffs' counsel, computational social scientists, and legal scholars studying algorithmic harm (writing outside the benefiting parties) attest the founding problem — distinguishing culpable concealment from ordinary risk — has been overtaken by a structurally different harm class (emergent, undocumented, diffuse-mechanism) for which the tobacco/Purdue evidentiary template was never designed and now functions primarily as a liability shield rather than a culpability filter.
narrative_ontology:disappearance_verdict(technician_intent_reading, world_rearranges).
narrative_ontology:founding_problem_status(technician_intent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technician_intent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(technician_intent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technician_intent_reading, 0.68, 'claude-sonnet-5', 'algorithmic_authorless_harm_2026_20260813_215102', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technician_intent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technician_intent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technician_intent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 by interval end because the reading transfers the cost of demonstrable, scaled algorithmic harm onto injured parties whenever no documentary mental-state anchor exists — which, for emergent-path harms, is close to always, since the harm was never authored as an explicit theory by any individual. Suppression (0.71) reflects that this is not merely an evidentiary preference but an actively defended threshold: courts cite it to dismiss claims at the pleading or summary-judgment stage, and defense counsel actively engineers recordkeeping to keep the threshold unmet. Theater ratio (0.42) captures that a meaningful share of litigation activity around this standard is performative — discovery fights over document production that both sides know will not surface the required memo because, for emergent systems, no such single-authored memo was ever created.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator and defense counsel seats, this reading is a legitimate, principled line between culpable concealment and good-faith engineering uncertainty — the coordination function is real and prevents liability from attaching to every unfortunate outcome. From the emergent-harm plaintiff seat, the identical structure operates as a near-absolute shield: the standard was built for a documentary-evidence world (tobacco, Purdue) and transplanted wholesale onto a structurally different harm-generation process (emergent, distributed, undocumented), where it functions less as a culpability filter than as a liability sink.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators and algorithm engineers sit near the full-beneficiary end: the reading's evidentiary threshold is rarely met for emergent harms, so liability rarely attaches regardless of actual harm caused, and their exit options (arbitrage, mobile) let them route around jurisdictions or restructure documentation practices to preserve the shield. Emergent harm plaintiffs and algorithmically injured minors sit near the full-target end: they bear concrete harm, have no exit (trapped), and the reading's structure guarantees their claims fail at the evidentiary gate absent a document that, given the harm's emergent character, is structurally unlikely to exist. Corporate defense counsel functions as an active agenda-setter rather than a passive beneficiary — they do not merely benefit from the standard, they administer and reinforce it through discovery strategy and advise on document-retention policy that keeps the gate closed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing documented corporate concealment from good-faith risk-taking — was live and well-solved in the tobacco and Purdue contexts, where a documentary trail existed and the standard correctly separated culpable firms from non-culpable ones. Applied to emergent algorithmic harm, the founding problem has effectively died in its original form (there is rarely a discrete document to find) while the standard persists, now functioning primarily as a shield rather than a filter. This is exactly the mandatrophy pattern the founding_problem_status/disappearance_verdict mismatch is designed to surface: the reading's own beneficiaries assert the problem remains live (fear of hindsight liability), while parties and researchers outside that group attest the mechanism has shifted and the standard now forecloses valid claims rather than screening invalid ones.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentary_anchor_transplant_validity,
    'Is the tobacco/Purdue documentary-mental-state template structurally applicable to algorithmic harms at all, or does its transplant onto emergent, distributed harm-generation processes represent a category error that this reading imports uncritically?',
    'Comparative case analysis: track outcomes across cases with and without documentary anchors where harm and causal traceability are otherwise comparable, controlling for harm severity and system architecture (emergent vs. deliberately designed).',
    'If the transplant is a category error, this reading should be understood as producing false negatives systematically for an entire harm class, not as a neutral culpability filter; if the transplant is valid, the absence of documents in algorithmic cases may itself be meaningful evidence about culpability rather than a limitation of the standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentary_anchor_transplant_validity, conceptual, 'Whether the documentary-anchor evidentiary template is structurally suited to emergent algorithmic harm or a mismatched transplant from single-actor concealment cases.').

omega_variable(
    document_absence_endogeneity,
    'To what extent is the absence of documented theories-of-harm itself endogenous to firms'' knowledge that this reading is the operative liability standard — i.e., are firms avoiding documentation specifically because they know it would satisfy this reading''s threshold?',
    'Discovery of internal document-retention policy changes correlated with litigation risk assessments; comparison of documentation practices pre- and post- major algorithmic harm litigation.',
    'If document absence is strategically engineered rather than a genuine feature of emergent harm generation, the reading''s reliance on documentary anchors rewards concealment-by-design, which would undercut its claimed coordination function (separating culpable concealment from good-faith uncertainty) and support a reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(document_absence_endogeneity, empirical, 'Whether firms'' document-retention practices are strategically shaped to defeat this reading''s evidentiary threshold.').

omega_variable(
    sibling_reading_victim_set_divergence,
    'Given that the products_liability_reading and conduct_regulation_reading would recognize victims from the same underlying facts that this reading treats as having no attributable culpable mind, which reading should govern courts'' actual attribution practice for emergent algorithmic harm?',
    'This is fundamentally a normative/doctrinal choice about the purpose of tort attribution (deterrence vs. corrective justice vs. administrability) rather than an empirical question; legislative or appellate clarification would resolve it for a given jurisdiction, but the underlying value question persists across readings.',
    'Courts'' choice among these readings for emergent-harm cases determines whether an entire class of concretely injured plaintiffs has any path to recovery — the choice is not merely interpretive but dispositive of victim-set membership itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_victim_set_divergence, preference, 'Which kernel reading should govern attribution practice is a values choice about the purpose of liability, not a fact to be discovered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technician_intent_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technician_intent_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t6, technician_intent_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(tech_tr_t6, observed).
narrative_ontology:measurement(tech_tr_t12, technician_intent_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(tech_tr_t12, observed).
narrative_ontology:measurement(tech_tr_t18, technician_intent_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement_basis(tech_tr_t18, observed).
narrative_ontology:measurement(tech_tr_t24, technician_intent_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(tech_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technician_intent_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t6, technician_intent_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(tech_be_t6, observed).
narrative_ontology:measurement(tech_be_t12, technician_intent_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(tech_be_t12, observed).
narrative_ontology:measurement(tech_be_t18, technician_intent_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(tech_be_t18, observed).
narrative_ontology:measurement(tech_be_t24, technician_intent_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(tech_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technician_intent_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t6, technician_intent_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(tech_su_t6, observed).
narrative_ontology:measurement(tech_su_t12, technician_intent_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(tech_su_t12, observed).
narrative_ontology:measurement(tech_su_t18, technician_intent_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement_basis(tech_su_t18, observed).
narrative_ontology:measurement(tech_su_t24, technician_intent_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(tech_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technician_intent_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technician_intent_reading, 0.1).
narrative_ontology:affects_constraint(technician_intent_reading, products_liability_reading).
narrative_ontology:affects_constraint(technician_intent_reading, expressive_attribution_reading).
narrative_ontology:affects_constraint(technician_intent_reading, conduct_regulation_reading).

% DUAL FORMULATION NOTE:
% This story is one of four linked readings of the algorithmic_attribution kernel. technician_intent_reading (this story) anchors attribution in documented individual mental state, following tobacco/Purdue precedent, and forecloses liability for emergent-path harms with no discoverable memo. products_liability_reading anchors attribution in defect-and-causation analysis independent of documented intent — it recognizes a victim set that this reading does not. expressive_attribution_reading treats algorithmic output as attributable expressive conduct under a First Amendment framework. conduct_regulation_reading anchors attribution in the firm's deployment and maintenance conduct regardless of documented foresight. Each reading has its own epsilon, its own beneficiary/victim structure, and its own type; they are linked here rather than merged because they produce structurally different victim sets from the same underlying facts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
