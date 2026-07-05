% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Catastrophe-Memory Ritual as Dual-Register Encoding (Symbol + Competence)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the hybrid_encoding_reading of the
 *   catastrophe_memory_survival kernel: ritual complexes built around
 *   historical catastrophe (flood cycles, famine, epidemic) operate on two
 *   registers at once — symbolic boundary-maintenance (who belongs, what
 *   marks us as a continuous people) and embedded practical knowledge (timing
 *   of seasonal risk, resource thresholds, kin-based response protocols).
 *   This reading holds that survival of the community depends on BOTH
 *   registers functioning together, and that the practice is not usefully
 *   decomposed into 'really it's about identity' or 'really it's about
 *   encoded competence.' The low extractiveness reflects that separating the
 *   registers analytically produces no material transfer within the
 *   practicing community — the only cost identified is analytical, borne by
 *   researchers and policy-translators who must force a single frame and
 *   thereby produce distorted downstream accounts. This is a genuinely low-ε
 *   reading: the practice coordinates two functions well, at low overhead,
 *   for the community itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.12).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.18).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Catastrophe-Memory Ritual as Dual-Register Encoding (Symbol + Competence)").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'feb3d5b5-e0c0-40f5-9333-75f29d0f9664').
narrative_ontology:cs_kernel_codification('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', distributed).
narrative_ontology:cs_authority_grounding('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', practice).
narrative_ontology:cs_interpretation_layer_present('feb3d5b5-e0c0-40f5-9333-75f29d0f9664').
narrative_ontology:cs_reading_relation('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', foundational, registers_are_jointly_necessary_not_separable).
narrative_ontology:cs_axiom_status(registers_are_jointly_necessary_not_separable, holdable).
narrative_ontology:cs_axiom_grounding('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', registers_are_jointly_necessary_not_separable, empirically_contingent).
narrative_ontology:cs_axiom('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', secondary, theoretical_resolution_is_not_required_for_practice_survival).
narrative_ontology:cs_axiom_status(theoretical_resolution_is_not_required_for_practice_survival, holdable).
narrative_ontology:cs_axiom_grounding('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', theoretical_resolution_is_not_required_for_practice_survival, conventional).
narrative_ontology:cs_reference_frame('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', unresolved_dual_function_practice).
narrative_ontology:cs_drift_state('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', contemporary_ethnographic_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('feb3d5b5-e0c0-40f5-9333-75f29d0f9664', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_holders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__hybrid_encoding_reading, dual_register_ritual_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform and transmit the ritual as a single lived practice — the symbolic boundary-marking (who we are, what separates us from catastrophe) and the embedded practical content (when to move, how to ration, which kin obligations activate) are not experienced as separable acts. They maintain both registers simultaneously without needing a theory of which one 'really' does the work. Their exit from the ritual would mean losing both the identity marker and the practical schema at once, which is why the practice persists as a whole rather than fragmenting into a ceremony plus a manual.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, practicing_communities, agenda_setter).

% Elders, ritual specialists, and family transmitters who carry the practice forward. They benefit from the ritual's dual encoding because it lets them transmit high-stakes survival information (timing of floods, storms, famines) inside a form that also carries social authority and belonging — stripping either register would require them to justify the practical content on its own evidentiary terms or the symbolic content on its own doctrinal terms, both harder to transmit alone.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, intergenerational_knowledge_holders, beneficiary,
    moderate, generational, constrained, local).

% Ethnographers, folklorists, and disaster-resilience researchers who must publish findings that fit a single theoretical frame (ritual as symbol OR ritual as encoded competence). Forcing the hybrid practice into one register for publication, funding, or policy translation produces analyses that misdescribe the practice, strip out the register their frame doesn't recognize, and sometimes recommend interventions (e.g. 'extract the practical knowledge into a manual, disregard the ceremony') that damage the practice's actual transmission mechanism. Their cost is intellectual and downstream-practical, not physical; they are mobile and can walk away from the case, but the mischaracterization they produce persists in literature and policy.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classifying_analysts, payer,
    analytical, biographical, mobile, global).

% NGOs and government agencies that would want a clean, extractable protocol (evacuation timing, resource thresholds) but are not part of the community's internal transmission process. They receive only the analysts' binary-classified output, not the hybrid practice itself, and so their interventions are calibrated to whichever register the analyst happened to foreground.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, disaster_response_institutions, excluded,
    institutional, immediate, analytical, national).

% Scholars studying ritual survival across catastrophe-prone societies who take the hybrid-encoding structure itself as the object of study, rather than trying to sort ritual elements into symbol-bin or competence-bin. They document cases where forcing a single register broke transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__hybrid_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__hybrid_encoding_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves a genuine dual coordination problem simultaneously: it maintains group identity/boundary continuity across generations AND transmits practical catastrophe-survival knowledge (timing, resource allocation, kin protocols) in a form durable enough to survive disruption, illiteracy, and generational gaps.
% TRANSFER_FUNCTION: The practice does not primarily transfer resources between parties within the community — it transfers encoded information (both symbolic and practical) forward in time, from one generation of practitioners to the next. The only extractive transfer identified is analytical: mischaracterized, register-flattened accounts move from community practice into external literature and policy, sometimes returning as poorly-fitted interventions.
% ABSENT_VOICES: The practitioners whose ritual is being studied are rarely present in the theoretical debate about which register 'really' explains survival — that debate happens in academic and policy venues they are excluded from, using categories (symbolic vs. functional) that may not map onto their own experience of the practice as unified.
% DISAPPEARANCE_RATIONALE: If the hybrid-encoding reading itself disappeared (i.e., if the field only ever analyzed ritual through single-register lenses), practicing communities would likely notice nothing structural change in the short term — their practice would continue unaffected. But over the longer arc, sustained mischaracterization feeds into disaster-response policy and cultural-preservation funding decisions that DO reshape the material conditions communities operate under (e.g., funding a 'manual' that formalizes competence content while defunding the ceremonial context that made transmission reliable). Whether the world rearranges therefore depends on time horizon and which downstream institution acts on the flattened analysis — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Societies repeatedly exposed to catastrophe (flood, famine, epidemic, volcanic event) needed a transmission mechanism robust enough to survive literacy loss, generational rupture, and the unreliability of purely instrumental instruction — a mechanism that would be practiced even when the practical rationale was forgotten, and would preserve practical content even when the symbolic meaning drifted.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars and disaster-resilience researchers studying practice persistence across catastrophe cycles (independent of the communities themselves) attest that ritual complexes which retained BOTH registers show measurably higher transmission fidelity across generational disruption than complexes reduced to either pure symbol or pure extracted-manual competence — this is external corroboration from outside the beneficiary set. No community elder has been asked to theorize the dual-register structure explicitly; their corroboration is behavioral (continued joint practice) rather than propositional.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).
:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness stays low and nearly flat (0.08 to 0.12) across the interval because the practicing community incurs no ongoing extraction from maintaining the dual-register structure — if anything the cost is borne externally by analysts, not internally by practitioners. Theater ratio rises modestly (0.15 to 0.22) as the practice ages and some ceremonial elements outlive full comprehension of their practical referent (a common and expected feature of long-lived dual-register systems, not evidence of decay). Accessibility collapse (0.35) and resistance (0.4) are moderate rather than extreme: alternative framings (pure-symbol, pure-competence) are readily available to outside analysts — this reading does not foreclose them by force, it competes with them intellectually — and communities show some resistance to having their unified practice split by external classification schemes.
 *
 * DIRECTIONALITY LOGIC:
 *   Practicing communities and knowledge-holders are the structural beneficiaries: the hybrid form is precisely what lets them maintain both identity continuity and practical survival information in one durable vehicle, at low cost, without needing to resolve a theoretical question they have no stake in answering. Binary-classifying analysts are the payer group in this reading — not victims in a coercive sense, but the party who bears the cost of the analytical distortion when a unified practice is forced into a single-register account for publication or policy translation. Disaster-response institutions are excluded from the community's actual transmission process and only receive analysts' flattened output, which is why their interventions sometimes misfire.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid-encoding reading is precisely the reading that PREVENTS mandatrophy misclassification in both directions: reading the ritual as pure symbol (symbol_survival_reading) risks declaring the practical-knowledge function obsolete once literacy or formal instruction exists, prematurely dismissing a still-live transmission function; reading it as pure competence-transmission (competence_transmission_reading) risks declaring the ritual obsolete once a 'better' extracted manual exists, discarding the identity-continuity function that keeps the manual's content socially transmissible at all. This reading holds the founding problem (dual robust transmission under catastrophic disruption) as still live and resists premature closure from either single-register frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_vs_single_register_framing,
    'Is the dual-register structure a genuine irreducible feature of how these rituals function, or is it an artifact of this reading''s own refusal to resolve which register is doing the causal work — i.e., is ''hybrid encoding'' itself just a third theoretical stance rather than a resolution of the other two?',
    'Longitudinal cases where one register is artificially stripped (e.g., a symbolic ceremony continues after practical relevance is fully obsolete via modern infrastructure, or a practical protocol is extracted into secular procedure without ceremony) and transmission fidelity is compared to intact hybrid cases across multiple generations.',
    'If stripped-register practices show equivalent long-run transmission fidelity to hybrid ones, the hybrid_encoding_reading''s low-ε coordination claim weakens and the constraint''s beneficiary claim (communities benefit from NOT resolving the registers) becomes harder to sustain. If stripped-register practices reliably fail to transmit, the hybrid reading''s coordination function is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_vs_single_register_framing, empirical, 'Whether dual-register hybridity is functionally necessary or a residual theoretical stance.').

omega_variable(
    analyst_victim_status_ambiguity,
    'Is it structurally sound to name binary-classifying analysts as a ''victim'' group, given that they are mobile, analytical-power agents choosing a theoretical frame, rather than agents coerced into bearing a cost?',
    'Track whether analysts who adopt single-register frames experience career, funding, or credibility costs traceable to publication mischaracterization versus whether the cost is purely conceptual (a less accurate account) with no material consequence to the analyst.',
    'If the cost to analysts is purely conceptual with no material consequence, the victim declaration overstates the constraint''s extractive character and this reading edges closer to pure rope with no payer at all; if mischaracterization produces real funding/policy costs that loop back onto analysts'' credibility, the payer designation is well-founded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(analyst_victim_status_ambiguity, conceptual, 'Whether analysts constitute a genuine payer class or are merely holding a contestable theoretical position.').

omega_variable(
    kernel_reading_selection_evidence,
    'What specific ethnographic or historical signal would distinguish this reading (hybrid_encoding) from the sibling readings for a given catastrophe-memory ritual, rather than defaulting to hybrid_encoding as the ''safe'' synthesis position?',
    'Case-by-case ethnographic tests: does removing the symbolic frame while preserving practical content (secularized protocol) preserve transmission? Does removing practical content while preserving symbolic form (empty ceremony) preserve transmission? Differential survival outcomes across these natural experiments would locate which reading actually fits a given ritual complex.',
    'This omega documents the CS-framing under-determination directly: the choice between hybrid_encoding, symbol_survival, and competence_transmission readings is not always empirically forced by the case at hand, and different observers may reasonably favor different readings for the same ritual absent decisive natural-experiment data.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'What would empirically discriminate between the three kernel readings for a specific case.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.09).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.11).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 80, 0.115).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_survival__hybrid_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__hybrid_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_survival kernel, decomposed per the epsilon-invariance principle: symbol_survival_reading (identity/boundary continuity as the survival mechanism), competence_transmission_reading (practical knowledge transfer as the survival mechanism), and this hybrid_encoding_reading (both registers operating jointly, irreducibly). All three share the same underlying ritual practices but attribute the survival function differently, producing different beneficiary/victim structures and different epsilon values. This reading has the lowest epsilon of the three because it identifies no internal community-level extraction at all — its only named cost is external and analytical (mischaracterization borne by researchers), whereas the single-register siblings each risk classifying the other function's stakeholders (competence-holders under symbol_survival_reading; symbolic-authority holders under competence_transmission_reading) as victims of neglect or as passengers on a function they don't share credit for. Network edges reflect that flattened findings under either single-register sibling can distort resource allocation and legitimacy claims feeding back into this reading's own communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
