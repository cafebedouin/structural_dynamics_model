% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Hybrid Pragmatic Partition of Classification Authority (Deferential Realism Typology)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story classifies the Deferential Realism classification apparatus
 *   itself as it operates under the hybrid pragmatic reading: a partitioned
 *   authority structure in which core classifications (physical and
 *   coordination-grounded cases) are treated as observationally settled,
 *   while contested peripheral classifications turn on normative judgments
 *   about whose interests count as legitimate. The arrangement genuinely
 *   coordinates an interpretive community — it tells practitioners where to
 *   spend observational effort and where to deliberate — and it
 *   simultaneously concentrates discretionary verdict power that binds
 *   parties who never entered the process. The claim and the metrics are
 *   independent authored facts: the claim states the structure I believe true
 *   of the arrangement (coordination plus asymmetric, actively maintained
 *   extraction); the metrics describe its observed operation at medium
 *   intensity. Where the engine's per-seat computations diverge from this
 *   claim, that divergence is the datum. KEY AGENTS (by structural
 *   relationship): framework_maintainers (institutional/identity_locked) —
 *   administer the partition and configure peripheral adjudication;
 *   constraint_story_authors (moderate/constrained) — supply the raw
 *   material, absorb format enforcement and default-driven reshaping;
 *   classified_peripheral_subjects (powerless/trapped) — bear constructed
 *   verdicts with no rebuttal channel; downstream_policy_analysts
 *   (organized/mobile) — consume settled verdicts, inherit peripheral
 *   disputes; rival_reading_proponents (moderate/mobile) — hold readings the
 *   partition forecloses inside the kernel; methodology_reviewers
 *   (analytical/analytical) — assess whether the line tracks a real boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.46).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Hybrid Pragmatic Partition of Classification Authority (Deferential Realism Typology)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '38ac3278-6b69-4475-83b1-f9f3a51c38a3').
narrative_ontology:cs_kernel_codification('38ac3278-6b69-4475-83b1-f9f3a51c38a3', formalized).
narrative_ontology:cs_authority_grounding('38ac3278-6b69-4475-83b1-f9f3a51c38a3', expertise).
narrative_ontology:cs_interpretation_layer_present('38ac3278-6b69-4475-83b1-f9f3a51c38a3').
narrative_ontology:cs_reading_relation('38ac3278-6b69-4475-83b1-f9f3a51c38a3', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('38ac3278-6b69-4475-83b1-f9f3a51c38a3', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_axiom('38ac3278-6b69-4475-83b1-f9f3a51c38a3', foundational, core_classifications_observationally_fixed).
narrative_ontology:cs_axiom_status(core_classifications_observationally_fixed, holdable).
narrative_ontology:cs_axiom_grounding('38ac3278-6b69-4475-83b1-f9f3a51c38a3', core_classifications_observationally_fixed, empirically_contingent).
narrative_ontology:cs_axiom('38ac3278-6b69-4475-83b1-f9f3a51c38a3', foundational, peripheral_classification_normatively_constructed).
narrative_ontology:cs_axiom_status(peripheral_classification_normatively_constructed, holdable).
narrative_ontology:cs_axiom_grounding('38ac3278-6b69-4475-83b1-f9f3a51c38a3', peripheral_classification_normatively_constructed, conventional).
narrative_ontology:cs_reference_frame('38ac3278-6b69-4475-83b1-f9f3a51c38a3', partitioned_classification_authority).
narrative_ontology:cs_drift_state('38ac3278-6b69-4475-83b1-f9f3a51c38a3', contemporary_corpus_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('38ac3278-6b69-4475-83b1-f9f3a51c38a3', '2026-08-12T09:30:00Z').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, downstream_policy_analysts).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_story_authors).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, classified_peripheral_subjects).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, rival_reading_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_story_authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the classification apparatus: the story schema, the compiler that transforms authored narratives into machine-checkable form, and the engine that computes verdicts from structural data. They set the configuration defaults that shape how contested verdicts come out — override targets, metric floors, fallback values — and run the validation batteries that reject nonconforming submissions. Their professional standing is fused with the framework's adoption; leaving would mean abandoning the body of work and the community built around it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers, beneficiary).

% Write the structured narratives the apparatus consumes. They gain a shared vocabulary, working tooling, and a venue where their analyses are checked rather than merely asserted. They bear the friction of enforced formats, coverage rules, and alignment grids, and they bear it a second time when engine defaults or configured override targets reshape a verdict away from the claim they authored. Leaving means losing the corpus, the tooling, and the audience that reads it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_story_authors, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_story_authors, payer).

% Real-world actors — borrowers, gig workers, platform publishers, patients — whose arrangements are the subject matter of contested verdicts. Those verdicts are reached through normative judgment about whose interests count as legitimate rather than through any measurement they could answer with counter-evidence. The verdicts circulate in policy discourse and scholarly citation whether or not these actors ever learn of them, and there is no channel through which they can submit observational rebuttal, because the verdict's basis is by design not observational.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, classified_peripheral_subjects, payer,
    powerless, biographical, trapped, global).

% Researchers, journalists, and policy staff who consume finished verdicts. They rely on core classifications being settled enough to cite as background fact, and they inherit the burden of adjudicating contested-verdict disputes themselves, since the framework hands those over as openly normative. Switching to an alternative analytic tradition is possible at the cost of losing the corpus and its cross-references.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, downstream_policy_analysts, beneficiary,
    organized, biographical, mobile, continental).

% Scholars and practitioners who hold that the typology is either uniformly observational (misclassification is error, correctable by better measurement) or uniformly rhetorical (verdicts are declared, not discovered). Inside this framework their positions carry no standing — the partition assigns each of their claims to the wrong side of the line by their own lights. They remain active in the wider discourse, publishing critiques, but sit outside the framework's decision loop.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_reading_proponents, excluded,
    moderate, biographical, mobile, global).

% Philosophers of science, meta-methodologists, and institutional designers who assess the framework itself rather than any classified subject. They examine whether the partition tracks a real boundary in the subject matter or a convenience line drawn where current methods run out, publish assessments, and can shift the framework's legitimacy among adopters. They neither run the apparatus nor bear its verdicts.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, methodology_reviewers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interpretive-coordination problem of a community classifying social and epistemic constraints: it partitions disputes into observationally resolvable cases and normatively adjudicable cases, giving practitioners a shared rule for when to measure and when to deliberate, and thereby preventing both false settlement (contested allocations laundered as settled fact) and unresolvable regress (no verdict ever defensible).
% TRANSFER_FUNCTION: Moves epistemic authority and discretion. Settled observational verdicts on core classifications accrue to the framework's maintainers and flow outward to downstream users as citable background fact. Discretionary power over contested peripheral verdicts accrues to whoever holds the beneficiary-legitimacy criteria — in practice the maintainer-configured defaults and the authoring seat — while the consequences of constructed verdicts fall on classified subjects who never entered the process.
% ABSENT_VOICES: Classified peripheral subjects are absent in the strong sense: verdicts about their arrangements are computed from authored stories and configured defaults without their participation, and the framework offers them no submission channel. Rival-reading proponents are present in the surrounding discourse but excluded from the framework's decision loop. Both would contest the partition's assignment of their position to normative discretion or to error-correction respectively.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, the interpretive community would re-polarize between pure observationalism and pure constructivism. Core classifications would lose their settled status — every verdict contestable by normative argument — and peripheral verdicts would lose their deliberative channel — every dispute becoming a regime fight over whose values decide. The corpus's shared grammar for talking about constraints would fragment, and cross-framework citation of settled verdicts would stop.
% FOUNDING_PROBLEM: Early constraint-analysis practice oscillated between two failures: treating every social arrangement as an objective structure, which produced false settlements that laundered contested allocations as natural facts; and treating every arrangement as pure politics, which produced a regress in which no classification could be defended against the charge of being merely declared. The partition was built to route each dispute to the mechanism that can actually settle it.
% FOUNDING_PROBLEM_CORROBORATION: Methodology reviewers outside the benefiting parties attest that the oscillation problem is real and that some partition-style response is defensible, while disputing whether the drawn line tracks a real joint or the framework's convenience. Rival-reading proponents corroborate the problem but deny the partition solves it — the observational camp holds the periphery is measurable after all; the rhetorical camp holds nothing is discovered. Corroboration for the founding problem is broad; corroboration for this solution is contested.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).
:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the observational half of the arrangement is clean — settled core verdicts impose negligible burden on anyone and cost nothing to cite — while the peripheral half concentrates verdict authority shaped by maintainer-configured defaults, and its outputs bind absent parties; the reading's own transparency about construction tempers but does not remove the asymmetry. Suppression 0.52: enforcement is procedural rather than coercive — fail-closed validation, coverage rules, alignment-grid rejection — plus structural foreclosure of sibling readings inside the kernel; exit to other traditions remains open, capping suppression below coercive levels. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater ratio 0.28: most compliance ritual (validation, provenance, grids) catches real defects, but a growing share of ceremony converts into few corrected verdicts. Accessibility collapse 0.50: inside the framework alternatives collapse (both sibling readings foreclosed, nonconforming stories rejected), but rival traditions remain usable outside it, so collapse is partial. Resistance 0.55: live and channeled — sibling critiques persist, authors contest overrides, omega variables document framing disputes, audits log residue classes. The measurement series run on one shared time grid (points 0–24 at intervals of 4) so every tracked metric is authored at every examined time point; enforcement capacity visibly matured mid-interval (coverage and grid rules), which is why suppression_requirement is tracked alongside the other two series.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience four different instruments. From the maintainer seat the arrangement is a well-engineered tool whose flexibility at the periphery is a designed feature. From the classified-subject seat the same flexibility is unilateral discretion over verdicts about them, issued without a rebuttal channel. From the author seat it is helpful infrastructure with quiet steering in the defaults. From the rival-proponent seat it is an arbitrary line that disqualifies their position by construction. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework maintainers sit near the beneficiary pole: they run the arrangement, configure the periphery's adjudication criteria, and collect both authority and stability (low d). Constraint story authors sit near symmetric: genuine gains in tooling and venue, genuine payments in enforcement friction and default-driven reshaping (mid d). Classified peripheral subjects sit near the full-target pole: they bear constructed verdicts, are trapped in the vocabulary's circulation, and possess no observational rebuttal channel by design (high d). Downstream analysts sit near the beneficiary pole with mobile exit damping further (low d). Rival-reading proponents bear real foreclosure costs inside the kernel, pushing d high, though mobile exit to other traditions moderates it. Methodology reviewers are analytical and roughly symmetric by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading is itself a mandatrophy guard. By declaring the periphery constructed, it refuses the false-summit move of laundering discretionary verdicts as observations; by holding the core observational, it refuses the cynical move of dissolving settled facts into rhetoric. The founding problem — oscillation between false settlement and unresolvable regress — remains live wherever the framework is applied, so no sunset is declared and the arrangement's justification is steady-state allocation of dispute-resolution mechanisms, not a transition. The recorded risk: if peripheral contestation ever goes fully quiet (no reversals, no live disputes), the periphery half will have atrophied into administered verdicts while retaining its deliberative vocabulary — the accountability omega below instruments exactly that check.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_joint_reality,
    'Does the core/periphery line track a real joint in the epistemic structure of constraint classification, or is it a convenience boundary drawn where current observational methods happen to run out?',
    'Cross-assignment trials: apply observational protocols to paradigmatic peripheral cases and normative-beneficiary protocols to paradigmatic core cases; if core verdicts survive normative challenge and peripheral verdicts stabilize under observation, the line tracks structure rather than convenience.',
    'If the line is conventional, this reading collapses toward the rhetorical-scaffold sibling and the arrangement''s effective extraction rises, since the partition itself becomes cover for discretionary verdict power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_joint_reality, conceptual, 'Whether the partition marks a natural epistemic boundary or a methodological convenience.').

omega_variable(
    constructed_epsilon_accountability,
    'When peripheral verdicts rest on constructed rather than observed epsilon, is there any channel through which affected subjects can trigger reversal, or does verdict authority run one way?',
    'Track subject-initiated contest outcomes across the corpus: count peripheral verdicts reversed after challenge from classified parties versus upheld by default.',
    'A near-zero reversal rate would indicate the periphery operates as administered authority, raising effective extraction above the authored epsilon; a healthy reversal rate would confirm the periphery retains deliberative character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_epsilon_accountability, empirical, 'One-way versus contestable authority over constructed peripheral verdicts.').

omega_variable(
    sibling_drift_direction,
    'Under sustained pressure, will hybrid practice drift toward a sibling — treating peripheral verdicts as observational errors when discretion embarrasses the framework, or treating core verdicts as rhetorical declarations when observational warrants fail?',
    'Longitudinal coding of practitioner behavior at the two joints: frequency of observational language applied to peripheral disputes and of normative language applied to core disputes.',
    'Drift toward uniform observationalism would lower peripheral discretion but raise false-settlement risk; drift toward wholesale rhetoric would raise discretionary concentration; either outcome dissolves the partition this story classifies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_drift_direction, conceptual, 'Durability of the partition against drift toward either sibling reading.').

omega_variable(
    core_contamination_check,
    'Do any core verdicts certified as observationally settled themselves rest on constructed beneficiary structures — is the periphery''s method leaking into the core?',
    'Run beneficiary-declaration audits on certified core verdicts; any core verdict carrying identifiable beneficiaries triggers false-summit review of the certification chain.',
    'Contamination would undercut the partition''s central warrant, raising epsilon for the whole arrangement and strengthening the rhetorical-scaffold sibling''s critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_contamination_check, empirical, 'Whether the observational core is contaminated by constructed verdicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.43).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 24, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the constraint typology' decomposes into three structurally distinct readings of one kernel, linked per the epsilon-invariance principle. This story carries the hybrid-partition reading (medium suppression, hybrid epsilon measurement — observational for core cases, constructed for peripheral ones). The immutable-diagnostic sibling carries uniformly observational epsilon (lower constructed-discretion concentration, higher misclassification-as-error burden on challengers); the rhetorical-scaffold sibling carries wholly declared epsilon (highest discretionary concentration, lowest observational warrant). Epsilon differs across the family because the referent arrangement — who adjudicates what — differs, not because one constraint is measured three ways. Each sibling file links back here via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
