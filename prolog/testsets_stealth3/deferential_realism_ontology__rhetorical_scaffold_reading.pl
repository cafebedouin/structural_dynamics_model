% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology Deployed as Rhetorical Scaffold in Policy Critique
 *   domain: epistemology/normative theory/institutional design
 *
 * SUMMARY:
 *   This story instantiates the rhetorical_scaffold_reading of the
 *   deferential_realism_ontology kernel: the constraint typology operates as
 *   a normative vocabulary for policy critique, in which a classification is
 *   a declarative act — 'this mechanism serves illegitimate beneficiaries' —
 *   rather than the report of a measurement, and the vocabulary earns its
 *   keep by persuading. The standing arrangement under contest is the
 *   practice of deploying the typology this way, and epsilon is authored by
 *   this reading's own lights over that referent (never over the diagnostic
 *   arrangement it declines to endorse). Claimed type and metrics are
 *   independent authored facts: the reading takes the instrument to be a
 *   scaffold — transitional support for critique, retired application by
 *   application once the argument lands — while the metrics describe its
 *   observed discursive operation: a mild deployer advantage, near-absent
 *   coercion, rising ceremonial overhead. Where the computed classification
 *   diverges from the claim, that divergence is the datum the corpus exists
 *   to take. KEY AGENTS (by structural relationship): -
 *   policy_critics_advocacy_networks: primary beneficiary (organized/mobile)
 *   — wields the vocabulary as an opening move; collects agenda-setting
 *   initiative - framework_practitioner_community:
 *   administrator/agenda-setter (organized/constrained) — curates corpus and
 *   usage norms; careers bound to the instrument -
 *   labeled_mechanism_operators: primary target (powerful/trapped) — bears
 *   the defensive costs of contested characterizations -
 *   competing_framework_partisans: secondary target (moderate/constrained) —
 *   surrenders discursive ground as the labels gain currency -
 *   policy_commentary_audiences: incidental beneficiary and indirect cost
 *   bearer (moderate/mobile) - classified_mechanism_subjects: excluded voice
 *   (powerless/trapped) — lives inside the mechanisms debated over their
 *   heads - discourse_meta_analysts: analytical observer
 *   (analytical/analytical) — sees the full structure, collects from no side
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.38).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.18).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology Deployed as Rhetorical Scaffold in Policy Critique").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative theory/institutional design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '18e6e5dd-407a-4b6f-9aad-8a58fb281586').
narrative_ontology:cs_kernel_codification('18e6e5dd-407a-4b6f-9aad-8a58fb281586', formalized).
narrative_ontology:cs_authority_grounding('18e6e5dd-407a-4b6f-9aad-8a58fb281586', distributed).
narrative_ontology:cs_reading_relation('18e6e5dd-407a-4b6f-9aad-8a58fb281586', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('18e6e5dd-407a-4b6f-9aad-8a58fb281586', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('18e6e5dd-407a-4b6f-9aad-8a58fb281586', foundational, classification_is_declared_not_discovered).
narrative_ontology:cs_axiom_status(classification_is_declared_not_discovered, holdable).
narrative_ontology:cs_axiom_grounding('18e6e5dd-407a-4b6f-9aad-8a58fb281586', classification_is_declared_not_discovered, deontological).
narrative_ontology:cs_axiom('18e6e5dd-407a-4b6f-9aad-8a58fb281586', foundational, persuasive_efficacy_is_the_warrant).
narrative_ontology:cs_axiom_status(persuasive_efficacy_is_the_warrant, holdable).
narrative_ontology:cs_axiom_grounding('18e6e5dd-407a-4b6f-9aad-8a58fb281586', persuasive_efficacy_is_the_warrant, instrumental).
narrative_ontology:cs_reference_frame('18e6e5dd-407a-4b6f-9aad-8a58fb281586', open_advocacy_vocabulary).
narrative_ontology:cs_drift_state('18e6e5dd-407a-4b6f-9aad-8a58fb281586', contemporary_corpus_apparatus_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('18e6e5dd-407a-4b6f-9aad-8a58fb281586', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocacy_networks).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, framework_practitioner_community).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_commentary_audiences).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_operators).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, competing_framework_partisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_commentary_audiences).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_constructivism).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, persuasive_definition_pragmatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Coalitions of researchers, NGO campaigners, and opinion writers who reach for the typology when arguing that a lending regime, licensing scheme, or subsidy structure serves the wrong interests. The shared labels let them open debates with a ready-made structural frame, recruit allies across issue areas, and compress months of groundwork into a single characterization. Nothing binds them to the vocabulary; if a rival lexicon persuades better next cycle, they will switch.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocacy_networks, beneficiary,
    organized, biographical, mobile, national).

% Corpus maintainers, tool builders, and workshop organizers who curate the story collection, arbitrate formatting disputes, and train new users. Their publications, funding, and professional standing are bound up with the framework's circulation, which makes walking away costly even though no one compels their participation. They set usage conventions but claim no veto over how outsiders deploy the labels.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_practitioner_community, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, framework_practitioner_community, beneficiary).

% Legislative staff, journalists, and engaged readers who consume the classifications as shortcuts. The labels save them analytic labor; the same convenience risks their treating a contested characterization as a settled finding. They can ignore the vocabulary at negligible personal cost, and periodically do.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_commentary_audiences, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, policy_commentary_audiences, payer).

% Executives, agency heads, and trade associations whose mechanisms get characterized as serving improper beneficiaries. Each characterization commits them to a rebuttal cycle: commissioning studies, briefing journalists, mobilizing sympathetic experts. They cannot leave the arenas where the labels circulate — silence reads as concession — so defensive expenditure is unavoidable for as long as a characterization sticks.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_operators, payer,
    powerful, biographical, trapped, national).

% Scholars and commentators attached to rival ways of talking about institutions: older moral-economy idioms, law-and-economics framings, or the conviction that classifications are observations to be checked rather than moves to be answered. With each cycle in which the typology's labels gain currency, these writers surrender audience attention and must restate their positions in borrowed terms or lose the thread of the debate.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, competing_framework_partisans, payer,
    moderate, generational, constrained, global).

% Borrowers inside debt programs, licensees, gig workers, and residents of jurisdictions whose arrangements are being characterized upstream. The argument over how to label their situation runs in venues they do not attend; their lived experience reaches the debate only when an advocate elects to carry it there.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, classified_mechanism_subjects, excluded,
    powerless, generational, trapped, regional).

% Analysts studying how normative vocabularies reshape institutional argument. They track which characterizations persuade, which fade, and what the contest does to the underlying disputes; they collect from no side and pay no defensive costs.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, discourse_meta_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocacy_networks).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives geographically and topically dispersed critics a common classification language, so a characterization coined in one campaign can be recognized, reused, and reinforced in another without re-deriving the underlying structural analysis each time.
% TRANSFER_FUNCTION: Moves agenda-setting initiative and rhetorical authority toward successful deployers of the vocabulary and toward the community that curates it; moves defensive expenditure from operators of characterized mechanisms and discursive ground from holders of rival framings; moves decision-ready framings to audiences at the price of their independent scrutiny.
% ABSENT_VOICES: People living inside the mechanisms being characterized — borrowers, licensees, workers, residents — are structurally absent: the classification contest runs among critics, operators, and audiences, and their testimony enters only when an advocate chooses to ferry it. Their absence flatters any appearance of consensus among the seated parties: agreement partly reflects who was invited to the vocabulary war, not who lives with its verdicts.
% DISAPPEARANCE_RATIONALE: Nothing physical breaks overnight: the mechanisms under debate continue operating. What rearranges is the argument economy — critic coalitions would rebuild coordination from ad-hoc moral and economic reasoning at sharply higher cost, operators would enjoy cheaper rebuttals, rival framings would reclaim abandoned ground, and the practitioner community would lose its organizing object. The vocabulary is load-bearing for the current division of argumentative labor, which is what a working scaffold looks like from inside.
% FOUNDING_PROBLEM: Institutional critique lacked a portable structural language: each campaign had to rebuild its account of how a mechanism funnels advantage, so lessons learned in one domain (consumer lending, say) failed to transfer to structurally similar arrangements elsewhere (occupational licensing, subsidies, platform rules).
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the beneficiary set: investigative journalists and NGO strategists with no corpus role confirm the vocabulary lowers their cross-domain coordination costs. But the strongest external witnesses — operators of characterized mechanisms — reject the founding framing outright as tendentious, and nearly every other user gains rhetorically from adoption, so no fully disinterested attestation of the founding problem exists; that absence is itself signal.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.38: the vocabulary confers a real asymmetric advantage — deployers set the terms that targets must answer, and answering costs more than raising the characterization did — but the costs it imposes (rebuttal cycles, surrendered framing ground) are borne inside a voluntary argumentative arena, and this reading prices them accordingly. Suppression is low (0.18): rival framings remain fully available and legally unimpeded; competition is rhetorical, not coercive. Theater_ratio is 0.32 and climbing: validation displays, precision-formatted values, and corpus ceremonies increasingly serve persuasive presentation alongside whatever checking they perform — under this reading some rigor-display is functional (it persuades), which the theater_function_reflexivity omega flags as a definitional hazard. Accessibility_collapse is 0.20: grasping the typology collapses almost none of the alternative framings — moral idiom, economic analysis, and the diagnostic conception all remain one essay away, which is exactly what the kernel context predicts for a vocabulary that competes rather than commands. Resistance is 0.55: characterized operators and rival partisans contest deployments energetically; the vocabulary wins arguments, it does not silence them. Metrics were authored from this descriptive picture without reference to the scaffold claim; adjudication belongs to the engine.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the deployer seat the vocabulary is a gift: free coordination, compressed argumentation, initiative in setting agendas. From the operator seat the same labels are an ambush: a characterization arrives pre-loaded with moral valence, and the rebuttal tax recurs every news cycle. From the rival-partisan seat it is a quiet takeover of the discourse's operating system — positions must be restated in borrowed terms to stay audible. From the audience seat it is ambivalent convenience: analytic labor saved, independent scrutiny discounted. The engine computes per-seat classifications from the power, exit, and role data above; these lived asymmetries are what that computation should recover.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (critic networks, practitioner community, audiences collecting shortcuts) sit near the d≈0 end: the arrangement subsidizes their argumentative and consumption work. Payers (characterized operators, rival partisans) sit near d≈1: they fund the discourse's defense bill. Audiences straddle — shortcut gained, discernment cost paid — landing mid-range. Exit modulation does real work here: operators are trapped (a label follows them into any venue, and silence reads as concession), pushing them toward full-target treatment despite institutional-grade resources; critics are mobile (they can switch lexicons next campaign at trivial cost), preserving their beneficiary discount. Suppression, unlike extractiveness, is an unscaled structural property: its low authored value records that no barrier blocks rival framings, independent of scope or power, and it should not be inflated by the vocabulary's wide spatial circulation. No directionality overrides were needed — the beneficiary and victim declarations plus exit options reproduce the structural relationships faithfully.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — institutional critique lacking a portable structural language — remains live: new mechanisms proliferate faster than any lexicon exhausts them, so status=live pairs with verdict=world_rearranges without tripping the dead-problem mismatch flag. Mandatrophy discipline cuts both ways here. Recognizing the vocabulary's thin but genuine coordination function prevents over-reading it as mere faction branding (which would misclassify a working instrument as inert); the rising theater series guards the opposite error, preventing ceremonial growth from being mistaken for deepening function. The reading itself encodes its own retirement conditions: a classification's job ends when its target arrangement is reformed or the debate moves on. That is the scaffold intuition carried as lifecycle rather than clause — there is no formal sunset mechanism anywhere in the practice, and has_sunset_clause is therefore authored false even though the transitional character is the reading's core commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates one reading (rhetorical_scaffold_reading) of the deferential_realism_ontology kernel — which authored structural facts would the sibling readings re-author over the same referent?',
    'Generate the sibling constraint files (immutable_diagnostic_reading, hybrid_pragmatic_reading) over the identical deployment-practice referent and diff their epsilon values, claimed types, and stakeholder surfaces against this file.',
    'The diagnostic sibling would re-author epsilon as a measurement-validity residual — plausibly far below 0.38 — and recast operators'' rebuttal costs as noise around true values; the hybrid sibling would partition the seat set into fixed-core and normatively-contested-periphery sub-surfaces with bimodal epsilon. Divergence across the family confirms the decomposition; uniformity would indicate the kernel collapses into one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer positioning: this file is one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    epsilon_construction_comparability,
    'If epsilon values are constructed through normative judgment rather than discovered through measurement, are cross-story epsilon comparisons meaningful, or are they surveys of authorial temperament?',
    'Inter-author reliability studies on shared cases, plus calibration of constructed values against the diagnostic reading''s measured residuals wherever both readings have authored the same underlying mechanism.',
    'Systematic divergence between constructed and measured values on shared cases validates the three-file decomposition as genuinely distinct constraints; convergence would press this reading toward the diagnostic frame and erode the declaration axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_construction_comparability, conceptual, 'Whether constructed epsilon admits principled comparison across stories and readings.').

omega_variable(
    theater_function_reflexivity,
    'For a constraint whose declared function is persuasion, is the rising share of ceremonial activity (validation displays, decimal-precision values, corpus rituals) theater in the proxy-substitution sense, or is it the functional payload itself?',
    'Ablation-style comparison of persuasive outcomes for deployments presented with and without apparatus display (precision formatting, validation seals, temporal series).',
    'If display drives persuasion, theater_ratio is ill-defined for this reading and the rising series signals growing fitness rather than decay; if display is inert, the series tracks genuine ritualization and the instrument is drifting toward self-performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_function_reflexivity, conceptual, 'Reflexivity problem: measuring theatricality in a constraint whose product is rhetorical effect.').

omega_variable(
    legitimacy_judgment_provenance,
    'The reading settles classification by judging whose interests a mechanism serves — whose judgment of legitimacy licenses a given declaration, and is that account supplied inside the vocabulary or imported from host discourses case by case?',
    'Code deployed classifications for the legitimacy accounts they invoke (rights-based, welfare-based, proceduralist) and test whether persuasive outcomes track the imported account''s independent standing rather than the vocabulary''s own machinery.',
    'If legitimacy judgments are wholly parochial imports, the vocabulary''s coordination function thins toward faction-marking — an identity-coordination profile — and this story''s information-standard typing understates its social-boundary role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_judgment_provenance, conceptual, 'Where the normative standard inside ''illegitimate beneficiaries'' comes from.').

omega_variable(
    deployment_outcome_tracking,
    'Does rhetorical deployment of the typology actually produce policy movement, or does it mainly redistribute standing among professional commentators?',
    'Longitudinal matching of classification deployments to subsequent regulatory and legislative outcomes, against matched control debates conducted without the vocabulary.',
    'Null results would undercut the warrant axiom (persuasive efficacy) on its own instrumental grounds and push adherents toward the diagnostic sibling''s self-understanding of the framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deployment_outcome_tracking, empirical, 'Whether the framework''s persuasive power converts into material policy change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dr_rhet_scaffold_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t0, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.21).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t4, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t8, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t12, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t16, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t20, observed).
narrative_ontology:measurement(dr_rhet_scaffold_tr_t24, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(dr_rhet_scaffold_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(dr_rhet_scaffold_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t0, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t4, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t8, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t12, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t16, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t20, observed).
narrative_ontology:measurement(dr_rhet_scaffold_be_t24, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(dr_rhet_scaffold_be_t24, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferential_realism_ontology__rhetorical_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the constraint typology' covers three structurally distinct commitments, written as three linked stories. This file authors the rhetorical instantiation — epsilon constructed by normative judgment over the deployment-practice referent. The immutable_diagnostic_reading file authors the observational instantiation (epsilon as a measured residual over fixed referents; plausibly far lower, with misclassification as correctable error). The hybrid_pragmatic_reading file authors the split instantiation (discovered core, normatively contested periphery, bimodal epsilon). Directionality of influence: the diagnostic reading's accumulated credibility historically lends persuasive cover to rhetorical deployments — its precision is borrowed as display — so this reading sits downstream of both siblings while exerting competitive pressure back on them; edges here record this story's links into the family, and each sibling reciprocates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
