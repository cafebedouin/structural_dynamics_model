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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Constraint Typology as Rhetorical Vocabulary — Scaffold Reading
 *   domain: epistemological/normative/institutional
 *
 * SUMMARY:
 *   The kernel is the constraint typology itself — the standing practice of
 *   classifying mechanisms in policy critique. This story instantiates ONE
 *   reading of that kernel: the rhetorical scaffold reading, on which the
 *   typology is a normative vocabulary, a classification is declared rather
 *   than discovered when a mechanism serves illegitimate beneficiaries, and
 *   the framework's value lies in its persuasive power. The constraint under
 *   examination is the standing practice of rhetorical deployment: critics
 *   declare, audiences are moved, labeled operators bear rebuttal burdens,
 *   and an enforcement machinery accretes to hold the vocabulary's authority.
 *   Per the kernel-reading ε referent rule, ε is authored for that standing
 *   practice as THIS reading assesses it — not for any arrangement this
 *   reading would endorse in its place. Although the reading's name invokes
 *   scaffold imagery, the practice itself shows no sunset machinery and is
 *   actively maintained by identifiable beneficiaries, which is why the
 *   structural claim is tangled_rope rather than scaffold. The sibling
 *   readings (immutable_diagnostic, hybrid_pragmatic) are separate
 *   constraints linked in network.affects_constraints; the contest structure
 *   is carried in the omega variables, not inside this constraint's
 *   classification.
 *
 * KEY AGENTS:
 *   - framework_maintainers: agenda-setter (institutional / identity_locked) — administer the vocabulary, author its rules, enforce usage norms; institutional identity fused with the framework's persistence
 *   - policy_critics_advocates: primary beneficiary (moderate / mobile) — declare classifications and collect rhetorical authority at near-zero cost per deployment
 *   - labeled_mechanism_defenders: primary payer (powerful / constrained) — operate condemned mechanisms; must rebut a label that presents itself with the form of a finding
 *   - policy_deliberation_audiences: dual payer/beneficiary (moderate / mobile) — receive compact framing under a measurement-pretense; pay attention believing they receive diagnosis
 *   - unconsulted_mechanism_subjects: excluded (powerless / trapped) — the people governed by the labeled mechanisms, spoken for by declarations that never measure them
 *   - diagnostic_reading_proponents: payer (organized / constrained) — same-community peers whose measurement-based corrections are reframed as missing the point; bear eroded standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.32).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.3).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Constraint Typology as Rhetorical Vocabulary — Scaffold Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemological/normative/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'b5e971a2-b4a5-4be3-9790-1503164c900d').
narrative_ontology:cs_kernel_codification('b5e971a2-b4a5-4be3-9790-1503164c900d', formalized).
narrative_ontology:cs_authority_grounding('b5e971a2-b4a5-4be3-9790-1503164c900d', practice).
narrative_ontology:cs_interpretation_layer_present('b5e971a2-b4a5-4be3-9790-1503164c900d').
narrative_ontology:cs_reading_relation('b5e971a2-b4a5-4be3-9790-1503164c900d', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('b5e971a2-b4a5-4be3-9790-1503164c900d', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('b5e971a2-b4a5-4be3-9790-1503164c900d', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('b5e971a2-b4a5-4be3-9790-1503164c900d', classification_is_normative_declaration, conventional).
narrative_ontology:cs_axiom('b5e971a2-b4a5-4be3-9790-1503164c900d', foundational, persuasive_efficacy_grounds_value).
narrative_ontology:cs_axiom_status(persuasive_efficacy_grounds_value, holdable).
narrative_ontology:cs_axiom_grounding('b5e971a2-b4a5-4be3-9790-1503164c900d', persuasive_efficacy_grounds_value, instrumental).
narrative_ontology:cs_reference_frame('b5e971a2-b4a5-4be3-9790-1503164c900d', rhetorical_vocabulary_in_active_use).
narrative_ontology:cs_drift_state('b5e971a2-b4a5-4be3-9790-1503164c900d', contemporary_machinery_accretion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5e971a2-b4a5-4be3-9790-1503164c900d', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocates).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, framework_maintainers).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_defenders).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_deliberation_audiences).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, unconsulted_mechanism_subjects).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, diagnostic_reading_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_deliberation_audiences).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, classification_as_advocacy_doctrine).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, epsilon_constructivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the classification vocabulary, author its usage rules, and enforce them through validation machinery and community gatekeeping. Their standing, careers, and institutional identity are built on the framework they administer; abandoning it would dissolve the community they constitute. They collect relevance and standing from the framework's continued persuasive success and bear the labor of holding usage norms together against internal correction.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_maintainers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, framework_maintainers, beneficiary).

% Deploy the vocabulary in campaigns against mechanisms they judge to serve illegitimate beneficiaries. A declaration costs almost nothing to make and transfers the presumption immediately: the condemned arrangement must answer a verdict that arrives wearing the form of a finding. They can adopt any rival critical vocabulary at will and routinely mix several.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocates, beneficiary,
    moderate, immediate, mobile, national).

% Operate arrangements the vocabulary condemns. Their rebuttal burden is heavier than ordinary disagreement because the label arrives with diagnostic form — answering it requires disputing a finding rather than an opinion. They cannot exit the discourse while their arrangements operate, and their resources buy rebuttal capacity, not immunity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, labeled_mechanism_defenders, payer,
    powerful, biographical, constrained, global).

% Legislators, journalists, and citizens who consume the labels in deliberation. They receive genuinely compact framing that spares them re-deriving structural analyses, and they pay attention under the impression that the labels report measured structure rather than declared judgment. They can stop attending or switch sources at will.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_deliberation_audiences, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, policy_deliberation_audiences, beneficiary).

% The people actually governed by the condemned arrangements. The declarations claim to describe their situation, but no one measures them or asks them; the label speaks for them without their voice. Their only channel of response runs through the same critical practice that bypassed them, and they hold no independent standing in the discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, unconsulted_mechanism_subjects, excluded,
    powerless, biographical, trapped, local).

% Members of the same community who hold that misclassification is correctable error and press for measurement before declaration. When the vocabulary is used declaratively, their corrections are reframed as missing the point of the practice, and the standing of measurement-based critique erodes as declaration becomes the norm. They are bound to the shared vocabulary they critique and cannot exit the discourse without abandoning the framework's kernel.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, diagnostic_reading_proponents, payer,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics_advocates).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem for dispersed critics: a shared classification vocabulary lets them identify targets, communicate structural judgments compactly, and mobilize coalitions without each critic re-deriving the analysis from scratch.
% TRANSFER_FUNCTION: Moves rhetorical authority and audience attention from the condemned arrangements and their operators to the declaring critics: each declaration converts a normative judgment into the standing of a finding, transferring the presumption to the declarer and the rebuttal burden to the declared.
% ABSENT_VOICES: The people governed by the condemned arrangements would object if present — the declarations describe their situation without ever measuring or consulting them; they are absent because the practice speaks for them rather than to them. Also absent: any seat representing the audience's interest in knowing whether a label reports measured structure or declared judgment.
% DISAPPEARANCE_RATIONALE: Critique coalitions would lose their shared vocabulary and revert to ad-hoc framing; mobilization against condemned arrangements would slow; labeled operators would regain the presumption while answering mere opinion instead of verdicts; and the measurement-based corrective practice would lose its rhetorical counterweight. The discourse would reorganize around whatever rival vocabulary absorbed the coordination function.
% FOUNDING_PROBLEM: Dispersed policy critics each re-derived structural analyses in isolation, could not coordinate targets, and saw their normative judgments about arrangements carry no common frame that audiences could act on.
% FOUNDING_PROBLEM_CORROBORATION: The coordination need is attested outside the benefiting parties: policy-studies work on framing and agenda-setting documents the coordination cost independent of this framework; the persistence of ad-hoc coalition-building among critics without the vocabulary shows the problem predates and survives the practice; and the labeled operators' own objections target the declarative form of the solution, not the existence of a shared critical vocabulary. No one outside the benefiting parties attests that declaration-without-measurement is the necessary form of the solution.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.32 as THIS reading's own assessment of the standing practice: by the reading's lights most of the practice's force is legitimate persuasion — declaring a mechanism's illegitimate beneficiaries is the vocabulary working, and the labeled operators' objections are the ordinary friction of critique. The residual the reading candidly counts is (i) declarations that misfire against the reading's own legitimacy criterion and (ii) the persuasive surplus taken where audiences overestimate the diagnostic content of a declared label; both residuals grow as the practice scales, hence the gently rising series. Suppression (0.30) is low, unscaled raw structure: rival readings and plain non-use remain fully available, matching this reading's declared delta of low suppression of alternative framings. What rises is not suppression of alternatives but internal enforcement capacity — the usage machinery (validation gates, authoring rules, coverage mandates) accretes to hold the vocabulary's authority together, which is why the suppression_requirement series is authored for this story: the narrative specifically tracks enforcement-machinery growth, not a static enforcement picture. Theater (0.55) is authored descriptively and independently of the claim: the machinery's measurement-form is performative as measurement regardless of how functional the reading judges it for persuasion — the reading would call that form functional; the metric reports what the form is. Accessibility collapse is low (0.30): understanding the practice does not foreclose alternatives. Resistance is substantial (0.55): labeled operators, the diagnostic wing, and increasingly skeptical audiences actively contest declarations. The claimed type is authored from structure, not from the metrics: genuine coordination function (a shared vocabulary solving a real collective-action problem), asymmetric transfer (authority to declarers; rebuttal burdens and attention costs on the labeled and the audiences), and active enforcement (the community must police usage and defend the framework's standing against correction). All three series share one grid {0,4,8,12,16,20,24}.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the maintainer seat the practice is the community's constituting activity — identity-locked, generational, experienced as stewardship rather than transfer. From the critic seat it is a cheap, effective instrument — mobile exit means no lock, and gains are immediate. From the labeled-operator seat the same structure arrives as pseudo-measured delegitimation: powerful enough to rebut, constrained enough that rebuttal is mandatory. From the unconsulted-subject seat the practice is representation without measurement — the deepest extraction and the least visible, borne by the powerless agents the declarations claim to speak for; their coalition potential is structurally blocked because the coalition that would object is the one being spoken for. The diagnostic-proponent seat shows same-level lateral divergence: critics and diagnostic proponents share a community and nominal standing, but the practice costs the proponents eroded standing for correction while costing the critics nothing — same power atom, differentiated by exit (mobile vs constrained) and role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put policy_critics_advocates and framework_maintainers near the beneficiary end: declaration is cheap and standing accrues, and the maintainers additionally collect institutional relevance. Victim declarations put the remaining seats toward the target end: labeled_mechanism_defenders high but damped by resources; policy_deliberation_audiences near symmetric (genuine framing benefit against attention paid under a measurement-pretense); unconsulted_mechanism_subjects nearest full-target (powerless, trapped, extracted-from representationally); diagnostic_reading_proponents high (their corrective currency is devalued by the practice's success). The engine scales effective extraction by directionality and spatial scope — the practice's aspirationally global scope amplifies verification difficulty — while suppression enters the computation unscaled as raw structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dispersed critics lacking a compact shared language for structural judgments — is live and corroborated from outside the beneficiary set, so the practice has not outlived its function and the R5 mismatch check (live status crossed with a world_rearranges verdict) flags no zombie condition. The mandatrophy risk here is erosion rather than death: the theater series shows the measurement-form accreting beyond what the candid-declaration frame requires. If the vocabulary's structural content erodes fully — labels persuading purely by form — the practice would persist as performance with diffuse costs and no capturer, flipping toward piton dynamics. Fixing is prohibitive for the seat that could fix it: the maintainers could impose measurement discipline before declaration, but that would dissolve the framework's persuasive function — which by this reading IS the framework's value — along with the community's identity investment; the cost of fixing exceeds any benefit the coalition values.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_structure,
    'This constraint is one reading of the kernel deferential_realism_ontology — what would the sibling readings (immutable_diagnostic, hybrid_pragmatic) change structurally, and where is the disagreement located?',
    'Comparative classification across the three sibling stories: if the diagnostic reading''s discovered-epsilon and this reading''s constructed-epsilon converge on the same value for the standing practice, the kernel contest is verbal; if they diverge persistently, the epistemic-status disagreement is load-bearing.',
    'Adopting the diagnostic reading converts this practice''s epsilon from constructed to discovered and reclassifies misfires as correctable error rather than residual extraction; adopting the hybrid reading partitions the typology and moves part of this practice''s surface into a separate fixed-core constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_structure, conceptual, 'Committer structure: kernel membership, sibling deltas, and the location of the epistemic-status disagreement.').

omega_variable(
    epsilon_construction_status,
    'Is epsilon for the standing practice a constructed normative judgment (this reading) or a discovered measurable quantity (the diagnostic sibling)?',
    'Impose a measurement discipline on declarations: audit declared classifications against independently measured extraction of the condemned arrangements.',
    'If epsilon proves measurable and stable across judges, this reading''s constructed-epsilon premise weakens and the practice drifts toward the diagnostic sibling''s constraint; if judges diverge systematically, construction is confirmed and this story''s values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_construction_status, conceptual, 'Whether the practice''s epsilon is authored or found — the kernel''s central contest.').

omega_variable(
    declaration_misfire_rate,
    'What fraction of declarations misfire — mechanisms condemned without serving the illegitimate beneficiaries that this reading''s own criterion requires?',
    'Audit a sample of declarations against the reading''s own legitimacy criterion, judged by parties outside the declaring coalition.',
    'A high misfire rate raises the practice''s residual extraction even by this reading''s own lights, drifting seat-level classifications toward pure-extraction; a low rate supports reading the practice as near-pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_misfire_rate, empirical, 'The misfire residual: the extraction this reading must count even on its own terms.').

omega_variable(
    persuasive_surplus_transparency,
    'Does the practice''s persuasive force operate transparently (audiences know the labels are declarations) or does it depend on audiences overestimating the labels'' diagnostic content?',
    'Audience studies measuring the epistemic status audiences believe typology labels carry, versus the status the practice''s own documentation claims.',
    'If deception-dependent, the borrowed-form surplus is extraction even by this reading''s candid lights and epsilon rises above the authored 0.32; if transparent, epsilon falls toward pure-coordination levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persuasive_surplus_transparency, empirical, 'Whether the framework''s persuasive power rides on a measurement-pretense or works in full view.').

omega_variable(
    enforcement_ratchet_function,
    'Is the accreting enforcement machinery (authoring rules, validation gates, coverage mandates) stabilizing a genuine coordination standard, or entrenching the rhetorical function against internal correction?',
    'Trajectory analysis of what triggers each machinery addition: coordination failures (vocabulary drift, category confusion) versus correction attempts (measurement-based challenges to declarations).',
    'If entrenchment, suppression_requirement keeps climbing while external alternatives stay unsuppressed, and the practice drifts toward enforced extraction despite its low suppression profile; if stabilization, the ratchet plateaus at coordination-cost levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_function, empirical, 'What the rising enforcement series is for: standard maintenance or correction-blocking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t4, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t16, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement(rhetorical_scaffold_reading_tr_t24, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t4, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 4, 0.25).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t16, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(rhetorical_scaffold_reading_be_t24, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 24, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t4, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 4, 0.16).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t8, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 8, 0.2).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t12, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 12, 0.24).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t16, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 16, 0.27).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(rhetorical_scaffold_reading_su_t24, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 24, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel 'the constraint typology as a standing deployment practice' decomposes into three reading-stories per the epsilon-invariance principle — one kernel, one shared referent (the practice), three reading-indexed epsilon values. This file authors the rhetorical reading's epsilon (0.32, constructed by the reading's own lights: legitimate persuasion minus the misfire and borrowed-form residuals). The immutable_diagnostic sibling authors epsilon for the same practice as a discovered quantity its measurement standards are violated by (higher); the hybrid sibling partitions the typology into a fixed diagnostic core and a contested rhetorical periphery. The upstream/downstream structure runs diagnostic to rhetorical: the diagnostic reading's measurement standards are the form the rhetorical practice borrows, and the rhetorical practice's success erodes those standards' authority. All family members are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
