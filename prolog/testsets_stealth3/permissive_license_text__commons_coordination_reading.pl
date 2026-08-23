% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text — Commons Coordination Reading
 *   domain: technological/intellectual_property/governance
 *
 * SUMMARY:
 *   A short standard grant text (of the MIT/BSD/Apache family) lets anyone
 *   copy, modify, redistribute, and commercially embed published source code
 *   subject only to preserving a copyright-and-permission notice. Adoption is
 *   voluntary, enforcement is nearly absent (disputes are rare and usually
 *   settle quietly), and the pool of reusable components has become the
 *   default substrate of modern software construction. This story authors the
 *   arrangement as the commons_coordination_reading of the
 *   permissive_license_text kernel sees it: an unconditional grant instrument
 *   whose principal effect is removing per-artifact legal clearance from
 *   software reuse. The ε referent is the standing permissive-licensing
 *   arrangement as this reading assesses it — not any sibling reading's
 *   endorsed alternative.
 *
 * KEY AGENTS:
 *   - upstream_project_maintainers: agenda setter (moderate/mobile) — grant rights under the standard text, curate contributions, control relicensing
 *   - commercial_integrators: primary beneficiary (powerful/arbitrage) — absorb components into proprietary products at near-zero standing obligation
 *   - individual_open_source_developers: beneficiary (moderate/mobile) — reuse, study, and contribute to the pool
 *   - academic_and_public_sector_users: beneficiary (institutional/constrained) — embed components under procurement and legacy inertia
 *   - copyleft_license_projects: excluded (organized/identity_locked) — run the reciprocal alternative outside the pool
 *   - open_source_initiative: analytical observer (institutional/analytical) — reviews grant texts against the Open Source Definition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.1).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.05).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text — Commons Coordination Reading").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "technological/intellectual_property/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '301692b3-a388-496a-9e1f-ab1382111a87').
narrative_ontology:cs_kernel_codification('301692b3-a388-496a-9e1f-ab1382111a87', formalized).
narrative_ontology:cs_authority_grounding('301692b3-a388-496a-9e1f-ab1382111a87', self_enforcing).
narrative_ontology:cs_reading_relation('301692b3-a388-496a-9e1f-ab1382111a87', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('301692b3-a388-496a-9e1f-ab1382111a87', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('301692b3-a388-496a-9e1f-ab1382111a87', foundational, friction_minimization_maximizes_implementation_freedom).
narrative_ontology:cs_axiom_status(friction_minimization_maximizes_implementation_freedom, holdable).
narrative_ontology:cs_axiom_grounding('301692b3-a388-496a-9e1f-ab1382111a87', friction_minimization_maximizes_implementation_freedom, instrumental).
narrative_ontology:cs_axiom('301692b3-a388-496a-9e1f-ab1382111a87', secondary, unconditional_grant_sustains_universal_pool).
narrative_ontology:cs_axiom_status(unconditional_grant_sustains_universal_pool, holdable).
narrative_ontology:cs_axiom_grounding('301692b3-a388-496a-9e1f-ab1382111a87', unconditional_grant_sustains_universal_pool, empirically_contingent).
narrative_ontology:cs_reference_frame('301692b3-a388-496a-9e1f-ab1382111a87', unconditional_grant_commons_baseline).
narrative_ontology:cs_drift_state('301692b3-a388-496a-9e1f-ab1382111a87', contemporary_server_delivery_and_training_ingestion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('301692b3-a388-496a-9e1f-ab1382111a87', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, individual_open_source_developers).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, academic_and_public_sector_users).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, frictionless_redistribution_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and publish source code under a short standard grant text allowing anyone to use, modify, and redistribute it, keeping only a copyright-and-permission notice. They accept outside contributions, decide when to relicense or dual-license future versions, and answer occasional compatibility questions. Leaving the arrangement is inexpensive: they can relicense later versions, retire the project, or adopt a different grant text.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, upstream_project_maintainers, agenda_setter,
    moderate, biographical, mobile, global).

% Build and ship commercial products containing components published under the short grant text. They owe only preservation of attribution notices, may keep their own additions proprietary, and can swap components, obtain bespoke terms elsewhere, or fund internal replacements if the pool stops suiting them. Value flows in as zero-cost building blocks; obligations flowing out are limited to notice retention.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, commercial_integrators, beneficiary,
    powerful, generational, arbitrage, global).

% Reuse published components freely in personal and community projects, study production-quality code, and contribute fixes upstream. Nothing blocks them from starting their own projects or switching stacks; their main cost is keeping up with component churn.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, individual_open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Universities, labs, and agencies embed published components in research tools and internal systems. Procurement rules and legacy-system inertia slow their adoption and replacement cycles even though the code carries no fee, so they move more slowly than startups but face no legal barrier to using or replacing the components.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, academic_and_public_sector_users, beneficiary,
    institutional, generational, constrained, global).

% Publish code under reciprocal terms that require downstream distributions to carry the same terms, operating outside the unconditional pool. They participate in public licensing debates and share tooling with the wider ecosystem; their code is not part of this arrangement's redistribution surface, and their positions are voiced in forums this arrangement does not govern. Their commitment to reciprocity is constitutive of their projects' identity, making movement into the pool costly in self-conception rather than in logistics.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_license_projects, excluded,
    organized, generational, identity_locked, global).

% Reviews proposed grant texts against a published definition, maintains the approved list, and publishes commentary on ecosystem trends. It sets no terms for any project and collects nothing from the arrangement; its seat is observational, with standing to flag texts that fail its definition.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_initiative, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, pre-granted permission set attached to published source code, so any implementer can copy, modify, redistribute, and embed it without negotiating rights case by case; solves the per-artifact legal-clearance problem in collaborative software once, in the text itself.
% TRANSFER_FUNCTION: Moves irrevocable usage, modification, and redistribution rights from upstream copyright holders to an unrestricted implementer pool; moves contribution labor and attribution acknowledgment back toward upstream maintainers; money moves only insofar as integrators convert pooled components into product value.
% ABSENT_VOICES: Copyleft projects and critics who account uncompensated proprietary absorption as a loss are not parties to this arrangement's terms — they run parallel reciprocal pools and register objections in venues this arrangement does not govern; unpaid maintainers who perceive their upkeep as subsidizing commercial integrators likewise raise the point outside this reading's seat structure.
% DISAPPEARANCE_RATIONALE: If every short-form grant text vanished overnight, integration of existing components would require bilateral negotiation per artifact, product timelines would stretch on legal review, research and education stacks would fragment, and the accumulated pool of interoperable components would freeze — the software economy would reorganize around bespoke licensing or retreat to in-house code.
% FOUNDING_PROBLEM: Late-1970s and 1980s code exchange was choked by per-artifact legal negotiation: academic software sharing required bespoke written permissions, and commercial licenses fragmented research computing into incompatible islands. Short-form grant texts were built to eliminate that clearance friction so code could circulate without negotiation.
% FOUNDING_PROBLEM_CORROBORATION: Software-law scholarship documenting pre-open-source licensing overhead, corporate procurement records showing per-deal clearance costs for non-open code, and court dockets from early software licensing disputes all attest the founding problem from outside the pool's beneficiary set; the persistence of negotiated licensing costs today corroborates that the problem class remains live.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.10) because the only standing obligations are attribution-notice retention and occasional compatibility diligence — costs decoupled from the value transferred, and declining over the interval as texts shortened (the four-clause BSD advertising clause, historically the largest friction term, was retired in 1999). Suppression (0.05) is authored as a raw structural property, unscaled by any context dimension: adoption is voluntary, no participant is barred from leaving, and rival instruments (reciprocal licenses, proprietary terms, dual licensing) are not suppressed — they compete alongside; only extractiveness is scaled, engine-side, by directionality and scope. Theater (0.12) reflects that notice preservation is functional provenance rather than ritual, with a slow rise from automated compliance ceremony. Accessibility collapse (0.32) is moderate-low: understanding the arrangement collapses no alternative, since authors remain fully free to publish under reciprocal or proprietary terms; the residual reflects the pool's network gravity raising the deliberation cost of stepping outside. Resistance (0.28) captures ideological dissent from the reciprocity camp and corporate caution about publishing source — neither mounts a campaign against the text itself. The claimed type (rope) is asserted from this reading's structural assessment; the metrics are authored independently as descriptive judgments, and the engine computes per-seat classifications from the structural data. fixing_cost is 'cheap': any maintainer can relicense subsequent versions, and observed relicensing waves confirm the cost class relative to the benefit of changing terms.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical text. A commercial integrator sits near the full-subsidy end: it receives unrestricted rights and holds arbitrage-grade exit (fork, replace, renegotiate elsewhere). An upstream maintainer sits near symmetric: it grants rights outward but receives contribution labor, defect reports, and attribution credit inward. Academic seats experience mild procedural friction (procurement rules, legacy audits) despite zero fees. The copyleft projects' seat evaluates a different instantiation of the same kernel text altogether; its classification belongs to the sibling story, not this file. The engine derives these divergences from power, exit, and role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d downward for all three implementer classes; no victim group is declared, so no seat derives toward the target pole. Upstream maintainers carry a small positive component from the attribution duty they impose, offset by the contribution labor they receive — net near-symmetric. Scope is global, which the engine scales modestly upward for verification difficulty, but with ε near the information-standard floor the amplification is marginal. Residual extraction (notice-retention effort) converts to diffuse reputational signal spread across thousands of upstream projects; no named seat captures it, hence gain_flow 'diffuse'. That is an affirmative check across all seven seats, not a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — per-artifact legal clearance choking code exchange — remains live in attenuated forms (patent uncertainty, server-delivered functionality, model-training ingestion), so no mandatrophy resolution is declared. The watch item is theater drift: if compliance automation (license scanning, attestation pipelines) becomes ceremonial while the text's clearance function atrophies under consumption patterns the grant text never anticipated, theater_ratio rises and inertial persistence becomes possible. Current measurements show a slow, bounded rise (0.06 to 0.12) with the underlying function intact. Mandatrophy prevention here cuts both ways: reading this arrangement as pure gift avoids mislabeling genuine coordination as hidden extraction, while the omegas keep the possibility of an unseen payer seat open rather than letting the benign framing close the question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'Does the no-victim, low-friction profile authored here hold for the permissive_license_text kernel as THIS reading frames it, or does it depend on seat selection that sibling readings (corporate_moat_reading, copyleft_counterfactual_reading) would reject?',
    'Compile the two sibling stories and compare computed per-seat classifications and victim-set determinations across the family; divergence localized to victim identification indicates the disagreement is empirical rather than definitional.',
    'If sibling seats systematically identify payers this reading cannot see, the classification here is seat-relative and the family resolves toward asymmetric types for the contested seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Committer-frame routing: this constraint is one reading of kernel permissive_license_text; siblings instantiate different victim-set determinations of the same text.').

omega_variable(
    invisible_payer_seat,
    'Do unpaid maintainer burnout, uncompensated proprietary absorption, or non-redistributing consumption (server-side delivery, model-training ingestion) constitute a payer seat that this reading''s beneficiary-centered structure renders invisible?',
    'Longitudinal maintainer attrition and funding-flow studies correlated with commercial consumption intensity of maintained components.',
    'A demonstrated systematic payer seat converts the arrangement into a hybrid with coordinated beneficiaries and identified payers; demonstrated absence supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invisible_payer_seat, empirical, 'Whether a payer set exists that the commons reading''s seat cannot observe.').

omega_variable(
    friction_causality_warrant,
    'Is maximal implementation freedom actually caused by friction minimization, or would the pool tolerate (or benefit from) reciprocity conditions without reducing total freedom?',
    'Matched-domain comparison of ecosystem growth, contribution inflow, and reuse breadth across permissive and reciprocal pools.',
    'If reciprocity does not reduce total implementation freedom, the foundational instrumental axiom loses warrant and the reading''s drift toward axiom_overriding accelerates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_causality_warrant, empirical, 'Empirical warrant for the reading''s foundational means-ends axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__commons_coordination_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(perm_tr_t8, observed).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__commons_coordination_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(perm_tr_t16, observed).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__commons_coordination_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement_basis(perm_tr_t24, observed).
narrative_ontology:measurement(perm_tr_t32, permissive_license_text__commons_coordination_reading, theater_ratio, 32, 0.11).
narrative_ontology:measurement_basis(perm_tr_t32, observed).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__commons_coordination_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(perm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__commons_coordination_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement_basis(perm_be_t8, observed).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__commons_coordination_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement_basis(perm_be_t16, observed).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__commons_coordination_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement_basis(perm_be_t24, observed).
narrative_ontology:measurement(perm_be_t32, permissive_license_text__commons_coordination_reading, base_extractiveness, 32, 0.11).
narrative_ontology:measurement_basis(perm_be_t32, observed).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__commons_coordination_reading, base_extractiveness, 40, 0.1).
narrative_ontology:measurement_basis(perm_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__commons_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'permissive licensing' decomposes under ε-invariance into three structurally distinct readings of one kernel text: this file (commons coordination, low ε, no declared victim set), permissive_license_text__corporate_moat_reading (identifies uncompensated absorption with payer seats), and permissive_license_text__copyleft_counterfactual_reading (evaluates the necessity of reciprocity). Each carries its own ε, beneficiary structure, and classification; family linkage runs through affects_constraints. Ordering: the moat reading's absorption observation supplies the copyle ft counterfactual's evidentiary premise, while the commons reading stands as the baseline against which both contest the no-victim premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
