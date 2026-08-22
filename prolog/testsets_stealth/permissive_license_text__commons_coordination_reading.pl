% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text as Frictionless Commons Grant (Commons Coordination Reading)
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   The permissive license text (MIT/BSD/Apache-style) operates as a
 *   standardized, zero-negotiation permission grant: any party may use,
 *   modify, embed, redistribute, and commercialize the covered code subject
 *   only to preserving the copyright notice. This story instantiates the
 *   commons_coordination_reading of the kernel permissive_license_text: the
 *   arrangement is claimed as a pure coordination rope that maximizes
 *   universal implementation freedom by minimizing legal friction. The
 *   epsilon referent is the standing arrangement — permissive licensing as it
 *   actually operates — assessed by this reading's own lights; the sibling
 *   readings author different epsilon over the same referent and live in
 *   separate constraint files linked via network. The claim (rope) and the
 *   metrics (low extraction, negligible suppression) were authored
 *   independently and happen to agree; the structural data — every seated
 *   party benefits, exits are mobile, no trapped class, no victim set — is
 *   what the engine reads, not the claim. KEY AGENTS (by structural
 *   relationship): - upstream_permissive_authors: Grantor and agenda-setter
 *   (moderate/mobile) — attach the standard text, collect adoption and
 *   attribution, voluntarily forgo exclusivity - universal_implementer_pool:
 *   Primary beneficiary (moderate/mobile) — the diffuse global pool whose
 *   frictionless reuse is the constraint's product -
 *   proprietary_downstream_vendors: Secondary beneficiary
 *   (institutional/mobile) — build closed products on the commons with no
 *   reciprocity - copyleft_advocates: Excluded objector (organized/mobile) —
 *   hold no seat in the arrangement; dispute the no-reciprocity structure
 *   from the GPL ecosystem - open_source_license_stewards: Agenda steward
 *   (institutional/analytical) — maintain the canonical texts and their
 *   interpretation - open_source_governance_researchers: Analytical observer
 *   (institutional/analytical) — measure adoption, contribution flows, and
 *   ecosystem health
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.04).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text as Frictionless Commons Grant (Commons Coordination Reading)").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "economic/technological/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, 'dfad7268-9179-418a-9725-466eab58756d').
narrative_ontology:cs_kernel_codification('dfad7268-9179-418a-9725-466eab58756d', fixed_text).
narrative_ontology:cs_authority_grounding('dfad7268-9179-418a-9725-466eab58756d', lineage).
narrative_ontology:cs_interpretation_layer_present('dfad7268-9179-418a-9725-466eab58756d').
narrative_ontology:cs_reading_relation('dfad7268-9179-418a-9725-466eab58756d', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfad7268-9179-418a-9725-466eab58756d', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('dfad7268-9179-418a-9725-466eab58756d', foundational, frictionless_reuse_maximizes_implementation).
narrative_ontology:cs_axiom_status(frictionless_reuse_maximizes_implementation, holdable).
narrative_ontology:cs_axiom_grounding('dfad7268-9179-418a-9725-466eab58756d', frictionless_reuse_maximizes_implementation, instrumental).
narrative_ontology:cs_axiom('dfad7268-9179-418a-9725-466eab58756d', foundational, freedom_includes_commercial_derivation).
narrative_ontology:cs_axiom_status(freedom_includes_commercial_derivation, holdable).
narrative_ontology:cs_axiom_grounding('dfad7268-9179-418a-9725-466eab58756d', freedom_includes_commercial_derivation, deontological).
narrative_ontology:cs_reference_frame('dfad7268-9179-418a-9725-466eab58756d', frictionless_commons_grant).
narrative_ontology:cs_drift_state('dfad7268-9179-418a-9725-466eab58756d', contemporary_cloud_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('dfad7268-9179-418a-9725-466eab58756d', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, upstream_permissive_authors).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, proprietary_downstream_vendors).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, frictionless_grant_adoption_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish code under a standard permissive text of their choosing. They set the terms once, at upload, and thereafter collect what flows back: adoption of their work, attribution in derivative notices, bug reports and occasional patches, and reputation. What they give up is exclusivity — anyone, including a direct commercial competitor, may build on the code with no payment and no obligation to share improvements. Where they hold the copyright they can relicense later, so the commitment is reversible.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, upstream_permissive_authors, agenda_setter,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, upstream_permissive_authors, beneficiary).

% Individual developers, small teams, and startups who take permissively licensed components, modify them, embed them in larger systems, and ship the result without contacting the author, negotiating terms, or paying. The license text is the entire transaction. If the terms ever stopped suiting them they could switch to copyleft or proprietary components or write their own — the pool routinely mixes all of these.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    moderate, biographical, mobile, global).

% Corporations that incorporate permissively licensed components into closed, revenue-generating products. They satisfy the notice-preservation requirement, contribute nothing back as a rule, and face no fee or reciprocity demand. Their legal departments prefer this text precisely because its obligations are minimal and settled, and they retain full freedom to make their derivatives proprietary.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, proprietary_downstream_vendors, beneficiary,
    institutional, generational, mobile, global).

% Foundations, projects, and activists in the GPL tradition who hold that permission without reciprocity lets firms drain the shared code commons. They run a parallel copyleft ecosystem and publish their critique, but hold no seat in the permissive arrangement itself: the license text was fixed before their objection and each upstream author adopts it unilaterally. Their exit is real — they build and govern their own reciprocal commons — so they object from outside rather than from inside.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_advocates, excluded,
    organized, generational, mobile, global).

% Maintainers of the canonical license texts and the approval bodies that certify licenses as open source. They rarely change the texts — stability is part of the value — and their role is custodial: keeping the standard text available, unambiguous, and interpretable when disputes arise. They collect nothing and pay nothing; their interest is the arrangement's continued smooth operation.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_license_stewards, agenda_setter,
    institutional, generational, analytical, global).

% Academic and industry analysts who measure license adoption, contribution flows, and ecosystem health. They observe the arrangement from outside, publish on which license terms correlate with which outcomes, and supply the longitudinal data any dispute about the arrangement's effects would draw on.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_governance_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, upstream_permissive_authors).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mass-permission problem: how to grant every party unconditional rights to use, modify, embed, redistribute, and commercialize a body of code without case-by-case negotiation. The standardized text makes the permission grant self-executing — any implementer can proceed on reading the notice, with no contact, no fee, and no infringement exposure — coordinating a global commons of reusable code at near-zero transaction cost.
% TRANSFER_FUNCTION: Moves implementation rights (use, modify, redistribute, commercialize) from upstream authors to the universal implementer pool and its proprietary subset, in exchange for preserving attribution notices. Nothing else moves: no money, no code back, no obligation to contribute. Under this reading what flows is freedom itself, not value siphoned from anyone.
% ABSENT_VOICES: Copyleft advocates would contest both the no-reciprocity structure and the freedom-maximizing framing; they are vocal in the broader licensing discourse but hold no seat inside the permissive arrangement — no upstream author must answer them to adopt the text. End users of closed derivatives built on permissive code are also absent: they consume the outputs without ever encountering the licensing choice that shaped what they received.
% DISAPPEARANCE_RATIONALE: If every permissive grant vanished overnight, the modern software supply chain would seize: thousands of shipping products embed MIT/BSD/Apache components whose legal basis would evaporate. Ecosystems would reorganize around copyleft and proprietary licensing, renegotiation costs would be enormous, and the implementer pool would shrink to those able to negotiate terms case by case — the exact opposite of what the arrangement exists to do.
% FOUNDING_PROBLEM: Copyright's default — all rights reserved — made sharing source code legally hazardous: every reuse required bespoke permission, and well-meaning sharing was infringement. The permissive texts (BSD in the early 1980s, MIT shortly after) were drafted to solve exactly this: a short, standardized grant that removes the legal friction of the default for all comers.
% FOUNDING_PROBLEM_CORROBORATION: The copyleft tradition corroborates the founding problem from outside this reading's beneficiary set: the GPL's own preamble is premised on the same default-friction problem even while prescribing the opposite remedy. Copyright-law scholarship and the historical record of 1980s software-sharing disputes corroborate it independently. No party disputes that the default-friction problem existed; the readings dispute the remedy, not the problem.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.08, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.08) because the grant's sole string — notice preservation — costs implementers effectively nothing, and the license imposes no fee, no reciprocity, no field-of-use limit. Suppression is near-floor (0.04): nothing is coerced; any party may decline the grant and use proprietary, copyleft, or self-written code, and all those alternatives demonstrably thrive, which is why accessibility_collapse is low (0.22) rather than mountain-grade. Theater is minimal (0.04): the license text performs its entire function — granting rights — on every use; there is no enforcement apparatus whose activity could be mostly performance. Resistance is low (0.12): the copyleft camp disputes the arrangement's wisdom but mounts no resistance to the text itself, which costs them nothing to ignore. The two temporal series run on one shared grid (T=0..40 at 8-year steps, roughly 1985–2025); suppression_requirement is deliberately not authored as a series because the enforcement picture is static — a latent copyright backstop, almost never invoked — which the scalar suppression value already carries. Suppression is authored as the raw structural property; only extractiveness is scaled by the engine (by directionality and scope), and here the scope amplification operates on a near-zero base.
 *
 * PERSPECTIVAL GAP:
 *   From the implementer seat the arrangement computes as costless freedom — pure coordination. From the upstream author seat it is coordination plus a trivial receipt (attribution). From the excluded copyleft seat the same structure is contested terrain — but that contest is the sibling readings' subject matter, not this constraint's; Rule 1 keeps the contest out of this file and routes it to omegas. The structural data (all seated parties benefit, mobile exits, no victim set) computes rope from every seated position; divergence between this verdict and the siblings' computed verdicts is the cross-reading signal the constraint family exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party is a beneficiary, so derived directionality sits near the beneficiary end for all of them: implementers and vendors receive the grant; upstream authors both set the terms and receive the arrangement's only extraction (the attribution value of notice preservation), which is why gain_flow names that seat. The copyleft advocates are excluded rather than victimized — they bear none of the arrangement's costs and collect none of its benefits — so no directionality override is needed and no victim set is declared. The derivation chain produces accurate d values from the beneficiary declarations alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — copyright's all-rights-reserved default making code sharing legally hazardous — remains live: the legal default has not changed, so the license's friction-removal function has not atrophied and there is nothing to sunset. Rope (not scaffold) is the correct steady-state claim: this is not a transitional arrangement awaiting completion. The classification also does mislabel-prevention work across the kernel: the corporate moat reading would classify the same arrangement as extraction-enabling and the copyleft reading as commons-draining; this story's structural data — no trapped party, no suppressed alternative, cheap exit via relicensing, cheap fixing for any upstream author who wants different terms — is what the engine reads to test those claims against the arrangement as this reading sees it. Because the founding problem is corroborated from outside the beneficiary set (the GPL preamble premises itself on the same default-friction problem), the arrangement's persistence cannot be dismissed as a cover story for a dead mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the permissive_license_text kernel — the commons_coordination_reading. Do the sibling readings (corporate_moat_reading, copyleft_counterfactual_reading) address the same standing arrangement, and where exactly does the disagreement between readings bite?',
    'Compare each sibling story''s authored epsilon and victim set over the identical referent (the permissive license text as it actually operates). The disagreement is located in a single structural element: whether uncompensated commercial reuse counts as the freedom the grant confers (this reading), as uncompensated extraction (corporate_moat_reading), or as structural exploitation requiring reciprocity (copyleft_counterfactual_reading).',
    'If the siblings author high epsilon with victim sets over the same referent, the kernel is genuinely contested and cross-reading classification divergence is the expected signal. If the siblings are actually describing different arrangements (specific corporate behaviors rather than the license text itself), the family decomposes further and this story''s links are redrawn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: this story instantiates one reading of a three-reading kernel; siblings author different epsilon over the same referent.').

omega_variable(
    reciprocity_necessity_dispute,
    'Does a no-reciprocity commons sustain long-term contribution, or does permission without reciprocity structurally drain the commons the way the copyleft reading predicts?',
    'Longitudinal ecosystem data: contribution rates, maintenance-burden concentration, and the firm-versus-individual contribution mix across permissive-dominant versus copyleft-dominant ecosystems over decades.',
    'Sustained contribution supports this reading''s low epsilon as genuine coordination cost. Contribution decay would reveal the low epsilon as a subsidy flowing to non-reciprocating firms, strengthen the copyleft sibling''s classification of the same arrangement, and shift the founding-problem status toward contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_dispute, empirical, 'Whether the no-reciprocity structure is sustainable commons coordination or slow-motion commons depletion.').

omega_variable(
    attribution_compliance_rate,
    'How reliably is the license''s sole obligation — preserving the copyright notice and license text in derivative distributions — actually honored?',
    'Compliance audits of redistributed binaries and commercial products for embedded license notices, together with measured non-compliance rates and enforcement actions.',
    'Widespread non-compliance would raise the theater ratio and suggest the notice obligation functions as ritual rather than constraint; near-universal compliance confirms the grant''s single string is real and cheap, keeping the arrangement on the rope side of the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_compliance_rate, empirical, 'Whether the license''s one obligation is honored in practice or is decorative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__commons_coordination_reading, theater_ratio, 8, 0.02).
narrative_ontology:measurement_basis(perm_tr_t8, observed).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__commons_coordination_reading, theater_ratio, 16, 0.03).
narrative_ontology:measurement_basis(perm_tr_t16, observed).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__commons_coordination_reading, theater_ratio, 24, 0.03).
narrative_ontology:measurement_basis(perm_tr_t24, observed).
narrative_ontology:measurement(perm_tr_t32, permissive_license_text__commons_coordination_reading, theater_ratio, 32, 0.04).
narrative_ontology:measurement_basis(perm_tr_t32, observed).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__commons_coordination_reading, theater_ratio, 40, 0.04).
narrative_ontology:measurement_basis(perm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__commons_coordination_reading, base_extractiveness, 8, 0.05).
narrative_ontology:measurement_basis(perm_be_t8, observed).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__commons_coordination_reading, base_extractiveness, 16, 0.06).
narrative_ontology:measurement_basis(perm_be_t16, observed).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__commons_coordination_reading, base_extractiveness, 24, 0.07).
narrative_ontology:measurement_basis(perm_be_t24, observed).
narrative_ontology:measurement(perm_be_t32, permissive_license_text__commons_coordination_reading, base_extractiveness, 32, 0.07).
narrative_ontology:measurement_basis(perm_be_t32, observed).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__commons_coordination_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(perm_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__commons_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'permissive license' decomposes into one kernel with three readings, all authored over the same referent — the permissive license text as a standing legal arrangement — with reading-indexed epsilon values (OQ-26). This story instantiates the commons_coordination_reading: the same uncompensated commercial reuse the siblings count as harm is, under this reading, the freedom the grant was made to confer, so epsilon stays near the coordination floor (0.08) and no victim set is declared. The corporate_moat_reading authors high epsilon over the identical referent (uncompensated proprietary derivation as extraction from upstream authors); the copyleft_counterfactual_reading treats the no-reciprocity structure itself as the harm and prescribes viral reciprocity. The commons reading structurally INFLUENCES the copyleft sibling — the permissive commons' growth changes the resource environment (share of reciprocally-licensed code, contribution flows) in which the copyleft claim is evaluated — without logically ruling it out; it merely COEXISTS with the corporate moat sibling. Each reading is a separate constraint file; this family's cross-reading classification divergence is the measurement the decomposition exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
