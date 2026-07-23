% ============================================================================
% CONSTRAINT STORY: mechanism_defensibility_burden
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mechanism_defensibility_burden, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mechanism_defensibility_burden
 *   human_readable: In-Text Defensibility Requirement for All 27 Cited Mechanisms
 *   domain: governance/institutional_economics/distributed_ledger_design
 *
 * SUMMARY:
 *   A single-authored reference-design paper on distributed-ledger governance
 *   mechanisms draws its 27 mechanisms from a larger unpublished doctrinal
 *   corpus. Because the paper's own rule (§1, §8, and the adoption clause)
 *   forbids citing anything unpublished, every mechanism that crosses the
 *   quarantine boundary into the paper must arrive with its own complete
 *   in-text argumentative defense — it cannot lean on the unpublished corpus
 *   for support, even where the corpus contains a fuller justification. This
 *   constraint (the 'defensibility burden') is structurally downstream of,
 *   but distinct from, the quarantine boundary itself: the boundary decides
 *   WHAT crosses; the burden decides what happens to everything that does
 *   cross, regardless of why the boundary sits where it does. All four
 *   sibling readings of the paper_ready_boundary kernel (citation_purity,
 *   ip_provenance, operational_security, design_philosophy) agree that once
 *   material is admitted, it must be self-defending — they disagree only
 *   about why material is admitted or excluded in the first place. This story
 *   measures the resulting load: 27 of 27 mechanisms carry an explicit
 *   defense-core field, and the paper's length and argument density are
 *   shaped by that universal requirement.
 *
 * KEY AGENTS:
 *   - accountable_member_author: sole author bound by the no-unpublished-citation rule; bears the full drafting cost of making every mechanism self-defending
 *   - paper_readers: benefit from a self-contained argument they can evaluate without trusting an unseen archive
 *   - future_replicators: benefit because a self-defended mechanism can be adopted or critiqued without needing access to the corpus
 *   - unpublished_corpus_community: analytical bystander — the community whose doctrine is quarantined, not a payer or beneficiary of THIS specific burden, though implicated in why the boundary exists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mechanism_defensibility_burden, 0.18).
domain_priors:suppression_score(mechanism_defensibility_burden, 0.12).
domain_priors:theater_ratio(mechanism_defensibility_burden, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mechanism_defensibility_burden, extractiveness, 0.18).
narrative_ontology:constraint_metric(mechanism_defensibility_burden, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(mechanism_defensibility_burden, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mechanism_defensibility_burden, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(mechanism_defensibility_burden, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mechanism_defensibility_burden, rope).
narrative_ontology:human_readable(mechanism_defensibility_burden, "In-Text Defensibility Requirement for All 27 Cited Mechanisms").
narrative_ontology:topic_domain(mechanism_defensibility_burden, "governance/institutional_economics/distributed_ledger_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mechanism_defensibility_burden, 'accf335a-d8c8-48f1-a65e-291b9c537f85').
narrative_ontology:cs_kernel_codification('accf335a-d8c8-48f1-a65e-291b9c537f85', formalized).
narrative_ontology:cs_authority_grounding('accf335a-d8c8-48f1-a65e-291b9c537f85', practice).
narrative_ontology:cs_created_at('accf335a-d8c8-48f1-a65e-291b9c537f85', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mechanism_defensibility_burden, paper_readers).
narrative_ontology:constraint_beneficiary(mechanism_defensibility_burden, accountable_member_author).
narrative_ontology:constraint_beneficiary(mechanism_defensibility_burden, future_replicators).
narrative_ontology:constraint_vindicates(mechanism_defensibility_burden, self_contained_argument_standard).
narrative_ontology:constraint_vindicates(mechanism_defensibility_burden, no_unpublished_citation_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote and adopted the no-unpublished-citation rule and now must satisfy it for every one of 27 mechanisms drawn from a private doctrinal corpus. Bears the drafting cost of constructing a complete defense core for each mechanism in-text, but also gains a stronger, more citable, self-contained paper as a result. Could revise the rule in a future work; nothing traps them into it beyond their own declared method.
narrative_ontology:constraint_stakeholder(mechanism_defensibility_burden, accountable_member_author, agenda_setter,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(mechanism_defensibility_burden, accountable_member_author, beneficiary).

% Read the paper without needing access to the unpublished corpus. Because every mechanism carries its own defense, they can evaluate, critique, or adopt each mechanism on the paper's own terms. Pay no cost for this benefit; their only 'exit' would be declining to read a paper they find unpersuasive, which is not constrained by anything this rule does.
narrative_ontology:constraint_stakeholder(mechanism_defensibility_burden, paper_readers, beneficiary,
    moderate, biographical, mobile, global).

% Researchers or implementers who might later build on or adopt one of the 27 mechanisms. Because each mechanism is self-defended, they can assess it without needing the corpus, lineage, or the author's continued availability. Fully free to ignore the paper if a mechanism's defense does not convince them.
narrative_ontology:constraint_stakeholder(mechanism_defensibility_burden, future_replicators, beneficiary,
    moderate, generational, mobile, global).

% The doctrinal community from which the 27 mechanisms were drawn. Not directly affected by the defensibility burden itself (that burden concerns only what is already admitted), though implicated in the separate, contested question of where the quarantine boundary is drawn and why. Watches from outside the paper's argument structure.
narrative_ontology:constraint_stakeholder(mechanism_defensibility_burden, unpublished_corpus_community, observer,
    organized, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mechanism_defensibility_burden, diffuse).
narrative_ontology:fixing_cost_class(mechanism_defensibility_burden, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to write a paper that cites nothing unpublished without becoming a set of unsupported assertions: by requiring every admitted mechanism to carry its own complete in-text defense, the paper remains evaluable by a stranger with no access to the author's private archive.
% TRANSFER_FUNCTION: Moves argumentative labor from the unpublished corpus (where a fuller justification may already exist) into the paper's own text, at the author's drafting cost, so that no reader ever needs to trust an unseen source to follow the argument.
% ABSENT_VOICES: The unpublished corpus community has no voice in how the defensibility burden is satisfied — this is a rule about the paper's construction, not about the corpus's disclosure — so their absence here is not the same absence that matters for the separate quarantine-boundary kernel.
% DISAPPEARANCE_RATIONALE: If the defensibility burden vanished (i.e., if the author were permitted to cite the unpublished corpus directly), the paper's readers and future replicators would lose the ability to independently evaluate each mechanism without trusting the author's private archive, but no party currently depends on the burden's continued enforcement for income, standing, or survival — removing it would degrade the paper's rigor rather than rearrange anyone's material arrangements.
% FOUNDING_PROBLEM: The author needed a way to publish 27 mechanisms drawn from a larger private doctrinal corpus without citing that corpus, since the corpus itself is unpublished and, under at least one kernel reading, its exposure would be undesirable or premature.
% FOUNDING_PROBLEM_CORROBORATION: The requirement is self-imposed and self-attested by the author in §1 and §8 of the paper itself; no external body was asked to corroborate it. This story states plainly that no corroboration from outside the author's own declared method exists — the founding problem's liveness rests entirely on the paper's own text, which is why the omega on Goodharting (defense-core presence vs. quality) remains open rather than resolved.
narrative_ontology:disappearance_verdict(mechanism_defensibility_burden, world_unchanged).
narrative_ontology:founding_problem_status(mechanism_defensibility_burden, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mechanism_defensibility_burden, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(mechanism_defensibility_burden, 'none', 1).
narrative_ontology:epsilon_provenance(mechanism_defensibility_burden, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mechanism_defensibility_burden_tests).
:- end_tests(mechanism_defensibility_burden_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) and roughly flat across the interval: the burden imposes real drafting cost on the author but transfers nothing extracted from a victim — it is a self-imposed discipline, not a toll collected from a captive party. Suppression is low (0.12): the author chose the no-unpublished-citation rule and could in principle revise it; nothing traps the author into defensibility except the paper's own declared standard. Theater ratio is low and only slightly rising (0.05 to 0.08) because the 27/27 ratio is a binary presence measure, not a quality measure — the omega on Goodharting captures the residual risk that presence could decouple from substance, but nothing in the current record supports that it has. Accessibility collapse is moderate (0.35): once a reader understands the rule, the alternative of 'just cite the corpus' is foreclosed by the paper's own declared method, but the author retains the alternative of not writing the paper, or revising the method in a future paper. Resistance is low-moderate (0.22): the burden meets friction only in the ordinary sense that writing complete defenses is harder than citing a source, not in the sense of anyone actively opposing the requirement.
 *
 * PERSPECTIVAL GAP:
 *   From the author's seat, the burden is heavy but chosen — a cost accepted in service of a self-contained paper. From the reader's seat, the same requirement is pure benefit: they get an argument they can evaluate without needing corpus access. There is no seat from which the burden operates as extraction, because there is no party who is coerced into bearing a cost they did not choose and does not benefit from. This is what marks it as rope rather than tangled_rope: the coordination function (self-contained, evaluable argument) is real and there is no identifiable victim paying an asymmetric price for someone else's extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The accountable member author is simultaneously the one who bears the drafting cost and a beneficiary (the resulting paper is stronger, more citable, and more defensible against challenge because it never leans on unstated authority). Readers and future replicators are pure beneficiaries with no cost — they receive a completed, self-defending argument at zero cost to themselves. There is no victim group: the corpus community is affected by the quarantine boundary's existence (a separate constraint) but not by the defensibility burden itself, which only concerns what happens to material AFTER it is admitted.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope (not tangled_rope or snare) prevents mislabeling a self-imposed rigor standard as extraction. The founding problem — how to write a paper that cites nothing unpublished without becoming an unsupported assertion — remains fully live: as long as the no-unpublished-citation rule holds, every admitted mechanism must still carry its own defense. There is no drift toward inertial theater visible in the measurements; the ratio has stayed near 27/27 by design, and the slight rise in theater_ratio is flagged as an open question rather than a resolved verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quarantine_purpose_ambiguity,
    'Is the no-unpublished-citation rule (and the resulting defensibility burden on all 27 mechanisms) grounded in citation-purity ethics, IP/identity protection, operational security for a living community, or a pure epistemic-hygiene test — the four sibling readings of the paper_ready_boundary kernel disagree on WHY the boundary sits where it does, even though all four agree on the delta this story measures: every mechanism must carry its own defense.',
    'Examine whether any admitted mechanism''s defense core would fail under a stricter reading (e.g., would design_philosophy_reading exclude a §8-listed category item that lacks a constructible generic defense, or admit a non-listed item that has one) — a divergence between the categorical list and the defense-core test would favor design_philosophy_reading over the other three.',
    'This ambiguity does not change ε for THIS constraint (the defensibility burden is structurally identical under all four readings — 27/27 mechanisms carry defense cores regardless of why the quarantine line is drawn), but it changes which sibling constraint (the quarantine boundary itself, under whichever reading) this rope is downstream of.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quarantine_purpose_ambiguity, conceptual, 'Four incompatible readings of the kernel that produces this defensibility requirement; the requirement''s existence is invariant across readings even though its justification is not.').

omega_variable(
    density_cost_tradeoff_direction,
    'Does universal defense-core coverage (27/27) reflect genuine argumentative completeness, or does it reflect a Goodharted proxy where ''has a defense-core field'' substitutes for ''is actually well-defended''?',
    'Independent review scoring defense-core quality (not just presence) against a rubric, compared against the binary presence/absence ratio this story measures.',
    'If defense-core presence is decoupled from defense-core quality, the rope''s coordination function (readers can trust the paper is self-contained) weakens toward theater without the theater_ratio metric currently reflecting it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(density_cost_tradeoff_direction, empirical, 'Whether the 27/27 ratio measures real argumentative load-bearing or a checklist artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mechanism_defensibility_burden, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mech_tr_t0, mechanism_defensibility_burden, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mech_tr_t2, mechanism_defensibility_burden, theater_ratio, 2, 0.06).
narrative_ontology:measurement(mech_tr_t4, mechanism_defensibility_burden, theater_ratio, 4, 0.07).
narrative_ontology:measurement(mech_tr_t6, mechanism_defensibility_burden, theater_ratio, 6, 0.07).
narrative_ontology:measurement(mech_tr_t8, mechanism_defensibility_burden, theater_ratio, 8, 0.08).
narrative_ontology:measurement(mech_tr_t10, mechanism_defensibility_burden, theater_ratio, 10, 0.08).
narrative_ontology:measurement(mech_tr_t12, mechanism_defensibility_burden, theater_ratio, 12, 0.08).

% Extraction over time
narrative_ontology:measurement(mech_be_t0, mechanism_defensibility_burden, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(mech_be_t2, mechanism_defensibility_burden, base_extractiveness, 2, 0.15).
narrative_ontology:measurement(mech_be_t4, mechanism_defensibility_burden, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(mech_be_t6, mechanism_defensibility_burden, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(mech_be_t8, mechanism_defensibility_burden, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(mech_be_t10, mechanism_defensibility_burden, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(mech_be_t12, mechanism_defensibility_burden, base_extractiveness, 12, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(mechanism_defensibility_burden, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mechanism_defensibility_burden, information_standard).
narrative_ontology:boltzmann_floor_override(mechanism_defensibility_burden, 0.05).
narrative_ontology:affects_constraint(mechanism_defensibility_burden, paper_ready_boundary_citation_purity_reading).
narrative_ontology:affects_constraint(mechanism_defensibility_burden, paper_ready_boundary_ip_provenance_reading).
narrative_ontology:affects_constraint(mechanism_defensibility_burden, paper_ready_boundary_operational_security_reading).
narrative_ontology:affects_constraint(mechanism_defensibility_burden, paper_ready_boundary_design_philosophy_reading).

% DUAL FORMULATION NOTE:
% This story is downstream of the paper_ready_boundary kernel family: the kernel decides which mechanisms may cross into the paper at all (a contested question with four incompatible readings); this story measures what happens to every mechanism that does cross (an uncontested, structurally uniform requirement across all four readings). The two are linked but must not be merged — ε for the boundary question would need to vary by reading (identity-protection vs. epistemic-hygiene vs. citation-ethics vs. operational-security framings each draw the line differently), whereas ε for the defensibility burden is stable and low across all four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
