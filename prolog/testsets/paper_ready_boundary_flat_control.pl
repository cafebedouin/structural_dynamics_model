% ============================================================================
% CONSTRAINT STORY: paper_ready_boundary_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paper_ready_boundary_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: paper_ready_boundary_flat_control
 *   human_readable: Paper-Ready Quarantine Boundary (§1, §8, Adoption Clause)
 *   domain: governance_system_design/institutional_economics
 *
 * SUMMARY:
 *   The constraint under analysis is the quarantine boundary itself — the
 *   joint operation of §1 (definitional scope), §8 (procedural gate), and the
 *   adoption clause — that determines which material from an unpublished,
 *   doctrinally-named working corpus is permitted to cross into the publicly
 *   citable paper. This is authored as a single flat constraint: the rule as
 *   it actually functions, not decomposed into competing readings of what the
 *   rule 'really' means. The boundary genuinely solves a coordination problem
 *   (readers need a stable, vetted reference rather than the entire churning
 *   corpus) while simultaneously operating as a credit-allocation and
 *   access-control mechanism that concentrates citability and reputational
 *   capital on a narrow set of already-advantaged authors and editors.
 *
 * KEY AGENTS:
 *   - editorial_board: administers §1/§8 and the adoption clause, institutional power, arbitrage exit — sets the gate and captures reputational upside from every crossing
 *   - cited_authors_of_record: powerful, mobile exit — primary beneficiaries of selection into the citable paper
 *   - unpublished_corpus_contributors: moderate power, constrained exit — bear the cost of labor absorbed without individual credit
 *   - doctrinally_named_working_groups: organized but trapped — their doctrinal apparatus is stripped at the boundary rather than carried through as citable content
 *   - external_replication_researchers: powerless, trapped, global scope — cannot access the quarantined corpus needed to verify published claims
 *   - downstream_protocol_implementers: organized, mobile — benefit from the trust signal the boundary manufactures without bearing its costs
 *   - governance_auditors: analytical observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paper_ready_boundary_flat_control, 0.42).
domain_priors:suppression_score(paper_ready_boundary_flat_control, 0.51).
domain_priors:theater_ratio(paper_ready_boundary_flat_control, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paper_ready_boundary_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(paper_ready_boundary_flat_control, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(paper_ready_boundary_flat_control, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paper_ready_boundary_flat_control, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(paper_ready_boundary_flat_control, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paper_ready_boundary_flat_control, tangled_rope).
narrative_ontology:human_readable(paper_ready_boundary_flat_control, "Paper-Ready Quarantine Boundary (§1, §8, Adoption Clause)").
narrative_ontology:topic_domain(paper_ready_boundary_flat_control, "governance_system_design/institutional_economics").

domain_priors:requires_active_enforcement(paper_ready_boundary_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(paper_ready_boundary_flat_control, paper_ready_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paper_ready_boundary_flat_control, editorial_board).
narrative_ontology:constraint_beneficiary(paper_ready_boundary_flat_control, cited_authors_of_record).
narrative_ontology:constraint_beneficiary(paper_ready_boundary_flat_control, downstream_protocol_implementers).
narrative_ontology:constraint_victim(paper_ready_boundary_flat_control, unpublished_corpus_contributors).
narrative_ontology:constraint_victim(paper_ready_boundary_flat_control, doctrinally_named_working_groups).
narrative_ontology:constraint_victim(paper_ready_boundary_flat_control, external_replication_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers §1, §8, and the adoption clause: decides what counts as 'paper-ready,' controls the quarantine gate between the unpublished doctrinally-named corpus and the publicly citable paper. Justifies the boundary as quality control and citation integrity. Its own imprint and reputational capital accrue every time material crosses the gate under its adoption stamp.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, editorial_board, agenda_setter,
    institutional, generational, arbitrage, national).

% Their contributions are the ones selected to cross the boundary; citation credit, career capital, and standing accrue to them once material is designated paper-ready. They have influence over which drafts get proposed for adoption and can route around the corpus entirely by publishing independently if the gate becomes too costly.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, cited_authors_of_record, beneficiary,
    powerful, biographical, mobile, national).

% Produce doctrinally-named working material that feeds the corpus but rarely crosses the quarantine boundary themselves. Their labor is absorbed into drafts credited to others once adopted, or remains permanently uncitable if never selected. Exit means abandoning the corpus's institutional legitimacy and publishing outside the recognized channel, which strips their work of the credibility the boundary itself manufactures.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, unpublished_corpus_contributors, payer,
    moderate, biographical, constrained, national).

% Maintain the naming conventions and internal doctrine that make the corpus navigable, but their doctrinal apparatus is treated as scaffolding to be stripped away at the boundary, not as citable content in its own right. They cannot publish under their own doctrinal framing without going through the adoption clause, which requires translating their work into the board's preferred form.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, doctrinally_named_working_groups, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(paper_ready_boundary_flat_control, doctrinally_named_working_groups, excluded).

% Attempt to verify or extend published claims but cannot access the unpublished doctrinally-named corpus that produced them, since the quarantine boundary is precisely what keeps that material non-public. They would object to the opacity but have no standing in the adoption process and no channel to request access.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, external_replication_researchers, excluded,
    powerless, biographical, trapped, global).

% Build on the publicly citable paper once material clears the boundary, benefiting from the stability and citability the quarantine rule manufactures. They rarely interact with the unpublished corpus directly and treat the paper-ready designation as a trust signal they did not have to produce themselves.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, downstream_protocol_implementers, beneficiary,
    organized, generational, mobile, global).

% Study the adoption clause's operation, comparing who proposes, who is credited, and who is quarantined indefinitely, without directly participating in the corpus or the board's decisions.
narrative_ontology:constraint_stakeholder(paper_ready_boundary_flat_control, governance_auditors, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable definition of what counts as citable, publicly reliable material, so downstream implementers and external readers do not have to individually vet the entire doctrinally-named working corpus before relying on a claim.
% TRANSFER_FUNCTION: Moves citation credit, reputational capital, and downstream reliance value from the diffuse pool of doctrinal contributors and working groups to the narrower set of authors and editors whose drafts are selected to cross the boundary under the adoption clause.
% ABSENT_VOICES: Unpublished corpus contributors whose labor is absorbed without individual credit, and external replication researchers who cannot access the quarantined material to verify or contest published claims, would object to the opacity of the selection process but have no seat in §1/§8 administration.
% DISAPPEARANCE_RATIONALE: If the quarantine boundary vanished overnight, the entire doctrinally-named corpus would become simultaneously citable, collapsing the distinction the paper's authority currently rests on; downstream implementers would lose the trust signal, credited authors would lose their gatekept advantage, and corpus contributors would gain direct citability they currently lack — the institutional structure built on the paper/corpus split would need to reorganize around a flat, undifferentiated corpus.
% FOUNDING_PROBLEM: The doctrinally-named working corpus grew faster and messier than any single reader could vet; a stable boundary was needed so that a publicly citable paper could exist as a trustworthy, load-bearing reference distinct from provisional internal drafts.
% FOUNDING_PROBLEM_CORROBORATION: The editorial board attests the boundary is still necessary for citation integrity and quality control. Governance auditors and external replication researchers, examining the adoption clause's actual operation, attest that the selection process now functions substantially as a credit-allocation mechanism favoring already-credited authors, with the original vetting function partially displaced by reputational sorting — this reading is corroborated from outside the beneficiary set, though no fully independent audit of the unpublished corpus itself has been permitted.
narrative_ontology:disappearance_verdict(paper_ready_boundary_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(paper_ready_boundary_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paper_ready_boundary_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(paper_ready_boundary_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(paper_ready_boundary_flat_control, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paper_ready_boundary_flat_control_tests).
:- end_tests(paper_ready_boundary_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the boundary's primary function — producing a stable, citable reference — is genuine and real value accrues to downstream implementers who benefit from not having to vet the raw corpus. But extraction has been rising steadily (0.28 to 0.42) as the adoption clause has increasingly functioned to sort credit toward already-credited authors rather than toward the strongest unpublished material. Suppression is moderate (0.51) and rests mainly on the structural fact that the unpublished corpus is definitionally non-public — there is no formal ban on corpus contributors publishing independently, but doing so forfeits the institutional legitimacy the boundary itself manufactures, which functions as a soft coercive lock rather than an outright prohibition. Theater ratio (0.33, rising) reflects that a growing share of §8's procedural apparatus now performs legitimacy rather than doing vetting work, though the vetting function has not fully atrophied — this is short of piton territory.
 *
 * PERSPECTIVAL GAP:
 *   From the editorial board's seat, the boundary is coordination: a necessary filter protecting the public record from unvetted doctrinal churn. From the unpublished corpus contributors' seat, the identical structure operates as extraction: their labor is a raw material absorbed into a paper credited to others, with no reciprocal path to citability. The engine should compute these as structurally different experiences of the same rule, driven by the asymmetry in exit options (arbitrage for the board and mobile for cited authors, versus constrained/trapped for corpus contributors and working groups) rather than by any difference in the rule's text.
 *
 * DIRECTIONALITY LOGIC:
 *   The editorial board and cited authors of record sit near the beneficiary end: the board administers the gate and accrues institutional standing from every successful crossing, while cited authors collect the citation credit the crossing manufactures. Unpublished corpus contributors and doctrinally-named working groups sit near the target end: their labor and doctrinal framing are consumed by the boundary's operation without a reciprocal citability path, and their exit options (constrained, trapped) are narrow because leaving means forfeiting the institutional legitimacy the boundary itself produces. External replication researchers are excluded rather than coordinated — their exclusion from the unpublished corpus is a structural consequence of the same quarantine that makes the paper citable, so their inability to replicate is not incidental but constitutive of the mechanism. Downstream implementers are genuine near-symmetric beneficiaries: they get a reliability signal they did not have to produce, at negligible direct cost to themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unvetted, sprawling doctrinal corpus that no reader could individually verify — remains partially live: some vetting function is still performed by §8. But the founding problem's status is contested rather than cleanly dead or live, because the rising theater ratio and rising extraction over the interval indicate the boundary has drifted from pure vetting toward partial credit-allocation. Classifying this as tangled_rope rather than snare prevents mislabeling a structure with a real coordination core as pure extraction; classifying it as tangled_rope rather than rope prevents treating the asymmetric, enforced cost borne by corpus contributors and excluded researchers as if it were a Pareto-improving coordination mechanism with no victims. The tangled_rope label captures both truths at once: real coordination value for downstream implementers, real extraction from corpus contributors, both riding on the same §1/§8/adoption-clause machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vetting_function_versus_credit_sorting,
    'What proportion of the adoption clause''s actual selection activity is genuine quality vetting versus credit-allocation favoring already-credited authors?',
    'Blind review of a sample of adoption decisions, comparing selected versus rejected drafts on quality metrics independent of author identity, cross-checked against the rising theater_ratio trend.',
    'If vetting dominates, the tangled_rope classification''s coordination component is robust and extraction is a minor byproduct; if credit-sorting dominates, the constraint is closer to a snare wearing a vetting justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vetting_function_versus_credit_sorting, empirical, 'Whether the adoption clause primarily vets quality or sorts credit.').

omega_variable(
    quarantine_necessity_versus_convenience,
    'Is the quarantine of the unpublished doctrinally-named corpus structurally necessary for the paper''s citability function, or would a more open, tiered-access model achieve the same trust signal at lower cost to contributors and replication researchers?',
    'Comparative study of governance systems using tiered or graduated access (e.g. embargoed-but-accessible corpora) versus hard quarantine, measuring citability trust outcomes under each.',
    'If a tiered model achieves equivalent trust at lower extraction, the current hard quarantine''s suppression component is excess beyond coordination need, sharpening the case toward snare; if hard quarantine is structurally required, the suppression is closer to genuine coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quarantine_necessity_versus_convenience, conceptual, 'Whether hard quarantine is functionally necessary or a costlier-than-required design choice.').

omega_variable(
    corpus_contributor_consent_ambiguity,
    'Did doctrinally-named working groups and corpus contributors knowingly consent to a structure where their labor could be absorbed into differently-credited published material, or is this an emergent property they did not anticipate when joining the corpus?',
    'Review of founding governance documents and onboarding materials for explicit disclosure of the credit-allocation dynamic versus retrospective accounts from long-tenured contributors.',
    'Informed consent to the arrangement would weaken the victim framing for corpus contributors; emergent, undisclosed absorption would strengthen it and support characterizing part of the suppression as internalized normalization rather than purely structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corpus_contributor_consent_ambiguity, empirical, 'Whether corpus contributors consented to the credit-absorption dynamic or it emerged undisclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paper_ready_boundary_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pape_tr_t0, paper_ready_boundary_flat_control, theater_ratio, 0, 0.18).
narrative_ontology:measurement(pape_tr_t4, paper_ready_boundary_flat_control, theater_ratio, 4, 0.21).
narrative_ontology:measurement(pape_tr_t8, paper_ready_boundary_flat_control, theater_ratio, 8, 0.24).
narrative_ontology:measurement(pape_tr_t12, paper_ready_boundary_flat_control, theater_ratio, 12, 0.27).
narrative_ontology:measurement(pape_tr_t16, paper_ready_boundary_flat_control, theater_ratio, 16, 0.29).
narrative_ontology:measurement(pape_tr_t20, paper_ready_boundary_flat_control, theater_ratio, 20, 0.31).
narrative_ontology:measurement(pape_tr_t24, paper_ready_boundary_flat_control, theater_ratio, 24, 0.33).

% Extraction over time
narrative_ontology:measurement(pape_be_t0, paper_ready_boundary_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pape_be_t4, paper_ready_boundary_flat_control, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(pape_be_t8, paper_ready_boundary_flat_control, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(pape_be_t12, paper_ready_boundary_flat_control, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(pape_be_t16, paper_ready_boundary_flat_control, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(pape_be_t20, paper_ready_boundary_flat_control, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(pape_be_t24, paper_ready_boundary_flat_control, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(pape_su_t0, paper_ready_boundary_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(pape_su_t4, paper_ready_boundary_flat_control, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(pape_su_t8, paper_ready_boundary_flat_control, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(pape_su_t12, paper_ready_boundary_flat_control, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(pape_su_t16, paper_ready_boundary_flat_control, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(pape_su_t20, paper_ready_boundary_flat_control, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(pape_su_t24, paper_ready_boundary_flat_control, suppression_requirement, 24, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paper_ready_boundary_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paper_ready_boundary_flat_control, 0.12).

% DUAL FORMULATION NOTE:
% This story treats the quarantine boundary (§1, §8, adoption clause) as a single flat constraint per the construction-perturbation control instruction: it does not decompose into separate readings of the boundary's function, and no sibling reading files exist for this control condition. If a future analysis finds the boundary's ε value shifts materially depending on whether it is evaluated as a vetting mechanism versus a credit-allocation mechanism, that would indicate two distinct constraints requiring decomposition per the ε-invariance principle — but this control story deliberately holds the single-constraint frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
