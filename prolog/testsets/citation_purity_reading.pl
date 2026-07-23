% ============================================================================
% CONSTRAINT STORY: citation_purity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_citation_purity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: citation_purity_reading
 *   human_readable: Non-Self-Citation Quarantine (Reader-Verifiability Reading)
 *   domain: governance/institutional_economics/scholarly_publishing
 *
 * SUMMARY:
 *   This story instantiates one reading of the paper-ready boundary kernel:
 *   the boundary is understood strictly as a reader-verifiability device.
 *   Under this reading, the question that determines what gets quarantined is
 *   never 'whose source is this' or 'is this a security risk' or 'does this
 *   reflect our design philosophy' — it is only 'would accepting this claim
 *   require a stranger to trust something they cannot check.' A private
 *   notebook derivation the author is happy to share is still quarantined if
 *   it is not actually published; an internal doctrine everyone in the field
 *   knows informally is still quarantined if a reader from outside that
 *   tradition cannot verify it. This reading draws the victim set narrowly
 *   around the paper's own credibility and the beneficiary set around the
 *   anonymous reading audience, and it is deliberately indifferent to
 *   ownership, security, or aesthetic/design considerations that the sibling
 *   readings (ip_provenance_reading, operational_security_reading,
 *   design_philosophy_reading) treat as central.
 *
 * KEY AGENTS:
 *   - peer_review_gatekeepers: administer the boundary at submission (institutional/analytical) — enforce but do not personally collect
 *   - reading_audience: strangers who cannot check unpublished sources (organized/analytical) — the boundary's beneficiary
 *   - authors_with_legitimate_private_priors: pay the cost of quarantine even when their private source is honestly held (moderate/constrained)
 *   - cross_disciplinary_researchers: disproportionately burdened where field norms rely on informally transmitted, unpublished knowledge (moderate/constrained)
 *   - editorial_boards: set and adjust the operational test across many cases (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(citation_purity_reading, 0.31).
domain_priors:suppression_score(citation_purity_reading, 0.42).
domain_priors:theater_ratio(citation_purity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(citation_purity_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(citation_purity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(citation_purity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(citation_purity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(citation_purity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(citation_purity_reading, rope).
narrative_ontology:human_readable(citation_purity_reading, "Non-Self-Citation Quarantine (Reader-Verifiability Reading)").
narrative_ontology:topic_domain(citation_purity_reading, "governance/institutional_economics/scholarly_publishing").

domain_priors:requires_active_enforcement(citation_purity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(citation_purity_reading, 'fe9a6d92-33c5-4922-94b5-8ea6e072da0a').
narrative_ontology:cs_kernel_codification('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', formalized).
narrative_ontology:cs_authority_grounding('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', practice).
narrative_ontology:cs_interpretation_layer_present('fe9a6d92-33c5-4922-94b5-8ea6e072da0a').
narrative_ontology:cs_reading_relation('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', paper_ready_boundary__ip_provenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', paper_ready_boundary__operational_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', paper_ready_boundary__design_philosophy_reading, influences).
narrative_ontology:cs_axiom('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', foundational, verifiability_to_strangers_is_the_only_relevant_test).
narrative_ontology:cs_axiom_status(verifiability_to_strangers_is_the_only_relevant_test, holdable).
narrative_ontology:cs_axiom_grounding('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', verifiability_to_strangers_is_the_only_relevant_test, instrumental).
narrative_ontology:cs_axiom('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', secondary, good_faith_of_source_holder_is_irrelevant_to_quarantine).
narrative_ontology:cs_axiom_status(good_faith_of_source_holder_is_irrelevant_to_quarantine, holdable).
narrative_ontology:cs_axiom_grounding('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', good_faith_of_source_holder_is_irrelevant_to_quarantine, conventional).
narrative_ontology:cs_reference_frame('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', peer_review_stranger_verification_norm).
narrative_ontology:cs_drift_state('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', contemporary_preprint_and_open_science_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('fe9a6d92-33c5-4922-94b5-8ea6e072da0a', '').
narrative_ontology:cs_kernel_id(citation_purity_reading, paper_ready_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(citation_purity_reading, reading_audience).
narrative_ontology:constraint_beneficiary(citation_purity_reading, peer_review_gatekeepers).
narrative_ontology:constraint_victim(citation_purity_reading, authors_with_legitimate_private_priors).
narrative_ontology:constraint_victim(citation_purity_reading, cross_disciplinary_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the non-self-citation boundary at submission review: any reference to an unpublished, privately-controlled source (internal memo, proprietary dataset, unshared code, a working relationship the author cannot disclose) is flagged and the paper is held until the citation is replaced or removed. They do not personally collect anything from enforcement but their institutional legitimacy as gatekeepers depends on the boundary holding.
narrative_ontology:constraint_stakeholder(citation_purity_reading, peer_review_gatekeepers, agenda_setter,
    institutional, generational, analytical, global).

% Strangers to the author who cannot inspect the author's private archive, notebooks, or internal communications. They rely on the paper being self-contained and verifiable from the published record alone. The quarantine is what lets them extend trust to a claim without personally auditing the author's private holdings.
narrative_ontology:constraint_stakeholder(citation_purity_reading, reading_audience, beneficiary,
    organized, biographical, analytical, global).

% Have real intellectual debts to unpublished internal work — a private notebook derivation, an unshared internal report, a controlled dataset used to sanity-check the published result. Under this reading, any such debt that a stranger cannot verify must be quarantined regardless of its good-faith origin, even when the private source is honestly held and not competitively withheld. Removing or laundering the citation costs them time and sometimes forces re-derivation of results from only publicly defensible steps.
narrative_ontology:constraint_stakeholder(citation_purity_reading, authors_with_legitimate_private_priors, payer,
    moderate, biographical, constrained, national).

% Work in fields where key background knowledge is transmitted through unpublished lab traditions, internal technical reports, or private correspondence that never became formal literature. The purity reading treats any reliance on that transmission as a verifiability gap requiring quarantine, which falls disproportionately on subfields with weaker publication norms even though the underlying knowledge is genuinely shared and not privately hoarded.
narrative_ontology:constraint_stakeholder(citation_purity_reading, cross_disciplinary_researchers, payer,
    moderate, biographical, constrained, global).

% Set the formal policy that operationalizes the boundary and adjudicate disputed cases where an author claims a source is not really private control but shared or defensible knowledge. They watch the boundary's enforcement pattern across many papers and can loosen or tighten the test.
narrative_ontology:constraint_stakeholder(citation_purity_reading, editorial_boards, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(citation_purity_reading, editorial_boards, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Lets a stranger extend evidentiary trust to a published claim without personally auditing the author's private archive, by guaranteeing that nothing load-bearing in the argument depends on a source only the author can see.
% TRANSFER_FUNCTION: Moves the burden of verifiability from the reader (who would otherwise have to trust the author's private holdings) to the author (who must either publish the supporting source or forgo relying on it), at the cost of authors' time and sometimes at the cost of demonstrably true but unverifiable background knowledge being excluded from the visible chain of reasoning.
% ABSENT_VOICES: Authors whose private priors are honestly held and not competitively withheld (e.g., an internal lab notebook, a shared-but-unpublished disciplinary convention) are not heard in the adjudication of what counts as a verifiability gap; the test asks only whether a stranger would have to trust the archive, not whether the archive-holder is acting in good or bad faith.
% DISAPPEARANCE_RATIONALE: If the quarantine vanished, strangers reading a paper would have no structural guarantee that its claims are checkable from the public record; peer review would have to substitute either blind trust in author reputation or costly independent verification of private sources, and citation practice would likely re-fragment along lines of who personally trusts whom rather than what is publicly checkable.
% FOUNDING_PROBLEM: A reader who is a stranger to the author has no way to check a citation to something only the author controls (an unpublished dataset, an internal memo, a private notebook derivation) — so a paper that rests its claims on such sources is not actually verifiable by the community it addresses, even if the private source is entirely honest.
% FOUNDING_PROBLEM_CORROBORATION: Independent replication studies and meta-science audits of irreproducible results repeatedly identify undisclosed reliance on private, unpublished materials as a contributing factor — this is attested by replication researchers and methodology auditors who are not party to the specific papers being quarantined, i.e. from outside the set of authors and gatekeepers who benefit from any particular boundary decision.
narrative_ontology:disappearance_verdict(citation_purity_reading, world_rearranges).
narrative_ontology:founding_problem_status(citation_purity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(citation_purity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(citation_purity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(citation_purity_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(citation_purity_reading_tests).
:- end_tests(citation_purity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31) because the coordination function is genuine and the cost imposed on authors is mostly friction (rewriting, re-deriving, or forgoing a citation) rather than rent extraction — no party collects a toll from the boundary's operation the way a gatekeeper collects a fee. Suppression (0.42) reflects that the test is applied categorically ('would a stranger have to trust the archive') rather than case-by-case on the merits of the author's honesty, which does foreclose some legitimate citations. Theater ratio is modest and rising slightly (0.15→0.28) as editorial boards accumulate boilerplate compliance language around the test without always interrogating whether a given source really opens a verifiability gap. Accessibility collapse is moderate (0.58): once a source is understood to be privately held, the only path forward is publication or removal — there is no partial credit.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading audience is the clean beneficiary: the entire point of the boundary is to let them trust a paper without personal verification of the author's private holdings, so their directionality sits near full beneficiary. Authors with legitimate private priors and cross-disciplinary researchers are the targets: the quarantine falls on them regardless of the private source's actual trustworthiness, because the test is verifiability-only, not honesty-only. Peer review gatekeepers and editorial boards are structurally symmetric — they neither collect from the boundary nor bear its costs directly, but their institutional legitimacy depends on consistent enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (strangers cannot check private sources) remains fully live — publication and archiving technology has not eliminated the gap between what an author privately knows and what a reader can verify. This keeps the reading from being a pure mandatrophy case: the coordination function this reading isolates has not atrophied. Where mandatrophy risk enters is in the theater ratio's slow rise — as editorial boards accumulate rote compliance checks, some enforcement drifts from 'does this open a real verifiability gap' toward 'does this citation superficially resemble a private source,' which is exactly the kind of proxy substitution the boundary's other readings (ip_provenance, opsec, design_philosophy) would not measure the same way, because their tests are about ownership, risk, or aesthetics rather than verifiability per se.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_vs_ownership_boundary_divergence,
    'When a private source is both unverifiable to strangers AND owned/controlled by the author in a proprietary sense, does the citation_purity_reading and the ip_provenance_reading draw the quarantine line in the same place, or does the purity reading quarantine sources the ownership reading would permit (and vice versa)?',
    'Compare adjudicated cases across editorial boards operating under each framing: identify cases where a source is freely shareable (no IP claim) but still unverifiable (never actually published), and cases where a source is IP-protected but independently verifiable through a different public channel.',
    'If the two readings diverge substantially in practice, that confirms they are genuinely distinct constraints (per the ε-invariance principle) rather than two descriptions of the same enforcement pattern; if they converge in nearly all real cases, the decomposition into separate stories may be less load-bearing than assumed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verifiability_vs_ownership_boundary_divergence, conceptual, 'Whether the reader-verifiability test and the ownership test actually diverge in practice or merely in framing.').

omega_variable(
    good_faith_private_prior_treatment,
    'Should an author''s honestly-held, non-competitive private prior (e.g., an internal lab notebook they are willing to share on request) be treated identically to a competitively withheld proprietary source, given that both fail the strict verifiability test?',
    'Track outcomes for authors who voluntarily offer their private source for reviewer inspection versus authors who cannot or will not share it; if reviewer-inspection cases are treated more leniently in practice despite the formal test being categorical, that reveals an implicit informal exception the strict reading doesn''t officially recognize.',
    'If good-faith and bad-faith unverifiable sources are treated identically, the boundary''s extraction from authors_with_legitimate_private_priors is higher than the coordination story alone would justify, pushing this reading closer to tangled_rope; if an informal good-faith exception operates in practice, the effective extractiveness is lower than the formal rule implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_private_prior_treatment, empirical, 'Whether the strict verifiability test is applied uniformly regardless of the private source-holder''s good faith.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(citation_purity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cita_tr_t0, citation_purity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cita_tr_t6, citation_purity_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(cita_tr_t12, citation_purity_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(cita_tr_t18, citation_purity_reading, theater_ratio, 18, 0.25).
narrative_ontology:measurement(cita_tr_t24, citation_purity_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(cita_be_t0, citation_purity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cita_be_t6, citation_purity_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(cita_be_t12, citation_purity_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(cita_be_t18, citation_purity_reading, base_extractiveness, 18, 0.3).
narrative_ontology:measurement(cita_be_t24, citation_purity_reading, base_extractiveness, 24, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(citation_purity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(citation_purity_reading, information_standard).
narrative_ontology:boltzmann_floor_override(citation_purity_reading, 0.05).
narrative_ontology:affects_constraint(citation_purity_reading, ip_provenance_reading).
narrative_ontology:affects_constraint(citation_purity_reading, operational_security_reading).
narrative_ontology:affects_constraint(citation_purity_reading, design_philosophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the paper_ready_boundary kernel (citation_purity_reading, ip_provenance_reading, operational_security_reading, design_philosophy_reading). Each reading draws the same underlying quarantine ('defer to nothing you privately control') along a different structural test — verifiability-to-strangers here, ownership/provenance, security risk, and stylistic convention in the siblings — producing different victim sets and different epsilon values. They are linked via network edges rather than merged into one story, per the epsilon-invariance principle: an observable that changes which sources get quarantined (verifiability vs. ownership vs. risk vs. style) changes which constraint is being measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
