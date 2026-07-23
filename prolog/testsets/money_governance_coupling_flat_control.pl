% ============================================================================
% CONSTRAINT STORY: money_governance_coupling_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_money_governance_coupling_flat_control, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: money_governance_coupling_flat_control
 *   human_readable: Citation Quarantine Rule as Paper Legitimacy Kernel
 *   domain: constitutional political economy / blockchain governance design
 *
 * SUMMARY:
 *   A governance paper — proposing a design for coupling money and governance
 *   mechanisms in a blockchain protocol — grounds its own claim to authority
 *   not in the substance of its arguments but in a fixed procedural rule: the
 *   paper cites no unpublished corpus and none of its own member's writing.
 *   This quarantine is presented as the guarantee of the paper's objectivity
 *   — it cannot be accused of self-serving citation because it structurally
 *   cannot cite itself or its authors' private notes. The document becomes
 *   authoritative for the working group at the moment members sign it,
 *   adoption-by-signature standing in for substantive ratification. The
 *   constraint under analysis is this quarantine-plus-signature apparatus
 *   itself, treated as one flat structure: a procedural commitment that
 *   coordinates a real problem (citation self-dealing, circular authority)
 *   while simultaneously creating an asymmetric filter that benefits whoever
 *   controls the drafting process and disadvantages members whose relevant
 *   knowledge lives outside the admissible published record.
 *
 * KEY AGENTS:
 *   - founding_drafting_committee: administers the quarantine rule, selects the admissible citation pool, and drafts the document that signatories will ratify (institutional/arbitrage) — the agenda-setting seat
 *   - signatory_members: collectively adopt the document by signature, converting a drafted text into an authoritative governance paper (organized/constrained) — nominal co-authors of legitimacy but downstream of the quarantine's already-completed filtering
 *   - excluded_unpublished_researchers: hold relevant expertise or prior analysis that exists only in unpublished form and is therefore structurally inadmissible regardless of merit (powerless/trapped)
 *   - member_authors_of_prior_work: committee members whose own prior writing on the exact topic cannot be cited under the rule, forcing them to either omit their strongest evidence or restate it as if newly discovered through a third party (moderate/constrained)
 *   - protocol_token_holders: the downstream constituency whose governance rights are shaped by whatever the paper concludes, without a seat in either the drafting or the citation-quarantine process (organized/constrained)
 *   - external_academic_reviewers: analytical observers who can assess whether the quarantine's stated neutrality function matches its actual citation outcomes (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(money_governance_coupling_flat_control, 0.42).
domain_priors:suppression_score(money_governance_coupling_flat_control, 0.58).
domain_priors:theater_ratio(money_governance_coupling_flat_control, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(money_governance_coupling_flat_control, extractiveness, 0.42).
narrative_ontology:constraint_metric(money_governance_coupling_flat_control, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(money_governance_coupling_flat_control, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(money_governance_coupling_flat_control, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(money_governance_coupling_flat_control, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(money_governance_coupling_flat_control, tangled_rope).
narrative_ontology:human_readable(money_governance_coupling_flat_control, "Citation Quarantine Rule as Paper Legitimacy Kernel").
narrative_ontology:topic_domain(money_governance_coupling_flat_control, "constitutional political economy / blockchain governance design").

domain_priors:requires_active_enforcement(money_governance_coupling_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(money_governance_coupling_flat_control, '7f49f867-a189-4252-8aa0-c25a61627e52').
narrative_ontology:cs_kernel_codification('7f49f867-a189-4252-8aa0-c25a61627e52', formalized).
narrative_ontology:cs_authority_grounding('7f49f867-a189-4252-8aa0-c25a61627e52', extraction).
narrative_ontology:cs_interpretation_layer_present('7f49f867-a189-4252-8aa0-c25a61627e52').
narrative_ontology:cs_created_at('7f49f867-a189-4252-8aa0-c25a61627e52', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(money_governance_coupling_flat_control, money_governance_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(money_governance_coupling_flat_control, founding_drafting_committee).
narrative_ontology:constraint_beneficiary(money_governance_coupling_flat_control, signatory_members).
narrative_ontology:constraint_victim(money_governance_coupling_flat_control, excluded_unpublished_researchers).
narrative_ontology:constraint_victim(money_governance_coupling_flat_control, member_authors_of_prior_work).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(money_governance_coupling_flat_control, signatory_members).
narrative_ontology:constraint_victim(money_governance_coupling_flat_control, protocol_token_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the citation quarantine, decides in practice what counts as 'published' and admissible, and drafts the text that will later be adopted by signature. Its own authority as the credible source of the governance design is what the finished document certifies; it bears essentially no cost from the quarantine since it controls the drafting process the quarantine constrains.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, founding_drafting_committee, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(money_governance_coupling_flat_control, founding_drafting_committee, beneficiary).

% Adopt the drafted document by signature, an act that converts a committee-drafted text into the working group's authoritative governance paper. Benefit from having a settled reference document to coordinate around, but had no input into which sources were excluded before the draft reached them; withholding signature does not stop the document from becoming binding among those who sign.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, signatory_members, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(money_governance_coupling_flat_control, signatory_members, payer).

% Possess analysis or evidence directly relevant to the governance design, but it exists only in unpublished form and is therefore categorically inadmissible under the quarantine rule regardless of its quality or relevance. Have no procedural channel to contest the exclusion short of formally publishing their work through an outside venue on their own timeline, which the drafting schedule does not accommodate.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, excluded_unpublished_researchers, excluded,
    powerless, biographical, trapped, national).

% Committee or working-group members whose own prior writing on the precise topic under discussion cannot be cited under the self-citation ban. Must either omit their strongest existing evidence, restate it through a third-party citation if one happens to exist, or lose the ability to substantiate their position — while committee members drawing on already-published third-party literature face no equivalent constraint.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, member_authors_of_prior_work, payer,
    moderate, biographical, constrained, national).

% Downstream constituency governed by whatever money-governance coupling design the paper ultimately certifies as authoritative. Have no seat in either the drafting process or the citation-quarantine enforcement, and inherit the design's blind spots (whatever the quarantine happened to exclude) without having consented to the exclusion itself.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, protocol_token_holders, payer,
    organized, generational, constrained, global).

% Can examine the finished document's citation pool against the universe of relevant unpublished and member-authored work to assess whether the quarantine's neutrality claim matches its actual filtering effect. Take no side in the adoption process but are the primary check on whether the rule is functioning as claimed.
narrative_ontology:constraint_stakeholder(money_governance_coupling_flat_control, external_academic_reviewers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(money_governance_coupling_flat_control, founding_drafting_committee).
narrative_ontology:fixing_cost_class(money_governance_coupling_flat_control, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a self-referential drafting body from citing only its own unpublished notes or its own members' private writing as evidence for its conclusions, which would otherwise let the paper certify itself without any external check.
% TRANSFER_FUNCTION: Moves evidentiary weight and substantiation power away from unpublished researchers and member-authors of directly relevant prior work, and toward whichever already-published third-party literature the drafting committee selects as citable — with the committee's authority as certifying body flowing from the appearance of neutrality this filtering produces.
% ABSENT_VOICES: Unpublished researchers whose work would bear directly on the money-governance coupling design are not in the room when the quarantine rule is drafted or applied, and have no mechanism to contest an exclusion that turns on the accident of publication status rather than the merit of their analysis.
% DISAPPEARANCE_RATIONALE: If the quarantine and signature-adoption apparatus vanished, the drafting committee would lose its primary defense against charges of self-dealing citation, and the working group would need some other mechanism to establish that the paper's conclusions were not simply invented by its own authors — a real rearrangement. But protocol token holders and excluded researchers might see little change: the same committee would likely retain effective control over the document's content either way, since the quarantine's removal does not by itself grant excluded parties a voice in drafting. Whether the world 'rearranges' or stays the same depends on which function of the rule — genuine external check, or gatekeeping cover — is doing more of the actual work, which is exactly the open question the omegas name.
% FOUNDING_PROBLEM: Small drafting groups producing foundational governance documents face an acute self-dealing risk: if a paper can cite its own authors' unpublished notes as evidence, it can manufacture the appearance of independent corroboration for whatever the authors already believed, making the paper unfalsifiable by outside parties.
% FOUNDING_PROBLEM_CORROBORATION: The founding drafting committee attests the problem remains live and that the quarantine continues to protect the paper's credibility. External academic reviewers examining the actual citation pool across drafting cycles are the corroborating source outside the benefiting parties; their assessment (per the omega on quarantine neutrality) is that the rule's protective function and its gatekeeping function cannot currently be cleanly separated from the outside, which is itself evidence that the founding problem's current 'solved' status is asserted mainly by those who administer the rule rather than independently confirmed.
narrative_ontology:disappearance_verdict(money_governance_coupling_flat_control, contested).
narrative_ontology:founding_problem_status(money_governance_coupling_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(money_governance_coupling_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-23',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(money_governance_coupling_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(money_governance_coupling_flat_control, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(money_governance_coupling_flat_control_tests).
:- end_tests(money_governance_coupling_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.42) rather than high: the quarantine rule does solve a real problem (a self-referential paper citing only its own unpublished notes would have no external check), so there is a genuine coordination function underneath the constraint, not pure pretext. But extraction rises over the measured interval as the rule's application appears to systematically favor citation trails aligned with drafting-committee priors while the harder-to-verify claim that self-citation bans purely protect neutrality wears thinner with each drafting cycle. Suppression (0.58) reflects that the rule is actively enforced — drafts are checked against the citation ban before circulation — but is not maximal because the rule's text is public and its application is at least nominally auditable, unlike a fully opaque gatekeeping mechanism. Theater ratio rises from 0.18 to 0.40 across the interval: as more drafting cycles occur, an increasing share of the quarantine's defense in internal discussion is procedural ('we followed the rule') rather than substantive ('the rule produced a better document'), consistent with a rule whose original coordination function is increasingly invoked ceremonially to close down challenge rather than to actually test claims against outside literature.
 *
 * PERSPECTIVAL GAP:
 *   From the drafting committee's seat, the quarantine is a rope: a minimal-overhead device that prevents the paper from being dismissed as self-serving, purchased at low cost (don't cite your own drafts) for a large legitimacy gain (external credibility). From the seat of a member-author whose most relevant prior analysis cannot be cited, the same rule computes as extractive: their labor and expertise are structurally devalued relative to committee-favored published sources, and the rule cannot be argued with because arguing against it looks like special pleading for one's own citability. The engine's per-seat computation should reflect this divergence directly from the declared power/exit asymmetry rather than from any claim about the rule's intent.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding drafting committee sits nearest the beneficiary end: it administers the quarantine, decides what counts as 'published' and therefore admissible, and its own institutional authority is what the finished, signed document ultimately certifies. Signatory members are structurally closer to symmetric — they benefit from having an authoritative document to coordinate around, but they bear the cost of having had no input into which sources were quarantined out before they ever saw a draft to sign. Excluded unpublished researchers and member-authors-of-prior-work sit at the target end: their most relevant knowledge is categorically barred from the document that will govern the system they are also subject to, and they have no procedural lever to contest the ban except abstention from signing, which does not stop the document from becoming authoritative among those who do sign.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing a self-referential paper from citing only itself as evidence of its own conclusions — is not dead; self-dealing citation remains a live risk in any small drafting group. But the rule's current operation has drifted from solving that problem toward also solving a second, unacknowledged problem: protecting the committee's drafting choices from challenge by anyone whose strongest evidence happens to be unpublished. Classifying this as tangled_rope rather than snare or rope preserves both halves: the coordination function is real and would need replacing if removed (hence disappearance_verdict below), but the asymmetric cost borne by excluded researchers and member-authors is also real and requires active enforcement (checking every draft against the ban) to persist. Collapsing either half — calling it pure snare, or pure rope — would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quarantine_neutrality_vs_gatekeeping,
    'Does the ''no unpublished corpus, no member''s own writing'' rule function as a genuine neutrality safeguard against self-dealing citation, or does it operate as a gatekeeping device that privileges whichever body of already-published literature happens to align with the drafting committee''s priors while excluding member expertise that would otherwise strengthen or challenge the draft?',
    'Audit the citation pool actually used across finalized drafts: measure what fraction of excluded member-authored or unpublished material would have changed substantive conclusions, and compare the ideological/institutional distribution of the published sources actually admitted.',
    'If the rule mainly excludes material that would contest the committee''s preferred framing, the ''neutrality'' claim is a false summit and the constraint is better read as tangled_rope leaning toward snare; if exclusion is roughly symmetric across viewpoints, the coordination reading holds more weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quarantine_neutrality_vs_gatekeeping, empirical, 'Whether the quarantine rule is neutral procedure or selective gatekeeping.').

omega_variable(
    signature_adoption_as_authority_source,
    'Is adoption-by-signature a genuine consent mechanism that confers legitimacy because signatories freely deliberated and could have withheld assent, or is it a ratification ritual where the quarantine rule and drafting process have already foreclosed the substantive space signatories are asked to approve?',
    'Examine the sequencing: were signatories present during drafting and able to contest the quarantine rule''s application, or were they presented a completed document under time pressure with signature as the only available action?',
    'If signatories had genuine input, adoption-by-signature is real coordination; if the quarantine rule pre-committed the outcome before signatories saw it, the signature step is theater and effective control sits entirely with whoever administered the quarantine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signature_adoption_as_authority_source, conceptual, 'Whether signature-adoption is deliberative consent or ratification theater.').

omega_variable(
    self_citation_ban_symmetry,
    'Does barring members from citing their own writing burden all members equally, or does it disproportionately silence members whose relevant expertise exists mainly in their own unpublished or self-authored work (e.g., practitioners, newer scholars) while leaving unaffected members whose views are already well-represented in third-party published literature?',
    'Map each member''s citable third-party literature footprint against their own unpublished/self-authored contributions; members with thin third-party coverage are structurally silenced more than those with thick coverage.',
    'Asymmetric burden would mean the rule''s formally neutral language masks differential extraction from specific member subgroups, strengthening the victim declaration for member_authors_of_prior_work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_citation_ban_symmetry, empirical, 'Whether the self-citation ban burdens members symmetrically or selectively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(money_governance_coupling_flat_control, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, money_governance_coupling_flat_control, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mone_tr_t6, money_governance_coupling_flat_control, theater_ratio, 6, 0.24).
narrative_ontology:measurement(mone_tr_t12, money_governance_coupling_flat_control, theater_ratio, 12, 0.3).
narrative_ontology:measurement(mone_tr_t18, money_governance_coupling_flat_control, theater_ratio, 18, 0.36).
narrative_ontology:measurement(mone_tr_t24, money_governance_coupling_flat_control, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, money_governance_coupling_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mone_be_t6, money_governance_coupling_flat_control, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(mone_be_t12, money_governance_coupling_flat_control, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(mone_be_t18, money_governance_coupling_flat_control, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(mone_be_t24, money_governance_coupling_flat_control, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, money_governance_coupling_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(mone_su_t6, money_governance_coupling_flat_control, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(mone_su_t12, money_governance_coupling_flat_control, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(mone_su_t18, money_governance_coupling_flat_control, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(mone_su_t24, money_governance_coupling_flat_control, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(money_governance_coupling_flat_control, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This story treats the citation-quarantine-plus-signature-adoption apparatus as a single flat constraint, per the construction-perturbation control: it is not decomposed into separate readings (e.g. 'quarantine as neutrality safeguard' vs 'quarantine as gatekeeping device'). Where those readings diverge, the divergence is carried in the omegas and in the perspectival gap across stakeholder seats rather than in separate constraint_ids.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
