% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Woodruff Manifesto as Endogenous Reinterpretation (Living-Prophet Reading)
 *   domain: religious/political-theology/commitment-systems
 *
 * SUMMARY:
 *   On September 23-24, 1890, Wilford Woodruff issued the declaration that
 *   ceased the public practice of plural marriage, presenting the decision as
 *   a response to divine manifestation — a vision showing what would befall
 *   the church if the practice continued. This story instantiates the
 *   endogenous-reinterpretation reading of that event: the reversal is
 *   treated as genuine internal revelation reinterpreting God's will under
 *   changed circumstances, and the constraint under classification is that
 *   interpretive settlement itself — the standing arrangement by which a
 *   prior binding command is suspended through the living prophet's
 *   revelatory authority rather than through concession. The claim and the
 *   metrics are independent authored facts: the settlement is CLAIMED as
 *   tangled_rope (a real coordination achievement carrying asymmetric costs),
 *   and the metrics describe its documented operation without being tuned to
 *   any predicted verdict. Per the epsilon-invariance principle, the
 *   colloquial label 'the 1890 Manifesto' decomposes into a constraint
 *   family: this file, the exogenous-override sibling, and the
 *   doctrine-practice-gap sibling each carry their own stable epsilon, victim
 *   set, and classification, linked through network edges.
 *
 * KEY AGENTS:
 *   - lds_first_presidency: Primary beneficiary and agenda-setter (institutional/arbitrage) — preserves and demonstrates interpretive authority through the revelation framing
 *   - quorum_of_twelve_apostles: Secondary beneficiary with payer overlay (institutional/constrained) — lends collective legitimacy, absorbs private reconciliation cost
 *   - existing_plural_families: Primary target (moderate/trapped) — bears covenant-disruption loss and legal jeopardy
 *   - rank_and_file_believers: Dual-positioned (moderate/constrained) — absorbs theological-consistency cost while gaining civic relief
 *   - dissenting_priesthood_holders: Resisting target (organized/identity_locked) — ideological fusion with the revoked principle
 *   - federal_prosecutorial_authorities: Outcome recipient (powerful/mobile) — the changed-circumstance backdrop, receiving compliance
 *   - religious_historians: Analytical observer — sees the full documentary structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Woodruff Manifesto as Endogenous Reinterpretation (Living-Prophet Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious/political-theology/commitment-systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, '443180c3-a76a-4d31-9a00-c8338c2ceb8b').
narrative_ontology:cs_kernel_codification('443180c3-a76a-4d31-9a00-c8338c2ceb8b', fixed_text).
narrative_ontology:cs_authority_grounding('443180c3-a76a-4d31-9a00-c8338c2ceb8b', lineage).
narrative_ontology:cs_interpretation_layer_present('443180c3-a76a-4d31-9a00-c8338c2ceb8b').
narrative_ontology:cs_reading_relation('443180c3-a76a-4d31-9a00-c8338c2ceb8b', marriage_commitment_reversal__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('443180c3-a76a-4d31-9a00-c8338c2ceb8b', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('443180c3-a76a-4d31-9a00-c8338c2ceb8b', foundational, divine_will_reinterpreted_under_changed_circumstances).
narrative_ontology:cs_axiom_status(divine_will_reinterpreted_under_changed_circumstances, holdable).
narrative_ontology:cs_axiom_grounding('443180c3-a76a-4d31-9a00-c8338c2ceb8b', divine_will_reinterpreted_under_changed_circumstances, theological).
narrative_ontology:cs_axiom('443180c3-a76a-4d31-9a00-c8338c2ceb8b', secondary, living_prophet_interpretation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(living_prophet_interpretation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('443180c3-a76a-4d31-9a00-c8338c2ceb8b', living_prophet_interpretation_supersedes_prior_command, theological).
narrative_ontology:cs_reference_frame('443180c3-a76a-4d31-9a00-c8338c2ceb8b', living_oracle_continuity).
narrative_ontology:cs_drift_state('443180c3-a76a-4d31-9a00-c8338c2ceb8b', post_manifesto_unauthorized_sealings_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('443180c3-a76a-4d31-9a00-c8338c2ceb8b', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, lds_first_presidency).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_prosecutorial_authorities).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, existing_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_priesthood_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives and announces the September 1890 declaration, presenting the cessation of plural marriage as divine guidance responsive to changed circumstances. Holds sole authority to interpret God's will for the church; the declaration demonstrates that this authority extends to suspending a prior command. Controls the official record of how the decision was reached, disciplines dissent through church courts, and issues clarifications when compliance falters.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, lds_first_presidency, agenda_setter,
    institutional, generational, arbitrage, continental).

% Signs and sustains the declaration, lending collective quorum legitimacy to the reversal. Several members sign reluctantly or after weeks of deliberation; two later resign or are removed from office over continued tolerance of new plural marriages. They share the preserved authority structure while bearing the private labor of reconciling the reversal with doctrines they had personally preached for decades.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, quorum_of_twelve_apostles, payer).

% Husbands, wives, and children in existing plural households. Their sealings were taught as eternal covenants; the declaration halts new plural sealings and requires existing families to maintain themselves privately while the church publicly renounces the practice. Husbands face arrest if they resume open family life; wives face separation and legal ambiguity for their children. Leaving the community means forfeiting perceived salvation, kin networks, and standing; staying means living inside the contradiction between covenant and law.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, existing_plural_families, payer,
    moderate, biographical, trapped, regional).

% Accept the declaration as revelation and revise their understanding of God's will accordingly. They gain relief from prosecution and raids, a path toward Utah statehood, and normalized civic standing; they bear the cost of integrating a reversed command into a theology of an unchanging God. Departure means losing community and perceived salvation; remaining requires accepting that the living prophet may reinterpret what a prior prophet bound.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_believers, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_believers, beneficiary).

% Apostles and elders who regard the original command as irrevocable and the declaration as error or surrender. Some withhold endorsement; others continue performing or sheltering new sealings after 1890. Church discipline removes several from office; over the following decades their successors form separate communions claiming the pre-1890 authority line. Their convictions are fused with the principle they believe was set aside; abandoning that conviction would dissolve the self they have built around it.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_priesthood_holders, payer,
    organized, biographical, identity_locked, regional).

% Congress, federal courts, and marshals who criminalized plural marriage, dissolved the church corporation, and escheated its property through the 1880s. They receive the practical outcome of the declaration: the public practice ceases, prosecutions wind down, and Utah's statehood path opens. Their statutes and prosecutions constitute the changed circumstances the declaration addresses; they receive the outcome without participating in the doctrinal decision.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_prosecutorial_authorities, beneficiary,
    powerful, generational, mobile, national).

% Reconstruct the decision's documentary trail — diary entries, multiple drafts of the declaration, correspondence with counsel, timing relative to Supreme Court rulings — and compare the published account against the record. They hold no position in the authority structure and publish outside its control.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, lds_first_presidency).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves, by a single authoritative act, a dilemma no member could solve individually: eternal covenants stood in direct opposition to federal criminal law. The declaration synchronizes the entire community's public surrender of the practice at once, preserves the authority structure that issued the original command, and supplies every member with a common account of why the change occurred.
% TRANSFER_FUNCTION: Moves covenant-disruption losses and theological-reconciliation labor from the presiding quorums to plural families and the general membership; moves observable compliance to federal authorities; returns undiminished interpretive authority and institutional continuity to the presiding quorums.
% ABSENT_VOICES: Plural wives themselves were not seated in any council that decided the matter; they bore separation, stigma, and legal jeopardy for their children, and their objections survive mainly in private diaries and letters. Dissenting priesthood voices were heard and then marginalized rather than accommodated. Both groups stood outside the room where the settlement was drafted and announced.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, federal prosecutions and property seizure resume, plural families face renewed impossible choices between covenant and prison, the statehood path closes, and the community fractures immediately over whether to obey God-as-previously-spoken or Caesar — the authority question the settlement answered would reopen with no resolution mechanism in place.
% FOUNDING_PROBLEM: How can a community that believes God commanded plural marriage by revelation (Section 132, 1843) survive a sovereign that criminalizes that marriage — without conceding that the command was never divine?
% FOUNDING_PROBLEM_CORROBORATION: Contemporary diaries and letters of plural wives, federal court records of the 1880s prosecutions, and the later fundamentalist communions' own testimony all corroborate that the covenant-versus-sovereignty dilemma was live and severe. No source outside the benefiting parties corroborates that the resolution operated by revelation rather than circumstantial compulsion — that attribution rests on the institution's own account and on a vision whose content is privately held.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.58 at interval end) because the settlement transfers real, concentrated losses — covenant disruption for plural families, reconciliation labor for the membership — against a real coordination payoff: institutional survival, civic peace, and a synchronized community response to an otherwise unsolvable dilemma. Suppression (0.62) is a raw structural property, unscaled by power or scope: enforcement ran through church courts, deposition of dissenting apostles, and the 1904 Second Manifesto ratchet, but never approached totality — fundamentalist persistence proves alternatives survived. Theater (0.42) is substantial but not dominant: the published account performs divine spontaneity and inevitability over a documented months-long deliberative and politically timed process, yet the reversal itself was real and consequential, not staged. Accessibility collapse (0.50): alternatives existed throughout — emigration colonies in Mexico and Canada, quiet continuation, eventual schism — but each carried severe spiritual and social costs, so alternatives narrowed without vanishing. Resistance (0.55): sustained minority resistance across four decades, from reluctant signatures to organized successor communions. The measurement series run on one shared time grid; the suppression series traces genuine enforcement-capacity change (raid-era intensity, the 1890 turn, statehood relaxation, the 1904 ratchet), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical documents. From the First Presidency's position the settlement is providential deliverance that vindicates living-oracle authority; from the plural families' position it is abandonment of covenants taught as eternal, borne by the least mobile participants; from the dissenters' position it is institutional apostasy. The identity-lock mechanism for dissenters is ideological: their self-concept, authority claims, and salvation expectations are constituted through the revoked principle, so exit is unthinkable without self-dissolution — remove that fusion and their computed position would soften toward ordinary constrained dissent. The engine derives these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency sits nearest the beneficiary pole: it collects interpretive authority and institutional continuity, with arbitrage-grade control over framing. The quorum shares the benefit but pays reconciliation costs, moderating its derived directionality. Existing plural families sit nearest the target pole: trapped by covenant, kinship, and criminal exposure, they bear the settlement's concentrated losses. Rank-and-file believers are dual-positioned — material beneficiaries (relief, statehood) paying a diffuse coherence cost — placing them mid-range. Dissenting priesthood holders derive near-full-target directionality through identity lock despite organized power. Federal authorities receive the outcome with mobile exit, damping their effective extraction toward the beneficiary side. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct per-seat relationships, and the dual-positioned seats are handled through secondary_role rather than override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a believed-divine command with a hostile sovereign — was live and severe, and the settlement genuinely addressed it; the mandate is not dead, so no zombie flag applies. But the settlement's persistence beyond the instance creates a distinct hazard: the interpretive precedent quietly outlives the crisis that justified it, available for reuse in every future collision between revelation and circumstance. The classification guards against mislabeling in both directions: calling the settlement pure extraction ignores the real coordination achievement (an unsolvable collective dilemma, solved once, centrally); calling it pure coordination ignores who paid — the least mobile believers absorbed the concentrated losses while the presiding quorums emerged with undiminished authority. The tangled-rope designation holds both truths in one structure. The long-run risk is drift toward inertial performance if the narrative becomes pure theater while the precedent operates unexamined; the theater-ratio series is the early-warning instrument for that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_locus_kernel_reading,
    'This story instantiates one reading of the kernel marriage_commitment_reversal — the endogenous reading, in which the reversal''s operative cause is internal revelation reinterpreting God''s will. The sibling reading exogenous_override_reading relocates the cause to external coercion without internal doctrinal revision; the sibling practice_doctrine_gap treats the surviving Section-132/suspended-practice ambiguity as the primary structure. Which causal locus is structurally true, and how does the answer move the victim set?',
    'Comparative analysis of the contemporaneous documentary record (diaries, drafts, counsel correspondence) against the published narrative, read alongside the sibling stories compiled as separate constraints; the disagreement is located specifically at the attribution of the reversal''s cause.',
    'Under the exogenous sibling, the victim set expands to include the truthfulness of the revelatory channel itself and effective extraction rises; under this endogenous reading the victim set centers on theological consistency and covenant disruption. Under the gap sibling, the classification keys on the doctrine-practice residue rather than the reversal event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_locus_kernel_reading, conceptual, 'Kernel-level causal-locus ambiguity: internal revelation versus external coercion versus structural gap, routed as sibling constraints per epsilon-invariance.').

omega_variable(
    visionary_sincerity,
    'Was the September 23 vision sincerely received by Woodruff as he reported it, or was the revelatory framing constructed after the deliberative and political decision had effectively been made?',
    'Documentary forensics on diary contemporaneity, the sequence of declaration drafts, and the consistency of Woodruff''s private accounts with the public narrative.',
    'Sincere reception lowers the theater component and supports treating the narrative as functional rather than performative; post-hoc construction raises theater substantially and pushes the computed type toward the snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(visionary_sincerity, empirical, 'Whether the revelation narrative reflects genuine experience or retrospective framing.').

omega_variable(
    consistency_cost_amortization,
    'Does the theological-consistency cost borne by believers amortize toward zero across generations as the reversal recedes from living memory, or does it persist structurally wherever the precedent is invoked?',
    'Longitudinal analysis of member belief surveys, general-conference discourse corpora, and curriculum treatment of the reversal across successive generations.',
    'Full amortization lowers end-state extraction below the measured 0.58; persistent structural invocation (each new appeal to prophetic reinterpretation reactivating the cost) holds extraction at or above the measured level indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consistency_cost_amortization, empirical, 'Generational distribution and persistence of the theological-consistency burden.').

omega_variable(
    precedent_scope,
    'Does the 1890 settlement establish a general license for prophetic reversal of binding commands under changed circumstances, or is it bounded to this single instance?',
    'Analysis of subsequent invocations: whether later doctrinal changes cite the settlement as precedent, and whether the institution''s own teaching limits its scope.',
    'A general license converts the settlement into standing infrastructure for future reversals, raising long-run extraction well beyond the historical episode; a bounded reading confines the measured extraction to the 1885-1910 window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_scope, conceptual, 'Whether the interpretive precedent is general or instance-bound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1885, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcr_endo_tr_t1885, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1885, 0.15).
narrative_ontology:measurement_basis(mcr_endo_tr_t1885, observed).
narrative_ontology:measurement(mcr_endo_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement_basis(mcr_endo_tr_t1890, observed).
narrative_ontology:measurement(mcr_endo_tr_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1892, 0.28).
narrative_ontology:measurement_basis(mcr_endo_tr_t1892, observed).
narrative_ontology:measurement(mcr_endo_tr_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.32).
narrative_ontology:measurement_basis(mcr_endo_tr_t1896, observed).
narrative_ontology:measurement(mcr_endo_tr_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.38).
narrative_ontology:measurement_basis(mcr_endo_tr_t1904, observed).
narrative_ontology:measurement(mcr_endo_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.42).
narrative_ontology:measurement_basis(mcr_endo_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(mcr_endo_be_t1885, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1885, 0.62).
narrative_ontology:measurement_basis(mcr_endo_be_t1885, observed).
narrative_ontology:measurement(mcr_endo_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.66).
narrative_ontology:measurement_basis(mcr_endo_be_t1890, observed).
narrative_ontology:measurement(mcr_endo_be_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1892, 0.64).
narrative_ontology:measurement_basis(mcr_endo_be_t1892, observed).
narrative_ontology:measurement(mcr_endo_be_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.6).
narrative_ontology:measurement_basis(mcr_endo_be_t1896, observed).
narrative_ontology:measurement(mcr_endo_be_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.63).
narrative_ontology:measurement_basis(mcr_endo_be_t1904, observed).
narrative_ontology:measurement(mcr_endo_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.58).
narrative_ontology:measurement_basis(mcr_endo_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(mcr_endo_su_t1885, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1885, 0.7).
narrative_ontology:measurement_basis(mcr_endo_su_t1885, observed).
narrative_ontology:measurement(mcr_endo_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.76).
narrative_ontology:measurement_basis(mcr_endo_su_t1890, observed).
narrative_ontology:measurement(mcr_endo_su_t1892, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1892, 0.71).
narrative_ontology:measurement_basis(mcr_endo_su_t1892, observed).
narrative_ontology:measurement(mcr_endo_su_t1896, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.6).
narrative_ontology:measurement_basis(mcr_endo_su_t1896, observed).
narrative_ontology:measurement(mcr_endo_su_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.72).
narrative_ontology:measurement_basis(mcr_endo_su_t1904, observed).
narrative_ontology:measurement(mcr_endo_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.62).
narrative_ontology:measurement_basis(mcr_endo_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims. This file authors the endogenous-reinterpretation settlement (revelation reframes God's will; moderate extraction; victim set centered on theological consistency and covenant disruption). The sibling exogenous_override_reading authors a coercion-driven suspension with doctrine untouched — a different epsilon and a victim set expanded to include the revelatory channel itself. The sibling practice_doctrine_gap authors the persistent Section-132/suspended-practice ambiguity as the primary structure. Each story carries its own stable epsilon, stakeholders, and classification; the upstream endogenous reading structurally influences the gap reading because revelation-framed suspension is precisely what leaves doctrine intact while practice ceases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
