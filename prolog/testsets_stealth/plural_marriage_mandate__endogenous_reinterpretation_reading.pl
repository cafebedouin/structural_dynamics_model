% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Post-Manifesto Plural Marriage Suspension Regime (Endogenous Reinterpretation Reading)
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   This story authors the endogenous reading of the plural-marriage kernel
 *   as a clean, epsilon-invariant constraint: the standing arrangement under
 *   contest is the post-1890 regime in which contracting plural marriage is
 *   prohibited by prophetic declaration, enforced through recommend
 *   interviews and church courts, while the underlying doctrine remains
 *   canon. Within this reading the declaration is genuine revelation
 *   suspending — not abrogating — the practice to preserve the church's
 *   salvific mission; the arrangement therefore presents as covenantal
 *   coordination around a new directive, and the claimed type is rope from
 *   this seat. The authored metrics describe the arrangement's actual
 *   operation: real enforcement machinery, real disciplinary casualties, and
 *   a documented enforcement ratchet after 1904. Claim and metrics are
 *   independent facts; where the engine's per-seat computations diverge from
 *   the rope claim, that divergence is the measurement the corpus exists to
 *   take. KEY AGENTS (by structural relationship): see key_agents. The
 *   committer structure — sibling readings and what they would change — is
 *   routed to the omega variables, not folded into this constraint.
 *
 * KEY AGENTS:
 *   - first_presidency_and_quorum_of_twelve: Agenda setter (institutional/constrained) — issues and administers the directive
 *   - lds_church_corporate_institution: Primary beneficiary (institutional/trapped) — collects legal survival, restored property, statehood
 *   - lds_church_membership: Beneficiary with payer drag (organized/identity_locked) — trades the practice for security and temple access
 *   - post_manifesto_plural_marriage_practitioners: Primary target (moderate/identity_locked) — bears excommunication and revoked sealings
 *   - dissenting_apostles_taylor_and_cowley: Same-office target (powerful/identity_locked) — resigns or is dropped rather than comply
 *   - fundamentalist_movement_leaders: Expelled holders of the original reading (powerless/identity_locked) — organize outside
 *   - us_federal_government: External enforcer turned observer (institutional/mobile) — supplies the coercive backdrop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.54).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Post-Manifesto Plural Marriage Suspension Regime (Endogenous Reinterpretation Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(plural_marriage_mandate__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '2a7e3514-ec77-4d23-8034-a3a9c7647b5c').
narrative_ontology:cs_kernel_codification('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', fixed_text).
narrative_ontology:cs_authority_grounding('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', lineage).
narrative_ontology:cs_interpretation_layer_present('2a7e3514-ec77-4d23-8034-a3a9c7647b5c').
narrative_ontology:cs_reading_relation('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', plural_marriage_mandate__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', plural_marriage_mandate__institutional_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', foundational, manifesto_is_binding_divine_revelation).
narrative_ontology:cs_axiom_status(manifesto_is_binding_divine_revelation, holdable).
narrative_ontology:cs_axiom_grounding('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', manifesto_is_binding_divine_revelation, theological).
narrative_ontology:cs_axiom('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', foundational, eternal_principle_temporally_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(eternal_principle_temporally_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', eternal_principle_temporally_suspended_not_abrogated, theological).
narrative_ontology:cs_axiom('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', secondary, living_prophet_directs_church_practice).
narrative_ontology:cs_axiom_status(living_prophet_directs_church_practice, holdable).
narrative_ontology:cs_axiom_grounding('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', living_prophet_directs_church_practice, theological).
narrative_ontology:cs_reference_frame('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', prophetic_continuity_framework).
narrative_ontology:cs_drift_state('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', contemporary_post_manifesto_generations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2a7e3514-ec77-4d23-8034-a3a9c7647b5c', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_corporate_institution).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, first_presidency_and_quorum_of_twelve).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_membership).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, post_manifesto_plural_marriage_practitioners).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, dissenting_apostles_taylor_and_cowley).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_movement_leaders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 declaration as a binding prophetic directive, administers temple recommend interviews asking about plural-marriage compliance, and convenes disciplinary councils for members who contract new plural marriages. Before 1890 its members faced imprisonment, disfranchisement, and asset seizure personally; afterward they secure the institution's legal footing. Exiting would mean dissolving their own callings and the authority structure they occupy.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, first_presidency_and_quorum_of_twelve, agenda_setter,
    institutional, generational, constrained, global).

% Holds the church's property, temples, and legal standing. Under the Edmunds-Tucker Act its corporate charter was dissolved, its property seized, and Utah statehood withheld; the compliance path opened by the declaration leads to the 1893 congressional petition restoring church property, Utah statehood in 1896, and uninterrupted temple operation and missionary expansion. It cannot exit its own history or relocate its identity.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_corporate_institution, beneficiary,
    institutional, generational, trapped, global).

% Ordinary members trade a practice taught for two generations as essential to exaltation for legal security, reopened temples, and an expanding missionary field. Compliance is elicited through testimony, covenantal obligation, and recommend interviews; leaving means losing community, kinship networks, and the salvation framework their lives are built around. Many quietly grieve the surrendered principle; most sustain the directive in public conference votes.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_membership, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_membership, payer).

% Members who contract additional marriages or maintain plural households after enforcement hardens. They face stake interviews, disciplinary councils, excommunication, and revocation of sealings and temple access. Remaining inside costs them standing and fellowship; leaving costs them the community and the salvation framework their identity is fused with.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, post_manifesto_plural_marriage_practitioners, payer,
    moderate, biographical, identity_locked, regional).

% Two apostles who performed or defended post-declaration plural marriages and refuse to denounce the practice under questioning in the 1904-1906 hearings. Rather than comply, one resigns and the other is dropped from the Quorum; both face later discipline. They hold the same office as compliant colleagues yet bear the arrangement's costs personally, because their conviction leaves them unable to sign the required statements.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, dissenting_apostles_taylor_and_cowley, payer,
    powerful, biographical, identity_locked, national).

% Men such as Lorin C. Woolley and his associates who assert that the original authority to solemnize plural marriage was never withdrawn, organize outside the church after excommunication, and recruit from families with plural-marriage heritage. Expelled from the councils where the directive is administered, they build parallel institutions at the margins and carry the movement's legal and social costs with little recourse.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_movement_leaders, excluded,
    powerless, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_movement_leaders, payer).

% Congress and the federal courts supply the coercive backdrop: the Edmunds-Tucker Act dissolves the church's corporation, seizes its property, disfranchises members, and criminalizes new plural marriages. After 1890 it verifies conformity through Utah statehood conditions and Senate committee hearings, then withdraws from direct involvement once compliance is established.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__endogenous_reinterpretation_reading, us_federal_government, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__endogenous_reinterpretation_reading, lds_church_corporate_institution).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Realigns member marriage conduct with United States law under a single prophetic directive, ending the legal siege of asset seizure, imprisonments, and disfranchisement, and restoring uniform access to temple ordinances and missionary work across the membership.
% TRANSFER_FUNCTION: Moves the cost of abandoning plural marriage onto dissenting practitioners (excommunication, revoked sealings) and onto the general membership (surrender of a principle taught as eternally necessary), while moving legal recognition, restored property, statehood, and institutional continuity to the church and its governing bodies.
% ABSENT_VOICES: Continuing practitioners and the nascent fundamentalist leadership are progressively removed from church councils through discipline and excommunication; women living in plural marriage held no formal seat in the decision-making bodies, though a few testified before the Senate committee. After 1904 the holders of the original reading survive inside the church only as an expelled margin.
% DISAPPEARANCE_RATIONALE: If the compliance arrangement vanished overnight and plural marriage resumed, federal prosecution would reignite, church property would again be exposed to seizure, temples would close, the Utah political settlement would unravel, and the missionary program would collapse abroad — the entire institutional configuration of the twentieth-century church depends on it.
% FOUNDING_PROBLEM: How the church can survive as a legal entity — retaining its temples, property, voting rights, and territorial settlement — while a core religious practice exposes it to destruction under federal law.
% FOUNDING_PROBLEM_CORROBORATION: The text of the Edmunds-Tucker Act, federal court dockets, and Senate hearing transcripts corroborate the coercive founding problem from outside the benefiting parties; non-Mormon historians of American religious history attest that the 1890 declaration resolved an existential legal crisis. No source outside the institution attests that the problem is dead — the church's continuing dependence on legal conformity is visible in its ongoing corporate posture.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.54 at interval end): costs concentrate catastrophically on a dissenting minority while a diffuse sacrifice (surrender of a cherished principle) spreads across the general membership, against large coordination gains. Suppression is high (0.72) because persistence depends on active machinery — recommend interviews, disciplinary councils, and the 1904 Second Manifesto's explicit excommunication threat — not on voluntary consensus alone. Theater is low-moderate (0.24): the document genuinely functioned (property restored 1893, statehood 1896, temples reopened), but the 1890-1904 gap between public proclamation and continued private solemnizations, and the open-ended 'temporal' framing carried forward by generations who never practiced, contribute performative elements. The temporal arc on one shared grid (t=0,8,16,24,32,40 mapping 1890-1930): theater peaks early while compliance is publicly proclaimed but privately evaded, collapses after 1904 when enforcement turns real; suppression ratchets sharply through the Second Manifesto and the 1906-1911 purges, then plateaus as steady-state enforcement against fundamentalism; extraction climbs gradually as the dissenting class accumulates losses and the doctrine-practice gap widens.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the leadership seat the arrangement is covenantal coordination: a directive from God, sacrifice sanctified, institution preserved. From the practitioner and fundamentalist seats the same structure operates as betrayal enforced by discipline — the original reading's holders excommunicated for maintaining what they were taught was eternal. The membership seat is genuinely mixed: security and temple access received, a core principle surrendered. The sharpest same-level divergence is inside the Quorum itself: compliant apostles and Taylor/Cowley held identical office and standing, yet the constraint-specific factor (their conduct and conviction regarding post-1890 marriages) placed one group in the coordinated beneficiary class and the other in the disciplined target class.
 *
 * DIRECTIONALITY LOGIC:
 *   The corporate institution and the presidency sit near the beneficiary end: they collect survival, property, and legal recognition, and administer the rules. The general membership derives low-to-mid directionality — declared beneficiary with a real payer drag from the surrendered principle. Practitioners, the dissenting apostles, and fundamentalist leaders sit near the target end; their identity_locked exit pushes them toward the full-target position, since exit means losing the community and salvation framework their identities are fused with. A directionality override is declared for the powerful atom: the derivation from power alone would read the dissenting apostles as insulated, but their power was office-derived and forfeited upon dissent — their actual structural position is full target (d 0.85). The federal government is an observer outside the directionality economy; its coercive pressure is the occasion this reading interprets as the circumstance of revelation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional survival under existential legal threat) remains live and the disappearance verdict is world_rearranges, so no zombie/capture flag arises: the arrangement still solves the problem it was built for. The rope claim preserves the genuine coordination function — survival coordination was real and large — from being mislabeled as pure extraction, while the declared victims keep it from being mislabeled as harmonious coordination without casualties. The theater series is the watch variable: if the founding problem ever died (a permanently changed legal environment) while the 'temporal suspension' framing persisted rhetorically, the arrangement would drift toward inertial, theatrically maintained persistence — the rising theater tail after t=24 is the earliest signal of that trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the plural_marriage_mandate kernel; what structural differences would the sibling readings (exogenous_override_reading, institutional_pragmatism_reading) produce if instantiated?',
    'Generate and compare the sibling stories: classify each reading''s own epsilon, beneficiary/victim sets, and type. The disagreement localizes in the normative status of the 1890 declaration — revelation versus coercion versus strategy.',
    'Under the exogenous reading the federal government enters as extractor and the whole membership as coerced victim, raising epsilon sharply; under the pragmatism reading theater_ratio rises and gains concentrate in the leadership seat. This story''s rope claim holds only within the endogenous frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three rival readings of the plural-marriage kernel.').

omega_variable(
    temporal_suspension_indefiniteness,
    'Is the suspension genuinely ''temporal'' — awaiting divine resumption — or has it become permanent in practice while the temporal framing persists rhetorically?',
    'Track official statements across generations for any resumption language; observe whether the doctrine (canon status of section 132) and the practice (universal monogamy teaching) continue to diverge without convergence.',
    'If permanent-in-practice, the temporal framing is performative maintenance, theater_ratio trends upward, and the arrangement drifts toward inertial persistence; if genuinely provisional, the coordination framing stays accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_suspension_indefiniteness, empirical, 'Whether ''temporal suspension'' is operative or rhetorical.').

omega_variable(
    revelation_authenticity_underdetermination,
    'Can the genuineness of the 1890 revelatory experience be established from evidence independent of the institution''s own later testimony?',
    'Contemporaneous sources outside later institutional framing: Wilford Woodruff''s 1889-1890 diary entries, contemporaneous correspondence, and the drafting history of the declaration.',
    'Strong independent attestation supports this reading''s coordination classification; thin attestation shifts evidential weight toward the sibling readings and raises the computed extraction attributable to the enforcement apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity_underdetermination, empirical, 'Evidential status of the revelation claim.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is member compliance with the suspension sustained by structural enforcement (interviews, church courts) or by internalized covenantal duty that would persist if enforcement relaxed?',
    'Observe compliance trajectories where enforcement capacity is minimal (the modern era): if recommend-interview questions soften without measurable compliance decay, the internalized share dominates.',
    'If internalized, effective suppression exceeds the structural measure and persists beyond the enforcement machinery; the omega splits the scalar suppression into mechanisms the single metric cannot distinguish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of measured suppression: structural versus internalized.').

omega_variable(
    membership_net_position,
    'Are rank-and-file members net beneficiaries (security, temples, missions) or net payers (surrendered principle, covenantal whiplash) once indirect effects are counted?',
    'Welfare comparison using period sources: member testimony in the Smoot hearings, emigration and activity rates 1890-1910, and temple attendance recovery curves.',
    'A net-beneficiary finding stabilizes the coordination computation; a net-payer finding pushes the membership seat toward target directionality and the overall type toward the hybrid coordination/extraction category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_net_position, empirical, 'Net directional position of the general membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmm_endogenous_tr_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pmm_endogenous_tr_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(pmm_endogenous_tr_t16, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(pmm_endogenous_tr_t24, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(pmm_endogenous_tr_t32, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(pmm_endogenous_tr_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 40, 0.24).

% Extraction over time
narrative_ontology:measurement(pmm_endogenous_be_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pmm_endogenous_be_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(pmm_endogenous_be_t16, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(pmm_endogenous_be_t24, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(pmm_endogenous_be_t32, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(pmm_endogenous_be_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(pmm_endogenous_su_t0, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pmm_endogenous_su_t8, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(pmm_endogenous_su_t16, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(pmm_endogenous_su_t24, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(pmm_endogenous_su_t32, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(pmm_endogenous_su_t40, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto' covers one event read three ways; per the epsilon-invariance principle each reading is a separate constraint with its own epsilon, beneficiaries, and victims. This file instantiates the endogenous reading (coordination claim, moderate extraction, enforcement ratchet). Siblings: exogenous_override_reading (coercion account — higher epsilon, federal government as extractor, entire membership as coerced) and institutional_pragmatism_reading (strategy account — elevated theater, gains captured in the leadership seat). Family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__endogenous_reinterpretation_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
