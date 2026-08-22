% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority Grounded in Communal Religious Tradition (Communal Autonomy Reading)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story is the communal_autonomy_reading of the marriage_authority
 *   kernel: it treats state-enforced, community-authored personal law as
 *   legitimate coordination against majoritarian imposition, grounded in
 *   communities' own religious tradition. It is a distinct constraint from
 *   the secularist_reading (which treats the same pluralism as a transitional
 *   anomaly), the gender_rights_reading (which treats it as an equality
 *   violation to be dismantled through judicial expansion), the
 *   federalist_millet_reading (which treats it as a deliberate anti-tyranny
 *   consociational design), and the judicial_harmonization_reading (which
 *   treats it as gradually yielding to a constitutional floor via case law).
 *   Each of these is authored as a separate constraint with its own ε and
 *   stakeholder structure; this file speaks only for the communal-autonomy
 *   account of the standing arrangement, not for any of the alternatives or
 *   their preferred end-states.
 *
 * KEY AGENTS:
 *   - religious_leadership: primary agenda-setter and structural beneficiary (institutional/arbitrage) — administers and interprets the personal law code
 *   - community_members_in_good_standing: diffuse beneficiary (moderate/constrained) — receives continuity and recognized family status
 *   - intra_community_dissenters: primary target (powerless/trapped) — bears doctrinal rigidity with no internal amendment channel
 *   - women_seeking_exit_from_personal_law_marriages: primary target (powerless/trapped) — bears asymmetric divorce/custody/maintenance rules
 *   - the_state: co-agenda-setter and analytical observer (institutional/analytical) — enforces without authoring
 *   - uniform_civil_code_advocates: excluded voice (organized/constrained) — structurally outside the amendment process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.42).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.51).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority Grounded in Communal Religious Tradition (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '4edd5585-01c9-4bb1-bda8-8805735ee543').
narrative_ontology:cs_kernel_codification('4edd5585-01c9-4bb1-bda8-8805735ee543', distributed).
narrative_ontology:cs_authority_grounding('4edd5585-01c9-4bb1-bda8-8805735ee543', lineage).
narrative_ontology:cs_interpretation_layer_present('4edd5585-01c9-4bb1-bda8-8805735ee543').
narrative_ontology:cs_reading_relation('4edd5585-01c9-4bb1-bda8-8805735ee543', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4edd5585-01c9-4bb1-bda8-8805735ee543', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4edd5585-01c9-4bb1-bda8-8805735ee543', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('4edd5585-01c9-4bb1-bda8-8805735ee543', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('4edd5585-01c9-4bb1-bda8-8805735ee543', foundational, religious_tradition_as_legitimate_marriage_authority).
narrative_ontology:cs_axiom_status(religious_tradition_as_legitimate_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('4edd5585-01c9-4bb1-bda8-8805735ee543', religious_tradition_as_legitimate_marriage_authority, conventional).
narrative_ontology:cs_axiom('4edd5585-01c9-4bb1-bda8-8805735ee543', foundational, community_consent_required_for_family_law_amendment).
narrative_ontology:cs_axiom_status(community_consent_required_for_family_law_amendment, holdable).
narrative_ontology:cs_axiom_grounding('4edd5585-01c9-4bb1-bda8-8805735ee543', community_consent_required_for_family_law_amendment, conventional).
narrative_ontology:cs_reference_frame('4edd5585-01c9-4bb1-bda8-8805735ee543', post_independence_communal_settlement).
narrative_ontology:cs_drift_state('4edd5585-01c9-4bb1-bda8-8805735ee543', contemporary_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4edd5585-01c9-4bb1-bda8-8805735ee543', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_institutional_continuity).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_exit_from_personal_law_marriages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_members_in_good_standing).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, religious_freedom_as_group_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the personal law code for the community — solemnizes marriages, adjudicates disputes, certifies divorces and inheritance according to religious doctrine. Negotiates directly with the state over the boundary of enforcement versus authorship. Collects social authority, adjudicative fees in some traditions, and institutional legitimacy from being the recognized interpreter of the tradition. Can shape doctrine through interpretive rulings; largely insulated from legislative override because the state defers to community consent for amendments.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary).

% Receive marriages, divorces, and inheritance settlements recognized by both community and state without having to navigate an unfamiliar civil code. Get continuity of identity and practice across generations. Exit is technically available (civil marriage exists) but socially costly — leaving the personal law system can mean leaving the community's recognition network.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, community_members_in_good_standing, beneficiary,
    moderate, biographical, constrained, regional).

% Object to specific doctrinal rules — unequal inheritance shares, unilateral divorce provisions, minimum marriage age interpretations — but have no forum inside the community structure to contest them, since amendment requires community consent controlled by the same leadership. Formal civil exit exists in law but triggers social excommunication, loss of family relationships, and loss of recognition for prior marriages/children in some traditions.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Face asymmetric divorce, custody, and maintenance rules under several personal law codes that differ from the civil code's equality guarantees. Because the state enforces but does not author these norms, they cannot appeal to ordinary legislative reform channels — any change must pass through the same religious leadership whose doctrine they are contesting. State courts sometimes intervene at the margins but generally defer to personal law within its recognized domain.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_seeking_exit_from_personal_law_marriages, payer,
    powerless, immediate, trapped, local).

% Enforces personal law rulings through civil registration, court recognition, and police power, but declines to legislate the content of family law for recognized communities, treating doctrinal content as outside its authorship. Benefits from social peace and reduced majoritarian friction by not forcing uniformity; bears reputational and constitutional-consistency costs when personal law outcomes conflict with equality guarantees elsewhere in its own legal order.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, the_state, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, the_state, observer).

% Argue that state enforcement without state authorship is an abdication that entrenches inequality and communal segmentation. Are not party to personal law amendment processes, which are structured as community-internal; can only act through litigation, legislative advocacy, or public campaigning, none of which currently has purchase on personal law's core content.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, uniform_civil_code_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a religiously and culturally plural population to marry, divorce, and inherit under norms continuous with communal identity and practice, without forcing a single legislature to adjudicate doctrinal disputes across incommensurable traditions — reducing majoritarian conflict over family law content.
% TRANSFER_FUNCTION: Moves interpretive and adjudicative authority over marriage, divorce, and inheritance from the state's ordinary legislative process to community religious leadership; moves the cost of doctrinal rigidity and internal inequality onto members with the least power to contest it, particularly women and internal dissenters, while leadership retains institutional standing and adjudicative control.
% ABSENT_VOICES: Intra-community dissenters and women disadvantaged by specific personal law provisions have no seat in the amendment process, which is structured as community-to-state negotiation mediated by leadership; uniform civil code advocates are excluded from personal law's internal governance entirely and can only act from outside via litigation or legislative pressure that this reading's own logic treats as illegitimate interference.
% DISAPPEARANCE_RATIONALE: If communal religious authority over marriage were withdrawn overnight and replaced with a single civil code, millions of existing marriages, divorces, and inheritance arrangements would face recognition uncertainty; religious leadership would lose adjudicative standing and institutional relevance; some community members would experience this as liberation from unequal rules, others as the loss of a recognized, continuous framework for family life. The rearrangement would be large and contested, not neutral.
% FOUNDING_PROBLEM: In a religiously plural polity, a single legislature imposing one family law code risked being read as majoritarian imposition on minority communities, threatening both social peace and communities' sense of continuity with their own traditions after independence or unification.
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership and many community members attest the founding problem remains live — majoritarian imposition of a single code is still a real risk in their account. Constitutional scholars, gender-rights litigants, and some judicial opinions from outside the beneficiary set attest that the arrangement has calcified into a mechanism that protects leadership authority and internal inequality more than it protects genuine communal continuity, and that the original anti-majoritarian problem could now be addressed through constitutionally-bounded pluralism rather than near-total communal insulation.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.42) is moderate: there is a genuine coordination function (avoiding majoritarian imposition, preserving communal continuity) but it rides alongside real, non-trivial extraction from dissenters and women within personal law systems whose content they cannot contest through ordinary channels. Suppression (0.51) reflects that exit is formally available (civil marriage exists) but carries substantial social and relational cost, which is a real but partial suppression mechanism, not full coercive lock-in — hence the moderate rather than high value. Theater ratio is low-moderate (0.22) because the adjudicative function is largely genuine, not merely performative, though its share of purely symbolic legitimation activity has grown gradually as the arrangement has calcified. All three temporal series share one time grid (T=0..70) so no metric is fabricated at a point the others weren't also authored for.
 *
 * PERSPECTIVAL GAP:
 *   From religious leadership's seat, the arrangement is functioning coordination it stewards on behalf of a community that would otherwise face imposed uniformity. From the seat of intra-community dissenters and women seeking exit, the same structure computes as extraction with no internal recourse, because the amendment gate is controlled by the same authority whose doctrine is being contested. The engine computes both seats from the shared structural data; this reading does not adjudicate which seat is correct — it authors the communal-autonomy account of the standing arrangement honestly, including its extractive residue.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership sits near the full-beneficiary end: it administers the constraint, controls its content, and is functionally insulated from override (arbitrage-grade exit from any accountability pressure). Community members in good standing sit closer to symmetric — real coordination benefit, some diffuse cost from rigidity. Intra-community dissenters and women seeking exit sit near the full-target end: trapped exit options (formal civil alternative exists but is socially near-prohibitive), and no internal channel to alter the rules that bind them. The state occupies an unusual position — it enforces without authoring, giving it institutional power without corresponding accountability for content, which is itself part of what this reading treats as legitimate restraint but the gender_rights_reading treats as abdication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing majoritarian legislative imposition on minority religious communities — was genuinely live at the arrangement's founding and remains partially live today (communities still credibly fear majoritarian codification). But the amendment-consent structure has hardened into a mechanism that also protects leadership's adjudicative monopoly and internal doctrinal content from any reform pressure, including reform originating from within the community itself. Classifying this as rope-with-extractive-residue (rather than either pure rope or pure snare) prevents two errors: treating all internal critique as illegitimate interference with legitimate pluralism (which would erase real victims), and treating the entire arrangement as pure extraction (which would erase the genuine anti-majoritarian coordination function this reading is built around).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_consent_gate_capture,
    'Is the requirement that amendments pass through community consent a genuine safeguard of collective self-determination, or has it been captured by religious leadership as a veto over reforms that would reduce their own adjudicative authority?',
    'Track whether amendment proposals originating from within the community (e.g., from women''s groups or reformist clergy) succeed at comparable rates to proposals favorable to existing leadership authority; a persistent asymmetry would indicate capture.',
    'If captured, the coordination function claimed by this reading is substantially cover for entrenchment, pushing the constraint toward tangled_rope or snare from the dissenter/women seats even while the state-level story remains rope-shaped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_consent_gate_capture, empirical, 'Whether the community-consent amendment gate is genuine self-governance or leadership capture.').

omega_variable(
    exit_cost_measurement,
    'How socially and materially costly is exit to the civil law alternative for a typical dissenting community member, versus a formally available but practically foreclosed option?',
    'Empirical study of outcomes for individuals who have exited personal law systems for civil marriage/divorce — social standing, family relationship continuity, economic consequences — compared across communities and over time.',
    'If exit costs are severe and systematic, suppression is closer to the trapped end of the spectrum than the moderate value authored here suggests, which would push effective extraction upward for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Whether nominally available civil exit functions as a real alternative or a formal fiction.').

omega_variable(
    kernel_framing_alternative,
    'Would this reading''s classification change if the kernel were framed not as ''marriage authority'' but as ''the state''s selective delegation of coercive enforcement power to non-state bodies'' — i.e., focusing on the state''s authorization act rather than the community''s traditional claim?',
    'Compare classification outcomes under a delegation-centered framing (where the state, not the community, is the primary agenda-setter and bears responsibility for outcomes it enforces) against this tradition-centered framing.',
    'A delegation-centered framing would likely shift more responsibility and directionality weight onto the_state seat and reduce the apparent autonomy of religious_leadership, potentially changing the computed type from the state''s own seat even though the community-facing structure is unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Alternative framing: state delegation of coercive power versus communal traditional authority, and whether this changes classification from the state''s seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__communal_autonomy_reading, theater_ratio, 14, 0.14).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__communal_autonomy_reading, theater_ratio, 28, 0.16).
narrative_ontology:measurement(marr_tr_t42, marriage_authority__communal_autonomy_reading, theater_ratio, 42, 0.18).
narrative_ontology:measurement(marr_tr_t56, marriage_authority__communal_autonomy_reading, theater_ratio, 56, 0.2).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__communal_autonomy_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(marr_be_t14, marriage_authority__communal_autonomy_reading, base_extractiveness, 14, 0.32).
narrative_ontology:measurement(marr_be_t28, marriage_authority__communal_autonomy_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(marr_be_t42, marriage_authority__communal_autonomy_reading, base_extractiveness, 42, 0.38).
narrative_ontology:measurement(marr_be_t56, marriage_authority__communal_autonomy_reading, base_extractiveness, 56, 0.4).
narrative_ontology:measurement(marr_be_t70, marriage_authority__communal_autonomy_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(marr_su_t14, marriage_authority__communal_autonomy_reading, suppression_requirement, 14, 0.43).
narrative_ontology:measurement(marr_su_t28, marriage_authority__communal_autonomy_reading, suppression_requirement, 28, 0.45).
narrative_ontology:measurement(marr_su_t42, marriage_authority__communal_autonomy_reading, suppression_requirement, 42, 0.47).
narrative_ontology:measurement(marr_su_t56, marriage_authority__communal_autonomy_reading, suppression_requirement, 56, 0.49).
narrative_ontology:measurement(marr_su_t70, marriage_authority__communal_autonomy_reading, suppression_requirement, 70, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the marriage_authority kernel, each authored as a separate constraint story per the ε-invariance principle. communal_autonomy_reading (this file) authors ε=0.42 for the standing personal-law arrangement as a moderate coordination/extraction hybrid; secularist_reading authors the same standing arrangement as a higher-ε transitional anomaly awaiting elimination; gender_rights_reading authors it as extraction targeting a specific victim class (women) via a judicially-remediable equality violation; federalist_millet_reading authors the fragmentation itself (not the religious grounding) as a low-ε anti-tyranny coordination mechanism; judicial_harmonization_reading authors the boundary as already shifting via case law, with a declining ε over its own interval. None of these five stories average into one ε — each is a distinct, ε-invariant constraint sharing the same underlying institutional terrain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
