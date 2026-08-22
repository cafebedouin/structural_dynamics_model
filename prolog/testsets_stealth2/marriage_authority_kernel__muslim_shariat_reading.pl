% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__muslim_shariat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__muslim_shariat_reading, []).

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
 *   constraint_id: marriage_authority_kernel__muslim_shariat_reading
 *   human_readable: Shariat-Based Marriage and Family Law Authority (Muslim Personal Law Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   Within India's plural family-law system, marriage, divorce, maintenance,
 *   and inheritance for the Muslim community are governed by Shariat-derived
 *   rules applied through community institutions — personal law boards that
 *   articulate orthodoxy, and qazis and darul qaza tribunals that adjudicate
 *   — rather than through a unified civil code. The arrangement performs a
 *   genuine coordination function: it resolves family disputes inside
 *   recognized communal channels, standardizes obligations (dower,
 *   waiting-period maintenance), and sustains identity continuity for a
 *   religious minority living under a constitutional order that promises both
 *   religious freedom and individual equality. The same structure embeds
 *   gender-asymmetric outcomes: divorce initiative historically rested with
 *   the husband (instant forms now statutorily banned), polygyny remains
 *   available to men while prohibited to women, and daughters inherit half of
 *   male siblings' shares. State intervention is contested by the
 *   arrangement's stewards as illegitimate intrusion on religious autonomy.
 *   KEY AGENTS (by structural relationship):
 *   all_india_muslim_personal_law_board: agenda setter
 *   (institutional/identity_locked) — interprets Shariat and defends
 *   jurisdiction; qazis_and_darul_qaza_adjudicators: adjudicative beneficiary
 *   (organized/identity_locked) — runs tribunals and collects fees and
 *   standing; muslim_husbands: primary beneficiary (moderate/constrained) —
 *   holds divorce initiative, polygyny option, larger spousal inheritance
 *   shares; muslim_wives: primary target (powerless/constrained) — bears
 *   divorce insecurity, polygyny exposure, maintenance enforcement burdens;
 *   muslim_daughters: secondary target (powerless/constrained) — receive
 *   half-shares; supreme_court_constitutional_benches: analytical observer
 *   (institutional/analytical) — adjudicate the arrangement's constitutional
 *   limits; muslim_womens_rights_organizations: excluded challenger
 *   (organized/mobile) — outside interpretive deliberation. CONSTRAINT FAMILY
 *   NOTE: this file instantiates ONE reading of the
 *   marriage_authority_kernel. Sibling readings are separate constraints with
 *   their own epsilon and victim sets: the secular_civil_reading's monogamy
 *   and equal-inheritance defaults yield near-zero gendered extraction; the
 *   hindu_codified_reading statutorily abolished bigamy for its population in
 *   1955, yielding lower gendered epsilon than this reading, which retains
 *   polygyny-permissive and half-share inheritance rules — the highest
 *   gendered epsilon among the communal readings. The colloquial label 'who
 *   governs marriage and family law' decomposes into these structurally
 *   distinct claims; the confusion lives in the language, not the structure.
 *
 * KEY AGENTS:
 *   - all_india_muslim_personal_law_board: Agenda setter (institutional/identity_locked) — articulates orthodox positions, issues fatwas, lobbies against civil-code displacement; its organizational reason to exist is the arrangement itself.
 *   - qazis_and_darul_qaza_adjudicators: Adjudicative beneficiary with secondary agenda-setting role (organized/identity_locked) — conduct arbitration, collect fees, hold fiqh-trained standing that flows only from the arrangement.
 *   - muslim_husbands: Primary beneficiary (moderate/constrained) — hold divorce-initiative rights, polygyny option value, and larger inheritance shares; civil-marriage exit exists but carries social cost.
 *   - muslim_wives: Primary target (powerless/constrained) — bear divorce insecurity, polygynous-household competition, half-share inheritance relative to brothers, and dependence on communal or state forums for maintenance.
 *   - muslim_daughters: Secondary target (powerless/constrained) — receive half of male siblings' inheritance shares; the differential compounds across generations; minimal procedural voice.
 *   - supreme_court_constitutional_benches: Analytical observer (institutional/analytical) — struck instant talaq in 2017; reshape the arrangement's boundaries without administering or collecting from it.
 *   - muslim_womens_rights_organizations: Excluded challenger (organized/mobile) — campaign for codified gender-equitable reform from outside the interpretive structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, 0.55).
domain_priors:suppression_score(marriage_authority_kernel__muslim_shariat_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__muslim_shariat_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__muslim_shariat_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__muslim_shariat_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__muslim_shariat_reading, "Shariat-Based Marriage and Family Law Authority (Muslim Personal Law Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__muslim_shariat_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__muslim_shariat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__muslim_shariat_reading, '8d62bf65-5bf4-4a02-a1d9-640a9dca8211').
narrative_ontology:cs_kernel_codification('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', fixed_text).
narrative_ontology:cs_authority_grounding('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', lineage).
narrative_ontology:cs_interpretation_layer_present('8d62bf65-5bf4-4a02-a1d9-640a9dca8211').
narrative_ontology:cs_reading_relation('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', foundational, shariat_supremacy_in_personal_status).
narrative_ontology:cs_axiom_status(shariat_supremacy_in_personal_status, holdable).
narrative_ontology:cs_axiom_grounding('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', shariat_supremacy_in_personal_status, theological).
narrative_ontology:cs_axiom('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', secondary, qualified_scholar_interpretive_authority).
narrative_ontology:cs_axiom_status(qualified_scholar_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', qualified_scholar_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', classical_shariat_personal_status_order).
narrative_ontology:cs_drift_state('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', contemporary_constitutional_india, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d62bf65-5bf4-4a02-a1d9-640a9dca8211', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__muslim_shariat_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, all_india_muslim_personal_law_board).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_qaza_adjudicators).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__muslim_shariat_reading, muslim_husbands).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_wives).
narrative_ontology:constraint_victim(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates orthodox positions on divorce, polygyny, and maintenance; issues fatwas; adjudicates legitimacy claims over who speaks for Shariat application; lobbies legislatures against civil-code displacement. Its organizational identity is constituted by the role of defending the arrangement — dissolving the arrangement dissolves the organization's reason to exist, so exit from that role is not a choice it can make while remaining itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, all_india_muslim_personal_law_board, agenda_setter,
    institutional, generational, identity_locked, national).

% Conduct arbitration of marriage and divorce disputes in community tribunals, charging fees and issuing rulings on separation, maintenance, and dower. Their training, livelihood, and local standing flow from the adjudicative role; rulings lack direct state enforceability but carry communal weight that shapes what parties do. Leaving the role would forfeit both income and the scholarly identity built on it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_qaza_adjudicators, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__muslim_shariat_reading, qazis_and_darul_qaza_adjudicators, beneficiary).

% Hold divorce-initiative rights, may contract polygynous marriages where practiced, and receive larger inheritance shares from spouses' estates than the mirror shares their widows receive. They can register civil marriages instead, but doing so typically costs family approval and community standing, so most remain inside the communal frame while collecting its asymmetric advantages.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_husbands, beneficiary,
    moderate, biographical, constrained, national).

% Bear the arrangement's asymmetric costs: insecurity of unilateral divorce forms (narrowed by statute since 2019 but not eliminated in all forms), competition within polygynous households where they occur, inheritance shares half their brothers', and dependence on communal tribunals or overburdened state forums to enforce maintenance and dower. A negotiated khula exit usually requires forfeiting financial claims; a civil-code exit via the Special Marriage Act exists but typically severs family and community ties, pricing the alternative beyond what most can pay.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_wives, payer,
    powerless, biographical, constrained, national).

% Receive half the share of male siblings under Shariat inheritance division. The differential recurs at every succession event and compounds across generations into durable wealth gaps. They have little procedural voice in the interpretive structure that sets their shares, and their remedy runs through constitutional litigation or statutory reform they do not control.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_daughters, payer,
    powerless, generational, constrained, national).

% Adjudicate whether personal-law practices survive fundamental-rights scrutiny — the 2017 judgment striking instant talaq as arbitrary redrew the arrangement's boundary. They neither administer the arrangement nor collect from it; their rulings reshape what the boards and tribunals may lawfully maintain.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, supreme_court_constitutional_benches, observer,
    institutional, generational, analytical, national).

% Campaign for codified, gender-equitable reform of divorce and inheritance rules from outside the interpretive structure. They are not seated in board deliberations and their demands are answered as external attack rather than internal interpretation; they operate through constitutional litigation, public advocacy, and coalition-building across sects and classes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__muslim_shariat_reading, muslim_womens_rights_organizations, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__muslim_shariat_reading, muslim_husbands).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__muslim_shariat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified communal framework governing marriage, divorce, maintenance, dower, and inheritance for the Muslim community: disputes resolve through recognized religious adjudicators instead of fragmented custom or adversarial state litigation, obligations are standardized across a religiously defined population, and family-status continuity is maintained under minority conditions.
% TRANSFER_FUNCTION: Moves adjudicative authority and family-law decision rights from individuals — especially wives — and state courts to husbands and the religious-adjudicator class; moves material value through inheritance divisions and divorce outcomes asymmetrically toward male kin; moves interpretive legitimacy from constitutional equality guarantees to scriptural interpretation mediated by the boards.
% ABSENT_VOICES: Muslim women seeking equal divorce and inheritance terms sit outside board deliberation, which historically seated no women; secular constitutional egalitarians are excluded from the interpretive process by construction; reformist theologians proposing internal ijtihad are marginalized as inauthentic. They would object that unanimity in favor of the arrangement reflects who was never invited into the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, marriage registration, divorce, maintenance, and inheritance for the community would reorganize under the civil code: adjudication would migrate to state courts, inheritance shares would equalize, polygyny would end, and the boards and qazi tribunals would lose their function — a large-scale rearrangement of family life, communal representation, and minority politics, not a continuation of the status quo by other means.
% FOUNDING_PROBLEM: Under colonial rule, Muslim family status was governed through Anglo-Muhammadan law filtered by British judges and variable local customs; the movement behind the 1937 Shariat Application Act sought to displace custom and judicial discretion with uniform classical Shariat application to marriage, divorce, and inheritance, securing communal self-governance of personal status.
% FOUNDING_PROBLEM_CORROBORATION: Colonial legislative history and the documented political advocacy of 1930s Muslim organizations corroborate the founding problem from outside the current benefiting parties; the Supreme Court's competing opinions in the 2017 litigation attest the live dispute over whether that purpose survives or has become jurisdictional defense; Muslim women's organizations testify from outside the beneficiary set that the arrangement now functions substantially to preserve male prerogatives. No attestation of the founding problem's current status comes solely from the boards or their adjudicators.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__muslim_shariat_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__muslim_shariat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__muslim_shariat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__muslim_shariat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__muslim_shariat_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__muslim_shariat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__muslim_shariat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the gendered transfer is substantial (inheritance halves for daughters, divorce-initiative asymmetry, polygyny exposure) but bounded — dower is the wife's property, waiting-period maintenance is obligatory, instant-talaq forms were statutorily eliminated in 2019, and part of the arrangement's uptake is affirmative rather than imposed. Suppression is authored at 0.58 as a raw structural property (the engine scales only extractiveness, by directionality and scope): community enforcement, ostracism risk for civil-code exits, and forum control, elevated above its pre-1985 baseline by the defensive mobilization that followed the maintenance ruling of 1985 and only partially relaxed by the 2017-2019 interventions. Theater ratio at 0.40 reflects a real adjudicative function increasingly mixed with jurisdictional defense — a growing share of board activity is performative assertion of authority against constitutional scrutiny rather than dispute resolution, though it remains below the proxy-replacement threshold. Accessibility collapse at 0.45: alternatives (civil marriage, statutory remedies, constitutional litigation) exist and are used, but carry heavy social cost, so alternatives are narrowed rather than collapsed. Resistance at 0.60: sustained litigation, organized women's movements, internal reformist voices, and state-level civil-code experimentation meet the arrangement continuously. The measurement series run on one shared time grid (every tracked metric authored at every examined point); the trajectories are event-stepped (1937 enactment, 1985-86 mobilization, 2017 judgment, 2019 statute) rather than cyclic — no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the payer seats (wives, daughters), the arrangement is experienced as enforced asymmetry: exit exists on paper but is priced in family and community ties. From the beneficiary seat (husbands), the same rules are experienced as divinely sanctioned entitlement and familial obligation. From the agenda-setter seats (boards, qazis), the arrangement is a sacred trust under siege — institutional identity fusion makes jurisdictional defense indistinguishable from the organization's selfhood, so the identity-lock mechanism is professional-institutional for the adjudicator class and ideological for adherents who experience critique of the rules as critique of revelation itself. From the observer seat (constitutional benches), the arrangement is a rights conflict to be adjudicated. If the identity frame broke — if boards reconstituted as service providers rather than jurisdictional guardians — the payer-seat experience would converge toward ordinary legal-service consumption and the extraction profile would drop sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: muslim_husbands sit near the beneficiary end (the asymmetry subsidizes them; constrained exit keeps them engaged but subsidized); muslim_wives and muslim_daughters sit near the target end (they bear the transfer, and constrained exit — civil marriage available but socially costly — traps them nearer the full-target position than mobile agents would sit); the boards and qazis sit nearest the beneficiary pole (they run the arrangement and collect standing and fees from it, with identity-locked exit amplifying their investment in its persistence); the constitutional benches hold the analytical seat with no directional stake. National spatial scope modestly amplifies effective extraction for targets by raising verification difficulty of informal tribunal practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Reading the arrangement as pure rope (the steward seat's framing — voluntary communal law) erases the identifiable victims and the enforcement machinery that holds the asymmetry in place. Reading it as pure snare (the strongest reformist framing — patriarchal imposition wearing religious dress) erases the genuine coordination function, the standardized protections, and the documented affirmative uptake. The genealogy interview locates the drift: the founding problem (replacing colonial judicial discretion and custom-variants with uniform Shariat application, securing communal self-governance) is contested rather than dead — uniform application was achieved, but the arrangement's center of gravity has shifted from adjudication toward jurisdictional defense, which is exactly what the rising theater_ratio traces. The status-by-verdict pairing (contested founding problem, world_rearranges disappearance) flags the arrangement as partially mandate-surviving rather than resolved-mandatrophy: it still does real work, but an increasing fraction of its activity defends its own persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the marriage_authority_kernel (muslim_shariat_reading). Would adoption of a sibling reading — hindu_codified_reading, christian_canonical_reading, parsi_communal_reading, or secular_civil_reading — change the structural classification, and where exactly is the disagreement located?',
    'Constitutional adjudication and democratic deliberation over the source of family-law authority; the disagreement is located in the source-of-authority premise itself (divine text via qualified interpreters versus codified statute versus civil individual-right code), which determines the adjudicative structure and the victim set.',
    'Adopting the secular_civil_reading would relocate adjudication to state courts with monogamy and equal-inheritance defaults, collapsing this reading''s beneficiary/victim structure to near-nil; adopting another communal reading would substitute a different victim set. This story''s epsilon and classification are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    voluntary_uptake_vs_imposition,
    'Survey evidence indicates substantial affirmative preference for personal-law retention among community members, including many women. Does voluntary uptake reduce effective extraction below the measured gendered transfer, or does expressed preference itself track constrained awareness of alternatives?',
    'Choice-quality studies comparing preferences in contexts with genuinely accessible civil exit (awareness campaigns, cost-free Special Marriage Act access) against preferences where exit is nominal only.',
    'High-quality voluntary uptake would push the arrangement toward the rope side of the tangled-rope boundary; findings that preference persists only under constrained awareness would push toward the snare side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_uptake_vs_imposition, empirical, 'Whether affirmative community preference offsets the measured gendered extraction.').

omega_variable(
    internal_fiqh_pluralism,
    '''Shariat'' is not one rule-set: jurisprudential schools differ on divorce forms, khula terms, and maintenance scope, and reformist ijtihad circulates alongside board orthodoxy. Which internal reading actually governs adjudication, and does the boards'' interpretive monopoly reflect community consensus or institutional gatekeeping?',
    'Documentary audit of which school''s norms darul qaza rulings actually apply, plus uptake tracking of reformist fatwas and women-negotiated khula settlements.',
    'If reformist interpretations govern in practice, epsilon falls and the coordination function strengthens; if board orthodoxy monopolizes interpretation, the measured extraction and suppression stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_fiqh_pluralism, conceptual, 'Which internal jurisprudential variant governs, and whether the interpretive monopoly is consensual.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic dependency, family pressure, social ostracism, exclusion from forums) or internalized (piety-framed belief that questioning divine law is itself transgressive)?',
    'Post-exit suppression trajectory: track women who complete civil-code exits — if suppression symptoms persist after structural barriers are removed, a substantial internalized component is established.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after formal reform; statutory fixes alone would not release constrained targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in a communal-religious constraint.').

omega_variable(
    statutory_carveout_durability,
    'Will the 2019 statutory ban on instant talaq and emerging state-level civil-code experiments generalize into durable erosion of the asymmetric practices, or remain isolated carve-outs around a stabilized personal-law core?',
    'Track compliance rates, board adaptation of divorce doctrine, further state civil-code adoptions, and parliamentary movement on a uniform civil code.',
    'Generalization continues the measured extractiveness decline; carve-out stabilization freezes epsilon near its current value and entrenches the tangled-rope configuration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_carveout_durability, empirical, 'Durability of statutory erosion of the asymmetric core practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__muslim_shariat_reading, 0, 88).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t18, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(marr_tr_t36, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement(marr_tr_t48, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 48, 0.35).
narrative_ontology:measurement(marr_tr_t66, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 66, 0.33).
narrative_ontology:measurement(marr_tr_t80, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement(marr_tr_t82, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 82, 0.42).
narrative_ontology:measurement(marr_tr_t88, marriage_authority_kernel__muslim_shariat_reading, theater_ratio, 88, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t18, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(marr_be_t36, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 36, 0.59).
narrative_ontology:measurement(marr_be_t48, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(marr_be_t66, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 66, 0.64).
narrative_ontology:measurement(marr_be_t80, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(marr_be_t82, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 82, 0.56).
narrative_ontology:measurement(marr_be_t88, marriage_authority_kernel__muslim_shariat_reading, base_extractiveness, 88, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t18, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(marr_su_t36, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(marr_su_t48, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(marr_su_t66, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 66, 0.61).
narrative_ontology:measurement(marr_su_t80, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(marr_su_t82, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 82, 0.59).
narrative_ontology:measurement(marr_su_t88, marriage_authority_kernel__muslim_shariat_reading, suppression_requirement, 88, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__muslim_shariat_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__muslim_shariat_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the marriage_authority_kernel: the single colloquial question 'from where does marriage/family law authority derive?' resolves into five structurally distinct readings, each with its own stable epsilon, adjudicative structure, and victim set. This reading (muslim_shariat_reading) links to all four siblings; the upstream/downstream pressure runs chiefly between this reading and secular_civil_reading, whose generalization each blocks politically while neither logically eliminates the other — hybrid state-enacted applications of Shariat-derived rules demonstrate that a single framework can hold blended forms, which is why the relation is structural influence rather than foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
