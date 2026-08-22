% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Mutual Recognition of Dual Legitimacy with 1967 Boundaries as Compromise Framework
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The framework under examination is the mutual-recognition settlement
 *   architecture: both peoples' legitimacy dating from 1948 is accepted, the
 *   1967 line serves as the partition basis, the refugee right of return is
 *   realized inside the Palestinian state rather than inside Israel, and
 *   joint security cooperation replaces zero-sum competition. It has
 *   organized official diplomacy since Madrid and Oslo, carries broad
 *   international endorsement, and has never been consummated. KEY AGENTS (by
 *   structural relationship): international_mediating_powers — agenda-setter
 *   (institutional/arbitrage) — drafts and conditions the framework;
 *   israeli_state_institutions — primary beneficiary with payer exposure
 *   (institutional/constrained) — collects recognition and security, owes
 *   withdrawal; palestinian_authority_institutions — beneficiary with heavy
 *   payer exposure (organized/trapped) — holds the statehood promise and the
 *   compliance burden; regional_arab_states — secondary beneficiary
 *   (institutional/mobile) — collect normalization dividends;
 *   palestinian_refugee_diaspora — primary target (powerless/identity_locked)
 *   — the return limitation lands on them; west_bank_settler_communities —
 *   secondary target (organized/identity_locked) — the boundary endpoint
 *   removes them; hamas_rejectionist_factions — excluded seat whose
 *   suppression the framework's security pillar performs;
 *   international_legal_community — analytical observer. The claim/metric gap
 *   is deliberate: the framework is CLAIMED as tangled_rope on structural
 *   grounds while the metrics describe its actual operation independently —
 *   the engine measures the divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.66).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Mutual Recognition of Dual Legitimacy with 1967 Boundaries as Compromise Framework").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '4548dfdf-928d-49d2-b65e-58f62f9cb8da').
narrative_ontology:cs_kernel_codification('4548dfdf-928d-49d2-b65e-58f62f9cb8da', fixed_text).
narrative_ontology:cs_authority_grounding('4548dfdf-928d-49d2-b65e-58f62f9cb8da', lineage).
narrative_ontology:cs_interpretation_layer_present('4548dfdf-928d-49d2-b65e-58f62f9cb8da').
narrative_ontology:cs_reading_relation('4548dfdf-928d-49d2-b65e-58f62f9cb8da', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('4548dfdf-928d-49d2-b65e-58f62f9cb8da', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_axiom('4548dfdf-928d-49d2-b65e-58f62f9cb8da', foundational, mutual_recognition_of_dual_1948_legitimacy).
narrative_ontology:cs_axiom_status(mutual_recognition_of_dual_1948_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4548dfdf-928d-49d2-b65e-58f62f9cb8da', mutual_recognition_of_dual_1948_legitimacy, deontological).
narrative_ontology:cs_axiom('4548dfdf-928d-49d2-b65e-58f62f9cb8da', secondary, partition_along_1967_lines_as_settlement_basis).
narrative_ontology:cs_axiom_status(partition_along_1967_lines_as_settlement_basis, holdable).
narrative_ontology:cs_axiom_grounding('4548dfdf-928d-49d2-b65e-58f62f9cb8da', partition_along_1967_lines_as_settlement_basis, conventional).
narrative_ontology:cs_reference_frame('4548dfdf-928d-49d2-b65e-58f62f9cb8da', dual_recognition_1967_compromise_frame).
narrative_ontology:cs_drift_state('4548dfdf-928d-49d2-b65e-58f62f9cb8da', contemporary_post_oslo_collapse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4548dfdf-928d-49d2-b65e-58f62f9cb8da', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, regional_arab_states).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_mediating_powers).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the negotiation architecture (Madrid, Oslo sponsorship, the Quartet, Annapolis), draft the framework documents, and condition aid and diplomatic standing on adherence to the mutual-recognition track. When a track collapses they can disengage and pivot to another doctrine at low cost to themselves; the parties cannot leave the geography.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_mediating_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive formal recognition of sovereign legitimacy, normalized regional relations, and security cooperation from the framework. Pay by accepting the 1967 line as the settlement basis, which commits them to withdrawing from territory and absorbing the cost of relocating communities beyond it. In the interim they administer the territory and control facts on the ground. Leaving the framework entirely would cost them international standing, but geography means they cannot exit the relationship itself.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions, payer).

% Receive the framework's promise of statehood, recognition, and a territorial endgame, which is the sole internationally sanctioned path to sovereignty available to them. Pay through binding security-coordination obligations, public concession on the refugee return question, and steady erosion of legitimacy among their own constituents for delivering neither state nor return. There is no alternative patron and no rival sovereignty track to defect to.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions, beneficiary,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_institutions, payer).

% Collect stability dividends and normalization opportunities from a consummated framework, and supplied its principal external endorsement through the 2002 Arab Peace Initiative. Their exposure is diplomatic rather than existential; if the framework stalls they can redirect alignment toward other patrons and doctrines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_arab_states, beneficiary,
    institutional, generational, mobile, regional).

% Camp and exile populations in Lebanon, Syria, Jordan, and elsewhere whose claim to return to homes inside Israel is extinguished by the framework, with remedy confined to citizenship in the new Palestinian state. The return claim is constitutive of their collective identity across generations; abandoning it is not a choice available to them. They hold no vote in any forum that decides the framework; representation runs indirectly through negotiating institutions they do not control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, identity_locked, regional).

% Communities beyond the Green Line whose presence past the 1967 line is the thing the framework's boundary provision terminates, by evacuation or by incorporation into a Palestinian state. They are ideologically committed to remaining and cannot voluntarily abandon the settlement project. Their political weight inside the stronger party lets them delay implementation indefinitely, but it does not change the framework's endpoint for them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, west_bank_settler_communities, payer,
    organized, generational, identity_locked, regional).

% Reject mutual recognition with the stronger party outright and are barred from the negotiation architecture the framework runs on. Were they seated they would veto the deal; their exclusion is maintained by the framework's own security-cooperation pillar, enforced by both the Palestinian security services and the stronger party.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, hamas_rejectionist_factions, excluded,
    organized, biographical, trapped, regional).

% Scholars, advisory bodies, and human-rights machinery that assess the framework against international law, producing opinions on occupation legality, annexation, and the status of the 1967 line. They neither collect from nor pay into the arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_legal_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of two peoples holding legitimate self-determination claims to the same territory: mutual recognition converts a zero-sum sovereignty contest into two coexisting states, the 1967 line supplies a shared reference for partition that neither side must first concede as a moral verdict, and joint security cooperation replaces competitive violence as the mechanism for handling spoilers.
% TRANSFER_FUNCTION: Moves concessions asymmetrically between the parties: the refugee population transfers its claim to return inside the stronger state (remedy confined to the new Palestinian state); the stronger party transfers formal recognition of Palestinian statehood and territory beyond the Green Line; the weaker party transfers security assurance and suppression of rejectionist factions to the stronger party; mediating powers move aid, recognition, and diplomatic standing to whichever party complies.
% ABSENT_VOICES: The refugee diaspora bearing the framework's largest concentrated cost has never voted on it directly; camp populations in Lebanon and Syria are represented only through institutions they do not control. Rejectionist factions on both sides are excluded by design. Jewish communities displaced from Arab countries are silent parties whose parallel claims are implicitly traded away by the bilateral framing. All of these voices sit outside the conference rooms where the framework's terms are drafted.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, the Oslo-era architecture built on it (security coordination, aid flows, the PA's mandate, regional normalization sequencing) would lose its organizing basis; annexation pressures, insurgent dynamics, and interstate realignment would resume in open form. A large set of existing arrangements depends on the framework continuing to define the horizon.
% FOUNDING_PROBLEM: After 1967, two peoples each holding a legitimate national claim governed the same land under military occupation with no agreed formula for sharing it; the framework was built to convert that standoff into mutual recognition and partition before permanent annexation or expulsion foreclosed either outcome.
% FOUNDING_PROBLEM_CORROBORATION: UN Security Council Resolution 242 articulated the land-for-peace formula in 1967, before the current beneficiary configuration existed, and the 2002 Arab Peace Initiative offered full regional recognition contingent on withdrawal — both external attestations that the founding problem remains live and that these terms are the recognized currency for it. Refugee advocacy organizations outside the beneficiary set corroborate the problem's liveness while dissenting on the framework's limitation of return, which is corroboration of the diagnosis and documented disagreement with the prescription.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.60) and rising across the interval: the return limitation extinguishes the claim of the weakest party in the arrangement, and the framework's long non-consummation has let the stronger party accumulate the very facts (settlement footprint) that the boundary term was supposed to reverse. Suppression (0.66) reflects that persistence depends on active enforcement — aid conditionality, the security-coordination apparatus aimed at rejectionist factions, and diplomatic insulation of the framework from alternatives — not on voluntary participant preference. Theater is elevated (0.62) and rose fastest after 2003: summits, roadmaps, and process language continue while the core transaction does not occur, a Goodhart drift from solving the problem to performing the attempt. Accessibility collapse is moderate (0.40): rival formulas (binational one-state, confederation, annexation) remain live and legally arguable, which is itself evidence this is a constructed arrangement rather than a natural limit. Resistance is high (0.68): rejectionism on both sides has repeatedly collapsed negotiation tracks. All three metric series run on one shared seven-point grid (1991–2025) so temporal analysis samples every metric at every examined time point; the trajectories are monotonic rather than cyclical, driven by accumulating non-consummation rather than oscillating crisis phases.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the mediating-powers seat the framework is a coordination achievement they authored and can exit at will; from the refugee seat the same structure is the instrument that extinguishes their claim while offering no vote on the extinguishment; from the settler seat it is a dispossession timetable their political weight can slow but not repeal; from the Palestinian Authority seat it is simultaneously the only available path to statehood and a compliance regime that drains their domestic legitimacy. The engine derives these divergent per-seat classifications from the declared roles, power levels, and exit options — the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the subsidized end: mediating powers lowest (arbitrage exit damps further), regional Arab states next (mobile exit, diffuse gains), the two state parties higher because each also pays (Israel owes withdrawal, the PA owes compliance and concedes return). Targets sit near the full-target end: the refugee diaspora highest — powerless, trapped, and identity-locked, with the constraint aimed precisely at their identity-constituting claim; settler communities nearly as high despite organized power, because their power purchases delay rather than exemption and the framework's endpoint terminates their presence beyond the line. No directionality overrides are authored: the structural derivation from declared roles and exits captures these relationships, and the organized-power class here spans genuinely opposed directionalities (settlers versus PA versus rejected factions), so an atom-level override would distort more than it corrects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two legitimate claims to one land under occupation — is live, so the framework is not mandatrophy-resolved and the flag is deliberately left unset. The classification guards both directions of mislabeling: calling this a pure rope would hide the asymmetric extraction concentrated on the refugee diaspora through the same structure that delivers recognition; calling it a snare would erase the genuine coordination achievement — mutual recognition did rearrange regional incentives, and the Arab Peace Initiative's recognition offer is real currency the framework minted. The live risk pathway is drift toward piton: theater_ratio has risen monotonically while consummation has stalled, and if the boundary term becomes physically infeasible (see the partition_physical_feasibility omega) the framework would persist as ceremonial two-state discourse maintaining nothing — the theater series is the early-warning instrument for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the territorial_legitimacy_dual kernel — the two_state_coexistence_reading. How would the classification change under the sibling readings, and what exactly do the readings disagree about?',
    'Generate the sibling stories (zionist_refuge_reading, palestinian_autochthony_reading) with their own epsilon values, victim sets, and stakeholders, and compare computed classifications across the family. The disagreement is located in the legitimacy ground: persecution-and-promise versus continuous habitation-and-return versus mutual recognition of dual 1948 legitimacy.',
    'Under the zionist_refuge_reading, the framework''s demand for territorial withdrawal reads as extraction imposed on the stronger party and epsilon falls for Israeli seats; under the palestinian_autochthony_reading, the return limitation is the central extraction and epsilon rises sharply for refugee seats. The victim set itself changes with the reading — this file''s victim set is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story instantiates one of three readings of a contested kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    refugee_binding_consent,
    'Does acceptance of the framework by negotiating institutions bind the refugee diaspora whose return claim it extinguishes?',
    'A referendum among registered refugee populations (UNRWA registration rolls provide the sampling frame), or systematic survey of camp populations in Lebanon, Syria, and Jordan on the limited-return formula.',
    'If refugees repudiate the limitation, the framework''s coordination function loses its counterparty legitimacy on the return clause specifically, the consent-based damping of effective extraction fails for that seat, and the arrangement shifts toward extraction sustained purely by enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_binding_consent, empirical, 'Whether the framework''s largest cost-bearers have actually consented through any representative channel.').

omega_variable(
    partition_physical_feasibility,
    'Does partition along the 1967 line remain physically implementable given the scale of settlement growth beyond the line since 1993?',
    'Settlement population and footprint trend analysis against contiguity requirements for a viable Palestinian state; comparison of current built area with the evacuation precedents of comparable disengagements.',
    'If infeasible, the framework''s boundary component can no longer function and the arrangement persists as ceremonial maintenance of an unusable formula — the claimed type would migrate from tangled_rope toward piton with theater_ratio carrying the diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_physical_feasibility, empirical, 'Whether the framework''s core spatial term has been overtaken by facts on the ground.').

omega_variable(
    land_swap_equivalence,
    'Do ''mutually agreed land swaps'' preserve the equity of the 1967 baseline, or do proposed swap ratios systematically favor the stronger party?',
    'Compare the land quantity, aquifer access, and agricultural quality offered in swap proposals against what sits beyond the Green Line; audit proposed ratios against the Green Line baseline.',
    'If swaps ratchet in the stronger party''s favor, the boundary component is extraction riding on a coordination label and effective extraction for the weaker party''s seats rises above the authored estimate; if roughly equivalent, the boundary component is genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(land_swap_equivalence, empirical, 'Whether the flexibility built into the boundary term functions as adjustment or as asymmetric transfer.').

omega_variable(
    security_coordination_symmetry,
    'Is the framework''s security cooperation a reciprocal arrangement between future equals, or asymmetric control in which the weaker party''s security services execute the stronger party''s priorities?',
    'Compare the operational directives, intelligence flows, and accountability structures of the joint security apparatus: who tasks whom, whose threats are prioritized, and what recourse the weaker party has when coordination conflicts with its own population''s interests.',
    'If asymmetric, the security pillar is an enforcement instrument aimed partly at the weaker party''s own constituency, raising effective suppression and extraction for Palestinian seats; if genuinely reciprocal, the pillar supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_coordination_symmetry, empirical, 'Whether the security pillar coordinates two parties or administers one by the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1991, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tld_two_state_tr_t1991, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1991, 0.18).
narrative_ontology:measurement(tld_two_state_tr_t1997, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1997, 0.26).
narrative_ontology:measurement(tld_two_state_tr_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2003, 0.41).
narrative_ontology:measurement(tld_two_state_tr_t2009, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2009, 0.49).
narrative_ontology:measurement(tld_two_state_tr_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2015, 0.54).
narrative_ontology:measurement(tld_two_state_tr_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2020, 0.59).
narrative_ontology:measurement(tld_two_state_tr_t2025, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(tld_two_state_be_t1991, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1991, 0.32).
narrative_ontology:measurement(tld_two_state_be_t1997, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(tld_two_state_be_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2003, 0.47).
narrative_ontology:measurement(tld_two_state_be_t2009, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2009, 0.51).
narrative_ontology:measurement(tld_two_state_be_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2015, 0.54).
narrative_ontology:measurement(tld_two_state_be_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(tld_two_state_be_t2025, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tld_two_state_su_t1991, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1991, 0.34).
narrative_ontology:measurement(tld_two_state_su_t1997, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1997, 0.43).
narrative_ontology:measurement(tld_two_state_su_t2003, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2003, 0.56).
narrative_ontology:measurement(tld_two_state_su_t2009, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2009, 0.61).
narrative_ontology:measurement(tld_two_state_su_t2015, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(tld_two_state_su_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(tld_two_state_su_t2025, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the territorial_legitimacy_dual kernel. The colloquial label 'the two-state solution / the legitimacy question' covers three structurally distinct claims: this file (mutual recognition of dual 1948 legitimacy, 1967-line partition, limited return, security cooperation — tangled_rope with moderate extraction concentrated on refugees and settlers); zionist_refuge_reading (legitimacy from persecution, promise, and partition acceptance — different beneficiary structure, different epsilon); palestinian_autochthony_reading (legitimacy from habitation, trauma, and return — different victim set, higher epsilon for the same territory). The upstream reading with the strongest institutional anchoring (this one, via UNSC 242 and the Oslo instruments) exerts structural pressure on the autochthony sibling by consuming the time and demographic margin its claim depends on, which is recorded as a reading_relation edge and mirrored in the sibling's network when that file is generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
