% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Obligation (Consensus-Gated Modification Reading)
 *   domain: international law / nuclear non-proliferation / treaty compliance
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel: the 2015 Joint
 *   Comprehensive Plan of Action read as a binding multilateral obligation —
 *   an instrument whose modification or dissolution requires the consensus of
 *   its parties, whose sanctions-reimposition path runs through Security
 *   Council gating, and whose alleged violations route through a sequenced
 *   multilateral dispute process before any snapback. The epsilon referent is
 *   the standing arrangement under contest — the accord as actually operated,
 *   including its post-2018 erosion after the United States' departure —
 *   assessed by this reading's own lights: a lawful, consent-based
 *   obligation, so that measured extraction reflects uncompensated
 *   third-party externalities, institutional authority rents, and the
 *   post-departure asymmetry between retained obligations and withdrawn
 *   relief, never the merits of some alternative framework. The colloquial
 *   label 'JCPOA bindingness' decomposes into three structurally distinct
 *   claims (this reading plus the transactional-provisional and
 *   graduated-compliance siblings, authored as separate files); the family is
 *   linked through network.affects_constraints, and the reading-level
 *   disagreement is carried in omega variables rather than folded into this
 *   constraint. KEY AGENTS (by structural relationship): -
 *   eu_high_representative_office: Agenda-setter (institutional/constrained)
 *   — chairs the joint commission and administers the dispute sequence -
 *   unsc_permanent_members: Gatekeeping beneficiary
 *   (institutional/constrained) — hold veto-weighted authority over
 *   reimposition - iaea_verification_apparatus: Technical beneficiary
 *   (institutional/constrained) — holds the inspection facts every dispute
 *   references - iranian_government: Primary in-agreement payer
 *   (organized/trapped) — trades program caps for relief; outright exit
 *   forfeits standing - us_federal_government: Foreclosed unilateral actor
 *   (institutional/arbitrage) — the largest single capability the arrangement
 *   withdraws; demonstrated exit in 2018 - european_signatory_states:
 *   Dual-positioned signatories (institutional/constrained) — collect
 *   diplomatic returns, spend capital preserving the text -
 *   israeli_defense_establishment: Non-party bearer of security externalities
 *   (powerful/constrained) - gulf_arab_states: Non-party bearers with
 *   dependent security guarantees (organized/constrained) -
 *   iranian_hardline_factions: Domestic losers from accommodation
 *   (organized/identity_locked) - npt_nonweapons_states: Regime-level
 *   observers (organized/analytical)
 *
 * KEY AGENTS:
 *   - eu_high_representative_office: Agenda-setter (institutional/constrained) — convenes the joint commission, sequences disputes, issues escalation findings
 *   - unsc_permanent_members: Gatekeeping beneficiary (institutional/constrained) — veto-weighted authority over reimposition; collect procedural significance from every routing
 *   - iaea_verification_apparatus: Technical beneficiary (institutional/constrained) — continuous monitoring and access-verified inspection; mandate expands and contracts with the arrangement
 *   - iranian_government: Primary in-agreement payer (organized/trapped) — caps, stockpile limits, centrifuge reductions, intrusive inspection in exchange for relief; exit priced at isolation
 *   - us_federal_government: Foreclosed unilateral actor (institutional/arbitrage) — surrendered unilateral Iran-policy flexibility; exercised arbitrage-grade exit in 2018 and absorbed the costs
 *   - european_signatory_states: Dual-positioned signatories (institutional/constrained) — trade and diplomatic returns during implementation; workaround construction and exposed firms after the departure
 *   - israeli_defense_establishment: Non-party bearer of security externalities (powerful/constrained) — preserved rival infrastructure, lapse timetables, relieved resources funding a hostile regional network
 *   - gulf_arab_states: Non-party bearers (organized/constrained) — same externalities with dependent security guarantees and no seat in the process
 *   - iranian_hardline_factions: Domestic payers (organized/identity_locked) — sanctioned-era rent streams dissolved by the opening; ideological rejection of accommodation
 *   - npt_nonweapons_states: Regime-level observers (organized/analytical) — register the precedent value of the arrangement's fate in review conferences and voting blocs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.47).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.36).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Obligation (Consensus-Gated Modification Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international law / nuclear non-proliferation / treaty compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'b0a31a80-4dcd-4e13-ab84-5e7b292564ea').
narrative_ontology:cs_kernel_codification('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', formalized).
narrative_ontology:cs_authority_grounding('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', lineage).
narrative_ontology:cs_interpretation_layer_present('b0a31a80-4dcd-4e13-ab84-5e7b292564ea').
narrative_ontology:cs_reading_relation('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', foundational, consensus_exclusive_modification_authority).
narrative_ontology:cs_axiom_status(consensus_exclusive_modification_authority, holdable).
narrative_ontology:cs_axiom_grounding('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', consensus_exclusive_modification_authority, conventional).
narrative_ontology:cs_axiom('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', secondary, unilateral_withdrawal_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', unilateral_withdrawal_constitutes_breach, conventional).
narrative_ontology:cs_reference_frame('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', consensus_bound_multilateral_obligation).
narrative_ontology:cs_drift_state('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', post_unilateral_withdrawal_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b0a31a80-4dcd-4e13-ab84-5e7b292564ea', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatory_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, israeli_defense_establishment).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, gulf_arab_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_hardline_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatory_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_government).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_treaty_bindingness_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_stability).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, consensus_based_dispute_resolution_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chairs the joint commission that oversees implementation and runs the sequenced dispute process when a party alleges non-performance. Convenes the parties, schedules reviews, and issues the findings that determine whether an issue escalates toward reimposition. The office exists inside the process it administers and has no way to step outside it without ending its coordinating role.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_high_representative_office, agenda_setter,
    institutional, generational, constrained, global).

% Hold veto-weighted authority over any council action touching the arrangement, including restoration of the multilateral sanctions that were lifted. Every dispute that routes through New York adds to their procedural significance. They are bound by their own endorsement of the text: dissenting members can block escalation but cannot rewrite the instrument.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, agenda_setter).

% Conducts continuous monitoring and access-verified inspection inside Iran, publishes quarterly assessments, and holds the technical facts that every dispute references. Its mandate, staffing, and budget expanded with the arrangement and contract if the arrangement lapses.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus, agenda_setter).

% Accepted enrichment caps, stockpile limits, centrifuge reductions, and intrusive inspection in exchange for sanctions relief and reintegration into banking and oil markets. After the principal counterparty's 2018 departure it maintained nominal adherence for a period while the relief evaporated, then incrementally exceeded limits. Leaving outright would forfeit remaining international standing and invite collective penalties; staying imposes costs without full return.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_government, beneficiary).

% Signed as the E3/EU, gained trade and diplomatic weight during implementation, then spent years building workarounds — special-purpose vehicles, barter channels — to preserve the arrangement after allied secondary sanctions cut their firms out. Preserving it consumes diplomatic capital and exposes their companies to penalties; abandoning it forfeits the normative position they publicly staked.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatory_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, european_signatory_states, payer).

% Import-dependent merchants, reconnected banks, oil buyers, and shipping insurers whose transactions resumed when relief flowed. They absorbed the reversal after 2018 as renewed isolation and frozen assets, with no channel to influence either the arrangement's terms or its erosion.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy, beneficiary,
    moderate, biographical, constrained, national).

% As an originating party it accepted restraint on unilateral Iran policy in exchange for verified caps on a rival's program. Domestic coalitions opposed the restraint from the start; in 2018 the executive withdrew and rebuilt an independent pressure campaign, absorbing allied friction and lost inspection access as costs it chose to pay. Its freedom to act alone is the largest single capability this arrangement withdraws from any party.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_government, payer,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, us_federal_government, beneficiary).

% Never a participant in the negotiation, yet bears the security externalities: the arrangement preserved and eventually legitimizes components of a rival's nuclear infrastructure, schedules several restrictions to lapse on published timetables, and returns resources to a state whose regional network it opposes. It campaigned against the arrangement through allied capitals and intelligence disclosures and retains unilateral military options it regards as last resorts.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, israeli_defense_establishment, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, israeli_defense_establishment, excluded).

% Regional monarchies facing the same externalities with less independent military capacity. They objected through Washington and coordinated public messaging but had no seat in the process that allocated the costs they carry; their security guarantee depends on the departing party rather than on the arrangement itself.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, gulf_arab_states, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, gulf_arab_states, excluded).

% Domestic constituencies whose political economy rested on sanctioned-era rent streams and whose ideology rejects accommodation with the sanctioning coalition. The opening of trade channels empowered rival domestic factions and shrank their revenue base; they pressed maximalist positions in negotiations and treated the arrangement's erosion as vindication.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_hardline_factions, payer,
    organized, biographical, identity_locked, national).

% The broader non-proliferation treaty membership watches whether a coalition of states can bind a threshold program and whether great powers keep their own bargains. Review conferences and voting blocs register the precedent value of the arrangement's fate; they hold no operational role in it.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_nonweapons_states, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a sanctions-war-versus-breakout standoff into a verified exchange: the coalition pools verification in one inspectorate and gates reimposition behind a collective decision procedure, so neither side's defection can be disguised as routine policy; the sequenced dispute path substitutes investigation-before-trigger for accusation-then-sanction.
% TRANSFER_FUNCTION: Moves sanctions relief, asset unfreezes, and market reaccess from the coalition economies to Iran; moves enrichment ceilings, stockpile limits, centrifuge reductions, and inspection access from Iran to the coalition; moves procedural authority over reimposition to the Security Council gate and the joint commission's sequencing.
% ABSENT_VOICES: The states bearing the largest security externalities — the Israeli defense establishment and the Gulf Arab monarchies — were deliberately kept outside the negotiating room and could object only from the press conference; populations living under the regional proxy networks financed by relieved resources had no representation at all; inside Iran, the constituencies whose rents the opening dissolved were heard only as domestic opposition, never as parties.
% DISAPPEARANCE_RATIONALE: Verification access, the dispute sequencing, and the reimposition gate are load-bearing: without them the inspectorate loses its baseline, the coalition loses its common decision procedure and fragments into national Iran policies, Tehran's incremental program advances lose their last multilateral reference point, and every party's strategy visibly reorganizes — as it already did in miniature after 2018.
% FOUNDING_PROBLEM: Verifiably extend the timeline of an advancing uranium and plutonium program away from weapons capability by ten to fifteen years while unwinding a sanctions war that was simultaneously driving the target toward breakout and the coalition toward another regional war.
% FOUNDING_PROBLEM_CORROBORATION: For the problem's original existence, corroboration exists outside the benefiting parties: the inspectorate's pre-deal baseline reporting and the withdrawing party's own intelligence assessments (published 2017-2018) independently attested the breakout timeline the bargain addressed. For its present liveness no neutral attestation exists — the inspectorate attests physical facts but not the bargain's vitality, and every other attestor sits inside one of the disputing camps; that absence is itself the signal.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim — tangled_rope — records my structural read: a genuine coordination core (verified caps exchanged for relief, pooled verification replacing bilateral suspicion) operating with asymmetric costs (non-consenting regional bearers, foreclosed unilateral capability, institutional rents) and sustained by active enforcement machinery. The metrics describe operation. Extractiveness 0.47 reflects the post-departure asymmetry in which nominal obligations persisted while relief withdrew. Suppression 0.55 reflects real coercive machinery — gated reimposition, the secondary-sanctions legacy, sustained diplomatic coercion — holding a partially willing arrangement together; suppression is authored as a raw structural property and is not scaled by power or scope anywhere in the engine's arithmetic, whereas extractiveness is. Theater 0.46 traces preservation becoming performance: special-purpose vehicles processing single transactions, ministerial communiques substituting for operative measures. Accessibility collapse is low (0.36) because exit demonstrably remained available — one founder exercised it — and resistance is high (0.62) because the arrangement drew organized opposition from inside every major capital. The three temporal series share one grid (t=0,2,3,5,7,9,12); the trajectory is drift with a single 2018 inflection, not a cycle, so no intermittent-reinforcement reading applies. Values at t=12 are projections and are marked as such. Receipt is diffuse by affirmative check: the inspectorate's mandate growth and the gatekeepers' procedural relevance are real but partial accruals, and no seat captures the bulk — the proceeds dissipate into maintaining the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the engine owns that computation. From the verification apparatus and gatekeeping seats the arrangement is a functioning legal order worth defending — low experienced extraction. From the non-consenting regional seats the same structure is an externality allocation they never agreed to — high experienced extraction with no exit from geography. From Tehran's seat the bargain inverted in 2018: obligations retained while compensation withdrew, with exit priced at total isolation, which amplifies effective extraction. The American seat is the sharpest divergence: formally a payer whose largest unilateral capability was withdrawn, yet holding arbitrage-grade exit — it left, absorbed the allied friction, and rebuilt an independent pressure campaign — which damps its effective extraction toward the middle. Iranian hardline factions are identity-locked rather than merely constrained: a revolutionary self-conception in which accommodation is capitulation, so their opposition persists independent of payoff calculation and would survive any renegotiated term sheet short of full capitulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (verification apparatus, signatory states, civilian economy, gatekeeping members) derive low directionality; victim declarations (Israeli defense establishment, Gulf Arab states, hardline factions) derive high directionality, amplified by constrained or locked exit. Iran sits payer-first with a relief secondary role and trapped exit, placing it near the target end. The United States is the deliberate tension case: payer role, but arbitrage exit and large secondary benefits — verified caps on a rival program — pull its derived directionality toward symmetry. No directionality overrides are authored: the override surface keys on power atoms, and this story's institutional seats diverge internally (the American government and the inspection agency share an atom but sit at opposite structural ends), so per-atom corrections would corrupt the finer-grained values the role-and-exit derivation already produces. Global spatial scope modestly amplifies effective extraction for targets, since verification difficulty scales with scope; the engine owns that modifier.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifiably delaying a threshold program while unwinding a sanctions war — is contested rather than dead: proponents cite continuing verification value, critics say the bargain died in 2018. Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer registers no dead-mandate flag. The drift data nonetheless show the classic aging signature: theater rising monotonically toward the substitution threshold, gains diffusing across seats with no capturer, and fixing priced as prohibitive — under this reading's own consensus axiom, no single dissatisfied party can repair the arrangement, and assembling consensus among opposed gatekeepers costs more than any seat's benefit. That combination (diffuse receipt, prohibitive fixing) marks the piton cell as the trajectory's terminus if revival fails. The classification prevents mislabeling in both directions: a rope reading would erase the non-consenting bearers; a snare reading would erase the real coordination core. The temporal record documents the drift toward inertial maintenance without asserting that it has arrived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_multilateral_reading_commitment,
    'This story instantiates the binding_multilateral_reading of kernel jcpoa_treaty_bindingness. What would the sibling readings (transactional_provisional_reading, graduated_compliance_reading) change structurally, and where exactly is the disagreement located?',
    'Compile and compare the sibling stories: the disagreement locates in dissolution and modification authority — consensus-body exclusivity (this reading) versus unilateral bad-faith voidability (transactional) versus proportional compliance assessment (graduated). Read each sibling''s victim set and epsilon against this file''s.',
    'Sibling readings relocate both the victim set and the extraction profile: transactional voidability concentrates exposure on the weaker party''s reliance interests; graduated assessment redistributes enforcement burdens toward whichever party is measured non-compliant; this reading fixes victims as non-consenting regional bearers plus foreclosed unilateral actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_multilateral_reading_commitment, conceptual, 'Committer-frame omega: one reading of the JCPOA-bindingness kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    snapback_post_withdrawal_operability,
    'Does the consensus-gated reimposition path remain legally operable after a founding participant''s unilateral departure and the subsequent activation attempts by the remaining signatories?',
    'Security Council procedural outcomes and member-state legal positions during the reimposition-activation episode: whether the gate processed the trigger, whether objections blocked it, and which legal theories the contending missions advanced.',
    'If inoperable, this reading''s enforcement core fails and the arrangement collapses into declaratory status; if operable, the consensus gate reasserts itself and the binding reading''s machinery survives its founder''s exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_post_withdrawal_operability, empirical, 'Whether the reimposition gate survived the founding participant''s withdrawal.').

omega_variable(
    sunset_timetable_extraction_deferral,
    'Do the arrangement''s built-in lapse timetables convert long-run coordination into deferred imposition on future parties, who inherit a threshold-state baseline without the compensating controls?',
    'Compare proliferation-relevant indicators in the post-lapse decade against the capped-period baseline, and audit which parties at t0 priced the lapse dates into their commitments.',
    'Confirmation raises effective extraction for later-period parties and pushes the back-end arrangement toward extraction-dominant classification; disconfirmation supports the steady-coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_timetable_extraction_deferral, empirical, 'Whether the deal''s sunset architecture defers extraction onto future parties.').

omega_variable(
    nonparty_externality_compensability,
    'Can the security costs borne by the non-participating regional states be compensated within this framework at all?',
    'Search the arrangement''s instruments and successor diplomacy for any compensation or consultation channel ever offered to non-parties; absence of any channel across the full interval is the finding.',
    'Absence keeps those seats structurally locked as bearers and sustains the hybrid classification; a real compensation channel would move the arrangement toward pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nonparty_externality_compensability, empirical, 'Whether non-consenting regional bearers have any compensability path.').

omega_variable(
    consensus_gate_as_target_shield,
    'Does consensus-gated reimposition function as protection for the constrained party rather than as enforcement against it?',
    'Observe which gatekeeping members blocked or diluted escalation paths during dispute episodes, and what concessions moved them; trace whether alleged violations ever completed the sequenced path to reimposition.',
    'Confirmation flips the gatekeeping seats'' directionality from beneficiary toward extractor and raises measured extraction; disconfirmation supports the gate as genuine collective enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_gate_as_target_shield, empirical, 'Whether the consensus gate shields the constrained party from enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_bindmulti_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t0, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t2, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 3, 0.24).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t3, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t5, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 7, 0.41).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t7, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t9, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 9, 0.46).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t9, observed).
narrative_ontology:measurement(jcpoa_bindmulti_tr_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 12, 0.49).
narrative_ontology:measurement_basis(jcpoa_bindmulti_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(jcpoa_bindmulti_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t0, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.31).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t2, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 3, 0.39).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t3, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t5, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 7, 0.43).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t7, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t9, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t9, observed).
narrative_ontology:measurement(jcpoa_bindmulti_be_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(jcpoa_bindmulti_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_bindmulti_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t0, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2, 0.44).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t2, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t3, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t5, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 7, 0.6).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t7, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t9, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 9, 0.57).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t9, observed).
narrative_ontology:measurement(jcpoa_bindmulti_su_t12, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(jcpoa_bindmulti_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_safeguards_verification_regime).

% DUAL FORMULATION NOTE:
% 'JCPOA bindingness' is a colloquial label covering three structurally distinct claims with different epsilon values and victim sets: this binding-multilateral reading (consensus-exclusive modification authority; victims include non-consenting regional bearers), the transactional-provisional reading (unilateral voidability; extraction concentrates on the weaker party's reliance interests), and the graduated-compliance reading (proportional enforcement; burdens redistribute toward the party measured non-compliant). The upstream npt_safeguards_verification_regime supplies the inspection substrate this reading cites as evidence that bindingness is workable. Each family member links the others through affects_constraints per the decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
