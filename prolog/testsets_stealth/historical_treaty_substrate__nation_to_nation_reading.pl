% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historic Treaty Substrate — Nation-to-Nation Reading (Ongoing Consent between Sovereign Equals)
 *   domain: legal/indigenous/constitutional
 *
 * SUMMARY:
 *   From the nation-to-nation reading, the standing arrangement under contest
 *   is this: the settler state administers the historic treaty territories
 *   unilaterally — authorizing logging, mining, hydro, and pipeline
 *   development, collecting the fiscal returns, and defining through its own
 *   courts what the founding agreements require — while the signatory nations
 *   hold that the agreements are continuing compacts between sovereign equals
 *   whose terms require ongoing consent to territorial change. The
 *   arrangement retains a real coordination function (peace, defined land
 *   use, a standing relationship, some benefit flows the nations affirm) and
 *   simultaneously operates with sharp asymmetry: the decisions, the
 *   revenues, and the interpretive authority sit with the state and its
 *   licensees, while the nations bear the siting of development and receive a
 *   fraction of the value moved. The claim/metric gap is deliberate:
 *   claimed_type states my structural belief about the standing arrangement
 *   (a genuine coordination core carrying asymmetric, actively enforced
 *   extraction); the metrics describe its operation as this reading assesses
 *   it. The engine computes each seat's classification from the structural
 *   data. KEY AGENTS (by structural relationship): -
 *   settler_state_governments: agenda-setter and principal beneficiary
 *   (institutional/arbitrage) — administers, authorizes, collects, and
 *   reinterprets - domestic_courts: interpretive agenda-setter
 *   (institutional/constrained) — decide which reading of the founding texts
 *   governs - resource_extraction_industries: beneficiary
 *   (powerful/arbitrage) — hold state-issued rights, book the resource value
 *   - indigenous_treaty_nations: principal payer (organized/identity_locked)
 *   — bear the siting and the loss, hold the continuing-compact understanding
 *   - settler_municipalities_and_landholders: incidental beneficiary
 *   (moderate/constrained) — hold titles the arrangement underwrites -
 *   unceded_territory_nations: excluded (organized/identity_locked) — outside
 *   both the benefit flows and the implementation tables -
 *   international_treaty_bodies: analytical observer
 *   (institutional/analytical) — review against instruments the state has
 *   adopted FAMILY NOTE: this story is one reading of the
 *   historical_treaty_substrate kernel. The extinguishment_reading authors ε
 *   over the same referent near zero (a completed transaction leaves no
 *   ongoing extraction to measure); the stewardship_reading authors ε high
 *   but with a different structure (breach of mutual obligations binding both
 *   parties). This reading authors ε high: the standing arrangement moves
 *   value and decision authority without consent. Same referent,
 *   reading-indexed values (OQ-26) — the stories are linked as one constraint
 *   family via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.78).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.7).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historic Treaty Substrate — Nation-to-Nation Reading (Ongoing Consent between Sovereign Equals)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/indigenous/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '024a0caf-fcc6-4747-911a-db4b902e25a4').
narrative_ontology:cs_kernel_codification('024a0caf-fcc6-4747-911a-db4b902e25a4', fixed_text).
narrative_ontology:cs_authority_grounding('024a0caf-fcc6-4747-911a-db4b902e25a4', lineage).
narrative_ontology:cs_interpretation_layer_present('024a0caf-fcc6-4747-911a-db4b902e25a4').
narrative_ontology:cs_reading_relation('024a0caf-fcc6-4747-911a-db4b902e25a4', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('024a0caf-fcc6-4747-911a-db4b902e25a4', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('024a0caf-fcc6-4747-911a-db4b902e25a4', foundational, inherent_sovereignty_persists).
narrative_ontology:cs_axiom_status(inherent_sovereignty_persists, holdable).
narrative_ontology:cs_axiom_grounding('024a0caf-fcc6-4747-911a-db4b902e25a4', inherent_sovereignty_persists, deontological).
narrative_ontology:cs_axiom('024a0caf-fcc6-4747-911a-db4b902e25a4', foundational, ongoing_consent_required_for_territorial_change).
narrative_ontology:cs_axiom_status(ongoing_consent_required_for_territorial_change, holdable).
narrative_ontology:cs_axiom_grounding('024a0caf-fcc6-4747-911a-db4b902e25a4', ongoing_consent_required_for_territorial_change, conventional).
narrative_ontology:cs_reference_frame('024a0caf-fcc6-4747-911a-db4b902e25a4', sovereign_equals_covenant_continuity).
narrative_ontology:cs_drift_state('024a0caf-fcc6-4747-911a-db4b902e25a4', post_s35_undrip_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('024a0caf-fcc6-4747-911a-db4b902e25a4', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_municipalities_and_landholders).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the terms on which treaty territories are used: issues resource permits, collects royalties and taxes, legislates over reserve and treaty lands, and controls the commissions and tables through which treaty implementation proceeds. It interprets its own obligations through the honor-of-the-Crown doctrine while retaining unilateral authority to authorize territorial change. Its exit from the relationship is not departure but reinterpretation — it can redefine what the agreements require through legislation, litigation strategy, or new process frameworks, and it collects the fiscal and jurisdictional returns of the arrangement it administers.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments, beneficiary).

% Decides what the founding agreements mean: whether the obligations continue, what consultation or consent requires, and whether state authorizations on treaty territories are lawful. Its interpretive doctrine — the honor of the Crown, the weight given to oral histories against written texts — determines which understanding of the founding bargain governs in practice. It cannot decline the interpretive role or leave it, and it operates within precedent and a constitutional structure the legislature can amend.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, agenda_setter,
    institutional, generational, constrained, national).

% Obtains state-issued rights to log, mine, dam, and pipe across treaty territories and books the resource value those rights yield. Capital and projects move between jurisdictions; firms can shift to basins where obligations are lighter or enforcement slower. They bear few of the arrangement's ongoing costs, which fall on the territories and on the nations whose agreements cover them.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% Signed the founding agreements as polities and hold them as the standing terms of coexistence on their territories. They carry the siting of development, the loss of land and water, and subordination to a legal order they did not author, while receiving annuities, reserve parcels, and defined services far narrower than what their oral records describe as promised. The relationship is constitutive: the nations understand themselves, their territories, and their governance orders through these agreements, and they cannot abandon the relationship or the territory without ceasing to be the polities they are. Their avenues are litigation, state-convened negotiation tables, land defense, and international bodies.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, payer,
    organized, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_treaty_nations, beneficiary).

% Live, farm, and build on lands the founding agreements cover, under titles and municipal charters the state issued. They receive the property value, services, and infrastructure the arrangement makes possible. Their holdings are fixed to particular territory; selling relocates the household but not the underlying claim structure, and most play no direct role in the treaty relationship beyond receiving its benefits.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_municipalities_and_landholders, beneficiary,
    moderate, biographical, constrained, regional).

% Hold territories where no founding agreement was ever signed, and so stand outside the arrangement's benefit flows and its implementation machinery alike: no treaty protections, no seat at the implementation tables, while state-authorized development proceeds across their lands. They press their claims through title litigation, direct action, and international advocacy — from outside the bilateral structures the arrangement maintains.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, unceded_territory_nations, excluded,
    organized, generational, identity_locked, continental).

% Review the state's conduct against the international instruments it has adopted — treaty-body reviews, human-rights mechanisms, and the UN declaration on Indigenous rights — and issue findings and recommendations. They hold no enforcement power inside the state's legal order; their leverage is reputational, procedural, and precedential for other forums.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The founding agreements coordinate the coexistence of two sets of polities on shared territory: they fix peace and alliance, define which lands each party uses, and establish a standing relationship — councils, annuities, shared resource terms — through which the parties continue to deal with one another.
% TRANSFER_FUNCTION: Moves land, timber, minerals, water, and jurisdictional authority from Indigenous nations to settler state governments and to the industries the state licenses; moves back annuity payments, reserve parcels, and defined services (health, education, hunting-rights clauses) to the nations.
% ABSENT_VOICES: The nations whose territories were never covered by any founding agreement are outside the arrangement entirely — they would object that its protections and its implementation tables were built for treaty nations while development proceeds unregulated on their lands. Indigenous women's governance authorities, excluded from the historic negotiations, remain marginal in implementation. Both are present as litigants, intervenors, and land defenders — outside the bilateral tables where the terms are actually set.
% DISAPPEARANCE_RATIONALE: If the arrangement and its interpretive machinery vanished overnight, the state's authorization regime for treaty territories, the property titles and municipal charters built on it, and the resource economy operating under it would all lose their legal foundation, and the nations' governance orders and claim structures would move to the center of territorial decision-making. The world rearranges because the standing arrangement is the load-bearing structure of the settler state's territorial authority — not a vestige.
% FOUNDING_PROBLEM: Two peoples occupying the same territory without a shared state needed terms that would prevent war and make settlement and coexistence orderly: the Crown needed alliance, trade, and land access; the nations needed peace, protection of their ways of life, and the means to persist as polities.
% FOUNDING_PROBLEM_CORROBORATION: The nations' oral histories attest the continuing-compact understanding, materially corroborated by wampum belts, treaty medals, and contemporaneous commission journals; the Royal Commission on Aboriginal Peoples, UN treaty-body reviews, and independent historiography of the negotiation records corroborate from outside the beneficiary set. No source outside the beneficiary set attests that the founding problem is closed — the closure claim rests on the state's own jurisprudence and legislation, which is the arrangement speaking about itself.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Authored from the nation-to-nation seat. Extractiveness is high (0.78 at interval end) because the value of treaty territories — timber, minerals, water, power — is authorized and priced by the state without the consent this reading holds the agreements to require, and the nations receive annuities and defined services that are a small fraction of the value moved. Suppression (0.70) is structural, not internalized: the state controls the forums in which its obligations are adjudicated, polices land defense through injunctions and enforcement operations, and can legislate over the nations' objections; the nations' resistance is high and organized, which is itself evidence the suppression is externally maintained. Theater (0.65) is the interval's sharpest drift: the duty-to-consult jurisprudence and the reconciliation-era process architecture built a procedural layer that performs the consent function the agreements require without transferring any decision authority — a proxy replacing the function it stands in for, visible in the theater series rising 0.20 → 0.65 while decision authority did not move. Accessibility collapse is moderate (0.45): alternatives exist — domestic litigation has produced real wins, international mechanisms are open, FPIC statutes create new levers — but each runs through machinery the state staffs, funds, and keeps the records of. Resistance is high (0.72): litigation waves, blockades and land defense, international advocacy, and statutory campaigns. All three series run on one shared grid (1982, 1990, 1998, 2005, 2012, 2019, 2024). The suppression series is authored because the story specifically tracks enforcement-capacity change: the build-up of injunction and policing machinery through 2019 (Wet'suwet'en enforcement, injunctions against rail and road blockades), easing slightly by 2024 as FPIC legislation channels part of the conflict into process. The arc is monotonic rather than cyclical: each procedural layer added to manage conflict has also licensed the next round of authorization.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state's seat the arrangement is a functioning honor-of-the-Crown relationship it administers in good faith under its own precedents — coordination-flavored, with the consultation record as evidence of diligence. From the nations' seat the same structure is a continuing compact being breached: decisions taken without consent, value moved without agreement, and a consultation layer that records their objection without weighing it. From the industries' seat it is a stable permit regime — low friction, predictable cost. From the municipalities' seat it is a background condition of property and services. The engine computes these divergences from the structural data (power, exit, role); the divergence between the state seat and the nations seat is the kernel contest itself, seated.
 *
 * DIRECTIONALITY LOGIC:
 *   The nations are the declared victims and sit near the full-target end of d: they bear the siting, the loss, and the jurisdictional subordination, and their identity-locked exit (the relationship and the territory are constitutive — they cannot leave without ceasing to be the polities they are) removes the damping that mobile exit would provide. The state sits near the beneficiary end: agenda-setter and collector, with arbitrage-grade exit — it can redefine the obligation by statute, litigation strategy, or new process architecture, and it controls the verification record across a national scope, which the engine's scope amplification treats as structurally apt (the verifier sits inside the verified). The industries sit near the beneficiary end with arbitrage exit: mobile capital, relocatable projects. The municipalities are beneficiaries with constrained exit — fixed holdings keep some cost exposure on them, so they sit less far toward the beneficiary end. The courts are administered into the arrangement's interpretive layer: near-symmetric by direct flows, but their constrained exit and constitutive role place them on the arrangement's maintenance side. The international bodies hold the analytical seat: no flows, minimal d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two sets of polities sharing one territory and needing terms of coexistence — is live, and the parties dispute whether the founding terms remain open. That dispute is why this is not a snare from this seat: the nations' demand is performance of the agreements, not exit from them, and a snare reading would mispredict the remedy the victims themselves seek. It is not a rope either: the extraction is asymmetric, enforced, and rising, and the beneficiaries hold the interpretive authority. Tangled rope is the honest structural claim — a genuine coordination core (peace, defined coexistence, benefit flows the nations affirm) carrying asymmetric, actively enforced extraction. The mismatch consumer reads founding_problem_status=contested × disappearance_verdict=world_rearranges: no zombie flag — the arrangement is load-bearing, not vestigial. The mandatrophy-adjacent signal is the theater trajectory: the consultation layer is drifting toward performing the consent function while the function itself goes unperformed. Mandatrophy is not resolved; the mandate is contested, not outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the historical_treaty_substrate kernel: if the extinguishment_reading or the stewardship_reading governed instead, which structural elements of this story change?',
    'Track which reading the state''s courts and legislatures adopt — the interpretive trajectory of treaty jurisprudence and implementing statutes. The readings are held by different parties and cannot be averaged; each is a separate constraint.',
    'Under the extinguishment_reading the victim set empties (a completed transaction leaves no ongoing extraction to measure, and ε collapses toward zero); under the stewardship_reading the beneficiary set expands to co-steward nations and the breach runs in both directions. This story''s beneficiary/victim structure and high ε hold only under the nation-to-nation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the historic-treaty kernel governs determines the entire beneficiary/victim structure of this story.').

omega_variable(
    oral_history_evidentiary_weight,
    'Can the nations'' oral records of the founding agreements — with the material record (wampum, medals, commission journals) — be given equal interpretive weight with the written texts, so that the ongoing-consent premise is grounded in the agreements themselves?',
    'Evidentiary rulings and interpretive doctrine: whether courts admit and weigh oral histories as the parties'' shared understanding (the Delgamuukw line) or confine interpretation to the written text.',
    'Equal weight grounds the consent requirement in the texts and strengthens the reading''s enforcement; text-only interpretation leaves the consent premise resting on extrinsic moral claim and weakens it, pushing the standing arrangement''s classification toward the extinguishment frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_history_evidentiary_weight, empirical, 'Whether the evidentiary treatment of oral treaty records sustains the ongoing-consent premise.').

omega_variable(
    fpic_binding_or_consultative,
    'Does implementing the UN declaration''s free, prior, and informed consent standard produce a binding requirement over territorial authorization, or a strengthened consultation duty?',
    'Implementation record: whether any state-authorized project on treaty territory has been halted or restructured for want of nation consent under the new statutes.',
    'A binding standard moves the standing arrangement toward this reading''s frame and lowers measured extraction and theater; a consultative-only standard deepens the consultation-theater pattern already visible in the theater_ratio series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fpic_binding_or_consultative, empirical, 'Whether FPIC legislation binds territorial authorization or repackages consultation.').

omega_variable(
    sovereignty_pluralism_compatibility,
    'Can the settler state''s constitutional order recognize co-equal continuing sovereignty at all, or does the nation-to-nation reading route enforcement through international fora by structural necessity?',
    'Constitutional doctrine: whether domestic courts can give effect to continuing sovereign equality without collapsing it into delegated domestic authority; observe where enforcement actually lands over the next implementation cycle.',
    'If domestic recognition is structurally unavailable, the reading''s enforcement migrates to international mechanisms — changing the nations'' exit structure (from identity-locked domestic litigation toward international leverage) and the state''s exposure, and shifting which seats the engine computes as constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_pluralism_compatibility, conceptual, 'Whether co-equal sovereignty is recognizable within the settler constitutional order or only internationally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1982, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(hist_tr_t1990, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(hist_tr_t1998, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1998, 0.32).
narrative_ontology:measurement(hist_tr_t2005, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(hist_tr_t2012, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2012, 0.55).
narrative_ontology:measurement(hist_tr_t2019, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2019, 0.62).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(hist_be_t1982, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(hist_be_t1990, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(hist_be_t1998, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1998, 0.62).
narrative_ontology:measurement(hist_be_t2005, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(hist_be_t2012, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2012, 0.7).
narrative_ontology:measurement(hist_be_t2019, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2019, 0.75).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1982, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(hist_su_t1990, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1990, 0.52).
narrative_ontology:measurement(hist_su_t1998, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1998, 0.55).
narrative_ontology:measurement(hist_su_t2005, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(hist_su_t2012, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2012, 0.63).
narrative_ontology:measurement(hist_su_t2019, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'historic treaties' conflates three structurally distinct claims about the same texts: a completed property transaction (extinguishment_reading), a continuing compact between sovereign equals requiring ongoing consent (this story), and a relational co-stewardship pact binding both parties (stewardship_reading). Each is authored as a separate constraint with its own ε over the same referent — the standing arrangement of state administration of treaty territories — per the ε-invariance principle. The readings are linked here as one constraint family; this reading's high ε is reading-indexed, not a measurement of the texts themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
